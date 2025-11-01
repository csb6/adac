------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                B I N D E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.8 $                              --
--                                                                          --
--             Copyright (c) 1992,1993, NYU, All Rights Reserved            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms  of the GNU  General  Public  License  as  published  by the  Free --
-- Software  Foundation;  either version 2,  or (at your option)  any later --
-- version.  GNAT is distributed  in the hope  that it will be useful,  but --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANT- --
-- ABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public --
-- License  for  more details.  You should have received  a copy of the GNU --
-- General Public License along with GNAT;  see file COPYING. If not, write --
-- to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. --
--                                                                          --
------------------------------------------------------------------------------

with Binderr; use Binderr;
with Butil;   use Butil;
with Debug;   use Debug;
with Namet;   use Namet;
with Opt;     use Opt;
with Output;  use Output;

package body Binde is

   --  The following data structures are used to represent the graph that is
   --  used to determine the elaboration order (using a topological sort).

   --  The following structures are used to record successors

   type Successor_Id is new Nat;
   --  Identification of single successor entry

   No_Succ : constant Successor_Id := 0;
   --  Used to indicate end of list of successors

   type Succ_Reason is (Withed, Elab, Elab_All, Spec_First);
   --  Reason for existence of successor link

   type Successor_Link is record
      Before : Unit_Id;
      --  Predecessor unit

      After : Unit_Id;
      --  Successor unit

      Next : Successor_Id;
      --  Next successor on this list

      Reason : Succ_Reason;
      --  Reason for this link
   end record;

   package Succ is new Table (
      Component_Type => Successor_Link,
      Index_Type     => Successor_Id,
      Low_Bound      => 1,
      Initial        => 500,
      Increment      => 200,
      Table_Name     => "Succ");

   --  A Unit_Node record is built for each active unit

   type Unit_Node_Record is record

      Successors : Successor_Id;
      --  Pointer to list of links for successor nodes

      Num_Pred : Nat;
      --  Number of predecessors for this unit

      Nextnp : Unit_Id;
      --  Forward pointer for list of units with no predecessors

      Elab_Order : Nat;
      --  Position in elaboration order (zero = not placed yet)

      Visited : Boolean;
      --  Used in computing transitive closure for elaborate all and
      --  also in locating cycles in Diagnose_Circularity.

      Done : Boolean;
      --  Flag set when unit is chosen and placed in elaboration order

   end record;

   package UNR is new Table (
      Component_Type => Unit_Node_Record,
      Index_Type     => Unit_Id,
      Low_Bound      => First_Unit_Entry,
      Initial        => 500,
      Increment      => 200,
      Table_Name     => "UNR");

   No_Pred : Unit_Id;
   --  Head of list of items with no predecessors

   Num_Left : Int;
   --  Number of entries not yet dealt with

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Better_Choice (U1, U2 : Unit_Id) return Boolean;
   --  U1 and U2 are both candidates for selection as the next unit to be
   --  elaborated. Both have number of predecessors set to zero, and neither
   --  is an instance of a spec with pragma Elaborate_Body set where the
   --  body cannot also be immediately placed. This function determines
   --  whether U1 is a better choice than U2 (i.e. should be elaborated
   --  first), and if so, returns True.

   procedure Build_Link (Before, After : Unit_Id; R : Succ_Reason);
   --  Establish a successor link, Before must be elaborated before After,
   --  and the reason for the link is R.

   procedure Choose (Chosen : Unit_Id);
   --  Chosen is the next entry chosen in the elaboration order. This
   --  procedure updates all data structures appropriately.

   function Corresponding_Body (U : Unit_Id) return Unit_Id;
   --  Given a unit which is a spec for which there is a separate body,
   --  return the unit id of the body. It is an error to call this routine
   --  with a unit that is not a spec, or which does not have a separate body.

   function Corresponding_Spec (U : Unit_Id) return Unit_Id;
   --  Given a unit which is a body for which there is a separate spec,
   --  return the unit id of the spec. It is an error to call this routine
   --  with a unit that is not a body, or which does not have a separate spec.

   procedure Diagnose_Circularity;
   --  Called when a circular elaboration order is found to print out
   --  appropriate diagnostic information giving details of the cycle

   procedure Elab_All_Links (Before, After : Unit_Id);
   --  Used to compute the transitive closure of elaboration links for an
   --  Elaborate_All pragma. Unit After has a pragma Elaborate_All, and
   --  unit Before must be previously elaborated. First a link is built
   --  making sure that unit Before is elaborated before After, then a
   --  recursive call ensures that we also build links for any units needed
   --  by Before (i.e. these units must also be elaborated before After).

   procedure Gather_Dependencies;
   --  Compute dependencies, building the Succ and UNR tables

   function Unit_Id_Of (Uname : Unit_Name_Type) return Unit_Id;
   --  This function uses the Info field set in the names table to obtain
   --  the unit Id of a unit, given its name id value.

   procedure Write_Dependencies;
   --  Write out dependencies (called only if appropriate option is set)

   -------------------
   -- Better_Choice --
   -------------------

   function Better_Choice (U1, U2 : Unit_Id) return Boolean is

      function Body_Unit (U : Unit_Id) return Boolean;
      --  Determines if given unit is a body

      function Waiting_Body (U : Unit_Id) return Boolean;
      --  Determines if U is a waiting body, defined as a body which has not
      --  been elaborated, but whose spec has been elaborated.

      function Body_Unit (U : Unit_Id) return Boolean is
      begin
         return Unit.Table (U).Utype = Is_Body
           or else Unit.Table (U).Utype = Is_Body_Only;
      end Body_Unit;

      function Waiting_Body (U : Unit_Id) return Boolean is
      begin
         return Unit.Table (U).Utype = Is_Body and then
            UNR.Table (Corresponding_Spec (U)).Done;
      end Waiting_Body;

   begin
      --  Anything is better than nothing

      if U2 = No_Unit_Id then
         return True;
      end if;

      --  Always prefer a waiting body to any other case

      if Waiting_Body (U1) and not Waiting_Body (U2) then
         return True;
      elsif Waiting_Body (U2) and not Waiting_Body (U1) then
         return False;
      end if;

      --  Always prefer a body to a spec

      if Body_Unit (U1) and not Body_Unit (U2) then
         return True;
      elsif Body_Unit (U2) and not Body_Unit (U1) then
         return False;
      end if;

      --  Otherwise decide on the basis of alphabetical order

      return Uname_Less (Unit.Table (U1).Uname, Unit.Table (U2).Uname);
   end Better_Choice;

   ----------------
   -- Build_Link --
   ----------------

   procedure Build_Link (Before, After : Unit_Id; R : Succ_Reason) is
   begin
      Succ.Increment_Last;
      Succ.Table (Succ.Last).Before := Before;
      Succ.Table (Succ.Last).After  := After;
      Succ.Table (Succ.Last).Next   := UNR.Table (Before).Successors;
      UNR.Table (Before).Successors := Succ.Last;
      Succ.Table (Succ.Last).Reason := R;
      UNR.Table (After).Num_Pred    := UNR.Table (After).Num_Pred + 1;
   end Build_Link;

   ------------
   -- Choose --
   ------------

   procedure Choose (Chosen : Unit_Id) is
      S : Successor_Id;
      U : Unit_Id;

   begin
      if Debug_Flag_C then
         Write_Str ("Choosing Unit ");
         Write_Unit_Name (Unit.Table (Chosen).Uname);
         Write_Eol;
      end if;

      --  Add to elaboration order unless preelaborable

      if not Unit.Table (Chosen).Preelab then
         Elab_Order.Increment_Last;
         Elab_Order.Table (Elab_Order.Last) := Chosen;
      end if;

      --  Remove from No_Pred list. This is a little inefficient and may
      --  be we should doubly link the list, but it will do for now!

      if No_Pred = Chosen then
         No_Pred := UNR.Table (Chosen).Nextnp;

      else
         U := No_Pred;

         while U /= No_Unit_Id loop
            if UNR.Table (U).Nextnp = Chosen then
               UNR.Table (U).Nextnp := UNR.Table (Chosen).Nextnp;
               exit;
            end if;

            U := UNR.Table (U).Nextnp;
         end loop;
      end if;

      --  For all successors, decrement the number of predecessors, and
      --  if it becomes zero, then add to no predecessor list.

      S := UNR.Table (Chosen).Successors;

      while S /= No_Succ loop
         U := Succ.Table (S).After;
         UNR.Table (U).Num_Pred := UNR.Table (U).Num_Pred - 1;

         if Debug_Flag_N then
            Write_String ("  decrementing Num_Pred for unit ");
            Write_Unit_Name (Unit.Table (U).Uname);
            Write_String (" new value = ");
            Write_Int (Int (UNR.Table (U).Num_Pred));
            Write_Eol;
         end if;

         if UNR.Table (U).Num_Pred = 0 then
            UNR.Table (U).Nextnp := No_Pred;
            No_Pred := U;
         end if;

         S := Succ.Table (S).Next;
      end loop;

      --  All done, adjust number of units left count and set flag for done

      Num_Left := Num_Left - 1;
      UNR.Table (Chosen).Done := True;
   end Choose;

   ------------------------
   -- Corresponding_Body --
   ------------------------

   --  Currently if the body and spec are separate, then they appear as
   --  two separate units in the same ALI file, with the body appearing
   --  first and the spec appearing second.

   function Corresponding_Body (U : Unit_Id) return Unit_Id is
   begin
      if Unit.Table (U).Utype /= Is_Spec then
         Binder_Abort;
      else
         return U - 1;
      end if;
   end Corresponding_Body;

   ------------------------
   -- Corresponding_Spec --
   ------------------------

   --  Currently if the body and spec are separate, then they appear as
   --  two separate units in the same ALI file, with the body appearing
   --  first and the spec appearing second.

   function Corresponding_Spec (U : Unit_Id) return Unit_Id is
   begin
      if Unit.Table (U).Utype /= Is_Body then
         Binder_Abort;
      else
         return U + 1;
      end if;
   end Corresponding_Spec;

   --------------------------
   -- Diagnose_Circularity --
   --------------------------

   procedure Diagnose_Circularity is
      Marked : Boolean;
      S : Successor_Id;

      Revisited_Node : Unit_Id;
      --  Node that marks the closure of the cycle

      Printing : Boolean;
      --  Used to control output during the recursive search. If set on the
      --  way up, it means that output describing the path taken is to be
      --  output. It is reset as soon as Revisited_Node is reached.

      --  The following is the recursive routine used to locate the cycle
      --  it is called once the only non-Done nodes in the list are known
      --  to be parts of cycles.

      procedure Visit_Node (U : Unit_Id) is
         S  : Successor_Id;
         US : Unit_Id;

      begin
         if UNR.Table (U).Visited then
            Revisited_Node := U;
            Printing := True;
            return;
         end if;

         UNR.Table (U).Visited := True;

         --  Look for a successor that is also part of the cycle

         S := UNR.Table (U).Successors;

         loop

            US := Succ.Table (S).After;

            if not UNR.Table (US).Done then
               Visit_Node (US);

               if Printing then
                  Error_Msg_Name_1 := Unit.Table (U).Uname;
                  Error_Msg_Name_2 := Unit.Table (US).Uname;

                  case Succ.Table (S).Reason is
                     when Withed =>
                        Error_Msg
                          ("& must be elaborated before & (with)");

                     when Elab =>
                        Error_Msg
                          ("& must be elaborated before & (Elaborate)");

                     when Elab_All =>
                        Error_Msg
                          ("& must be elaborated before & (Elaborate_All)");

                     when Spec_First =>
                        Error_Msg
                          ("& must be elaborated before & (spec first)");
                  end case;

                  if U = Revisited_Node then
                     Printing := False;
                  end if;

                  return;
               end if;
            end if;

            S := Succ.Table (S).Next;
         end loop;
      end Visit_Node;

   --  Start of processing for Diagnose_Circularity

   begin
      Error_Msg ("Elaboration circularity detected");

      --  The approach is to sweep through the unit list repeatedly,
      --  marking any nodes that have no successors that are not marked
      --  Done, as Done. When we have no more to mark, the remaining
      --  nodes must form the culprit cycle.

      loop
         Marked := False;

         for U in Unit.First .. Unit.Last loop
            if not UNR.Table (U).Done then
               S := UNR.Table (U).Successors;

               loop
                  exit when S = No_Succ;
                  exit when not UNR.Table (Succ.Table (S).After).Done;
                  S := Succ.Table (S).Next;
               end loop;

               if S = No_Succ then
                  UNR.Table (U).Done := True;
                  Marked := True;
               end if;
            end if;
         end loop;

         exit when not Marked;
      end loop;

      --  OK, remaining units in the table (there must be at least one)
      --  form one or more cycles. We will just pick out one cycle to
      --  complain about (a situation with more than one cycle is rare).

      for U in Unit.First .. Unit.Last loop
         UNR.Table (U).Visited := False;
      end loop;

      for U in Unit.First .. Unit.Last loop
         if not UNR.Table (U).Done then
            Visit_Node (U);
            exit;
         end if;
      end loop;

   end Diagnose_Circularity;

   --------------------
   -- Elab_All_Links --
   --------------------

   procedure Elab_All_Links (Before, After : Unit_Id) is
   begin
      if UNR.Table (Before).Visited then
         return;
      end if;

      --  Build the direct link for Before

      UNR.Table (Before).Visited := True;
      Build_Link (Before, After, Elab_All);

      --  Process all units With'ed by Before recursively

      for W in
        Unit.Table (Before).First_With .. Unit.Table (Before).Last_With
      loop

         --  Skip if no ALI file for this with, happens with generics now,
         --  will happen only with specialized generics like unchecked
         --  stuff when we finally fix generics???

         if Withs.Table (W).Afile /= No_File then
            Elab_All_Links (Unit_Id_Of (Withs.Table (W).Uname), After);
         end if;
      end loop;

      --  Process corresponding body, if there is one

      if Unit.Table (Before).Utype = Is_Spec then
         Elab_All_Links (Corresponding_Body (Before), After);
      end if;
   end Elab_All_Links;

   ---------------------
   -- Find_Elab_Order --
   ---------------------

   procedure Find_Elab_Order is
      U : Unit_Id;
      Best_So_Far : Unit_Id;

   begin
      Succ.Init;
      Num_Left := Int (Unit.Last - Unit.First + 1);

      --  Initialize unit table for elaboration control

      for U in Unit.First .. Unit.Last loop
         UNR.Increment_Last;
         UNR.Table (UNR.Last).Successors := No_Succ;
         UNR.Table (UNR.Last).Num_Pred := 0;
         UNR.Table (UNR.Last).Nextnp := No_Unit_Id;
         UNR.Table (UNR.Last).Elab_Order := 0;
         UNR.Table (UNR.Last).Done := False;
      end loop;

      --  Gather dependencies and outpu them if option set

      Gather_Dependencies;

      --  Output elaboration dependencies if option is set

      if Elab_Dependency_Output then
         Write_Dependencies;
      end if;

      --  Initialize the no predecessor list

      No_Pred := No_Unit_Id;

      for U in UNR.First .. UNR.Last loop
         if UNR.Table (U).Num_Pred = 0 then
            UNR.Table (U).Nextnp := No_Pred;
            No_Pred := U;
         end if;
      end loop;

      --  OK, now we determine the elaboration order proper. All we do is to
      --  select the best choice from the no predecessor list till the list
      --  is empty. Note that preelaborable units are not treated specially
      --  until the point at which they are chosen. At this point a test makes
      --  sure that they are not actually added to the preelaboration order.

      while No_Pred /= No_Unit_Id loop
         U := No_Pred;
         Best_So_Far := No_Unit_Id;

         --  Loop to choose best entry in No_Pred list

         loop
            --  We must ignore entries that have Pragma Elaborate_Body set
            --  unless both the spec and body have Num_Pred set to zero

            if not Unit.Table (U).Elaborate_Body
               or else UNR.Table (Corresponding_Body (U)).Num_Pred = 0
            then

               --  This is a candididate to be considered for choice

               if Better_Choice (U, Best_So_Far) then
                  Best_So_Far := U;
               end if;
            end if;

            U := UNR.Table (U).Nextnp;
            exit when U = No_Unit_Id;
         end loop;

         --  If no candididate chosen, it means that all the items with
         --  No_Pred set to zero are instances of specs with Elaborate_Body
         --  set where the bodies cannot be elaborated yet. This can only
         --  happen if there are other entries that cannot be elaborated
         --  because of a circular elaboration situation.

         exit when Best_So_Far = No_Unit_Id;
         Choose (Best_So_Far);

         --  If we just chose a spec with Elaborate_Body set, then we
         --  must immediately elaborate the spec, before any other units.

         if Unit.Table (Best_So_Far).Elaborate_Body then
            Choose (Corresponding_Body (Best_So_Far));
         end if;
      end loop;

      --  If we haven't elaborated all the entries, then we have some kind
      --  of circular elaboration dependency (assuming everything is working!)

      if Num_Left /= 0 then
         Diagnose_Circularity;
      end if;
   end Find_Elab_Order;

   -------------------------
   -- Gather_Dependencies --
   -------------------------

   procedure Gather_Dependencies is
      Withed_Unit : Unit_Id;

   begin
      --  Loop through all units

      for U in Unit.First .. Unit.Last loop

         --  If there is a body and a spec, then spec must be elaborated first
         --  Note that the corresponding spec immediately follows the body

         if Unit.Table (U).Utype = Is_Body then
            Build_Link (Corresponding_Spec (U), U, Spec_First);
         end if;

         --  Process WITH references for this unit ignoring generic units

         for W in Unit.Table (U).First_With .. Unit.Table (U).Last_With loop
            if Withs.Table (W).Sfile /= No_File then
               Withed_Unit :=
                 Unit_Id (Unit_Id_Of (Withs.Table (W).Uname));

               --  Pragma Elaborate case. We must build a link for the withed
               --  unit itself, and also the corresponding body if there is one

               --  However, skip this processing if there is no ALI file for
               --  the WITH entry, because this means it is a generic (even
               --  when we fix the generics so that an ALI file is present,
               --  we probably still will have no ALI file for unchecked
               --  and other special cases).

               if Withs.Table (W).Elaborate
                 and then Withs.Table (W).Afile /= No_File
               then
                  Build_Link (Withed_Unit, U, Withed);

                  if Unit.Table (Withed_Unit).Utype = Is_Spec then
                     Build_Link (Corresponding_Body (Withed_Unit), U, Elab);
                  end if;

               --  Pragma Elaborate_All case, for this we use the recursive
               --  Elab_All_Links procedure to establish the links.

               elsif Withs.Table (W).Elaborate_All then

                  --  Reset flags used to stop multiple visits to a given node
                  --  but set our flag to stop a self-recursive link.

                  for Uref in UNR.First .. UNR.Last loop
                     UNR.Table (Uref).Visited := False;
                  end loop;

                  UNR.Table (U).Visited := True;

                  --  Now establish all the links we need

                  Elab_All_Links (Withed_Unit, U);

               --  Case of normal WITH with no elaboration pragmas, just
               --  build the single link to the directly referenced unit

               else
                  Build_Link (Withed_Unit, U, Withed);
               end if;
            end if;
         end loop;
      end loop;
   end Gather_Dependencies;

   ----------------
   -- Unit_Id_Of --
   ----------------

   function Unit_Id_Of (Uname : Unit_Name_Type) return Unit_Id is
      Info : constant Int := Get_Name_Table_Info (Uname);

   begin
      if Info = 0 or else Unit_Id (Info) = No_Unit_Id then
         Binder_Abort;
      else
         return Unit_Id (Info);
      end if;
   end Unit_Id_Of;

   ------------------------
   -- Write_Dependencies --
   ------------------------

   procedure Write_Dependencies is
      Col : Pos;

   begin
      Write_Eol;
      Write_String
        ("                 ELABORATION ORDER DEPENDENCIES");
      Write_Eol;
      Write_Eol;
      Write_String
        ("Elaborate                   Before                      Because");
      Write_Eol;
      Write_String
        ("---------                   ------                      -------");
      Write_Eol;

      for S in Succ.First .. Succ.Last loop
         Write_Unit_Name (Unit.Table (Succ.Table (S).Before).Uname);
         Col := Name_Len + 1;

         if Col >= 29 then
            Write_Eol;
            Col := 1;
         end if;

         while Col < 29 loop
            Write_Char (' ');
            Col := Col + 1;
         end loop;

         Write_Unit_Name (Unit.Table (Succ.Table (S).After).Uname);
         Col := Col + Name_Len;

         if Col >= 57 then
            Write_Eol;
            Col := 1;
         end if;

         while Col < 57 loop
            Write_Char (' ');
            Col := Col + 1;
         end loop;

         case Succ.Table (S).Reason is
            when Withed      => Write_String ("with");
            when Elab        => Write_String ("Elaborate");
            when Elab_All    => Write_String ("Elaborate_All");
            when Spec_First  => Write_String ("spec first");
         end case;

         Write_Eol;
      end loop;

      Write_Eol;
   end Write_Dependencies;

end Binde;
