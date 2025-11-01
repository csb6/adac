------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M  - D I S P                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.11 $                             --
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

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Exp_Ch6;  use Exp_Ch6;
with Errout;   use Errout;
with Output;   use Output;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;

package body Sem_Disp is

   ---------------------------
   -- Find_Dispatching_Type --
   ---------------------------

   function Find_Dispatching_Type (Subp : Entity_Id) return Entity_Id is
      Formal      : Entity_Id := First_Formal (Subp);
      Tagged_Type : Entity_Id;
      Tagged_Seen : Entity_Id := Empty;

      procedure Check_Controlling_Type (T : Entity_Id) is
      begin
         if Is_Tagged_Type (T) then
            Tagged_Type := T;

         elsif (Is_Access_Type (T)
                  and then Is_Tagged_Type (Designated_Type (T)))
         then
            Tagged_Type := Designated_Type (T);

         else
            Tagged_Type := Empty;
         end if;

         if Present (Tagged_Type)
           and then not Is_Class_Type (Tagged_Type)
         then
            if Present (Tagged_Seen) and then Tagged_Type /= Tagged_Seen then
               Error_Msg_N
                 ("operation can be dispatching in only one type", Formal);
            else
               Tagged_Seen := Tagged_Type;
            end if;
         end if;
      end Check_Controlling_Type;

   --  Start of processing Find_Dispatching_Type

   begin
      while Present (Formal) loop
         Check_Controlling_Type (Etype (Formal));
         Formal := Next_Formal (Formal);
      end loop;

      --  The subprogram may also be dispatching on result

      if Present (Etype (Subp)) then
         Check_Controlling_Type (Etype (Subp));
      end if;

      return Tagged_Seen;
   end Find_Dispatching_Type;

   ----------------------------------
   -- Check_Dispatching_Operation  --
   ----------------------------------

   procedure Check_Dispatching_Operation (Subp : Entity_Id) is
      Tagged_Seen : constant Entity_Id := Find_Dispatching_Type (Subp);

   begin
      if Present (Tagged_Seen)
        and then not Is_Frozen (Tagged_Seen)
      then
         --  Subprogram is primitive operation of tagged type

         Append_Elmt (Subp, Primitive_Operations (Tagged_Seen));
         Set_Is_Dispatching_Operation (Subp, True);

         if Is_Abstract (Subp) 
           and then not Is_Abstract (Tagged_Seen) 
         then 
            Error_Msg_N
              ("only an abstract type can have dispatching operations", Subp);
         end if;
      else 
         Set_Is_Dispatching_Operation (Subp, False);      
      end if;
   end Check_Dispatching_Operation;

   ----------------------------
   -- Check_Dispatching_Call --
   ----------------------------

   procedure Check_Dispatching_Call (N : Node_Id) is
      Actual  : Node_Id;
      Control : Node_Id := Empty;

   begin
      --  Find a controlling argument, if any

      if List_Present (Parameter_Associations (N)) then
         Actual := First_Actual (N);

         while Present (Actual) loop
            Control := Find_Controlling_Arg (Actual);
            exit when Present (Control);
            Actual := Next_Actual (Actual);
         end loop;

         if Present (Control) then

            --  Verify that no controlling arguments are statically tagged

            if Debug_Flag_E then
               Write_Str ("Found Dispatching call");
               Write_Int (Int (N));
               Write_Eol;
            end if;

            Actual := First_Actual (N);

            while Present (Actual) loop
               if Actual /= Control then

                  if not Is_Tagged_Type (Etype (Actual)) then
                     null; -- can be anything

                  elsif (Is_Dynamically_Tagged (Actual)) then
                     null; --  valid parameter

                  elsif Is_Tag_Indeterminate (Actual) then

                     --  The tag is inherited from the enclosing call,
                     --  namely the node we are currently analyzing.
                     --  Expand the actual explicitly, because the previous
                     --  call to expand (From resolve_call) did not know
                     --  about the dispatching needed.

                     Propagate_Tag (Control, Actual);

                  else
                     Error_Msg_N
                         ("all dispatching call controlling arguments "
                     & "must be dynamically tagged", Actual);
                     return;
                  end if;
               end if;

               Actual := Next_Actual (Actual);
            end loop;

            --  Mark call as a dispatching call

            Set_Controlling_Argument (N, Control);
         end if;

      else
         --  If dispatching on result, the enclosing call, if any, will
         --  determine the controlling argument. Otherwise this is the
         --  primitive operation of the root type.

         null;
      end if;
   end Check_Dispatching_Call;

   ---------------------------
   -- Is_Dynamically_Tagged --
   ---------------------------

   function Is_Dynamically_Tagged (N : Node_Id) return Boolean is
   begin
      return Find_Controlling_Arg (N) /= Empty;
   end Is_Dynamically_Tagged;

   --------------------------
   -- Find_Controlling_Arg --
   --------------------------

   function Find_Controlling_Arg (N : Node_Id) return Node_Id is
      Orig_Node : constant Node_Id := Original_Node (N);

   begin
      if not Is_Tagged_Type (Etype (N)) then
         return Empty;

      elsif (Is_Name (N) 
               or else Nkind (N) = N_Explicit_Dereference
               or else Nkind (N) = N_Type_Conversion)
              and then Is_Class_Type (Etype (N))
      then
         return N;

      elsif Nkind (Orig_Node) = N_Function_Call
        and then Present (Controlling_Argument (Orig_Node))
      then
         return Controlling_Argument (Orig_Node);

      elsif Nkind (N) = N_Parenthesized_Expression
        or else Nkind (N) = N_Qualified_Expression
      then
         return Find_Controlling_Arg (Expression (N));

      else
         return Empty;
      end if;
   end Find_Controlling_Arg;

   --------------------------
   -- Is_Tag_Indeterminate --
   --------------------------

   function Is_Tag_Indeterminate (N : Node_Id) return Boolean is
      Nam       : Entity_Id;
      Actual    : Node_Id;
      Orig_Node : constant Node_Id := Original_Node (N);

   begin
      if Nkind (Orig_Node) = N_Function_Call then
         Nam := Entity (Name (Orig_Node));

         if List_Present (Parameter_Associations (Orig_Node)) then
            Actual := First_Actual (Orig_Node);

            while Present (Actual) loop
               if Is_Tagged_Type (Etype (Actual))
                 and then Is_Dynamically_Tagged (Actual)
               then
                  return False; -- one operand is dispatching
               end if;

               Actual := Next_Actual (Actual);
            end loop;

            return True;

         --  If there are no actuals, the call is tag-indeterminate

         else
            return True;
         end if;

      elsif Nkind (Orig_Node) = N_Parenthesized_Expression
        or else Nkind (Orig_Node) = N_Qualified_Expression
      then
         return Is_Tag_Indeterminate (Expression (Orig_Node));

      else
         return False;
      end if;
   end Is_Tag_Indeterminate;

   ------------------------------------
   -- Override_Dispatching_Operation --
   ------------------------------------

   procedure Override_Dispatching_Operation (Prev_Op, New_Op : Entity_Id) is
      Formal : Entity_Id := First_Entity (Prev_Op);
      Tagged_Type : Entity_Id := Empty;
      Prim_Ops    : Elist_Id;
      Op_Elmt     : Elmt_Id;

   begin
      while Present (Formal) loop
         if Is_Tagged_Type (Etype (Formal)) then
            Tagged_Type := Etype (Formal);
            exit;
         end if;

         Formal := Next_Entity (Formal);
      end loop;

      if No (Tagged_Type) then
         Tagged_Type := Etype (Prev_Op);
         --  Subprogram must be dispatching on result
      end if;

      Prim_Ops := Primitive_Operations (Tagged_Type);
      Op_Elmt := First_Elmt (Prim_Ops);

      while Op_Elmt /= No_Elmt loop
         if Id_Of (Op_Elmt) = Prev_Op then
            Insert_Elmt_Before (Op_Elmt, New_Op);
            Remove_Elmt (Op_Elmt);
            Remove_Elmt (Last_Elmt (Prim_Ops));
            exit;
         end if;

         Op_Elmt := Next_Elmt (Op_Elmt);
      end loop;
   end Override_Dispatching_Operation;

   -------------------
   -- Propagate_Tag --
   -------------------

   procedure Propagate_Tag (Control : Node_Id; Actual : Node_Id) is
      Call_Node : Node_Id;
      Arg       : Node_Id;

   begin
      if Nkind (Actual) = N_Function_Call then
         Call_Node := Actual;
      else
         --  Parenthesized or qualified expression.
         Call_Node := Expression (Actual);
      end if;
      Set_Controlling_Argument (Call_Node, Control);
      Arg := First_Actual (Call_Node);
      while Present (Arg) loop
         if Is_Tag_Indeterminate (Arg) then
            Propagate_Tag (Control,  Arg);
         end if;
         Arg := Next_Actual (Arg);
      end loop;

      Expand_Dispatch_Call (Call_Node);
   end Propagate_Tag;

end Sem_Disp;
