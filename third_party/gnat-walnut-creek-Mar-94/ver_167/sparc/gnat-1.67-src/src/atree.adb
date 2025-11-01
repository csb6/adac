------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                A T R E E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.55 $                             --
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

with Comperr; use Comperr;
with Debug;   use Debug;
with Output;  use Output;
with Unchecked_Conversion;

package body Atree is

--  WARNING: There is a C version of this package. Any changes to this source
--  file must be properly reflected in the C header a-atree.h (for inlined
--  bodies) and the C file a-atree.c (for remaining non-inlined bodies).

   use Unchecked_Access;
   --  We are allowed to see these from within our own body!

   use Atree_Private_Part;
   --  We are also allowed to see our private data structures!

   function E_To_N is new Unchecked_Conversion (Entity_Kind, Node_Kind);
   function N_To_E is new Unchecked_Conversion (Node_Kind, Entity_Kind);
   --  Functions used to store Entity_Kind value in Nkind field

   --  Default value used to initialize default nodes. Note that some of the
   --  fields get overwritten, and in particular, Nkind always gets reset.

   Default_Node : constant Node_Record := (
      Is_Extension => False,
      In_List      => False,
      Rewrite_Sub  => False,
      Rewrite_Ins  => False,
      Flag1        => False,
      Flag2        => False,
      Flag3        => False,
      Flag4        => False,

      Flag5        => False,
      Flag6        => False,
      Flag7        => False,
      Flag8        => False,
      Flag9        => False,
      Flag10       => False,
      Flag11       => False,
      Flag12       => False,

      Flag13       => False,
      Flag14       => False,
      Flag15       => False,
      Flag16       => False,
      Flag17       => False,
      Flag18       => False,
      Flag19       => False,
      Flag20       => False,

      Nkind        => N_Unused_At_Start,

      Sloc         => No_Location,
      Link         => Int (Empty),
      Field1       => Int (Empty),
      Field2       => Int (Empty),
      Field3       => Int (Empty),
      Field4       => Int (Empty),
      Field5       => Int (Empty));

   --  Default value used to initialize node extensions (i.e. the second
   --  component of an extended node)

   Default_Node_Extension : constant Node_Record := (
      Is_Extension => True,
      In_List      => False,
      Rewrite_Sub  => False,
      Rewrite_Ins  => False,
      Flag1        => False,
      Flag2        => False,
      Flag3        => False,
      Flag4        => False,

      Flag5        => False,
      Flag6        => False,
      Flag7        => False,
      Flag8        => False,
      Flag9        => False,
      Flag10       => False,
      Flag11       => False,
      Flag12       => False,

      Flag13       => False,
      Flag14       => False,
      Flag15       => False,
      Flag16       => False,
      Flag17       => False,
      Flag18       => False,
      Flag19       => False,
      Flag20       => False,

      Nkind        => E_To_N (E_Void),

      Field6       => Int (Empty),
      Field7       => Int (Empty),
      Field8       => Int (Empty),
      Field9       => Int (Empty),
      Field10      => Int (Empty),
      Field11      => Int (Empty),
      Field12      => Int (Empty));

   ----------------------------------
   -- Implementation of Node Lists --
   ----------------------------------

   --  To allow efficient access to the list, both for traversal, and for
   --  insertion of new entries at the end of the list, a list is stored
   --  using a circular format, as indicated by the following diagram:

   --  +--------+    +-------+    +-------+         +-------+
   --  |  List  |    |  1st  |    |  2nd  |         | Last  |
   --  |     ------->|   -------->|   ------>....-->|   -------+
   --  | Header |    | Entry |    | Entry |         | Entry |  |
   --  +-----|--+    +-------+    +-------+         +-------+  |
   --     ^  |                                          ^      |
   --     |  |                                          |      |
   --     |  + -----------------------------------------+      |
   --     +--- ------------------------------------------------+

   --  The list header is an entry in the Lists table. List_Id values
   --  are used to reference list headers.

   --  The First field of the list header contains Empty for a null list,
   --  or a standard Node_Id value pointing to the first item on the list.
   --  The Last field of the list header contains Empty for a null list or a
   --  standard Node_Id value pointing to the last item on the list.

   --  The nodes within the list use the Link field to hold a normal
   --  Node_Id value, which points to the next item in the list except for
   --  the last item in the list, which points to the list head, and is
   --  negative (i.e. it is a standard List_Id value referencing the containing
   --  list. This allows a quick check for the end of the list in a list
   --  traversal (test for negative), and also makes it possible to find the
   --  list containing any given node (find the end of the list by chasing
   --  link fields, and then the link field of this node references the list).

   --  All nodes that are elements of a list have the In_List flag set True.
   --  All nodes that are not list elements have the In_List flag set False.

   --  Note that since the Link field of a node is used both for a Parent
   --  pointer and for a forward link field in a list, that list elements
   --  cannot have direct parent pointers (and hence cannot be referenced
   --  directly from a field in another node). However, the list header
   --  itself does have a parent field.

   -------------------------------------------------------
   -- Internal subprograms for node list implementation --
   -------------------------------------------------------

   procedure Set_First (List : List_Id; Node : Node_Id);
   pragma Inline (Set_First);
   --  Used internally in the implementation of the list routines to set the
   --  first element of a list to point to a given node.

   procedure Set_Last (List : List_Id; Node : Node_Id);
   pragma Inline (Set_Last);
   --  Used internally in the implementation of the list routines to set the
   --  last element of a list to point to a given node.

   function Node_Link (Node : Node_Id) return Node_Id;
   pragma Inline (Node_Link);
   --  Used internally in the implementation of the list routines to return
   --  the contents of the Link field of a specified node as a node.

   function List_Link (Node : Node_Id) return List_Id;
   pragma Inline (List_Link);
   --  Used internally in the implementation of the list routines to return
   --  the contents of the Link field of a specified node as a list.

   procedure Set_Node_Link (Node : Node_Id; To : Node_Id);
   pragma Inline (Set_Node_Link);
   --  Used internally in the implementation of the list routines to set
   --  the Link field of a node to point to a given node.

   procedure Set_List_Link (Node : Node_Id; To : List_Id);
   pragma Inline (Set_List_Link);
   --  Used internally in the implementation of the list routines to set
   --  the Link field of a node to point to a given list.

   function Is_At_End_Of_List (Node : Node_Id) return Boolean;
   pragma Inline (Is_At_End_Of_List);
   --  Used internally in the implementation of the list routines to determine
   --  if a given node is the last element of a list. False for nodes that are
   --  not elements of lists.

   -------------------------------------
   -- Implementation of Element Lists --
   -------------------------------------

   --  Element lists are composed of three types of entities. The element
   --  list header, which references the first and last elements of the
   --  list, the elements themselves which are doubly linked and also
   --  reference the nodes on the list, and finally the nodes themselves.
   --  The following diagram shows how an element list is represented:

   --     +----------------------------------------------------+
   --     |  +------------------------------------------+      |
   --     |  |                                          |      |
   --     V  |                                          V      |
   --  +-----|--+    +-------+    +-------+         +-------+  |
   --  |  Elmt  |    |  1st  |    |  2nd  |         |  Last |  |
   --  |  List  |<-->|  Elmt |<-->|  Elmt  <--...-->|  Elmt ---+
   --  | Header |    |   |   |    |   |   |         |   |   |
   --  +--------+    +---|---+    +---|---+         +---|---+
   --                    |            |                 |
   --                    V            V                 V
   --                +-------+    +-------+         +-------+
   --                |       |    |       |         |       |
   --                | Node1 |    | Node2 |         | Node3 |
   --                |       |    |       |         |       |
   --                +-------+    +-------+         +-------+

   --  The list header is an entry in the Lists table. The values used for
   --  the type Elist_Id are subscripts into this table, biased so that
   --  they have values that are separated from normal List_Id values. The
   --  First_Elmt field (Lfield1) points to the first element on the list,
   --  or to No_Elmt in the case of an empty list. Similarly the Last_Elmt
   --  field (Lfield2) points to the last element on the list, or to No_Elmt
   --  in the case of an empty list.

   --  The elements themselves are also entries in the Lists table. Again
   --  the values for the type Elmt_Id are subscripts into this table, biased
   --  with a different bias value to separate them from List_Id and Elist_Id
   --  values. The Prev_Elmt field (Lfield1) points to the previous element,
   --  or to the list header for the first element of the list. Similarly
   --  The Next_Elmt field (Lfield2) points to the next element, or to the
   --  list header for the last element of the list.

   --  The nodes themselves are simply referenced by the Lnode field of the
   --  associated element (no values are stored or modified in the node
   --  itself as a result of putting the node in one or more element lists).

   -----------------------------------------------------------
   --  Internal subprograms for element list implementation --
   -----------------------------------------------------------

   function L_Id (Id : Elist_Id) return List_Id;
   function L_Id (Id : Elmt_Id) return List_Id;
   pragma Inline (L_Id);
   --  Functions used to convert Elist_Id and Elmt_Id values to a subscript
   --  in the Lists table (by subtracting the appropriate bias value)

   procedure Set_First_Elmt (List : Elist_Id; Val : Elmt_Id);
   pragma Inline (Set_First_Elmt);
   --  Procedure used to set First_Elmt field of a list element header

   procedure Set_Last_Elmt (List : Elist_Id; Val : Elmt_Id);
   pragma Inline (Set_Last_Elmt);
   --  Procedure used to set Last_Elmt field of a list element header

   function Is_First_Elmt (Elmt : Elmt_Id) return Boolean;
   pragma Inline (Is_First_Elmt);
   --  Function to determine if list element is first element of a list

   function Is_Last_Elmt (Elmt : Elmt_Id) return Boolean;
   pragma Inline (Is_Last_Elmt);
   --  Function to determine if list element is last element of a list

   function Get_Header_From_First_Elmt (Elmt : Elmt_Id) return Elist_Id;
   pragma Inline (Get_Header_From_First_Elmt);
   --  Function to get element list header from first element on list

   function Get_Header_From_Last_Elmt (Elmt : Elmt_Id) return Elist_Id;
   pragma Inline (Get_Header_From_Last_Elmt);
   --  Function to get element list header from last element on list

   procedure Set_Next_Elmt (Elmt : Elmt_Id; Val : Elmt_Id);
   procedure Set_Next_Elmt (Elmt : Elmt_Id; Val : Elist_Id);
   pragma Inline (Set_Next_Elmt);
   --  Procedures to set contents of Next_Elmt field of list element.
   --  Two forms needed to deal with normal case and end of list case.

   procedure Set_Prev_Elmt (Elmt : Elmt_Id; Val : Elmt_Id);
   procedure Set_Prev_Elmt (Elmt : Elmt_Id; Val : Elist_Id);
   pragma Inline (Set_Prev_Elmt);
   --  Procedures to set contents of Prev_Elmt field of list element.
   --  Two forms needed to deal with normal case and start of list case.

   procedure Set_Elmt_Links (List : Elist_Id; First_Elmt : Elmt_Id);
   procedure Set_Elmt_Links (Elmt1 : Elmt_Id; Elmt2 : Elmt_Id);
   procedure Set_Elmt_Links (Last_Elmt : Elmt_Id; List : Elist_Id);
   pragma Inline (Set_Elmt_Links);
   --  These procedures set bidirectional links. There are three versions,
   --  one for linking the header to the first element on the list, one
   --  for linking two elements, and the third for linking the last element
   --  on the list to the header.

   function New_Elmt (Node : Node_Id) return Elmt_Id;
   --  Function to create new list element pointing to indicated node.
   --  The Next_Elmt and Prev_Elmt fields are not initialized.

   --------------------------
   -- Debugging Procedures --
   --------------------------

   --  These debugging procedures are called only if assertions are enabled

   procedure Dcheck_Is_Node (N : Node_Id);
   --  Debug procedure used to check that the given value is the Id of a
   --  currently allocated node. A compiler abort is signalled if not.

   procedure Dcheck_Is_Extended_Node (N : Node_Id);
   --  Debug procedure used to check that the given value is the Id of a
   --  currently allocated extended node and that the node is an entity
   --  (since only entities can be extended nodes). A compiler abort is
   --  signalled if not.

   procedure Dcheck_Is_List (L : List_Id);
   --  Debug procedure used to check that the given value is the Id of a
   --  currently allocated node list header. A compiler abort is signalled
   --  if not, or if the argument is No_List.

   procedure Dcheck_Is_Elist (E : Elist_Id);
   --  Debug procedure used to check that the given value is the Id of a
   --  currently allocated element list header. A compiler abort is signalled
   --  if not, or if the argument is No_Elist.

   procedure Dcheck_List_Member (N : Node_Id);
   --  Debug procedure used to check that the given node is in a list.
   --  A compiler abort is signalled if not.

   procedure Dcheck_Not_List_Member (N : Node_Id);
   --  Debug procedure used to check that the given node is not in a list.
   --  A compiler abort is signalled if not.

   ---------------------------------------------
   -- Implementation of Tree Rewrite Routines --
   ---------------------------------------------

   --  The tree rewrite insert routines are essentially trivial, so they are
   --  included in this unit. The replacement routines are more complex and
   --  are split out into a package subunit.

   package TS is
      procedure Initialize_Sub;
      --  Initialize data structures for tree rewrite substitution processing.
      --  This procedure is called from Initialize_Tree.

      procedure Set_Substitute_Tree (Old_Node, New_Node : Node_Id);
      --  This is the implementation of Rewrite_Substitute_Tree

      function Get_Substitute_Tree (New_Node : Node_Id) return Node_Id;
      --  This function implements Original_Node for the case of a node
      --  that was inserted into the tree using Rewrite_Substitute_Tree.
      --  It is a fatal error to use it on any other node.
   end TS;

   package body TS is separate;

   --------------------
   -- Dcheck_Is_Node --
   --------------------

   procedure Dcheck_Is_Node (N : Node_Id) is
   begin
      if N < Empty
         or else N > Nodes.Last
         or else Nodes.Table (N).Is_Extension

      then
         Compiler_Error;
         Write_Eol;
         Write_String ("Out of range N = ");
         Write_Int (Int (N));
         Write_String (" Nodes.Last = ");
         Write_Int (Int (Nodes.Last));
         Write_Eol;
         Compiler_Abort;
      end if;
   end Dcheck_Is_Node;

   -----------------------------
   -- Dcheck_Is_Extended_Node --
   -----------------------------

   procedure Dcheck_Is_Extended_Node (N : Node_Id) is
   begin
      Dcheck_Is_Node (N);

      if not Nodes.Table (N + 1).Is_Extension then
         Compiler_Error;
         Write_Eol;
         Write_String ("Node N (Id = ");
         Write_Int (Int (N));
         Write_String (" ) is not extended node");
         Write_Eol;
         Compiler_Abort;

      elsif Nodes.Table (N).Nkind not in N_Entity then
         Compiler_Error;
         Write_Eol;
         Write_String ("Node N (Id = ");
         Write_Int (Int (N));
         Write_String (" ) is not an entity");
         Write_Eol;
         Compiler_Abort;

      end if;
   end Dcheck_Is_Extended_Node;

   --------------------
   -- Dcheck_Is_List --
   --------------------

   procedure Dcheck_Is_List (L : List_Id) is
   begin
      if L > Lists.Last then
         Compiler_Error;
         Write_Eol;
         Write_String ("Out of range L = ");
         Write_Int (Int (L));
         Write_String (" Lists.Last = ");
         Write_Int (Int (Lists.Last));
         Write_Eol;
         Compiler_Abort;
      end if;
   end Dcheck_Is_List;

   ---------------------
   -- Dcheck_Is_Elist --
   ---------------------

   procedure Dcheck_Is_Elist (E : Elist_Id) is
   begin
      if L_Id (E) > Lists.Last then
         Compiler_Error;
         Write_Eol;
         Write_String ("Out of range E = ");
         Write_Int (Int (E));
         Write_String (" Lists.Last = ");
         Write_Int (Int (Lists.Last));
         Write_Eol;
         Compiler_Abort;
      end if;
   end Dcheck_Is_Elist;

   ------------------------
   -- Dcheck_List_Member --
   ------------------------

   procedure Dcheck_List_Member (N : Node_Id) is
   begin
      Dcheck_Is_Node (N);

      if not Nodes.Table (N).In_List then
         Compiler_Error;
         Write_String ("Node Id = ");
         Write_Int (Int (N));
         Write_String (" is not a list member");
         Write_Eol;
         Compiler_Abort;
      end if;
   end Dcheck_List_Member;

   ----------------------------
   -- Dcheck_Not_List_Member --
   ----------------------------

   procedure Dcheck_Not_List_Member (N : Node_Id) is
   begin
      Dcheck_Is_Node (N);

      if Nodes.Table (N).In_List then
         Compiler_Error;
         Write_String ("Node Id = ");
         Write_Int (Int (N));
         Write_String (" is a list member");
         Write_Eol;
         Compiler_Abort;
      end if;
   end Dcheck_Not_List_Member;

   ------------------
   -- Last_Node_Id --
   ------------------

   function Last_Node_Id return Node_Id is
   begin
      return Nodes.Last;
   end Last_Node_Id;

   ------------------
   -- Last_List_Id --
   ------------------

   function Last_List_Id return List_Id is
   begin
      return Lists.Last;
   end Last_List_Id;

   ----------------------
   -- Initialize_Atree --
   ----------------------

   procedure Initialize_Atree is
      Dummy : Node_Id;

   begin
      Nodes.Init;
      Lists.Init;
      TS.Initialize_Sub;

      --  Allocate Empty and Error nodes

      Dummy := New_Node (N_Empty, No_Location);
      Set_Name1 (Empty, No_Name);
      Dummy := New_Node (N_Error, No_Location);
      Set_Name1 (Error, Error_Name);

      --  Allocate Error_List list header

      Lists.Increment_Last;
      Set_Parent (Error_List, Empty);
      Set_First (Error_List, Empty);
      Set_Last (Error_List, Empty);
   end Initialize_Atree;

   --------------
   -- New_Node --
   --------------

   function New_Node (New_Node_Kind : Node_Kind; New_Sloc : Source_Ptr)
     return Node_Id is

   begin
      Nodes.Increment_Last;
      Nodes.Table (Nodes.Last)        := Default_Node;
      Nodes.Table (Nodes.Last).Nkind  := New_Node_Kind;
      Nodes.Table (Nodes.Last).Sloc   := New_Sloc;

      if Debug_Flag_N then
         Write_String ("Allocate new node, Id = ");
         Write_Int (Int (Nodes.Last));
         Write_String (" Nkind = ");
         Write_String (Node_Kind'Image (New_Node_Kind));
         Write_Eol;
      end if;

      return Nodes.Last;
   end New_Node;

   -----------------
   -- Extend_Node --
   -----------------

   function Extend_Node (Node : Node_Id) return Node_Id is
      Result : Node_Id;

   begin
      if Node /= Nodes.Last then
         Result := New_Copy (Node);
      else
         Result := Node;
      end if;

      Nodes.Increment_Last;
      Nodes.Table (Nodes.Last) := Default_Node_Extension;

      if Debug_Flag_N then
         Write_String ("Allocate extension Id = ");
         Write_Int (Int (Nodes.Last));
         Write_Eol;
      end if;

      return Result;
   end Extend_Node;

   -------------------
   -- Has_Extension --
   -------------------

   function Has_Extension (N : Node_Id) return Boolean is
   begin
      pragma Debug (Dcheck_Is_Node (N));
      return N < Nodes.Last and then Nodes.Table (N + 1).Is_Extension;
   end Has_Extension;

   -----------------
   -- Change_Node --
   -----------------

   procedure Change_Node (N : Node_Id; New_Node_Kind : Node_Kind) is
      Save_Sloc    : Source_Ptr := Sloc (N);
      Save_In_List : Boolean  := Nodes.Table (N).In_List;
      Save_Link    : Int      := Nodes.Table (N).Link;

   begin
      Nodes.Table (N)         := Default_Node;
      Nodes.Table (N).Sloc    := Save_Sloc;
      Nodes.Table (N).In_List := Save_In_List;
      Nodes.Table (N).Link    := Save_Link;
      Nodes.Table (N).Nkind   := New_Node_Kind;
   end Change_Node;

   ---------------
   -- Copy_Node --
   ---------------

   procedure Copy_Node (Source : Node_Id; Destination : Node_Id) is
      Save_In_List : Boolean;
      Save_Link    : Int;

   begin
      pragma Debug  (Dcheck_Is_Node (Source));
      pragma Debug  (Dcheck_Is_Node (Destination));
      pragma Assert (Has_Extension (Source) = Has_Extension (Destination),
                        Compiler_Abort);

      Save_In_List := Nodes.Table (Destination).In_List;
      Save_Link    := Nodes.Table (Destination).Link;

      Nodes.Table (Destination)         := Nodes.Table (Source);
      Nodes.Table (Destination).In_List := Save_In_List;
      Nodes.Table (Destination).Link    := Save_Link;

      if Has_Extension (Source) then
         Nodes.Table (Destination + 1) := Nodes.Table (Source + 1);
      end if;
   end Copy_Node;

   --------------
   -- New_Copy --
   --------------

   function New_Copy (Source : Node_Id) return Node_Id is
      New_Id : Node_Id;

   begin
      pragma Debug (Dcheck_Is_Node (Source));

      if Source <= Empty_Or_Error then
         return Source;

      else
         Nodes.Increment_Last;
         New_Id := Nodes.Last;
         Nodes.Table (New_Id) := Nodes.Table (Source);
         Set_Node_Link (New_Id, Empty);
         Nodes.Table (New_Id).In_List := False;

         if Has_Extension (Source) then
            New_Id := Extend_Node (New_Id);
            Nodes.Table (New_Id + 1) := Nodes.Table (Source + 1);
         end if;

         if Is_Rewrite_Substitution (Source) then
            Nodes.Table (New_Id).Rewrite_Sub := False;
         end if;

         return New_Id;
      end if;
   end New_Copy;

   --------------------
   -- Exchange_Nodes --
   --------------------

   procedure Exchange_Nodes (Node1 : Node_Id; Node2 : Node_Id) is
      N1, N2 : Node_Record;

   begin
      pragma Debug (Dcheck_Is_Node (Node1));
      pragma Debug (Dcheck_Is_Node (Node2));
      pragma Assert (Has_Extension (Node1) = Has_Extension (Node2),
                      Compiler_Abort);

      N1 := Nodes.Table (Node1);
      N2 := Nodes.Table (Node2);
      Nodes.Table (Node1) := N2;
      Nodes.Table (Node2) := N1;

      Nodes.Table (Node1).In_List := N1.In_List;
      Nodes.Table (Node2).In_List := N2.In_List;
      Nodes.Table (Node1).Link    := N1.Link;
      Nodes.Table (Node2).Link    := N2.Link;

      if Has_Extension (Node1) then
         N1 := Nodes.Table (Node1 + 1);
         Nodes.Table (Node1 + 1) := Nodes.Table (Node2 + 1);
         Nodes.Table (Node2 + 1) := N1;
      end if;
   end Exchange_Nodes;

   --------------
   -- New_List --
   --------------

   function New_List return List_Id is
   begin
      Lists.Increment_Last;
      Set_Parent (Lists.Last, Empty);
      Set_First (Lists.Last, Empty);
      Set_Last (Lists.Last, Empty);

      if Debug_Flag_N then
         Write_String ("Allocate new list, returned ID = ");
         Write_Int (Int (Lists.Last));
         Write_Eol;
      end if;

      return (Lists.Last);
   end New_List;

   ----------------
   -- New_List_1 --
   ----------------

   --  Since this is a common case, we optimize to build the right list
   --  directly, rather than first building an empty list and then doing
   --  the insertion, which results in some unnecessary work.

   function New_List_1 (A : Node_Id) return List_Id is
   begin
      if A = Error then
         return New_List;
      else
         Lists.Increment_Last;
         Set_Parent (Lists.Last, Empty);
         Set_First (Lists.Last, A);
         Set_Last (Lists.Last, A);
         Set_List_Link (A, Lists.Last);
         Nodes.Table (A).In_List := True;
      end if;

      if Debug_Flag_N then
         Write_String ("Allocate new list, returned ID = ");
         Write_Int (Int (Lists.Last));
         Write_Eol;
      end if;

      return (Lists.Last);
   end New_List_1;

   ----------------
   -- New_List_2 --
   ----------------

   function New_List_2 (A, B : Node_Id) return List_Id is
      L : constant List_Id := New_List_1 (A);

   begin
      Append (B, L);
      return L;
   end New_List_2;

   ----------------
   -- New_List_3 --
   ----------------

   function New_List_3 (A, B, C : Node_Id) return List_Id is
      L : constant List_Id := New_List_1 (A);

   begin
      Append (B, L);
      Append (C, L);
      return L;
   end New_List_3;

   ----------------
   -- New_List_4 --
   ----------------

   function New_List_4 (A, B, C, D : Node_Id) return List_Id is
      L : constant List_Id := New_List_1 (A);

   begin
      Append (B, L);
      Append (C, L);
      Append (D, L);
      return L;
   end New_List_4;

   ----------------
   -- New_List_5 --
   ----------------

   function New_List_5 (A, B, C, D, E : Node_Id) return List_Id is
      L : constant List_Id := New_List_1 (A);

   begin
      Append (B, L);
      Append (C, L);
      Append (D, L);
      Append (E, L);
      return L;
   end New_List_5;

   ----------------
   -- New_List_6 --
   ----------------

   function New_List_6 (A, B, C, D, E, F : Node_Id) return List_Id is
      L : constant List_Id := New_List_1 (A);

   begin
      Append (B, L);
      Append (C, L);
      Append (D, L);
      Append (E, L);
      Append (F, L);
      return L;
   end New_List_6;

   ----------------
   -- New_List_7 --
   ----------------

   function New_List_7 (A, B, C, D, E, F, G : Node_Id) return List_Id is
      L : constant List_Id := New_List_1 (A);

   begin
      Append (B, L);
      Append (C, L);
      Append (D, L);
      Append (E, L);
      Append (F, L);
      Append (G, L);
      return L;
   end New_List_7;

   ----------------
   -- New_List_8 --
   ----------------

   function New_List_8 (A, B, C, D, E, F, G, H : Node_Id) return List_Id is
      L : constant List_Id := New_List_1 (A);

   begin
      Append (B, L);
      Append (C, L);
      Append (D, L);
      Append (E, L);
      Append (F, L);
      Append (G, L);
      Append (H, L);
      return L;
   end New_List_8;

   ----------------
   -- New_List_9 --
   ----------------

   function New_List_9 (A, B, C, D, E, F, G, H, I : Node_Id) return List_Id is
      L : constant List_Id := New_List_1 (A);

   begin
      Append (B, L);
      Append (C, L);
      Append (D, L);
      Append (E, L);
      Append (F, L);
      Append (G, L);
      Append (H, L);
      Append (I, L);
      return L;
   end New_List_9;

   -------------------
   -- New_List_Copy --
   -------------------

   function New_List_Copy (List : List_Id) return List_Id is
      NL : List_Id;
      E  : Node_Id;

   begin
      if List = No_List then
         return No_List;

      else
         NL := New_List;
         E := First (List);

         while Present (E) loop
            Append (New_Copy (E), NL);
            E := Next (E);
         end loop;

         return NL;
      end if;
   end New_List_Copy;

   -----------
   -- First --
   -----------

   function First (List : List_Id) return Node_Id is
   begin
      pragma Debug (Dcheck_Is_List (List));
      return Node_Id (Lists.Table (List).Lfield1);
   end First;

   ---------------
   -- Set_First --
   ---------------

   --  Note: for internal use only

   procedure Set_First (List : List_Id; Node : Node_Id) is
   begin
      pragma Debug (Dcheck_Is_List (List));
      Lists.Table (List).Lfield1 := Int (Node);
   end Set_First;

   ----------
   -- Last --
   ----------

   function Last (List : List_Id) return Node_Id is
   begin
      pragma Debug (Dcheck_Is_List (List));
      return Node_Id (Lists.Table (List).Lfield2);
   end Last;

   --------------
   -- Set_Last --
   --------------

   --  Note: for internal use only

   procedure Set_Last (List : List_Id; Node : Node_Id) is
   begin
      pragma Debug (Dcheck_Is_List (List)); 
      Lists.Table (List).Lfield2 := Int (Node);
   end Set_Last;

   ---------
   -- Nth --
   ---------

   function Nth (List : List_Id; Index : Pos) return Node_Id is
      Result : Node_Id;
      Count : Int := 1;

   begin
      Result := First (List);
      while Count < Index and then Result /= Empty loop
         Result := Next (Result);
      end loop;
      return Result;
   end Nth;

   ----------
   -- Next --
   ----------

   function Next (Node : Node_Id) return Node_Id is
   begin
      pragma Debug (Dcheck_List_Member (Node));

      if Is_At_End_Of_List (Node) then
         return Empty;
      else
         return Node_Link (Node);
      end if;
   end Next;

   ---------------
   -- Node_Link --
   ---------------

   --  Note: for internal use only

   function Node_Link (Node : Node_Id) return Node_Id is
   begin
      pragma Debug (Dcheck_Is_Node (Node));
      return Node_Id (Nodes.Table (Node).Link);
   end Node_Link;

   ---------------
   -- List_Link --
   ---------------

   --  Note: for internal use only

   function List_Link (Node : Node_Id) return List_Id is
   begin
      pragma Debug (Dcheck_Is_Node (Node));
      return List_Id (Nodes.Table (Node).Link);
   end List_Link;

   -------------------
   -- Set_Node_Link --
   -------------------

   --  Note: for internal use only

   procedure Set_Node_Link (Node : Node_Id; To : Node_Id) is
   begin
      pragma Debug (Dcheck_Is_Node (Node));
      Nodes.Table (Node).Link := Int (To);
   end Set_Node_Link;

   -------------------
   -- Set_List_Link --
   -------------------

   --  Note: for internal use only

   procedure Set_List_Link (Node : Node_Id; To : List_Id) is
   begin
      pragma Debug (Dcheck_Is_Node (Node));
      Nodes.Table (Node).Link := Int (To);
   end Set_List_Link;

   -----------------------
   -- Is_At_End_Of_List --
   -----------------------

   --  Note: for internal use only

   function Is_At_End_Of_List (Node : Node_Id) return Boolean is
   begin
      return (Nodes.Table (Node).Link in List_Range);
   end Is_At_End_Of_List;

   ----------
   -- Prev --
   ----------

   function Prev (Node : Node_Id) return Node_Id is
      P : Node_Id;

   begin
      P := First (List_Containing (Node));

      if P = Node then
         return Empty;

      else
         while Node_Link (P) /= Node loop
            P := Node_Link (P);
         end loop;

         return P;
      end if;
   end Prev;

   -------------------
   -- Is_Empty_List --
   -------------------

   function Is_Empty_List (L : List_Id) return Boolean is
   begin
      return First (L) = Empty;
   end Is_Empty_List;

   -----------------------
   -- Is_Non_Empty_List --
   -----------------------

   function Is_Non_Empty_List (L : List_Id) return Boolean is
   begin
      return First (L) /= Empty;
   end Is_Non_Empty_List;

   --------------------
   -- Is_List_Member --
   --------------------

   function Is_List_Member (Node : Node_Id) return Boolean is
   begin
      pragma Debug (Dcheck_Is_Node (Node));
      return Nodes.Table (Node).In_List;
   end Is_List_Member;

   ---------------------
   -- List_Containing --
   ---------------------

   function List_Containing (Node : Node_Id) return List_Id is
      N : Node_Id;

   begin
      pragma Debug (Dcheck_List_Member (Node));
      N := Node;

      while not Is_At_End_Of_List (N) loop
         N := Node_Link (N);
      end loop;

      return List_Link (N);
   end List_Containing;

   ------------
   -- Append --
   ------------

   procedure Append (Node : Node_Id; To : List_Id) is
   begin
      pragma Debug (Dcheck_Not_List_Member (Node));
      pragma Debug (Dcheck_Is_List (To));

      if Node = Error then
         return;
      end if;

      if Debug_Flag_N then
         Write_String ("Append node ");
         Write_Int (Int (Node));
         Write_String (" to list ");
         Write_Int (Int (To));
         Write_Eol;
      end if;

      if Last (To) = Empty then
         Set_First (To, Node);
      else
         Set_Node_Link (Last (To), Node);
      end if;

      Set_Last (To, Node);
      Set_List_Link (Node, To);
      Nodes.Table (Node).In_List := True;
   end Append;

   ---------------
   -- Append_To --
   ---------------

   procedure Append_To (To : List_Id; Node : Node_Id) is
   begin
      Append (Node, To);
   end Append_To;

   -----------------
   -- Append_List --
   -----------------

   procedure Append_List (List : List_Id; To : List_Id) is
   begin
      pragma Debug (Dcheck_Is_List (List));
      pragma Debug (Dcheck_Is_List (To));

      if Debug_Flag_N then
         Write_String ("Append list ");
         Write_Int (Int (List));
         Write_String (" to list ");
         Write_Int (Int (To));
         Write_Eol;
      end if;

      if Is_Empty_List (List) then
         return;

      else
         if Is_Empty_List (To) then
            Set_First (To, First (List));
         else
            Set_Node_Link (Last (To), First (List));
         end if;

         Set_Last (To, Last (List));
         Set_List_Link (Last (List), To);

         Set_Last (List, Empty);
         Set_First (List, Empty);
      end if;
   end Append_List;

   --------------------
   -- Append_List_To --
   --------------------

   procedure Append_List_To (To : List_Id; List : List_Id) is
   begin
      Append_List (List, To);
   end Append_List_To;

   ------------------
   -- Insert_After --
   ------------------

   procedure Insert_After (After : Node_Id; Node : Node_Id) is
   begin
      pragma Debug (Dcheck_Not_List_Member (Node));
      pragma Debug (Dcheck_List_Member (After));

      if Node = Error then
         return;
      end if;

      if Debug_Flag_N then
         Write_String ("Insert node");
         Write_Int (Int (Node));
         Write_String (" after node ");
         Write_Int (Int (After));
         Write_Eol;
      end if;

      if Is_At_End_Of_List (After) then
         Set_Last (List_Containing (After), Node);
         Set_List_Link (Node, List_Link (After));
      else
         Set_Node_Link (Node, Node_Link (After));
      end if;

      Set_Node_Link (After, Node);
      Nodes.Table (Node).In_List := True;
   end Insert_After;

   -----------------------
   -- Insert_List_After --
   -----------------------

   procedure Insert_List_After (After : Node_Id; List : List_Id) is
   begin
      pragma Debug (Dcheck_Is_List (List));
      pragma Debug (Dcheck_List_Member (After));

      if Debug_Flag_N then
         Write_String ("Insert list ");
         Write_Int (Int (List));
         Write_String (" after node ");
         Write_Int (Int (After));
         Write_Eol;
      end if;

      if Is_Empty_List (List) then
         return;

      else
         if Is_At_End_Of_List (After) then
            Set_Last (List_Containing (After), Last (List));
            Set_List_Link (Last (List), List_Link (After));
         else
            Set_Node_Link (Last (List), Node_Link (After));
         end if;

         Set_Node_Link (After, First (List));

         Set_First (List, Empty);
         Set_Last (List, Empty);
      end if;
   end Insert_List_After;

   -------------------
   -- Insert_Before --
   -------------------

   procedure Insert_Before (Before : Node_Id; Node : Node_Id) is
      L : List_Id;
      N : Node_Id;

   begin
      pragma Debug (Dcheck_Not_List_Member (Node));
      pragma Debug (Dcheck_List_Member (Before));

      if Node = Error then
         return;
      end if;

      if Debug_Flag_N then
         Write_String ("Insert node");
         Write_Int (Int (Node));
         Write_String (" before node ");
         Write_Int (Int (Before));
         Write_Eol;
      end if;

      L := List_Containing (Before);

      if First (L) = Before then
         Set_First (L, Node);

      else
         N := First (L);

         while Node_Link (N) /= Before loop
            N := Node_Link (N);
         end loop;

         Set_Node_Link (N, Node);
      end if;

      Set_Node_Link (Node, Before);
      Nodes.Table (Node).In_List := True;
   end Insert_Before;

   ------------------------
   -- Insert_List_Before --
   ------------------------

   procedure Insert_List_Before (Before : Node_Id; List : List_Id) is
      L : List_Id;
      N : Node_Id;

   begin
      pragma Debug (Dcheck_Is_List (List));
      pragma Debug (Dcheck_List_Member (Before));

      if Debug_Flag_N then
         Write_String ("Insert list ");
         Write_Int (Int (List));
         Write_String (" before node ");
         Write_Int (Int (Before));
         Write_Eol;
      end if;

      if Is_Empty_List (List) then
         return;

      else
         L := List_Containing (Before);

         if First (L) = Before then
            Set_First (L, First (List));

         else
            N := First (L);

            while Node_Link (N) /= Before loop
               N := Node_Link (N);
            end loop;

            Set_Node_Link (N, First (List));
         end if;

         Set_Node_Link (Last (List), Before);
      end if;
   end Insert_List_Before;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (Node : Node_Id; To : List_Id) is
   begin
      if Is_Empty_List (To) then
         Append (Node, To);
      else
         Insert_Before (First (To), Node);
      end if;
   end Prepend;

   ----------------
   -- Prepend_To --
   ----------------

   procedure Prepend_To (To : List_Id; Node : Node_Id) is
   begin
      Prepend (Node, To);
   end Prepend_To;

   ------------
   -- Remove --
   ------------

   procedure Remove (Node : Node_Id) is
      L : List_Id;
      N : Node_Id;

   begin
      L := List_Containing (Node);

      if Debug_Flag_N then
         Write_String ("Remove node ");
         Write_Int (Int (Node));
         Write_Eol;
      end if;

      if First (L) = Node then
         if Is_At_End_Of_List (Node) then
            Set_Last (L, Empty);
            Set_First (L, Empty);
         else
            Set_First (L, Node_Link (Node));
         end if;

      else
         N := First (L);

         while Node_Link (N) /= Node loop
            N := Node_Link (N);
         end loop;

         if Is_At_End_Of_List (Node) then
            Set_Last (L, N);
            Set_List_Link (N, List_Link (Node));
         else
            Set_Node_Link (N, Node_Link (Node));
         end if;
      end if;

      Set_Node_Link (Node, Empty);
      Nodes.Table (Node).In_List := False;
   end Remove;

   -----------------
   -- Remove_Head --
   -----------------

   function Remove_Head (List : List_Id) return Node_Id is
      N : Node_Id;

   begin
      pragma Debug (Dcheck_Is_List (List));

      if Debug_Flag_N then
         Write_String ("Remove head of list ");
         Write_Int (Int (List));
         Write_Eol;
      end if;

      N := First (List);

      if N = Empty then
         return Empty;

      else
         if Is_At_End_Of_List (N) then
            Set_Last  (List, Empty);
            Set_First (List, Empty);
         else
            Set_First (List, Node_Link (N));
         end if;

         Set_Node_Link (N, Empty);
         Nodes.Table (N).In_List := False;
         return N;
      end if;
   end Remove_Head;

   -----------------
   -- Remove_Next --
   -----------------

   function Remove_Next (Node : Node_Id) return Node_Id is
      Nxt : Node_Id;

   begin
      Nxt := Next (Node);

      if Nxt /= Empty then
         Nodes.Table (Node).Link := Nodes.Table (Nxt).Link;
         Set_Node_Link (Nxt, Empty);
         Nodes.Table (Nxt).In_List := False;
      end if;

      return Nxt;
   end Remove_Next;

   -----------------
   -- Delete_Node --
   -----------------

   procedure Delete_Node (Node : Node_Id) is
   begin
      if Is_List_Member (Node) then
         Remove (Node);
      end if;

      if Debug_Flag_N then                            
         Write_String ("Delete node ");               
         Write_Int (Int (Node));                      
         Write_Eol;                                   
      end if;                                         

      Nodes.Table (Node)       := Default_Node;
      Nodes.Table (Node).Nkind := N_Unused_At_Start;

      --  Note: for now, we are not bothering to reuse deleted nodes

   end Delete_Node;

   ----------
   -- L_Id --
   ----------

   function L_Id (Id : Elist_Id) return List_Id is
   begin
      return List_Id (Int (Id) - Elist_Bias);
   end L_Id;

   function L_Id (Id : Elmt_Id) return List_Id is
   begin
      return List_Id (Int (Id) - Elmt_Bias);
   end L_Id;

   -----------
   -- Id_Of --
   -----------

   function Id_Of (Elmt : Elmt_Id) return Node_Id is
   begin
      return Lists.Table (L_Id (Elmt)).Lnode;
   end Id_Of;

   -------------------
   -- New_Elmt_List --
   -------------------

   function New_Elmt_List return Elist_Id is
      E : Elist_Id;
   begin
      Lists.Increment_Last;
      E := Elist_Id (Lists.Last + Elist_Bias);
      Set_Parent (E, Empty);
      Set_First_Elmt (E, No_Elmt);
      Set_Last_Elmt (E, No_Elmt);

      if Debug_Flag_N then
         Write_String ("Allocate new element list, returned ID = ");
         Write_Int (Int (E));
         Write_Eol;
      end if;

      return E;
   end New_Elmt_List;

   ------------------------
   -- New_Elmt_List_Copy --
   ------------------------

   function New_Elmt_List_Copy (List : Elist_Id) return Elist_Id is
      NL : Elist_Id := New_Elmt_List;
      E  : Elmt_Id := First_Elmt (List);

   begin
      if List = No_Elist then
         return No_Elist;

      else
         NL := New_Elmt_List;
         E := First_Elmt (List);

         while E /= No_Elmt loop
            Append_Elmt (New_Copy (Id_Of (E)), NL);
            E := Next_Elmt (E);
         end loop;

         return NL;
      end if;
   end New_Elmt_List_Copy;

   ----------------
   -- First_Elmt --
   ----------------

   function First_Elmt (List : Elist_Id) return Elmt_Id is
   begin
      return Elmt_Id (Lists.Table (L_Id (List)).Lfield1);
   end First_Elmt;

   --------------------
   -- Set_First_Elmt --
   --------------------

   procedure Set_First_Elmt (List : Elist_Id; Val : Elmt_Id) is
   begin
      Lists.Table (L_Id (List)).Lfield1 := Int (Val);
   end Set_First_Elmt;

   -------------------
   -- Is_First_Elmt --
   -------------------

   function Is_First_Elmt (Elmt : Elmt_Id) return Boolean is
   begin
      return Lists.Table (L_Id (Elmt)).Lfield1 in Elist_Range;
   end Is_First_Elmt;

   --------------------------------
   -- Get_Header_From_First_Elmt --
   --------------------------------

   function Get_Header_From_First_Elmt (Elmt : Elmt_Id) return Elist_Id is
   begin
      return Elist_Id (Lists.Table (L_Id (Elmt)).Lfield1);
   end Get_Header_From_First_Elmt;

   ---------------
   -- Last_Elmt --
   ---------------

   function Last_Elmt (List : Elist_Id) return Elmt_Id is
   begin
      return Elmt_Id (Lists.Table (L_Id (List)).Lfield2);
   end Last_Elmt;

   -------------------
   -- Set_Last_Elmt --
   -------------------

   procedure Set_Last_Elmt (List : Elist_Id; Val : Elmt_Id) is
   begin
      Lists.Table (L_Id (List)).Lfield2 := Int (Val);
   end Set_Last_Elmt;

   -------------------
   -- Is_Last_Elmt --
   -------------------

   function Is_Last_Elmt (Elmt : Elmt_Id) return Boolean is
   begin
      return Lists.Table (L_Id (Elmt)).Lfield2 in Elist_Range;
   end Is_Last_Elmt;

   -------------------------------
   -- Get_Header_From_Last_Elmt --
   -------------------------------

   function Get_Header_From_Last_Elmt (Elmt : Elmt_Id) return Elist_Id is
   begin
      return Elist_Id (Lists.Table (L_Id (Elmt)).Lfield2);
   end Get_Header_From_Last_Elmt;

   --------------
   -- Nth_Elmt --
   --------------

   function Nth_Elmt (List : Elist_Id; Index : Pos) return Elmt_Id is
      Result : Elmt_Id;
      Count : Int := 1;

   begin
      Result := First_Elmt (List);
      while Count < Index and then Result /= No_Elmt loop
         Result := Next_Elmt (Result);
      end loop;
      return Result;
   end Nth_Elmt;

   ---------------
   -- Next_Elmt --
   ---------------

   function Next_Elmt (Elmt : Elmt_Id) return Elmt_Id is
   begin
      if Is_Last_Elmt (Elmt) then
         return No_Elmt;
      else
         return Elmt_Id (Lists.Table (L_Id (Elmt)).Lfield2);
      end if;
   end Next_Elmt;

   -------------------
   -- Set_Next_Elmt --
   -------------------

   procedure Set_Next_Elmt (Elmt : Elmt_Id; Val : Elmt_Id) is
   begin
      Lists.Table (L_Id (Elmt)).Lfield2 := Int (Val);
   end Set_Next_Elmt;

   procedure Set_Next_Elmt (Elmt : Elmt_Id; Val : Elist_Id) is
   begin
      Lists.Table (L_Id (Elmt)).Lfield2 := Int (Val);
   end Set_Next_Elmt;

   ---------------
   -- Prev_Elmt --
   ---------------

   function Prev_Elmt (Elmt : Elmt_Id) return Elmt_Id is
   begin
      if Is_First_Elmt (Elmt) then
         return No_Elmt;
      else
         return Elmt_Id (Lists.Table (L_Id (Elmt)).Lfield1);
      end if;
   end Prev_Elmt;

   -------------------
   -- Set_Prev_Elmt --
   -------------------

   procedure Set_Prev_Elmt (Elmt : Elmt_Id; Val : Elmt_Id) is
   begin
      Lists.Table (L_Id (Elmt)).Lfield1 := Int (Val);
   end Set_Prev_Elmt;

   procedure Set_Prev_Elmt (Elmt : Elmt_Id; Val : Elist_Id) is
   begin
      Lists.Table (L_Id (Elmt)).Lfield1 := Int (Val);
   end Set_Prev_Elmt;

   --------------------
   -- Set_Elmt_Links --
   --------------------

   procedure Set_Elmt_Links (List : Elist_Id; First_Elmt : Elmt_Id) is
   begin
      Set_First_Elmt (List, First_Elmt);
      Set_Prev_Elmt (First_Elmt, List);
   end Set_Elmt_Links;

   procedure Set_Elmt_Links (Elmt1 : Elmt_Id; Elmt2 : Elmt_Id) is
   begin
      Set_Next_Elmt (Elmt1, Elmt2);
      Set_Prev_Elmt (Elmt2, Elmt1);
   end Set_Elmt_Links;

   procedure Set_Elmt_Links (Last_Elmt : Elmt_Id; List : Elist_Id) is
   begin
      Set_Next_Elmt (Last_Elmt, List);
      Set_Last_Elmt (List, Last_Elmt);
   end Set_Elmt_Links;

   ------------------------
   -- Is_Empty_Elmt_List --
   ------------------------

   function Is_Empty_Elmt_List (L : Elist_Id) return Boolean is
   begin
      return First_Elmt (L) = No_Elmt;
   end Is_Empty_Elmt_List;

   ----------------------------
   -- Is_Non_Empty_Elmt_List --
   ----------------------------

   function Is_Non_Empty_Elmt_List (L : Elist_Id) return Boolean is
   begin
      return First_Elmt (L) /= No_Elmt;
   end Is_Non_Empty_Elmt_List;

   --------------
   -- New_Elmt --
   --------------

   function New_Elmt (Node : Node_Id) return Elmt_Id is
   begin
      Lists.Increment_Last;
      Lists.Table (Lists.Last).Lnode := Node;
      return Elmt_Id (Lists.Last + Elmt_Bias);
   end New_Elmt;

   -----------------
   -- Append_Elmt --
   -----------------

   procedure Append_Elmt (Node : Node_Id; To : Elist_Id) is
      E : Elmt_Id;

   begin
      E := New_Elmt (Node);

      if Is_Empty_Elmt_List (To) then
         Set_Elmt_Links (To, E);
      else
         Set_Elmt_Links (Last_Elmt (To), E);
      end if;

      Set_Elmt_Links (E, To);
   end Append_Elmt;

   ----------------------
   -- Append_Elmt_List --
   ----------------------

   procedure Append_Elmt_List (List : Elist_Id; To : Elist_Id) is
   begin
      if Is_Empty_Elmt_List (List) then
         return;

      else
         if Is_Empty_Elmt_List (To) then
            Set_Elmt_Links (To, First_Elmt (List));
            Set_Elmt_Links (Last_Elmt (List), To);

         else
            Set_Elmt_Links (Last_Elmt (To), First_Elmt (List));
            Set_Elmt_Links (Last_Elmt (List), To);
         end if;

         Set_First_Elmt (List, No_Elmt);
         Set_Last_Elmt  (List, No_Elmt);
      end if;
   end Append_Elmt_List;

   -----------------------
   -- Insert_Elmt_After --
   -----------------------

   procedure Insert_Elmt_After (After : Elmt_Id; Node : Node_Id) is
      E : Elmt_Id;

   begin
      E := New_Elmt (Node);

      if Is_Last_Elmt (After) then
         Set_Elmt_Links (E, Get_Header_From_Last_Elmt (After));
      else
         Set_Elmt_Links (E, Next_Elmt (After));
      end if;

      Set_Elmt_Links (After, E);
   end Insert_Elmt_After;

   ----------------------------
   -- Insert_Elmt_List_After --
   ----------------------------

   procedure Insert_Elmt_List_After (After : Elmt_Id; List : Elist_Id) is
   begin
      if Is_Empty_Elmt_List (List) then
         return;

      else
         if Is_Last_Elmt (After) then
            Set_Elmt_Links
               (Last_Elmt (List), Get_Header_From_Last_Elmt (After));
         else
            Set_Elmt_Links (Last_Elmt (List), Next_Elmt (After));
         end if;

         Set_Elmt_Links (After, First_Elmt (List));

         Set_First_Elmt (List, No_Elmt);
         Set_Last_Elmt  (List, No_Elmt);
      end if;
   end Insert_Elmt_List_After;

   ------------------------
   -- Insert_Elmt_Before --
   ------------------------

   procedure Insert_Elmt_Before (Before : Elmt_Id; Node : Node_Id) is
      E : Elmt_Id;

   begin
      E := New_Elmt (Node);

      if Is_First_Elmt (Before) then
         Set_Elmt_Links (Get_Header_From_First_Elmt (Before), E);
      else
         Set_Elmt_Links (Prev_Elmt (Before), E);
      end if;

      Set_Elmt_Links (E, Before);
   end Insert_Elmt_Before;

   -----------------------------
   -- Insert_Elmt_List_Before --
   -----------------------------

   procedure Insert_Elmt_List_Before (Before : Elmt_Id; List : Elist_Id) is
   begin
      if Is_Empty_Elmt_List (List) then
         return;

      else
         if Is_First_Elmt (Before) then
            Set_Elmt_Links
               (Get_Header_From_First_Elmt (Before), First_Elmt (List));
         else
            Set_Elmt_Links (Prev_Elmt (Before), First_Elmt (List));
         end if;

         Set_Elmt_Links (Last_Elmt (List), Before);

         Set_First_Elmt (List, No_Elmt);
         Set_Last_Elmt  (List, No_Elmt);
      end if;
   end Insert_Elmt_List_Before;

   -----------------
   -- Remove_Elmt --
   -----------------

   procedure Remove_Elmt (Elmt : Elmt_Id) is
      L : Elist_Id;
   begin
      if Is_Last_Elmt (Elmt) then
         L := Get_Header_From_Last_Elmt (Elmt);

         if Is_First_Elmt (Elmt) then
            Set_First_Elmt (L, No_Elmt);
            Set_Last_Elmt (L, No_Elmt);

         else
            Set_Elmt_Links (Prev_Elmt (Elmt), L);
         end if;

      elsif Is_First_Elmt (Elmt) then
         L := Get_Header_From_First_Elmt (Elmt);
         Set_Elmt_Links (L, Next_Elmt (Elmt));

      else
         Set_Elmt_Links (Prev_Elmt (Elmt), Next_Elmt (Elmt));
      end if;
   end Remove_Elmt;

   ----------------------
   -- Remove_Next_Elmt --
   ----------------------

   function Remove_Next_Elmt (Elmt : Elmt_Id) return Node_Id is
      Nxt : Elmt_Id;
   begin
      Nxt := Next_Elmt (Elmt);

      if Nxt = No_Elmt then
         return Empty;

      else
         Lists.Table (L_Id (Elmt)).Lfield2 :=
           Lists.Table (L_Id (Nxt)).Lfield2;
         return Id_Of (Nxt);
      end if;
   end Remove_Next_Elmt;

   ----------------------
   -- Remove_Head_Elmt --
   ----------------------

   function Remove_Head_Elmt (List : Elist_Id) return Node_Id is
      N : Node_Id;
   begin
      if Is_Empty_Elmt_List (List) then
         return Empty;
      else
         N := Id_Of (First_Elmt (List));
         Set_Elmt_Links (List, Next_Elmt (First_Elmt (List)));
      end if;
   end Remove_Head_Elmt;

   ----------------------------------
   -- Basic Field Access Functions --
   ----------------------------------

   function Nkind (N : Node_Id) return Node_Kind is
   begin
      pragma Debug (Dcheck_Is_Node (N));
      return Nodes.Table (N).Nkind;
   end Nkind;

   function Ekind (N : Node_Id) return Entity_Kind is
   begin
      pragma Debug (Dcheck_Is_Extended_Node (N));
      return N_To_E (Nodes.Table (N + 1).Nkind);
   end Ekind;

   function Sloc (N : Node_Id) return Source_Ptr is
   begin
      pragma Debug (Dcheck_Is_Node (N));
      return Nodes.Table (N).Sloc;
   end Sloc;

   function Parent (N : Node_Id) return Node_Id is
   begin
      pragma Debug (Dcheck_Is_Node (N));

      if Is_List_Member (N) then
         return List_Parent (List_Containing (N));
      else
         return Node_Id (Nodes.Table (N).Link);
      end if;
   end Parent;

   function List_Parent (L : List_Id) return Node_Id is
   begin
      Dcheck_Is_List (L);
      return Lists.Table (L).Lnode;
   end List_Parent;

   function Elist_Parent (E : Elist_Id) return Node_Id is
   begin
      Dcheck_Is_Elist (E);
      return Lists.Table (L_Id (E)).Lnode;
   end Elist_Parent;

   --------
   -- No --
   --------

   function No (N : Node_Id) return Boolean is
   begin
      return N = Empty;
   end No;

   -------------
   -- Present --
   -------------

   function Present (N : Node_Id) return Boolean is
   begin
      return N /= Empty;
   end Present;

   ------------------
   -- List_Present --
   ------------------

   function List_Present (L : List_Id) return Boolean is
   begin
      return L /= No_List;
   end List_Present;

   --------------------------
   -- Field Set Procedures --
   --------------------------

   procedure Set_Ekind (N : Node_Id; Val : Entity_Kind) is
   begin
      pragma Debug (Dcheck_Is_Extended_Node (N));
      Nodes.Table (N + 1).Nkind := E_To_N (Val);
   end Set_Ekind;

   procedure Set_Sloc (N : Node_Id; Val : Source_Ptr) is
   begin
      pragma Debug (Dcheck_Is_Node (N));
      Nodes.Table (N).Sloc := Val;
   end Set_Sloc;

   procedure Set_Parent (N : Node_Id; Val : Node_Id) is
   begin
      pragma Debug (Dcheck_Is_Node (N));
      pragma Debug (Dcheck_Not_List_Member (N));
      Nodes.Table (N).Link := Int (Val);
   end Set_Parent;

   procedure Set_Parent (L : List_Id; Val : Node_Id) is
   begin
      pragma Debug (Dcheck_Is_List (L));
      Lists.Table (L).Lnode := Val;
   end Set_Parent;

   procedure Set_Parent (E : Elist_Id; Val : Node_Id) is
   begin
      pragma Debug (Dcheck_Is_Elist (E));
      Lists.Table (L_Id (E)).Lnode := Val;
   end Set_Parent;

   ----------------------------
   -- Mark_Rewrite_Insertion --
   ----------------------------

   procedure Mark_Rewrite_Insertion (New_Node : Node_Id) is
   begin
      Nodes.Table (New_Node).Rewrite_Ins := True;
   end Mark_Rewrite_Insertion;

   --------------------------
   -- Is_Rewrite_Insertion --
   --------------------------

   function Is_Rewrite_Insertion (Node : Node_Id) return Boolean is
   begin
      return Nodes.Table (Node).Rewrite_Ins;
   end Is_Rewrite_Insertion;

   -----------------------------
   -- Rewrite_Substitute_Tree --
   -----------------------------

   procedure Rewrite_Substitute_Tree (Old_Node, New_Node : Node_Id) is
   begin
      TS.Set_Substitute_Tree (Old_Node, New_Node);
   end Rewrite_Substitute_Tree;

   -----------------------------
   -- Is_Rewrite_Substitution --
   -----------------------------

   function Is_Rewrite_Substitution (Node : Node_Id) return Boolean is
   begin
      return Nodes.Table (Node).Rewrite_Sub;
   end Is_Rewrite_Substitution;

   -------------------
   -- Original_Node --
   -------------------

   function Original_Node (Node : Node_Id) return Node_Id is
   begin
      if Nodes.Table (Node).Rewrite_Ins then
         return Empty;
      elsif Nodes.Table (Node).Rewrite_Sub then
         return TS.Get_Substitute_Tree (Node);
      else
         return Node;
      end if;
   end Original_Node;

   ------------------------------
   -- Unchecked Access Package --
   ------------------------------

   package body Unchecked_Access is

      function Field1 (N : Node_Id) return Int is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Field1;
      end Field1;

      function Field2 (N : Node_Id) return Int is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Field2;
      end Field2;

      function Field3 (N : Node_Id) return Int is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Field3;
      end Field3;

      function Field4 (N : Node_Id) return Int is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Field4;
      end Field4;

      function Field5 (N : Node_Id) return Int is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Field5;
      end Field5;

      function Field6 (N : Node_Id) return Int is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Field6;
      end Field6;

      function Field7 (N : Node_Id) return Int is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Field7;
      end Field7;

      function Field8 (N : Node_Id) return Int is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Field8;
      end Field8;

      function Field9 (N : Node_Id) return Int is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Field9;
      end Field9;

      function Field10 (N : Node_Id) return Int is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Field10;
      end Field10;

      function Field11 (N : Node_Id) return Int is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Field11;
      end Field11;

      function Field12 (N : Node_Id) return Int is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Field12;
      end Field12;

      function Node1 (N : Node_Id) return Node_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Node_Id (Nodes.Table (N).Field1);
      end Node1;

      function Node2 (N : Node_Id) return Node_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Node_Id (Nodes.Table (N).Field2);
      end Node2;

      function Node3 (N : Node_Id) return Node_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Node_Id (Nodes.Table (N).Field3);
      end Node3;

      function Node4 (N : Node_Id) return Node_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Node_Id (Nodes.Table (N).Field4);
      end Node4;

      function Node5 (N : Node_Id) return Node_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Node_Id (Nodes.Table (N).Field5);
      end Node5;

      function Node6 (N : Node_Id) return Node_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Node_Id (Nodes.Table (N + 1).Field6);
      end Node6;

      function Node7 (N : Node_Id) return Node_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Node_Id (Nodes.Table (N + 1).Field7);
      end Node7;

      function Node8 (N : Node_Id) return Node_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Node_Id (Nodes.Table (N + 1).Field8);
      end Node8;

      function Node9 (N : Node_Id) return Node_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Node_Id (Nodes.Table (N + 1).Field9);
      end Node9;

      function Node10 (N : Node_Id) return Node_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Node_Id (Nodes.Table (N + 1).Field10);
      end Node10;

      function Node11 (N : Node_Id) return Node_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Node_Id (Nodes.Table (N + 1).Field11);
      end Node11;

      function Node12 (N : Node_Id) return Node_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Node_Id (Nodes.Table (N + 1).Field12);
      end Node12;

      function List1 (N : Node_Id) return List_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return List_Id (Nodes.Table (N).Field1);
      end List1;

      function List2 (N : Node_Id) return List_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return List_Id (Nodes.Table (N).Field2);
      end List2;

      function List3 (N : Node_Id) return List_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return List_Id (Nodes.Table (N).Field3);
      end List3;

      function List4 (N : Node_Id) return List_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return List_Id (Nodes.Table (N).Field4);
      end List4;

      function List5 (N : Node_Id) return List_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return List_Id (Nodes.Table (N).Field5);
      end List5;

      function List6 (N : Node_Id) return List_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return List_Id (Nodes.Table (N + 1).Field6);
      end List6;

      function List7 (N : Node_Id) return List_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return List_Id (Nodes.Table (N + 1).Field7);
      end List7;

      function List8 (N : Node_Id) return List_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return List_Id (Nodes.Table (N + 1).Field8);
      end List8;

      function List9 (N : Node_Id) return List_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return List_Id (Nodes.Table (N + 1).Field9);
      end List9;

      function List10 (N : Node_Id) return List_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return List_Id (Nodes.Table (N + 1).Field10);
      end List10;

      function List11 (N : Node_Id) return List_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return List_Id (Nodes.Table (N + 1).Field11);
      end List11;

      function List12 (N : Node_Id) return List_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return List_Id (Nodes.Table (N + 1).Field12);
      end List12;

      function Elist1 (N : Node_Id) return Elist_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Elist_Id (Nodes.Table (N).Field1);
      end Elist1;

      function Elist2 (N : Node_Id) return Elist_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Elist_Id (Nodes.Table (N).Field2);
      end Elist2;

      function Elist3 (N : Node_Id) return Elist_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Elist_Id (Nodes.Table (N).Field3);
      end Elist3;

      function Elist4 (N : Node_Id) return Elist_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Elist_Id (Nodes.Table (N).Field4);
      end Elist4;

      function Elist5 (N : Node_Id) return Elist_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Elist_Id (Nodes.Table (N).Field5);
      end Elist5;

      function Elist6 (N : Node_Id) return Elist_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Elist_Id (Nodes.Table (N + 1).Field6);
      end Elist6;

      function Elist7 (N : Node_Id) return Elist_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Elist_Id (Nodes.Table (N + 1).Field7);
      end Elist7;

      function Elist8 (N : Node_Id) return Elist_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Elist_Id (Nodes.Table (N + 1).Field8);
      end Elist8;

      function Elist9 (N : Node_Id) return Elist_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Elist_Id (Nodes.Table (N + 1).Field9);
      end Elist9;

      function Elist10 (N : Node_Id) return Elist_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Elist_Id (Nodes.Table (N + 1).Field10);
      end Elist10;

      function Elist11 (N : Node_Id) return Elist_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Elist_Id (Nodes.Table (N + 1).Field11);
      end Elist11;

      function Elist12 (N : Node_Id) return Elist_Id is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Elist_Id (Nodes.Table (N + 1).Field12);
      end Elist12;

      function Name1 (N : Node_Id) return Name_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Name_Id (Nodes.Table (N).Field1);
      end Name1;

      function Name2 (N : Node_Id) return Name_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Name_Id (Nodes.Table (N).Field2);
      end Name2;

      function Name3 (N : Node_Id) return Name_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Name_Id (Nodes.Table (N).Field3);
      end Name3;

      function Name4 (N : Node_Id) return Name_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Name_Id (Nodes.Table (N).Field4);
      end Name4;

      function Name5 (N : Node_Id) return Name_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Name_Id (Nodes.Table (N).Field5);
      end Name5;

      function Str1 (N : Node_Id) return String_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return String_Id (Nodes.Table (N).Field1);
      end Str1;

      function Str2 (N : Node_Id) return String_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return String_Id (Nodes.Table (N).Field2);
      end Str2;

      function Str3 (N : Node_Id) return String_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return String_Id (Nodes.Table (N).Field3);
      end Str3;

      function Str4 (N : Node_Id) return String_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return String_Id (Nodes.Table (N).Field4);
      end Str4;

      function Str5 (N : Node_Id) return String_Id is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return String_Id (Nodes.Table (N).Field5);
      end Str5;

      function Char_Code1 (N : Node_Id) return Char_Code is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Char_Code (Nodes.Table (N).Field1 - Char_Code_Bias);
      end Char_Code1;

      function Char_Code2 (N : Node_Id) return Char_Code is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Char_Code (Nodes.Table (N).Field2 - Char_Code_Bias);
      end Char_Code2;

      function Char_Code3 (N : Node_Id) return Char_Code is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Char_Code (Nodes.Table (N).Field3 - Char_Code_Bias);
      end Char_Code3;

      function Char_Code4 (N : Node_Id) return Char_Code is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Char_Code (Nodes.Table (N).Field4 - Char_Code_Bias);
      end Char_Code4;

      function Char_Code5 (N : Node_Id) return Char_Code is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Char_Code (Nodes.Table (N).Field5 - Char_Code_Bias);
      end Char_Code5;

      function Uint1 (N : Node_Id) return Uint is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Uint (Nodes.Table (N).Field1);
      end Uint1;

      function Uint2 (N : Node_Id) return Uint is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Uint (Nodes.Table (N).Field2);
      end Uint2;

      function Uint3 (N : Node_Id) return Uint is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Uint (Nodes.Table (N).Field3);
      end Uint3;

      function Uint4 (N : Node_Id) return Uint is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Uint (Nodes.Table (N).Field4);
      end Uint4;

      function Uint5 (N : Node_Id) return Uint is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Uint (Nodes.Table (N).Field5);
      end Uint5;

      function Uint6 (N : Node_Id) return Uint is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Uint (Nodes.Table (N + 1).Field6);
      end Uint6;

      function Uint7 (N : Node_Id) return Uint is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Uint (Nodes.Table (N + 1).Field7);
      end Uint7;

      function Uint8 (N : Node_Id) return Uint is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Uint (Nodes.Table (N + 1).Field8);
      end Uint8;

      function Uint9 (N : Node_Id) return Uint is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Uint (Nodes.Table (N + 1).Field9);
      end Uint9;

      function Uint10 (N : Node_Id) return Uint is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Uint (Nodes.Table (N + 1).Field10);
      end Uint10;

      function Uint11 (N : Node_Id) return Uint is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Uint (Nodes.Table (N + 1).Field11);
      end Uint11;

      function Uint12 (N : Node_Id) return Uint is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Uint (Nodes.Table (N + 1).Field12);
      end Uint12;

      function Flag1 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag1;
      end Flag1;

      function Flag2 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag2;
      end Flag2;

      function Flag3 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag3;
      end Flag3;

      function Flag4 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag4;
      end Flag4;

      function Flag5 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag5;
      end Flag5;

      function Flag6 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag6;
      end Flag6;

      function Flag7 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag7;
      end Flag7;

      function Flag8 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag8;
      end Flag8;

      function Flag9 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag9;
      end Flag9;

      function Flag10 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag10;
      end Flag10;

      function Flag11 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag11;
      end Flag11;

      function Flag12 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag12;
      end Flag12;

      function Flag13 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag13;
      end Flag13;

      function Flag14 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag14;
      end Flag14;

      function Flag15 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag15;
      end Flag15;

      function Flag16 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag16;
      end Flag16;

      function Flag17 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag17;
      end Flag17;

      function Flag18 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag18;
      end Flag18;

      function Flag19 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag19;
      end Flag19;

      function Flag20 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         return Nodes.Table (N).Flag20;
      end Flag20;

      function Flag21 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).In_List;
      end Flag21;

      function Flag22 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Rewrite_Sub;
      end Flag22;

      function Flag23 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Rewrite_Ins;
      end Flag23;

      function Flag24 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag1;
      end Flag24;

      function Flag25 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag2;
      end Flag25;

      function Flag26 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag3;
      end Flag26;

      function Flag27 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag4;
      end Flag27;

      function Flag28 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag5;
      end Flag28;

      function Flag29 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag6;
      end Flag29;

      function Flag30 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag7;
      end Flag30;

      function Flag31 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag8;
      end Flag31;

      function Flag32 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag9;
      end Flag32;

      function Flag33 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag10;
      end Flag33;

      function Flag34 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag11;
      end Flag34;

      function Flag35 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag12;
      end Flag35;

      function Flag36 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag13;
      end Flag36;

      function Flag37 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag14;
      end Flag37;

      function Flag38 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag15;
      end Flag38;

      function Flag39 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag16;
      end Flag39;

      function Flag40 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag17;
      end Flag40;

      function Flag41 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag18;
      end Flag41;

      function Flag42 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag19;
      end Flag42;

      function Flag43 (N : Node_Id) return Boolean is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         return Nodes.Table (N + 1).Flag20;
      end Flag43;

      procedure Set_Nkind (N : Node_Id; Val : Node_Kind) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Nkind := Val;
      end Set_Nkind;

      procedure Set_Field1 (N : Node_Id; Val : Int) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field1 := Val;
      end Set_Field1;

      procedure Set_Field2 (N : Node_Id; Val : Int) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field2 := Val;
      end Set_Field2;

      procedure Set_Field3 (N : Node_Id; Val : Int) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field3 := Val;
      end Set_Field3;

      procedure Set_Field4 (N : Node_Id; Val : Int) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field4 := Val;
      end Set_Field4;

      procedure Set_Field5 (N : Node_Id; Val : Int) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field5 := Val;
      end Set_Field5;

      procedure Set_Field6 (N : Node_Id; Val : Int) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field6 := Val;
      end Set_Field6;

      procedure Set_Field7 (N : Node_Id; Val : Int) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field7 := Val;
      end Set_Field7;

      procedure Set_Field8 (N : Node_Id; Val : Int) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field8 := Val;
      end Set_Field8;

      procedure Set_Field9 (N : Node_Id; Val : Int) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field9 := Val;
      end Set_Field9;

      procedure Set_Field10 (N : Node_Id; Val : Int) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field10 := Val;
      end Set_Field10;

      procedure Set_Field11 (N : Node_Id; Val : Int) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field11 := Val;
      end Set_Field11;

      procedure Set_Field12 (N : Node_Id; Val : Int) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field12 := Val;
      end Set_Field12;

      procedure Set_Node1 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field1 := Int (Val);
      end Set_Node1;

      procedure Set_Node2 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field2 := Int (Val);
      end Set_Node2;

      procedure Set_Node3 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field3 := Int (Val);
      end Set_Node3;

      procedure Set_Node4 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field4 := Int (Val);
      end Set_Node4;

      procedure Set_Node5 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field5 := Int (Val);
      end Set_Node5;

      procedure Set_Node6 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field6 := Int (Val);
      end Set_Node6;

      procedure Set_Node7 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field7 := Int (Val);
      end Set_Node7;

      procedure Set_Node8 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field8 := Int (Val);
      end Set_Node8;

      procedure Set_Node9 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field9 := Int (Val);
      end Set_Node9;

      procedure Set_Node10 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field10 := Int (Val);
      end Set_Node10;

      procedure Set_Node11 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field11 := Int (Val);
      end Set_Node11;

      procedure Set_Node12 (N : Node_Id; Val : Node_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field12 := Int (Val);
      end Set_Node12;

      procedure Set_List1 (N : Node_Id; Val : List_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field1 := Int (Val);
      end Set_List1;

      procedure Set_List2 (N : Node_Id; Val : List_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field2 := Int (Val);
      end Set_List2;

      procedure Set_List3 (N : Node_Id; Val : List_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field3 := Int (Val);
      end Set_List3;

      procedure Set_List4 (N : Node_Id; Val : List_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field4 := Int (Val);
      end Set_List4;

      procedure Set_List5 (N : Node_Id; Val : List_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field5 := Int (Val);
      end Set_List5;

      procedure Set_List6 (N : Node_Id; Val : List_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field6 := Int (Val);
      end Set_List6;

      procedure Set_List7 (N : Node_Id; Val : List_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field7 := Int (Val);
      end Set_List7;

      procedure Set_List8 (N : Node_Id; Val : List_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field8 := Int (Val);
      end Set_List8;

      procedure Set_List9 (N : Node_Id; Val : List_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field9 := Int (Val);
      end Set_List9;

      procedure Set_List10 (N : Node_Id; Val : List_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field10 := Int (Val);
      end Set_List10;

      procedure Set_List11 (N : Node_Id; Val : List_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field11 := Int (Val);
      end Set_List11;

      procedure Set_List12 (N : Node_Id; Val : List_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field12 := Int (Val);
      end Set_List12;

      procedure Set_Elist1 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field1 := Int (Val);
      end Set_Elist1;

      procedure Set_Elist2 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field2 := Int (Val);
      end Set_Elist2;

      procedure Set_Elist3 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field3 := Int (Val);
      end Set_Elist3;

      procedure Set_Elist4 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field4 := Int (Val);
      end Set_Elist4;

      procedure Set_Elist5 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field5 := Int (Val);
      end Set_Elist5;

      procedure Set_Elist6 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field6 := Int (Val);
      end Set_Elist6;

      procedure Set_Elist7 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field7 := Int (Val);
      end Set_Elist7;

      procedure Set_Elist8 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field8 := Int (Val);
      end Set_Elist8;

      procedure Set_Elist9 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field9 := Int (Val);
      end Set_Elist9;

      procedure Set_Elist10 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field10 := Int (Val);
      end Set_Elist10;

      procedure Set_Elist11 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field11 := Int (Val);
      end Set_Elist11;

      procedure Set_Elist12 (N : Node_Id; Val : Elist_Id) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field12 := Int (Val);
      end Set_Elist12;

      procedure Set_Name1 (N : Node_Id; Val : Name_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field1 := Int (Val);
      end Set_Name1;

      procedure Set_Name2 (N : Node_Id; Val : Name_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field2 := Int (Val);
      end Set_Name2;

      procedure Set_Name3 (N : Node_Id; Val : Name_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field3 := Int (Val);
      end Set_Name3;

      procedure Set_Name4 (N : Node_Id; Val : Name_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field4 := Int (Val);
      end Set_Name4;

      procedure Set_Name5 (N : Node_Id; Val : Name_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field5 := Int (Val);
      end Set_Name5;

      procedure Set_Str1 (N : Node_Id; Val : String_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field1 := Int (Val);
      end Set_Str1;

      procedure Set_Str2 (N : Node_Id; Val : String_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field2 := Int (Val);
      end Set_Str2;

      procedure Set_Str3 (N : Node_Id; Val : String_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field3 := Int (Val);
      end Set_Str3;

      procedure Set_Str4 (N : Node_Id; Val : String_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field4 := Int (Val);
      end Set_Str4;

      procedure Set_Str5 (N : Node_Id; Val : String_Id) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field5 := Int (Val);
      end Set_Str5;

      procedure Set_Uint1 (N : Node_Id; Val : Uint) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field1 := Int (Val);
      end Set_Uint1;

      procedure Set_Uint2 (N : Node_Id; Val : Uint) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field2 := Int (Val);
      end Set_Uint2;

      procedure Set_Uint3 (N : Node_Id; Val : Uint) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field3 := Int (Val);
      end Set_Uint3;

      procedure Set_Uint4 (N : Node_Id; Val : Uint) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field4 := Int (Val);
      end Set_Uint4;

      procedure Set_Uint5 (N : Node_Id; Val : Uint) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field5 := Int (Val);
      end Set_Uint5;

      procedure Set_Uint6 (N : Node_Id; Val : Uint) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field6 := Int (Val);
      end Set_Uint6;

      procedure Set_Uint7 (N : Node_Id; Val : Uint) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field7 := Int (Val);
      end Set_Uint7;

      procedure Set_Uint8 (N : Node_Id; Val : Uint) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field8 := Int (Val);
      end Set_Uint8;

      procedure Set_Uint9 (N : Node_Id; Val : Uint) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field9 := Int (Val);
      end Set_Uint9;

      procedure Set_Uint10 (N : Node_Id; Val : Uint) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field10 := Int (Val);
      end Set_Uint10;

      procedure Set_Uint11 (N : Node_Id; Val : Uint) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field11 := Int (Val);
      end Set_Uint11;

      procedure Set_Uint12 (N : Node_Id; Val : Uint) is
      begin
         pragma Debug (Dcheck_Is_Extended_Node (N));
         Nodes.Table (N + 1).Field12 := Int (Val);
      end Set_Uint12;

      procedure Set_Char_Code1 (N : Node_Id; Val : Char_Code) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field1 := Int (Val) + Char_Code_Bias;
      end Set_Char_Code1;

      procedure Set_Char_Code2 (N : Node_Id; Val : Char_Code) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field2 := Int (Val) + Char_Code_Bias;
      end Set_Char_Code2;

      procedure Set_Char_Code3 (N : Node_Id; Val : Char_Code) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field3 := Int (Val) + Char_Code_Bias;
      end Set_Char_Code3;

      procedure Set_Char_Code4 (N : Node_Id; Val : Char_Code) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field4 := Int (Val) + Char_Code_Bias;
      end Set_Char_Code4;

      procedure Set_Char_Code5 (N : Node_Id; Val : Char_Code) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Field5 := Int (Val) + Char_Code_Bias;
      end Set_Char_Code5;

      procedure Set_Flag1 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag1 := Val;
      end Set_Flag1;

      procedure Set_Flag2 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag2 := Val;
      end Set_Flag2;

      procedure Set_Flag3 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag3 := Val;
      end Set_Flag3;

      procedure Set_Flag4 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag4 := Val;
      end Set_Flag4;

      procedure Set_Flag5 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag5 := Val;
      end Set_Flag5;

      procedure Set_Flag6 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag6 := Val;
      end Set_Flag6;

      procedure Set_Flag7 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag7 := Val;
      end Set_Flag7;

      procedure Set_Flag8 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag8 := Val;
      end Set_Flag8;

      procedure Set_Flag9 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag9 := Val;
      end Set_Flag9;

      procedure Set_Flag10 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag10 := Val;
      end Set_Flag10;

      procedure Set_Flag11 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag11 := Val;
      end Set_Flag11;

      procedure Set_Flag12 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag12 := Val;
      end Set_Flag12;

      procedure Set_Flag13 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag13 := Val;
      end Set_Flag13;

      procedure Set_Flag14 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag14 := Val;
      end Set_Flag14;

      procedure Set_Flag15 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag15 := Val;
      end Set_Flag15;

      procedure Set_Flag16 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag16 := Val;
      end Set_Flag16;

      procedure Set_Flag17 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag17 := Val;
      end Set_Flag17;

      procedure Set_Flag18 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag18 := Val;
      end Set_Flag18;

      procedure Set_Flag19 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag19 := Val;
      end Set_Flag19;

      procedure Set_Flag20 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N).Flag20 := Val;
      end Set_Flag20;

      procedure Set_Flag21 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).In_List := Val;
      end Set_Flag21;

      procedure Set_Flag22 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Rewrite_Sub := Val;
      end Set_Flag22;

      procedure Set_Flag23 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Rewrite_Ins := Val;
      end Set_Flag23;

      procedure Set_Flag24 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag1 := Val;
      end Set_Flag24;

      procedure Set_Flag25 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag2 := Val;
      end Set_Flag25;

      procedure Set_Flag26 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag3 := Val;
      end Set_Flag26;

      procedure Set_Flag27 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag4 := Val;
      end Set_Flag27;

      procedure Set_Flag28 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag5 := Val;
      end Set_Flag28;

      procedure Set_Flag29 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag6 := Val;
      end Set_Flag29;

      procedure Set_Flag30 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag7 := Val;
      end Set_Flag30;

      procedure Set_Flag31 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag8 := Val;
      end Set_Flag31;

      procedure Set_Flag32 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag9 := Val;
      end Set_Flag32;

      procedure Set_Flag33 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag10 := Val;
      end Set_Flag33;

      procedure Set_Flag34 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag11 := Val;
      end Set_Flag34;

      procedure Set_Flag35 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag12 := Val;
      end Set_Flag35;

      procedure Set_Flag36 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag13 := Val;
      end Set_Flag36;

      procedure Set_Flag37 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag14 := Val;
      end Set_Flag37;

      procedure Set_Flag38 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag15 := Val;
      end Set_Flag38;

      procedure Set_Flag39 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag16 := Val;
      end Set_Flag39;

      procedure Set_Flag40 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag17 := Val;
      end Set_Flag40;

      procedure Set_Flag41 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag18 := Val;
      end Set_Flag41;

      procedure Set_Flag42 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag19 := Val;
      end Set_Flag42;

      procedure Set_Flag43 (N : Node_Id; Val : Boolean) is
      begin
         pragma Debug (Dcheck_Is_Node (N));
         Nodes.Table (N + 1).Flag20 := Val;
      end Set_Flag43;

      procedure Set_Node1_With_Parent (N : Node_Id; Val : Node_Id) is
      begin
         if Val > Error then Set_Parent (Val, N); end if;
         Set_Node1 (N, Val);
      end Set_Node1_With_Parent;

      procedure Set_Node2_With_Parent (N : Node_Id; Val : Node_Id) is
      begin
         if Val > Error then Set_Parent (Val, N); end if;
         Set_Node2 (N, Val);
      end Set_Node2_With_Parent;

      procedure Set_Node3_With_Parent (N : Node_Id; Val : Node_Id) is
      begin
         if Val > Error then Set_Parent (Val, N); end if;
         Set_Node3 (N, Val);
      end Set_Node3_With_Parent;

      procedure Set_Node4_With_Parent (N : Node_Id; Val : Node_Id) is
      begin
         if Val > Error then Set_Parent (Val, N); end if;
         Set_Node4 (N, Val);
      end Set_Node4_With_Parent;

      procedure Set_Node5_With_Parent (N : Node_Id; Val : Node_Id) is
      begin
         if Val > Error then Set_Parent (Val, N); end if;
         Set_Node5 (N, Val);
      end Set_Node5_With_Parent;

      procedure Set_Node6_With_Parent (N : Node_Id; Val : Node_Id) is
      begin
         if Val > Error then Set_Parent (Val, N); end if;
         Set_Node6 (N, Val);
      end Set_Node6_With_Parent;

      procedure Set_Node7_With_Parent (N : Node_Id; Val : Node_Id) is
      begin
         if Val > Error then Set_Parent (Val, N); end if;
         Set_Node7 (N, Val);
      end Set_Node7_With_Parent;

      procedure Set_Node8_With_Parent (N : Node_Id; Val : Node_Id) is
      begin
         if Val > Error then Set_Parent (Val, N); end if;
         Set_Node8 (N, Val);
      end Set_Node8_With_Parent;

      procedure Set_Node9_With_Parent (N : Node_Id; Val : Node_Id) is
      begin
         if Val > Error then Set_Parent (Val, N); end if;
         Set_Node9 (N, Val);
      end Set_Node9_With_Parent;

      procedure Set_Node10_With_Parent (N : Node_Id; Val : Node_Id) is
      begin
         if Val > Error then Set_Parent (Val, N); end if;
         Set_Node10 (N, Val);
      end Set_Node10_With_Parent;

      procedure Set_Node11_With_Parent (N : Node_Id; Val : Node_Id) is
      begin
         if Val > Error then Set_Parent (Val, N); end if;
         Set_Node11 (N, Val);
      end Set_Node11_With_Parent;

      procedure Set_Node12_With_Parent (N : Node_Id; Val : Node_Id) is
      begin
         if Val > Error then Set_Parent (Val, N); end if;
         Set_Node12 (N, Val);
      end Set_Node12_With_Parent;

      procedure Set_List1_With_Parent (N : Node_Id; Val : List_Id) is
      begin
         if Val /= No_List and then Val /= Error_List then
            Set_Parent (Val, N);
         end if;
         Set_List1 (N, Val);
      end Set_List1_With_Parent;

      procedure Set_List2_With_Parent (N : Node_Id; Val : List_Id) is
      begin
         if Val /= No_List and then Val /= Error_List then
            Set_Parent (Val, N);
         end if;
         Set_List2 (N, Val);
      end Set_List2_With_Parent;

      procedure Set_List3_With_Parent (N : Node_Id; Val : List_Id) is
      begin
         if Val /= No_List and then Val /= Error_List then
            Set_Parent (Val, N);
         end if;
         Set_List3 (N, Val);
      end Set_List3_With_Parent;

      procedure Set_List4_With_Parent (N : Node_Id; Val : List_Id) is
      begin
         if Val /= No_List and then Val /= Error_List then
            Set_Parent (Val, N);
         end if;
         Set_List4 (N, Val);
      end Set_List4_With_Parent;

      procedure Set_List5_With_Parent (N : Node_Id; Val : List_Id) is
      begin
         if Val /= No_List and then Val /= Error_List then
            Set_Parent (Val, N);
         end if;
         Set_List5 (N, Val);
      end Set_List5_With_Parent;

      procedure Set_List6_With_Parent (N : Node_Id; Val : List_Id) is
      begin
         if Val /= No_List and then Val /= Error_List then
            Set_Parent (Val, N);
         end if;
         Set_List6 (N, Val);
      end Set_List6_With_Parent;

      procedure Set_List7_With_Parent (N : Node_Id; Val : List_Id) is
      begin
         if Val /= No_List and then Val /= Error_List then
            Set_Parent (Val, N);
         end if;
         Set_List7 (N, Val);
      end Set_List7_With_Parent;

      procedure Set_List8_With_Parent (N : Node_Id; Val : List_Id) is
      begin
         if Val /= No_List and then Val /= Error_List then
            Set_Parent (Val, N);
         end if;
         Set_List8 (N, Val);
      end Set_List8_With_Parent;

      procedure Set_List9_With_Parent (N : Node_Id; Val : List_Id) is
      begin
         if Val /= No_List and then Val /= Error_List then
            Set_Parent (Val, N);
         end if;
         Set_List9 (N, Val);
      end Set_List9_With_Parent;

      procedure Set_List10_With_Parent (N : Node_Id; Val : List_Id) is
      begin
         if Val /= No_List and then Val /= Error_List then
            Set_Parent (Val, N);
         end if;
         Set_List10 (N, Val);
      end Set_List10_With_Parent;

      procedure Set_List11_With_Parent (N : Node_Id; Val : List_Id) is
      begin
         if Val /= No_List and then Val /= Error_List then
            Set_Parent (Val, N);
         end if;
         Set_List11 (N, Val);
      end Set_List11_With_Parent;

      procedure Set_List12_With_Parent (N : Node_Id; Val : List_Id) is
      begin
         if Val /= No_List and then Val /= Error_List then
            Set_Parent (Val, N);
         end if;
         Set_List12 (N, Val);
      end Set_List12_With_Parent;

   end Unchecked_Access;

end Atree;
