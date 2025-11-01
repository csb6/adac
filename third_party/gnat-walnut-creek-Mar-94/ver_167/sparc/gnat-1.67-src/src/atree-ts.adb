------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             A T R E E . T S                             --
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

--  This package implements the tree substitution rewrite package

separate (Atree)
package body TS is

   ---------------------------------------
   -- Data structures for mapping table --
   ---------------------------------------

   --  A hash table is used to keep track of the mapping between substituted
   --  nodes and their corresponding original tree nodes.

   type Hash_Entry_Id is new Nat;
   --  Id of hash table entry is its subscript in the hash table

   No_Entry : constant Hash_Entry_Id := 0;
   --  Value used to mark no chain, or end of chain

   type Hash_Entry is record
      Next     : Hash_Entry_Id;
      Old_Node : Node_Id;
      New_Node : Node_Id;
   end record;

   Num_Hash_Headers : constant := 512;
   --  Number of headers in Hash_Headers array

   Hash_Headers : array (Nat range 0 .. Num_Hash_Headers - 1) of Hash_Entry_Id;
   --  Table of hash headers, each entry points to chain of entries in the
   --  hash table whose hash value matches the subscript in the header table.
   --  The hash code is simply the Node_Id value mod Num_Hash_Headers.

   package Hash_Sub is new Table (
      Component_Type => Hash_Entry,
      Index_Type     => Hash_Entry_Id,
      Low_Bound      => 1,
      Initial        => Alloc_Hash_Sub_Initial,
      Increment      => Alloc_Hash_Sub_Increment,
      Table_Name     => "Hash_Sub");

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Fix_Parent (Field : Int; Old_Node, New_Node : Node_Id);
   --  This subprogram is used to fixup parent pointers that are rendered
   --  incorrect because of the node exchange. Field is checked to see if
   --  it points to a node, list, or element list that has a parent that
   --  points to Old_Node. If so, the parent is reset to point to New_Node.

   --------------------
   -- Initialize_Sub --
   --------------------

   procedure Initialize_Sub is
   begin
      for I in Hash_Headers'range loop
         Hash_Headers (I) := No_Entry;
      end loop;

      Hash_Sub.Init;
   end Initialize_Sub;

   -------------------------
   -- Set_Substitute_Tree --
   -------------------------

   procedure Set_Substitute_Tree (Old_Node, New_Node : Node_Id) is
   begin
      pragma Debug (Dcheck_Is_Node (Old_Node));
      pragma Debug (Dcheck_Is_Node (New_Node));
      pragma Assert
        (not Has_Extension (Old_Node) and not Has_Extension (New_Node),
         Compiler_Abort);

      --  Do the exchange

      Exchange_Nodes (Old_Node, New_Node);
      Nodes.Table (Old_Node).Rewrite_Sub := True;

      --  Only remaining step is to fix up the parent pointers

      Fix_Parent (Field1 (Old_Node), New_Node, Old_Node);
      Fix_Parent (Field2 (Old_Node), New_Node, Old_Node);
      Fix_Parent (Field3 (Old_Node), New_Node, Old_Node);
      Fix_Parent (Field4 (Old_Node), New_Node, Old_Node);
      Fix_Parent (Field5 (Old_Node), New_Node, Old_Node);

      Fix_Parent (Field1 (New_Node), Old_Node, New_Node);
      Fix_Parent (Field2 (New_Node), Old_Node, New_Node);
      Fix_Parent (Field3 (New_Node), Old_Node, New_Node);
      Fix_Parent (Field4 (New_Node), Old_Node, New_Node);
      Fix_Parent (Field5 (New_Node), Old_Node, New_Node);

      --  Now, make an entry in the hash table. Note that we overwrite any
      --  existing entry that references Old_Node. This allows multiple steps
      --  of substitutions, and we will always get the real original.

      declare
         Hash_Code : Nat := Int (Old_Node) mod Num_Hash_Headers;
         Index : Hash_Entry_Id := Hash_Headers (Hash_Code);

      begin
         if Index = No_Entry then
            Hash_Sub.Increment_Last;
            Hash_Headers (Hash_Code) := Hash_Sub.Last;
            Hash_Sub.Table (Hash_Sub.Last).Next     := No_Entry;
            Hash_Sub.Table (Hash_Sub.Last).New_Node := Old_Node;
            Hash_Sub.Table (Hash_Sub.Last).Old_Node := New_Node;

         else
            loop
               if Hash_Sub.Table (Index).New_Node = Old_Node then
                  Hash_Sub.Table (Index).Old_Node := New_Node;
                  exit;

               elsif Hash_Sub.Table (Index).Next = No_Entry then
                  Hash_Sub.Increment_Last;
                  Hash_Sub.Table (Index).Next := Hash_Sub.Last;
                  Hash_Sub.Table (Hash_Sub.Last).Next     := No_Entry;
                  Hash_Sub.Table (Hash_Sub.Last).New_Node := Old_Node;
                  Hash_Sub.Table (Hash_Sub.Last).Old_Node := New_Node;
                  exit;

               else
                  Index := Hash_Sub.Table (Index).Next;
               end if;
            end loop;
         end if;
      end;

   end Set_Substitute_Tree;

   -------------------------
   -- Get_Substitute_Tree --
   -------------------------

   function Get_Substitute_Tree (New_Node : Node_Id) return Node_Id is
      Hash_Code : Nat := Int (New_Node) mod Num_Hash_Headers;
      Index : Hash_Entry_Id := Hash_Headers (Hash_Code);

   begin
      loop
         pragma Assert (Index /= No_Entry, Compiler_Abort);

         if New_Node = Hash_Sub.Table (Index).New_Node then
            return Hash_Sub.Table (Index).Old_Node;
         else
            Index := Hash_Sub.Table (Index).Next;
         end if;
      end loop;

   end Get_Substitute_Tree;

   ----------------
   -- Fix_Parent --
   ----------------

   procedure Fix_Parent (Field : Int; Old_Node, New_Node : Node_Id) is
   begin
      if Field in Node_Range
        and then Node_Id (Field) /= Empty
        and then Parent (Node_Id (Field)) = Old_Node
      then
         Set_Parent (Node_Id (Field), New_Node);

      elsif Field in List_Range
        and then List_Id (Field) /= No_List
        and then List_Parent (List_Id (Field)) = Old_Node
      then
         Set_Parent (List_Id (Field), New_Node);

      elsif Field in Elist_Range
        and then Elist_Id (Field) /= No_Elist
        and then Elist_Parent (Elist_Id (Field)) = Old_Node
      then
         Set_Parent (Elist_Id (Field), New_Node);
      end if;
   end Fix_Parent;

end TS;
