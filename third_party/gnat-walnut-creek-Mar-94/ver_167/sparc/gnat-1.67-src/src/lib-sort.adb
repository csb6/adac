------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . S O R T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.3 $                             --
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

--  The algorithm used for the sort here is Floyd's Treesort3, called
--  heap sort by Knuth in Art of Programming, with an optimization
--  mentioned in a Knuth excercise and described and analyzed in detail
--  in Dewar's Phd Thesis ("The Use of Computers to Solve the X-Ray
--  Phase Problem", University of Chicago, 1968).

--  Definition: a heap is a tree structure in which every node has a value
--  greater than or equal to its children (or child for a node that has
--  only one child). A leaf node is always considered to be a heap.

--  Mapping: the array represents a binary tree, in which (assuming ones
--  origin node numbering for a moment), the left son has a subscript equal
--  to 2 * the parent subscript, and the right son has a subscript equal
--  to the left son subscript plus one.

separate (Lib)
procedure Sort (Tbl : in out Unit_Ref_Table) is

   T : Unit_Ref_Table (1 .. Tbl'Last - Tbl'First + 1);
   --  Actual sort is done on this copy of the array with 1's origin
   --  subscripts. The addressing of the table is much neater with 1's origin,
   --  and it is cheaper to do an in-out copy than to fiddle with a low bound
   --  that is othr than 1.

   Num : Pos := T'Last;
   --  Number of elements in array. Gets reduced during second phase.

   Hole_Value : Unit_Number_Type;
   --  Value proposed for placing in Hole

   -------------
   -- Heapify --
   -------------

   --  Make the subtree starting at node N into a heap by rearranging nodes
   --  in the subtree as needed. The input assumption is that the subtrees
   --  rooted at the children of the given node are already in heap form.
   --  The given node must have at least one child. Before the call, the
   --  caller moves the value of T(N) into Hole_Value.

   procedure Heapify (N : Pos) is
      Hole : Pos;
      --  Location in the tree of hole that needs to be filled by Heapify

      Son : Pos;
      --  Location of larger son of hole position

      Father : Int;
      --  Location of parent of hold on way back up

   begin
      Hole := N;

      --  This loop moves the hole down the tree, pulling up the larger of
      --  the two sons, and terminates at the bottom of the tree.

      loop
         Son := Hole + Hole;
         exit when Son > Num;

         if Son < Num
           and then Uname_Gt (File.Table (T (Son + 1)).Unit_Name,
                              File.Table (T (Son)).Unit_Name)
         then
            Son := Son + 1;
         end if;

         T (Hole) := T (Son);
         Hole := Son;
      end loop;

      --  On exit from the loop, the hole is at the bottom of the tree, and
      --  we must check successive fathers until we find one that is large
      --  enough, smaller fathers get dragged back down.

      loop
         Father := Hole / 2;

         exit when Father < N
           or else Uname_Ge (File.Table (T (Father)).Unit_Name,
                             File.Table (Hole_Value).Unit_Name);
         T (Hole) := T (Father);
         Hole := Father;
      end loop;

      --  On exit from the second loop, the Hole is ready to be filled in

      T (Hole) := Hole_Value;
   end Heapify;

----------
-- Sort --
----------

begin
   for I in T'range loop
      T (I) := Tbl (I - 1 + Tbl'First);
   end loop;

   --  First phase of the sort converts the tree to a heap, by heapifying
   --  nodes Num/2, (Num/2 - 1), (Num/2 - 2) etc. in sequence

   for J in reverse 1 .. Num / 2 loop
      Hole_Value := T (J);
      Heapify (J);
   end loop;

   --  At this stage of sort, the root node is the largest node. Repeatedly
   --  exchange it with the last node in the array, reduce the size of the
   --  array, and re-heapify the tree from the root node.

   while Num > 1 loop
      Hole_Value := T (Num);
      T (Num) := T (1);
      Num := Num - 1;
      Heapify (1);
   end loop;

   --  Sort is complete, copy result back into place

   for I in T'range loop
      Tbl (I - 1 + Tbl'First) := T (I);
   end loop;

end Sort;
