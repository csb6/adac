------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               T R E E P R                                --
--                                                                          --
--                                 S p e c                                  --
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

with Types; use Types;
package Treepr is

--  This package provides printing routines for the abstract syntax tree
--  These routines are intended only for debugging use.

   procedure Tree_Dump (S : Char);
   --  This routine is called from the GNAT main program to dump trees as
   --  requested by debug options. The argument indicates which stage it
   --  is called at and is 'P' for the call after the parser, and 'S' for
   --  the call after semantics.

   procedure Print_Tree_Node (N : Node_Id; Label : Str := "");
   --  Prints a single tree node, without printing descendants. The Label 
   --  string is used to preface each line of the printed output.

   procedure Print_Tree_List (L : List_Id);
   --  Prints a single node list, without printing the descendants of any
   --  of the nodes in the list

   procedure Print_Tree_Elist (E : Elist_Id);
   --  Prints a single node list, without printing the descendants of any
   --  of the nodes in the list

   procedure Print_Node_Subtree (N : Node_Id);
   --  Prints the subtree routed at a specified tree node, including all
   --  referenced descendants.

   procedure Print_List_Subtree (L : List_Id);
   --  Prints the subtree consisting of the given node list and all its
   --  referenced descendants.

   procedure Print_Elist_Subtree (E : Elist_Id);
   --  Prints the subtree consisting of the given element list and all its
   --  referenced descendants.

   procedure PN (N : Node_Id);
   --  Debugging procedure (to be called within gdb)
   --  same as Print_Tree_Node with Label = ""

   procedure PT (N : Node_Id);
   --  Debugging procedure (to be called within gdb)
   --  same as Print_Node_Subtree

end Treepr;
