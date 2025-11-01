------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S P R I N T                                --
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

--  This package (source print) contains routines for printing the source
--  program corresponding to a specified syntax tree. These routines are
--  intended for debugging use in the compiler (not as a user level pretty
--  print tool). Only information present in the tree is output (e.g. no
--  comments are present in the output), and as far as possible we avoid
--  making any assumptions about the correctness of the tree, so a bad
--  tree may either blow up on a debugging check, or list incorrect source.

with Types; use Types;
package Sprint is

   -----------------------
   -- Syntax Extensions --
   -----------------------

   --  When the generated tree is printed, it contains constructs that are not
   --  pure Ada. For convenience, syntactic extensions to Ada have been defined
   --  purely for the purposes of this printout (they are not recognized by the
   --  parser)

   --    Unchecked conversion          target_type!(source_expression)
   --    Expression actions            [action; action; ...; value]
   --    Implicit type                 implicit type xxx is yyy
   --    Label declaration             labelname : label
   --    Freeze action                 freeze typename
   --    Rational literal              [numerator/denominator]
   --    Multiple concatenation        expr && expr && expr ... && expr
   --    Cleanup action                at end procedure name;
   --    Reference                     expression'reference

   -----------------
   -- Subprograms --
   -----------------

   procedure Source_Dump (S : Char);
   --  This routine is called from the GNAT main program to dump source as
   --  requested by debug options. The argument indicates which stage it
   --  is called at and is 'P' for the call after the parser, and 'S' for
   --  the call after semantics.

   procedure Sprint_Comma_List (List : List_Id);
   --  Prints the nodes in a list, with separating commas. If the list
   --  is empty then no output is generated.

   procedure Sprint_Paren_Comma_List (List : List_Id);
   --  Prints the nodes in a list, surrounded by parentheses, and separated
   --  by comas. If the list is empty, then no output is generated. A blank
   --  is output before the initial left parenthesis.

   procedure Sprint_Opt_Paren_Comma_List (List : List_Id);
   --  Same as normal Sprint_Paren_Comma_List procedure, except that
   --  an extra blank is output if List is non-empty, and nothing at all is
   --  printed it the argument is No_List.

   procedure Sprint_Node_List (List : List_Id);
   --  Prints the nodes in a list with no separating characters. This is used
   --  in the case of lists of items which are printed on separate lines using
   --  the current indentation amount. Note that Sprint_Node_List itself
   --  does not generate any New_Line calls.

   procedure Sprint_Opt_Node_List (List : List_Id);
   --  Like Sprint_Node_List, but prints nothing if List = No_List.

   procedure Sprint_Indented_List (List : List_Id);
   --  Like Sprint_Line_List, except that the indentation level is
   --  increased before outputting the list of items, and then decremented
   --  (back to its original level) before returning to the caller.

   procedure Sprint_Node (Node : Node_Id);
   --  Prints a single node. No new lines are output, except as required for
   --  splitting lines that are too long to fit on a single physical line.
   --  No output is generated at all if Node is Empty. No trailing or leading
   --  blank characters are generated.

   procedure Sprint_Opt_Node (Node : Node_Id);
   --  Same as normal Sprint_Node procedure, except that one leading
   --  blank is output before the node if it is non-empty.

end Sprint;
