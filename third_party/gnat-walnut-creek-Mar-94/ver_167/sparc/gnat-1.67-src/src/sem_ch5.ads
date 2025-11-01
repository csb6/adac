------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 5                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $                              --
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
package Sem_Ch5 is

   procedure Analyze_Statements                         (L : List_Id);
   procedure Analyze_Assignment                         (N : Node_Id);
   procedure Analyze_If_Statement                       (N : Node_Id);
   procedure Analyze_Case_Statement                     (N : Node_Id);
   procedure Analyze_Loop_Statement                     (N : Node_Id);
   procedure Analyze_Block_Statement                    (N : Node_Id);
   procedure Analyze_Exit_Statement                     (N : Node_Id);
   procedure Analyze_Return_Statement                   (N : Node_Id);
   procedure Analyze_Goto_Statement                     (N : Node_Id);
   procedure Analyze_Implicit_Label_Declaration         (N : Node_Id);

   type Case_Bounds is record
     Choice_Lo   : Node_Id;
     Choice_Hi   : Node_Id;
     Choice_Node : Node_Id;
   end record;

   type Case_Table_Type is array (Nat range <>) of Case_Bounds;
   --  Table type used by Check_Case_Choices procedure

   procedure Sort_Case_Table (Case_Table : in out Case_Table_Type);
   --  Sort the Case Table using the Lower Bound of each Choice as the key.
   --  A simple insertion sort is used since the number of choices in a case
   --  statement of variant part will usually be small and probably in near
   --  sorted order.

   procedure Check_Case_Choices (Case_Table     : in out Case_Table_Type;
                                 N              : Node_Id;
                                 Choice_Type    : Entity_Id;
                                 Others_Present : Boolean);
   --  This is the procedure which verifies that a set of case choices
   --  is correct (has no duplicates, and covers the range). Case_Table
   --  contains the choices, N is the node for the construct, Choice_Type
   --  is the type of the value used for selection, and Others_Present is
   --  A flag indicating whether or not an others choice is present.

   function Number_Of_Case_Choices (N : Node_Id) return Nat;
   --  Iterates through the choices of a case statement or variant part of
   --  a record counting all the Choice nodes except for Others.

   function Is_Variable (N : Node_Id) return Boolean;
   --  Determines if the tree referenced by N represents a variable

end Sem_Ch5;
