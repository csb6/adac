------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 4                               --
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

package Sem_Ch4  is
   procedure Analyze_Aggregate                          (N : Node_Id);
   procedure Analyze_Allocator                          (N : Node_Id);
   procedure Analyze_Arithmetic_Op                      (N : Node_Id);
   procedure Analyze_Call                               (N : Node_Id);
   procedure Analyze_Comparison_Op                      (N : Node_Id);
   procedure Analyze_Concatenation                      (N : Node_Id);
   procedure Analyze_Conversion                         (N : Node_Id);
   procedure Analyze_Equality_Op                        (N : Node_Id);
   procedure Analyze_Explicit_Dereference               (N : Node_Id);
   procedure Analyze_Expression_Actions                 (N : Node_Id);
   procedure Analyze_Indexed_Component                  (N : Node_Id);
   procedure Analyze_Logical_Op                         (N : Node_Id);
   procedure Analyze_Membership_Op                      (N : Node_Id);
   procedure Analyze_Negation                           (N : Node_Id);
   procedure Analyze_Null                               (N : Node_Id);
   procedure Analyze_N_Parenthesized_Expression         (N : Node_Id);
   procedure Analyze_Qualified_Expression               (N : Node_Id);
   procedure Analyze_Range                              (N : Node_Id);
   procedure Analyze_Reference                          (N : Node_Id);
   procedure Analyze_Selected_Component                 (N : Node_Id);
   procedure Analyze_Short_Circuit                      (N : Node_Id);
   procedure Analyze_Unary_Op                           (N : Node_Id);


   procedure Analyze_Expression (N : Node_Id);
   --  This is the top-level procedure for analyzing all expressions.
   --  For expressions that are not names, this is just a call to analyze.
   --  If the expression is a name,  it may be a call to a parameterless
   --  procedure, and if so must be converted into an explicit call node 
   --  and analyzed as such.

end Sem_Ch4;
