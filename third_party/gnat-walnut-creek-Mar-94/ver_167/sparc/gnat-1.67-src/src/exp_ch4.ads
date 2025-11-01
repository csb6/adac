------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 4                               --
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

--  Expand routines for chapter 4 constructs

with Types; use Types;

package Exp_Ch4 is

   procedure Expand_N_Allocator                   (N : Node_Id);
   procedure Expand_N_Op_And                      (N : Node_Id);
   procedure Expand_N_Op_Expon                    (N : Node_Id);
   procedure Expand_N_Op_Eq                       (N : Node_Id);
   procedure Expand_N_Op_Ge                       (N : Node_Id);
   procedure Expand_N_Op_Gt                       (N : Node_Id);
   procedure Expand_N_Op_In                       (N : Node_Id);
   procedure Expand_N_Op_Le                       (N : Node_Id);
   procedure Expand_N_Op_Lt                       (N : Node_Id);
   procedure Expand_N_Op_Not                      (N : Node_Id);
   procedure Expand_N_Op_Not_In                   (N : Node_Id);
   procedure Expand_N_Op_Or                       (N : Node_Id);
   procedure Expand_N_Op_Xor                      (N : Node_Id);
   procedure Expand_N_Parenthesized_Expression    (N : Node_Id);
   procedure Expand_N_Slice                       (N : Node_Id);

end Exp_Ch4;
