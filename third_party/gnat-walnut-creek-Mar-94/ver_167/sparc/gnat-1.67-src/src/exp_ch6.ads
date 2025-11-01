------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 6                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.5 $                              --
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

--  Expand routines for chapter 6 constructs

with Types; use Types;

package Exp_Ch6 is

   procedure Expand_N_Function_Call            (N : Node_Id);
   procedure Expand_N_Subprogram_Body          (N : Node_Id);
   procedure Expand_N_Procedure_Call_Statement (N : Node_Id);

   procedure Expand_Dispatch_Call (Call_Node : Node_Id);
   --  The subprogram call (Call_Node) is replaced a by a call to the correct
   --  primitive operation whose access is stored in the dispatch table pointed
   --  by the tag of the controlling argument.

end Exp_Ch6;
