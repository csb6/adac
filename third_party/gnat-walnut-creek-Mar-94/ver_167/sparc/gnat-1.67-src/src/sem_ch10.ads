------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 0                              --
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
package Sem_Ch10 is
   procedure Analyze_Compilation_Unit                   (N : Node_Id);
   procedure Analyze_With_Clause                        (N : Node_Id);
   procedure Analyze_Subprogram_Body_Stub               (N : Node_Id);
   procedure Analyze_Package_Body_Stub                  (N : Node_Id);
   procedure Analyze_Concurrent_Body_Stub               (N : Node_Id);
   procedure Analyze_Protected_Body_Stub                (N : Node_Id);
   procedure Analyze_Subunit                            (N : Node_Id);

   procedure Load_Needed_Body (N : Node_Id);
   --  Load and analyze the body of a context unit that is generic, or
   --  that contains generic units or inlined units. The body becomes
   --  part of the semantic dependency set of the unit that needs it.

end Sem_Ch10;

