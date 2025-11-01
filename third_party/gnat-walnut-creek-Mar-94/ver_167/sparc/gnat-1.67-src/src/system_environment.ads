------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M _ E N V I R O N M E N T                  --
--                                                                          --
--                                 S p e c                                  --
--                              (UNIX Version)                              --
--                                                                          --
--                            $Revision: 1.4 $                              --
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

--  This package contains some functions to access arg. on the command line

package System_Environment is 

   function Arg_Count return Positive;
   pragma Interface (C, Arg_Count);
   --  Return the number of arg in the command line (equivalent to argc in C)

   type A_String is access String;

   function Arg_Value (Arg_Num : Natural) return A_String;
   --  Return the desired arg. from the command line (0 is the command name)

end System_Environment;
