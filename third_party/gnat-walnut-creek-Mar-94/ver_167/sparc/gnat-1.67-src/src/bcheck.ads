------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               B C H E C K                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.3 $                              --
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

package Bcheck is

--  This package contains the routines to perform binder consistency checks

   procedure Check_Versions;
   --  Check correct library and standard versions used

   procedure Check_Consistency;
   --  This procedure performs checks that the ALI files are consistent
   --  with the corresponding source files and with one another. At the
   --  time this is called, the Source table has been completely built and
   --  contains either the time stamp from the actual source file if the
   --  Check_Source_Files mode is set, or the latest stamp found in any of
   --  the ALI files in the program.

end Bcheck;
