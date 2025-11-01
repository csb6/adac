-----------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G N A T V S N                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.59 $                             --
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

package Gnatvsn is

--  This package spec holds version information for GNAT and GBIND. It should
--  be updated periodically to reflect changes and in partciular should be
--  updated when bugs in the bug log are fixed.

   Gnat_Version_String : constant String := "1.67";
   --  Version output when GNAT itself is run (in verbose mode)

   Gnatbind_Version_String : constant String := "1.13";
   --  Version output when GNATBIND is run (in verbose mode)

   Gnatmake_Version_String : constant String := "1.01";
   --  Version output when GNATMAKE is run

   Standard_Version : constant Str (1 .. 16)  := "Generic V1.07   ";
   --  A string identifying the version of Standard. Used for library output
   --  and by the binder to check that all modules use the same version of
   --  Standard (otherwise the bind is not permitted). This is updated 
   --  whenever a change to Standard, or any other interface or data
   --  structure, requires a complete recompilation of all modules.

end Gnatvsn;
