------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S W I T C H                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.6 $                              --
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

--  This package scans switches. Note that the body of Usage must be
--  coordinated with the switches that are recognized by this package.
--  The Usage package also acts as the official documentation for the
--  switches that are recognized. In addition, package Debug documents
--  the otherwise undocumented debug switches that are also recognized.

package Switch is

   --  Note: the switch character is / in MS_DOS mode, and - in normal mode.
   --  In MS_DOS mode the - is allowed as an alternative switch, but in normal
   --  (Unix) mode, the / is not recognized since it is a directory character.

   --  Note: In GNAT, the case of switches is not significant, so switch
   --  characters, or letters appearing in the parameter to a switch, may
   --  be either upper case or lower case.

   -----------------
   -- Subprograms --
   -----------------

   procedure Scan_Switches (Switch_Chars : String);
   --  Procedure to scan out switches stored in the given string. The first
   --  character is known to be a valid switch character, and there are no
   --  blanks or other switch terminator characters in the string, so the
   --  entire string should consist of valid switch characters, except that
   --  an optional terminating Ascii.NUL character is allowed. A bad switch
   --  causes a fatal error exit and control does not return.

end Switch;
