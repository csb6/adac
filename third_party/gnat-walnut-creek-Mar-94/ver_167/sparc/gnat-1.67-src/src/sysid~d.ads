------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S Y S I D                                 --
--                                                                          --
--                                 S p e c                                  --
--                              (DOS Version)                               --
--                                                                          --
--                            $Revision: 1.1 $                             --
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
package Sysid is

--  This package contains the flag that shows what operating system is used

   MS_DOS : constant Boolean := True;
   --  This flag is True if operating under MS/DOS or an operating system that
   --  shares its conventions (such as OS/2). It affects the output format of
   --  library information files (CR/LF at end of lines, and EOF at end of
   --  file), the default character set (PC), and the output of the usage
   --  information (switch character displayed as slash instead of minus).
   --  Note that the handling of input files is not dependent on the setting
   --  of this flag. For input source and library information files, the line
   --  terminator may be either CR/LF or LF alone, and the EOF character is
   --  optional, but will be recognized as signalling the end of file (and
   --  no further information will be read) if it is encountered.

end Sysid; 
