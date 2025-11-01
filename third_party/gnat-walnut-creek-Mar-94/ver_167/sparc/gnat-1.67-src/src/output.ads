------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               O U T P U T                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.9 $                              --
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
package Output is

--  This package contains low level output routines used by the compiler

   function Column return Int;
   --  Returns column number about to be written (start of line = column 1)
   --  If a tab is output, this column count reflects the result of outputting
   --  an equivalent number of blanks (with standard positions 1,9,17..)

   procedure Set_Standard_Error;
   --  Sets subsequent output to appear on the standard error file (whatever
   --  that might mean for the host operating system, if anything).

   procedure Set_Standard_Output;
   --  Sets subsequent output to appear on the standard output file (whatever
   --  that might mean for the host operating system, if anything). This is
   --  the default mode before any call to either of the Set procedures.

   procedure Write_Char (C : Char);
   --  Write one character to the standard output file. Note that the
   --  character should not be LF or CR (use Write_Eol for end of line)

   procedure Write_Eol;
   --  Write an end of line (whatever is required by the system in use,
   --  e.g. CR/LF for DOS, or LF for Unix) to the standard output file.

   procedure Write_Int (I : Int);
   --  Write an unsigned integer value with no leading blanks or zeroes

   procedure Write_Str (S : Str);
   --  Write a string of characters to the standard output file. Note that
   --  end of line is handled separately using WRITE_EOL, so the string
   --  should not contain either of the characters LF or CR, but it may
   --  contain horizontal tab characters.

   procedure Write_String (S : String);
   --  Like Write_Str, but the argument is of type Standard.String and can
   --  contain only valid characters for this type. In particular, it cannot
   --  contain horizontal tab, or any other format effector characters.

   --------------------------
   -- Debugging Procedures --
   --------------------------

   --  The following procedures are intended only for debugging purposes, 
   --  for temporary insertion into the text in environments where a debugger
   --  is not available. They all have non-standard very short lower case
   --  names, precisely to make sure that they are only used for debugging!

   procedure ws (S : String);
   --  Dump string followed by line return

   procedure w (S : Str);
   --  Dump string followed by line return

   procedure w (I : Int);
   --  Dump integer followed by line return

   procedure w (B : Boolean);
   --  Dump Boolean followed by line return

   procedure ws (L : Str; S : String);
   --  Dump two strings separated by blanks, followed by line return

   procedure w (L : Str; S : Str);
   --  Dump two strings separated by blanks, followed by line return

   procedure w (L : Str; I : Int);
   --  Dump contents of string followed by blank, integer, line return

   procedure w (L : Str; B : Boolean);
   --  Dump contents of string followd by blank, Boolean, line return

end Output;
