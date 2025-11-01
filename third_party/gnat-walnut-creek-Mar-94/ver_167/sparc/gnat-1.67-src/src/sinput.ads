------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S I N P U T                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.13 $                             --
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

with Types;  use Types;
package Sinput is

--  This package contains the input routines used for reading the
--  input source file. The actual I/O routines are in OS_Interface,
--  with this module containing only the system independent processing.

--  General Note: throughout the compiler, we use the term line or source
--  line to refer to a physical line in the source, terminated by the end of
--  line sequence (either LF or CR/LF depending on the system), as opposed
--  to the Ada notion of logical line (which can be terminated by other
--  format effectors (form feed and vertical tab).

   procedure Backup_Line (P : in out Source_Ptr);
   --  Back up the argument pointer to the start of the previous line. On
   --  entry, P points to the start of a physical line in the source buffer.
   --  On return, P is updated to point to the start of the previous line.
   --  The caller has checked that a Line_Terminator character precedes P so
   --  that there definitely is a previous line in the source buffer.

   function Line_Start (P : Source_Ptr) return Source_Ptr;
   --  Finds the source position of the start of the line containing the
   --  given source location.

   function Get_Col_Number (P : Source_Ptr) return Column_Number_Type;
   --  The ones-origin column number of the specified Source_Ptr value is
   --  determined and returned. Tab characters if present are assumed to
   --  represent the standard 1,9,17.. spacing pattern.

   function Get_Line_Number (P : Source_Ptr) return Line_Number_Type;
   --  The line number of the specified source position is obtained by
   --  doing a binary search on the source positions in the lines table
   --  for the unit containing the given source position.

   function Get_Source_Buffer_Ptr (P : Source_Ptr) return Source_Buffer_Ptr;
   --  Returns the pointer to the source buffer containing the given source
   --  location. Fatal error if given value not within a valid source file.

   procedure Next_Line;
   --  Next_Line is called when the end of the current line is met. On entry
   --  Scan_Ptr points just past the sequence of format effectors terminating
   --  the previous line (i.e. to the start of the next line). On exit, the
   --  value in Scan_Ptr is unchanged. The purpose of this call is to make a
   --  possible entry in the Lines table.

   procedure Write_Location (P : Source_Ptr);
   --  Writes out a string of the form "fff", line nn(cc), where fff, nn, cc
   --  are the file name, line number and column corresponding to the given
   --  source location. No_Location and Standard_Location appear as the
   --  strings <no location> and <standard location>. The text is preceded
   --  by two blanks.

end Sinput;
