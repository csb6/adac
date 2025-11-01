------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               L I B F M T                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.21 $                             --
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

--  This package describes the format of the library information that is
--  associated with compiled source files. The method of this association
--  is potentially implementation dependent and is described and implemented
--  in package Osint. From the point of view of the description here, all
--  we need to know is that the information is represented as a string of
--  characters that is somehow associated with a compiled source file, and
--  can be retrieved when the source file is opened. If no library information
--  exists for a given source file, then we take this to mean that the given
--  source file has not been previously compiled.

with Types;  use Types;
package Libfmt is

   Library_Version : constant Str (1 .. 16) := "GNAT Lib v 1.0  ";
   --  Library version. This value must be updated whenever any change to the
   --  compiler affects the library formats in such a way as to obsolete
   --  previously compiled library modules.

   -----------------------------------
   -- Representation of Time Stamps --
   -----------------------------------

   --  All compiled units are marked with a time stamp which is derived from
   --  the source file (we assume that the host system has the concept of a
   --  file time stamp which is modified when a file is modified). These
   --  time stamps are used to ensure consistency of the set of units that
   --  constitutes a library. Time stamps are 12 character strings with
   --  with the following format:

   --     YYMMDDHHMMSS

   --       YY     year (2 low order digits)
   --       MM     month (2 digits 01-12)
   --       DD     day (2 digits 01-31)
   --       HH     hour (2 digits 00-23)
   --       MM     minutes (2 digits 00-59)
   --       SS     seconds (2 digits 00-59)

   --  Time stamps may be compared lexicographically (i.e. Ada comparison
   --  operations on strings) to determine which is later or earlier. However,
   --  in normal mode, only equality comparisons have any effect on the
   --  semantics of the library (later/earlier comparisons are used only for
   --  determining the most informative error messages to be given).

   -----------------------------------
   -- Format of Library Information --
   -----------------------------------

   --  The library information is written as a series of lines of the form:

   --    Key_Character parameter parameter ...

   --  The first two lines in the file identify the library output version
   --  and standard version (these are required to be consistent across the
   --  entire set of compilation units).

   --    V "xxxxxxxxxxxxxxxx" [MP]
   --
   --      This line indicates the library output version, as defined in this
   --      package spec. It ensures that separate object modules of a program
   --      are consistent. It has to be changed if anything changes which would
   --      affect successful binding of separately compiled modules. Examples
   --      of such changes are modifications in the format of the library info
   --      described in this package, or modifications to calling sequences,
   --      or to the way that data is represented.

   --    S  "xxxxxxxxxxxxxxxx"
   --
   --      This line identifies the version of the Standard package that was
   --      used in the previous compilation (see Standard_Version in the
   --      specification of package Stand).

   --  The next line is present only for a unit that can be a main program
   --  It has the form:

   --    M type

   --      The type parameter is either P for a parameterless procedure,
   --      or F for a function returning a value of integral type (the
   --      latter is for writing a main program that returns an exit status)

   --  Following these header lines, a set of information lines appears for
   --  each compilation unit that appears in the corresponding object file. In
   --  particular, when a package body or subprogram body is compiled, there
   --  will be two sets of information, one for the spec and one for the body.
   --  with the entry for the body appearing first. This is the only case in
   --  which a single ALI file contains more than one unit (in particular note
   --  that subunits do *not* count as compilation units for this purpose, and
   --  generate no library information, since they are inlined).

   --  The lines for each compilation unit have the following form.

   --    U unit-name source-name [PRE] [EB]
   --
   --      This line identifies the unit to which this section of the library
   --      information file applies. The first three parameters are the unit
   --      name in internal format, as described in package Uname, and the name
   --      of the source file containing the unit. The PRE parameter is present
   --      if the package is preelaborated, i.e. no runtime elaboration is
   --      required. The EB parameter is present if the pragma Elaborate_Body
   --      applies to this unit.

   --    W unit-name [source-name lib-name [E] [EA]]
   --
   --      One of these lines is present for each unit that is mentioned in
   --      an explicit with clause by the current unit. The first parameter
   --      is the unit name in internal format. The second parameter is the
   --      file name of the file that must be compiled to compile this unit
   --      (which is usually the file for the body, except for packages which
   --      have no body). The third parameter is the file name of the library
   --      is the file name of the library information file that contains the
   --      results of compiling this unit. The E and EA are present if the
   --      this case). The E and EA parameters are present if the pragmas
   --      pragmas Elaborate and Elaborate_All respectively apply to this
   --      unit. In the case of generic units, only the first parameter is
   --      present, since generic units do not need to be compiled, and
   --      generate no library information. Note that the elaborate pragmas
   --      can be given for generic units, but they are ignored.

   --  Following the unit information, is a series of lines that indicates
   --  the source files on which the compiled units depend. This is used by
   --  the binder for consistency checking.

   --    D source-name time-stamp optional-comments

   --  The optional comments, if present, must be separated from the time
   --  stamp by at least one blank. Currently the optional-comments field is
   --  used by the compiler for the RCS version number if the -dv flag is set.

   --  Note: blank lines are ignored when the library information is read,
   --  and separate sections of the file are separated by blank lines to
   --  ease readability. Blanks between fields are also ignored.

end Libfmt;
