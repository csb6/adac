------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                C S E T S                                 --
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

with Types; use Types;
package Csets is

   --  This package contains character tables for the various character
   --  sets that are supported for source representation. Character and
   --  string literals are not affected, only identifiers. For each set,
   --  the table in this package gives the mapping of letters to their
   --  upper case equivalent. Each table thus provides the information
   --  for building the table used to fold lower case to upper case, and
   --  also the table of flags showing which characters are allowed in
   --  identifiers.

   type Translate_Table is array (Char) of Char;
   --  Type used to describe translate tables

   type Char_Array_Flags is array (Char) of Boolean;
   --  Type used for character attribute arrays

   -----------------------------------------------
   --  Character Tables For Current Compilation --
   -----------------------------------------------

   procedure Initialize_Char_Tables;
   --  Routine to initialize following character tables, whose content depends
   --  on the character code being used to represent the source program. In
   --  particular, the use of the upper half of the 8-bit code set varies.
   --  The character set in use is specified by the value stored in
   --  Options.Character_Set, which has the following settings:

   --    '1'  Latin-1
   --    '2'  Latin-2
   --    '3'  Latin-3
   --    '4'  Latin-4
   --    'p'  PC
   --    'f'  Full upper set (all distinct)
   --    'n'  No upper characters (Ada/83 rules)

   function Is_Upper_Case_Letter (C : Char) return Boolean;
   pragma Inline (Is_Upper_Case_Letter);
   --  Determine if character is upper case letter

   function Is_Lower_Case_Letter (C : Char) return Boolean;
   pragma Inline (Is_Lower_Case_Letter);
   --  Determine if character is lower case letter

   Fold_Upper : Translate_Table;
   --  Table to fold lower case identifier letters to upper case

   Fold_Lower : Translate_Table;
   --  Table to fold upper case identifier letters to lower case

   Letter_Or_Digit : Char_Array_Flags;
   --  Test if character is either a digit or upper- or lower-case letter.

   Identifier_Char : Char_Array_Flags;
   --  This table is identical to Letter_Or_Digit, except that the entry for
   --  underscore is also set True, so that it includes all identifier chars.

end Csets;
