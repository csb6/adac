------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                A L L O C                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.7 $                              --
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

package Alloc is

--  This package contains definitions for initial sizes and growth increments
--  for the various dynamic arrays used for principle compiler data strcutures.
--  The indicated initial size is allocated for the start of each file, and
--  the increment value is a percentage used to increase the size of the table
--  when it needs expanding (e.g. a value of 100 = 100% increase = double)

   Alloc_Name_Chars_Initial : constant := 64_000;
   --  Initial allocation for name characters table (Namet)

   Alloc_Name_Chars_Increment : constant := 100;
   --  Incremental allocation for name characters table (Namet)

   Alloc_Names_Initial : constant := 4_000;
   --  Initial allocation of entries in names table (Namet)

   Alloc_Names_Increment : constant := 100;
   --  Incremental allocation of entries in names table (Namet)

   Alloc_String_Chars_Initial : constant := 64_000;
   --  Initial allocation for name characters table (Stringt)

   Alloc_String_Chars_Increment : constant := 150;
   --  Incremental allocation for name characters table (Stringt)

   Alloc_Strings_Initial : constant := 500;
   --  Initial allocation of entries in names table (Stringt)

   Alloc_Strings_Increment : constant := 150;
   --  Incremental allocation of entries in names table (Stringt)

   Alloc_Lines_Initial : constant := 4_000;
   --  Initial allocation for lines table (Scn)

   Alloc_Lines_Increment : constant := 150;
   --  Incremental allocation for lines table (Sinput)

   Alloc_Nodes_Initial : constant := 8_000;
   --  Initial allocation in nodes for tree (Tree)

   Alloc_Nodes_Increment : constant := 150;
   --  Incremental allocation for nodes table (Tree)

   Alloc_Lists_Initial : constant := 4_000;
   --  Initial allocation in list headers for tree (Tree)

   Alloc_Lists_Increment : constant := 150;
   --  Incremental allocation for list table (Tree)

   Alloc_Hash_Sub_Initial : constant := 2048;
   --  Initial allocation in tree substitution hash table (Tree.Sub)

   Alloc_Hash_Sub_Increment : constant := 300;
   --  Incremental allocation in tree substitution hash table (Tree.Sub)

   Alloc_Uints_Initial : constant := 500;
   --  Initial allocation for universal integer table in entries (Uintp)

   Alloc_Uints_Increment : constant := 100;
   --  Incremental allocation for universal integer table in digits (Uintp)

   Alloc_Udigits_Initial : constant := 5000;
   --  Initial allocation for Uint digits table (Uintp)

   Alloc_Udigits_Increment : constant := 100;
   --  Incremental allocation Uint digits table (Uintp)

end Alloc;
