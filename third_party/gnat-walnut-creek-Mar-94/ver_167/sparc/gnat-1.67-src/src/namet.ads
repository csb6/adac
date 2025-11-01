------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                N A M E T                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.33 $                             --
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

with Alloc;   use Alloc;
with Limits;  use Limits;
with Types;   use Types;
with Table;

package Namet is

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header file namet.h
--  which is created manually from namet.ads and namet.adb.

--  This package contains routines for handling the names table. The table
--  is used to store character strings for identifiers and operator symbols,
--  as well as other string values such as unit names and file names.

--  The forms of the entries are as follows:

--    Identifiers        Upper case letters folded to lower case.
--                       Character set encoding and folding conventions
--                       are the same as the source program.

--    Operator symbols   Stored with surrounding quotes. All letters are
--                       lower case.

--    Unit names         Stored with upper case letters folded to lower case.
--                       See package Uname for further details.

--    File names         Stored exactly as provided by Fname.Get_File_Name,
--                       including casing of letters.

--  The names are hashed so that a given name appears only once in the table.

--  The first 26 entries in the names table (with Name_Id values in the range
--  First_Name_Id .. First_Name_Id + 25) represent names which are the one
--  character lower case letters in the range a-z, and these names are created
--  and initialized by the Initialize_Namet routine.

--  Note: character literals are not hashed into this table, since they can be
--  represented directly using the appropriate Char_Code value.

--  Two values, one of type Int and one of type Byte, are stored with each
--  names table entry and subprograms are provided for setting and retrieving
--  these associated values. The usage of these values is up to the client.
--  In the compiler, the Int field is used to point to a chain of potentially
--  visible entities (see Sem.Ch8 for details), and the Byte field is used
--  to hold the Token_Type value for reserved words (see Sem for details).
--  In the binder, the Byte field is unused, and the Int field is used in
--  various ways depending on the name involved (see binder documentation).

   Name_Buffer : Str (1 .. Max_Name_Length);
   --  This buffer is used to set the name to be stored in the table for the
   --  Name_Find call, and to retrieve the name for the Get_Name_String call.

   Name_Len : Nat;
   --  Length of name stored in Name_Buffer. Used as an input parameter for
   --  Name_Find, and as an output value by Get_Name_String, or Write_Name.

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize_Namet;
   --  Initializes the names table, including initializing the first 26
   --  entries in the table (for the 1-character lower case names a-z)

   procedure Finalize_Namet;
   --  Called at the end of a use of the Namet package (before a subsequent
   --  call to Initialize_Namet). Currently this routine is only used to
   --  generate debugging output.

   function Last_Name_Id return Name_Id;
   pragma Inline (Last_Name_Id);
   --  Name_Id of last currently allocated name

   function Name_Find return Name_Id;
   --  Name_Find is called with a string stored in Name_Buffer whose length
   --  is in Name_Len (i.e. the characters of the name are in subscript
   --  positions 1 to Name_Len in Name_Buffer). It searches the names
   --  table to see if the string has already been stored. If so the Id of
   --  the existing entry is returned. Otherwise a new entry is created with
   --  its Name_Table_Info field set to zero.

   function Name_Enter return Name_Id;
   --  Name_Enter has the same calling interface as Name_Find. The difference
   --  is that it does not search the table for an existing match, and also
   --  subsequent Name_Find calls using the same name will not locate the
   --  entry created by this call. Thus multiple calls to Name_Enter with the
   --  same name will create multiple entries in the name table with different
   --  Name_Id values. This is useful in the case of created names, which are
   --  never expected to be looked up.

   function Length_Of_Name (Id : Name_Id) return Nat;
   pragma Inline (Length_Of_Name);
   --  Returns length of given name in characters

   procedure Get_Name_String (Id : Name_Id);
   --  Get_Name_String is used to retrieve the string associated with an entry
   --  in the names table. The resulting string is stored in Name_Buffer
   --  and Name_Len is set. It is an error to call Get_Name_String with one
   --  of the special name Id values (No_Name, Error_Name, or Child_Name).

   procedure Write_Name (Id : Name_Id);
   --  Write_Name writes the characters of the specified name using the
   --  standard output procedures in package Output. No end of line is
   --  written, just the characters of the name. On return Name_Buffer and
   --  Name_Len are set as for a call to Get_Name_String.

   function Get_Name_Table_Info (Id : Name_Id) return Int;
   pragma Inline (Get_Name_Table_Info);
   --  Fetches the Int value associated with the given name

   function Get_Name_Entity_Id (Id : Name_Id) return Entity_Id;
   pragma Inline (Get_Name_Entity_Id);
   --  Fetches the Entity_Id value associated with the given name.
   --  Identical to previous one,  but reinterprets the contents of the
   --  field as an Entity. This routine is the basic link between lexemes
   --  (identifiers) and semantic entities.

   procedure Set_Name_Table_Info (Id : Name_Id; Val : Int);
   pragma Inline (Set_Name_Table_Info);
   --  Sets the Int value associated with the given name

   procedure Set_Name_Entity_Id (Id : Name_Id; Val : Entity_Id);
   pragma Inline (Set_Name_Entity_Id);
   --  Sets the Entity_Id value associated with the given name.

   function Get_Name_Table_Byte (Id : Name_Id) return Byte;
   pragma Inline (Get_Name_Table_Byte);
   --  Fetches the Byte value associated with the given name

   procedure Set_Name_Table_Byte (Id : Name_Id; Val : Byte);
   pragma Inline (Set_Name_Table_Byte);
   --  Sets the Byte value associated with the given name

   -----------------------------
   -- Private Part Subpackage --
   -----------------------------

   --  The following package contains the definition of the data structure
   --  used by the implementation of the Namet package. Logically it really
   --  corresponds to the private part, hence the name. The reason that it
   --  is defined as a sub-package is to allow special access from clients
   --  that need to see the internals of the data structures.

   package Namet_Private_Part is

      --  This table stores the actual string names. Although logically there
      --  is no need for a terminating character (since the length is stored
      --  in the name entry table), we still store a NUL character at the end
      --  of every name (for convenience in interfacing to the C world).

      package Name_Chars is new Table (
         Component_Type => Char,
         Index_Type     => Int,
         Low_Bound      => 0,
         Initial        => Alloc_Name_Chars_Initial,
         Increment      => Alloc_Name_Chars_Increment,
         Table_Name     => "Name_Chars");

      type Name_Entry is record
         Name_Chars_Index : Int;
         --  Starting location of characters in the Name_Chars table minus
         --  one (i.e. pointer to character just before first character). The
         --  reason for the bias of one is that indexes in Name_Buffer are
         --  one's origin, so this avoids unnecessary adds and subtracts of 1.

         Name_Len : Short;
         --  Length of this name in characters

         Byte_Info : Byte;
         --  Byte value associated with this name

         Hash_Link : Name_Id;
         --  Link to next entry in names table for same hash code

         Int_Info : Int;
         --  Int Value associated with this name
      end record;

      --  This is the table that is referenced by Name_Id entries.
      --  It contains one entry for each unique name in the table.

      package Name_Entries is new Table (
         Component_Type => Name_Entry,
         Index_Type     => Name_Id,
         Low_Bound      => First_Name_Id,
         Initial        => Alloc_Names_Initial,
         Increment      => Alloc_Names_Increment,
         Table_Name     => "Name_Entries");

   end Namet_Private_Part;

end Namet;
