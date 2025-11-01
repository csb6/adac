------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S T R I N G T                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.12 $                             --
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

with Debug;   use Debug;
with Output;  use Output;
package body Stringt is

   use Stringt_Private_Part;
   --  We are allowed to see our own private data!

   ------------------------
   -- Initialize_Stringt --
   ------------------------

   procedure Initialize_Stringt is
   begin
      String_Chars.Init;
      Strings.Init;
   end Initialize_Stringt;

   --------------------
   -- Last_String_Id --
   --------------------

   function Last_String_Id return String_Id is
   begin
      return Strings.Last;
   end Last_String_Id;

   ------------------
   -- Start_String --
   ------------------

   procedure Start_String is
   begin
      Strings.Increment_Last;
      Strings.Table (Strings.Last).String_Index := String_Chars.Last + 1;
      Strings.Table (Strings.Last).Length := 0;
   end Start_String;

   -----------------------
   -- Store_String_Char --
   -----------------------

   procedure Store_String_Char (C : Char_Code) is
   begin
      String_Chars.Increment_Last;
      String_Chars.Table (String_Chars.Last) := C;
      Strings.Table (Strings.Last).Length :=
        Strings.Table (Strings.Last).Length + 1;
   end Store_String_Char;

   -------------------------
   -- Unstore_String_Char --
   -------------------------

   procedure Unstore_String_Char is
   begin
      String_Chars.Decrement_Last;
      Strings.Table (Strings.Last).Length :=
        Strings.Table (Strings.Last).Length - 1;
   end Unstore_String_Char;

   ----------------
   -- End_String --
   ----------------

   function End_String return String_Id is
   begin
      return Strings.Last;
   end End_String;

   -------------------
   -- String_Length --
   -------------------

   function String_Length (Id : String_Id) return Int is
   begin
      return Strings.Table (Id).Length;
   end String_Length;

   ---------------------
   -- Get_String_Char --
   ---------------------

   function Get_String_Char (Id : String_Id; Index : Int) return Char_Code is
   begin
      pragma Assert (Id in Strings.First .. Strings.Last
                       and then Index in 1 .. Strings.Table (Id).Length);

      return String_Chars.Table (Strings.Table (Id).String_Index + Index - 1);
   end Get_String_Char;

   ------------------------------
   -- Write_String_Table_Entry --
   ------------------------------

   procedure Write_String_Table_Entry (Id : String_Id) is
      C : Char_Code;

   begin
      Write_Char ('"');

      for I in 1 .. String_Length (Id) loop
         C := Get_String_Char (Id, I);

         if Char'Val (C) = '"' then
            Write_String ("""""");
         elsif C in 16#20# .. 16#7A# or else C in 16#7C# .. 16#7E# then
            Write_Char (Char'Val (C));
         else
            Write_Char ('{');
            Write_Int (Int (C));
            Write_Char ('}');
         end if;
      end loop;

      Write_Char ('"');
   end Write_String_Table_Entry;

end Stringt;
