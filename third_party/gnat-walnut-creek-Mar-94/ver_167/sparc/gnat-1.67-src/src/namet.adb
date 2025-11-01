------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                N A M E T                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.35 $                             --
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

package body Namet is

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header file namet.h
--  which is created manually from namet.ads and namet.adb.

   use Namet_Private_Part;
   --  We are allowed to see our own private data!

   Hash_Num : constant Int := 2**12;
   --  Number of headers in the hash table. Current hash algorithm is closely
   --  tailored to this choice, so it can only be changed if a corresponding
   --  change is made to the hash alogorithm.

   Hash_Max : constant Int := Hash_Num - 1;
   --  Indexes in the hash header table run from 0 to Hash_Num - 1

   subtype Hash_Index_Type is Int range 0 .. Hash_Max;
   --  Range of hash index values

   Hash_Table : array (Hash_Index_Type) of Name_Id;
   --  The hash table is used to locate existing entries in the names table.
   --  The entries point to the first names table entry whose hash value
   --  matches the hash code. Then subsequent names table entries with the
   --  same hash code value are linked through the Hash_Link fields.

   One_Char_Id_Base : constant :=
      Name_Id'Pos (First_Name_Id) - Char'Pos ('a');
   --  Bias to get Name_Id of one characte identifier name

   -----------------------
   -- Local Subprograms --
   -----------------------

   function CH return Hash_Index_Type;
   pragma Inline (CH);
   --  Compute hash code for name stored in Name_Buffer (length in Name_Len)


   function CH return Hash_Index_Type is separate;

   ----------------------
   -- Initialize_Namet --
   ----------------------

   procedure Initialize_Namet is

   begin
      Name_Chars.Init;
      Name_Entries.Init;

      --  Initialize entries for a-z

      for C in Char range 'a' .. 'z' loop
         Name_Entries.Increment_Last;
         Name_Entries.Table (Name_Entries.Last).Name_Chars_Index :=
           Name_Chars.Last;
         Name_Entries.Table (Name_Entries.Last).Name_Len  := 1;
         Name_Entries.Table (Name_Entries.Last).Hash_Link := No_Name;
         Name_Entries.Table (Name_Entries.Last).Int_Info  := 0;
         Name_Entries.Table (Name_Entries.Last).Byte_Info := 0;
         Name_Chars.Increment_Last;
         Name_Chars.Table (Name_Chars.Last) := C;
         Name_Chars.Increment_Last;
         Name_Chars.Table (Name_Chars.Last) := NUL;
      end loop;

      --  Clear hash table

      for I in Hash_Index_Type loop
         Hash_Table (I) := No_Name;
      end loop;
   end Initialize_Namet;

   --------------------
   -- Finalize_Namet --
   --------------------

   procedure Finalize_Namet is
      Max_Chain_Length : constant := 50;
      --  Max length of chains for which specific information is output

      F : array (Int range 0 .. Max_Chain_Length) of Int;
      --  N'th entry is number of chains of length N

      Probes : Int := 0;
      --  Used to compute average number of probes

      Nsyms : Int := 0;
      --  Number of symbols in table

   begin
      if Debug_Flag_H then

         for I in F'range loop
            F (I) := 0;
         end loop;

         for I in Hash_Index_Type loop
            if Hash_Table (I) = No_Name then
               F (0) := F (0) + 1;

            else
               Write_String ("Hash_Table (");
               Write_Int (Int (I));
               Write_String (") has ");

               declare
                  C : Int := 1;
                  N : Name_Id;
                  S : Int;

               begin
                  C := 0;
                  N := Hash_Table (I);

                  while N /= No_Name loop
                     N := Name_Entries.Table (N).Hash_Link;
                     C := C + 1;
                  end loop;

                  Write_Int (C);
                  Write_String (" entries");
                  Write_Eol;

                  if C < Max_Chain_Length then
                     F (C) := F (C) + 1;
                  else
                     F (Max_Chain_Length) := F (Max_Chain_Length) + 1;
                  end if;

                  N := Hash_Table (I);

                  while N /= No_Name loop
                     S := Name_Entries.Table (N).Name_Chars_Index;
                     Write_String ("      ");

                     for J in 1 .. Name_Entries.Table (N).Name_Len loop
                        Write_Char (Name_Chars.Table (S + Int (J)));
                     end loop;

                     Write_Eol;
                     N := Name_Entries.Table (N).Hash_Link;
                  end loop;
               end;
            end if;
         end loop;

         Write_Eol;

         for I in Int range 0 .. Max_Chain_Length loop
            if F (I) /= 0 then
               Write_Str ("Number of hash chains of length ");

               if I < 10 then
                  Write_Char (' ');
               end if;

               Write_Int (I);

               if I = Max_Chain_Length then
                  Write_Str (" or greater");
               end if;

               Write_Str (" = ");
               Write_Int (F (I));
               Write_Eol;

               if I /= 0 then
                  Nsyms := Nsyms + F (I);
                  Probes := Probes + F (I) * (1 + I) * 100;
               end if;
            end if;
         end loop;

         Write_Eol;
         Write_String ("Average number of probes for lookup = ");
         Probes := Probes / Nsyms;
         Write_Int (Probes / 200);
         Write_Char ('.');
         Probes := (Probes mod 200) / 2;
         Write_Char (Char'Val (48 + Probes / 10));
         Write_Char (Char'Val (48 + Probes mod 10));
         Write_Eol;
         Write_Eol;
      end if;
   end Finalize_Namet;

   ------------------
   -- Last_Name_Id --
   ------------------

   function Last_Name_Id return Name_Id is
   begin
      return Name_Entries.Last;
   end Last_Name_Id;

   ---------------
   -- Name_Find --
   ---------------

   function Name_Find return Name_Id is

      New_Id : Name_Id;
      --  Id of entry in hash search, and value to be returned

      S : Int;
      --  Pointer into string table

      Hash_Index : Hash_Index_Type;
      --  Computed hash index


   begin
      --  Quick handling for one character identifiers

      if Name_Len = 1 and then Name_Buffer (1) in 'a' .. 'z' then
         return Name_Id (One_Char_Id_Base + Char'Pos (Name_Buffer (1)));

      --  Otherwise search hash table for existing matching entry

      else
         Hash_Index := Namet.CH;
         New_Id := Hash_Table (Hash_Index);

         if New_Id = No_Name then
            Hash_Table (Hash_Index) := Name_Entries.Last + 1;

         else
            Search : loop
               if Name_Len /= Int (Name_Entries.Table (New_Id).Name_Len) then
                  goto No_Match;
               end if;

               S := Name_Entries.Table (New_Id).Name_Chars_Index;

               for I in 1 .. Name_Len loop
                  if Name_Chars.Table (S + I) /= Name_Buffer (I) then
                     goto No_Match;
                  end if;
               end loop;

               return New_Id;

               --  Current entry in hash chain does not match

               <<No_Match>>
                  if Name_Entries.Table (New_Id).Hash_Link /= No_Name then
                     New_Id := Name_Entries.Table (New_Id).Hash_Link;
                  else
                     Name_Entries.Table (New_Id).Hash_Link :=
                       Name_Entries.Last + 1;
                     exit Search;
                  end if;

            end loop Search;
         end if;

         --  We fall through here only if a matching entry was not found in the
         --  hash table. We now create a new entry in the names table. The hash
         --  link pointing to the new entry (Name_Entries.Last+1) has been set.

         Name_Entries.Increment_Last;
         Name_Entries.Table (Name_Entries.Last).Name_Chars_Index :=
           Name_Chars.Last;
         Name_Entries.Table (Name_Entries.Last).Name_Len  := Short (Name_Len);
         Name_Entries.Table (Name_Entries.Last).Hash_Link := No_Name;
         Name_Entries.Table (Name_Entries.Last).Int_Info  := 0;
         Name_Entries.Table (Name_Entries.Last).Byte_Info := 0;

         --  Set corresponding string entry in the Name_Chars table

         for I in 1 .. Name_Len loop
            Name_Chars.Increment_Last;
            Name_Chars.Table (Name_Chars.Last) := Name_Buffer (I);
         end loop;

         Name_Chars.Increment_Last;
         Name_Chars.Table (Name_Chars.Last) := NUL;

         return Name_Entries.Last;
      end if;
   end Name_Find;

   ----------------
   -- Name_Enter --
   ----------------

   function Name_Enter return Name_Id is
   begin
      Name_Entries.Increment_Last;
      Name_Entries.Table (Name_Entries.Last).Name_Chars_Index :=
        Name_Chars.Last;
      Name_Entries.Table (Name_Entries.Last).Name_Len  := Short (Name_Len);
      Name_Entries.Table (Name_Entries.Last).Hash_Link := No_Name;
      Name_Entries.Table (Name_Entries.Last).Int_Info  := 0;
      Name_Entries.Table (Name_Entries.Last).Byte_Info := 0;

      --  Set corresponding string entry in the Name_Chars table

      for I in 1 .. Name_Len loop
         Name_Chars.Increment_Last;
         Name_Chars.Table (Name_Chars.Last) := Name_Buffer (I);
      end loop;

      Name_Chars.Increment_Last;
      Name_Chars.Table (Name_Chars.Last) := NUL;

      return Name_Entries.Last;
   end Name_Enter;

   --------------------
   -- Length_Of_Name --
   --------------------

   function Length_Of_Name (Id : Name_Id) return Nat is
   begin
      return Int (Name_Entries.Table (Id).Name_Len);
   end Length_Of_Name;

   ---------------------
   -- Get_Name_String --
   ---------------------

   procedure Get_Name_String (Id : Name_Id) is
      S : Int;

   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      S := Name_Entries.Table (Id).Name_Chars_Index;
      Name_Len := Int (Name_Entries.Table (Id).Name_Len);

      for I in 1 .. Name_Len loop
         Name_Buffer (I) := Name_Chars.Table (S + I);
      end loop;
   end Get_Name_String;

   -------------------------
   -- Get_Name_Table_Info --
   -------------------------

   function Get_Name_Table_Info (Id : Name_Id) return Int is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      return Name_Entries.Table (Id).Int_Info;
   end Get_Name_Table_Info;

   ------------------------
   -- Get_Name_Entity_Id --
   ------------------------

   function Get_Name_Entity_Id (Id : Name_Id) return Entity_Id is
   begin
      return Entity_Id (Get_Name_Table_Info (Id));
   end Get_Name_Entity_Id;

   -------------------------
   -- Set_Name_Table_Info --
   -------------------------

   procedure Set_Name_Table_Info (Id : Name_Id; Val : Int) is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      Name_Entries.Table (Id).Int_Info := Val;
   end Set_Name_Table_Info;

   ------------------------
   -- Set_Name_Entity_Id --
   ------------------------

   procedure Set_Name_Entity_Id (Id : Name_Id; Val : Entity_Id) is
   begin
      Set_Name_Table_Info (Id, Int (Val));
   end Set_Name_Entity_Id;

   -------------------------
   -- Get_Name_Table_Byte --
   -------------------------

   function Get_Name_Table_Byte (Id : Name_Id) return Byte is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      return Name_Entries.Table (Id).Byte_Info;
   end Get_Name_Table_Byte;

   -------------------------
   -- Set_Name_Table_Byte --
   -------------------------

   procedure Set_Name_Table_Byte (Id : Name_Id; Val : Byte) is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      Name_Entries.Table (Id).Byte_Info := Val;
   end Set_Name_Table_Byte;

   -----------------
   --  Write_Name --
   -----------------

   procedure Write_Name (Id : Name_Id) is
   begin
      if Id >= First_Name_Id then
         Get_Name_String (Id);
         Write_Str (Name_Buffer (1 .. Name_Len));
      end if;
   end Write_Name;

end Namet;
