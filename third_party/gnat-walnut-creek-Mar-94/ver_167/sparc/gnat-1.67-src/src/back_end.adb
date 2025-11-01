------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            B A C K _ E N D                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $                             --
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

--  Call the back end with all the information needed

with Lib;     use Lib;
with Namet;   use Namet;
with Stringt; use Stringt;
with System;  use System;
with Atree;   use Atree;
with Types;   use Types;
with Uintp;   use Uintp;
procedure Back_End is

   use Namet_Private_Part;
   use Atree_Private_Part;
   use Stringt_Private_Part;
   use Uintp_Private_Part;
   --  We need to see the internal data structures to get the table addresses
   --  and lengths for the Back_End call.

   function Count_Files return Int is
      Count : Int := 0;
   begin
      for I in File.First .. File.Last loop
         if File.Table (I).Source /= null then Count := Count + 1; end if;
      end loop;
      return Count;
   end Count_Files;

begin
   declare
      Counter : Int;
      Number_Units : Int := Count_Files;

      --  The File_Record type has a lot of components that are meaningless
      --  to the back end, so a new record is created here to contain the
      --  needed information for each file.

      type Needed_File_Info_Type is record
         File_Name  : File_Name_Type;
         First_Sloc : Source_Ptr;
         Last_Sloc  : Source_Ptr;
         Last_Line  : Line_Number_Type;
      end record;
      for Needed_File_Info_Type use record
         File_Name  at 00 range 0 .. 31;
         First_Sloc at 04 range 0 .. 31;
         Last_Sloc  at 08 range 0 .. 31;
         Last_Line  at 12 range 0 .. 31;
      end record;

      --  Create one record of the above type and one Lines Table pointer for
      --  each unit.

      type File_Info_Array_Type is array
       (Int range 1 .. Number_Units) of Needed_File_Info_Type;

      type Lines_Ptr_Array_Type is array
       (Int range 1 .. Number_Units) of Address;

      File_Info_Array : File_Info_Array_Type;
      Lines_Ptr_Array : Lines_Ptr_Array_Type;


      procedure Gigi (
         Gnu_Root       : Int;
         Max_Gnat_Nodes : Int;
         Number_Names   : Int;
         Nodes_Ptr        : Address;
         Names_Ptr        : Address;
         Strings_Ptr      : Address;
         String_Chars_Ptr : Address;
         List_Headers_Ptr : Address;
         Name_Chars_Ptr   : Address;
         Uints_Ptr        : Address;
         Udigits_Ptr      : Address;
         Number_Units     : Int;
         File_Info_Ptr    : Address;
         Lines_Ptrs       : Address);


      pragma Interface (C, Gigi);
      pragma Interface_Name (Gigi, "gigi");

   begin

      Counter := 0;
      for I in File.First .. File.Last loop
         if File.Table (I).Source /= null then
            Counter := Counter + 1;
            File_Info_Array (Counter).File_Name  := File.Table (I).File_Name;
            File_Info_Array (Counter).First_Sloc :=
                                                   File.Table (I).Source'First;
            File_Info_Array (Counter).Last_Sloc  := File.Table (I).Source'Last;
            File_Info_Array (Counter).Last_Line  := File.Table (I).Last_Line;

            Lines_Ptr_Array (Counter) :=
              File.Table (I).Lines_Table.all'Address;
         end if;
      end loop;


      Gigi (
         Gnu_Root        => Int (File.Table (Main_Unit).Cunit),
         Max_Gnat_Nodes  => Int (Nodes.Last - Nodes.First + 1),
         Number_Names    => Int (Name_Entries.Last - Name_Entries.First + 1),
         Nodes_Ptr       => Nodes.Table (Nodes.First)'Address,
         Names_Ptr       => Name_Entries.Table (Name_Entries.First)'Address,
         Strings_Ptr     => Strings.Table (Strings.First)'Address,
         String_Chars_Ptr => String_Chars.Table (String_Chars.First)'Address,
         List_Headers_Ptr => Lists.Table (Lists.First)'Address,
         Name_Chars_Ptr   => Name_Chars.Table (Name_Chars.First)'Address,
         Uints_Ptr        => Uints.Table (Uints.First)'Address,
         Udigits_Ptr      => Udigits.Table (Udigits.First)'Address,
         Number_Units     => Number_Units,
         File_Info_Ptr    => File_Info_Array'Address,
         Lines_Ptrs       => Lines_Ptr_Array'Address);
   end;
end Back_End;
