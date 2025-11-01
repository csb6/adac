------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . L I S T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.16 $                             --
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

with Output; use Output;
separate (Lib)
procedure List is
   Num_Units : constant Int := Int (File.Last) - Int (File.First) + 1;
   --  Number of units in file table

   Sorted_Units : Unit_Ref_Table (1 .. Num_Units);
   --  Table of unit numbers that we will sort

   Unit_Node : Node_Id;
   --  Compilation unit node for current unit

   File_Length : constant := 26;
   --  Length of file name field in characters

   Unit_Length : constant := 22;
   --  Length of unit name field in characters

   Time_Length : constant := Time_Stamp_Length + 4;
   --  Length of time stamp field in characters

   Proc_Length : constant := 10;
   --  Length of processing field in characters

   Unit_Hed : constant Str (1 .. Unit_Length) := "Unit name             ";
   Unit_Und : constant Str (1 .. Unit_Length) := "---------             ";
   Unit_Bln : constant Str (1 .. Unit_Length) := "                      ";
   File_Hed : constant Str (1 .. File_Length) := "File name                 ";
   File_Und : constant Str (1 .. File_Length) := "---------                 ";
   File_Bln : constant Str (1 .. File_Length) := "                  ";
   Time_Hed : constant Str (1 .. Time_Length) := "Time stamp      ";
   Time_Und : constant Str (1 .. Time_Length) := "----------      ";
   Proc_Hed : constant Str (1 .. Proc_Length) := "Processing";
   Proc_Und : constant Str (1 .. Proc_Length) := "----------";
   --  Labels for headers

begin
   --  First step is to make a sorted table of units

   for I in 1 .. Num_Units loop
      Sorted_Units (I) := Unit_Number_Type (Int (File.First) + I - 1);
   end loop;

   Sort (Sorted_Units);

   --  Now we can generate the unit table listing

   Write_Eol;
   Write_Str (Unit_Hed);
   Write_Str (File_Hed);
   Write_Str (Time_Hed);
   Write_Str (Proc_Hed);
   Write_Eol;

   Write_Str (Unit_Und);
   Write_Str (File_Und);
   Write_Str (Time_Und);
   Write_Str (Proc_Und);
   Write_Eol;
   Write_Eol;

   for R in Sorted_Units'range loop
      if File.Table (Sorted_Units (R)).Source /= null then
         Unit_Node := File.Table (Sorted_Units (R)).Cunit;

         Write_Unit_Name (File.Table (Sorted_Units (R)).Unit_Name);

         if Name_Len > (Unit_Length - 1) then
            Write_Eol;
            Write_Str (Unit_Bln);
         else
            for I in Name_Len + 1 .. Unit_Length loop
               Write_Char (' ');
            end loop;
         end if;

         Write_Name (File.Table (Sorted_Units (R)).Full_File_Name);

         if Name_Len > (File_Length - 1) then
            Write_Eol;
            Write_Str (Unit_Bln);
            Write_Str (File_Bln);
         else
            for I in Name_Len + 1 .. File_Length loop
               Write_Char (' ');
            end loop;
         end if;

         Write_Str (File.Table (Sorted_Units (R)).Time_Stamp);

         if Analyzed (Unit_Node) then
            Write_Str ("    (Par,Sem)");
         else
            Write_Str ("    (Par)");
         end if;
      end if;

      Write_Eol;
   end loop;

   Write_Eol;
end List;
