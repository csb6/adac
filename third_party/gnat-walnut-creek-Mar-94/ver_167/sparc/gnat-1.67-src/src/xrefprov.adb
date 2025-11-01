------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              X R E F P R O V                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $                              --
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

with Namet;   use Namet;
with Osint;   use Osint;
with Types;   use Types;
with Unixlib; use Unixlib;
with Output;  use Output;

procedure Xrefprov is

   --  This procedure calculates provided interfaces from required
   --  interfaces given on the command line.


   --  The syntax is the following:
   --
   --            provcalc unit.org unit.client1.r unit.client2.r ...
   --
   --  unit.org is the recreated source of the original spec (it
   --  is used to find the correct order of declarations).
   --  unit.clientx.r are required interfaces of the clients for unit.
   --  Note: "unit", "client1", "client2" ... are placeholder for individual
   --        names.
   --  The outfile is named unit.p.


   ---------------
   -- Calculate --
   ---------------

   procedure Calculate is

      --  Variables to handle the input sources:

--  CC
      subtype Ind is Int range 1 .. Number_Of_Files;
      Un : constant Ind := 1;
      File_Buffer : array (Ind) of Source_Buffer_Ptr;
      Line_Start  : array (1 .. Number_Of_Files) of Source_Ptr;
      Line_End    : array (1 .. Number_Of_Files) of Source_Ptr;
      --  Entries into source files without leading spaces (indent
      --  is not constant!!). Line_End normally points to LF.

      Ref_Line_Start : Source_Ptr;
      --  Points to the beginning of the current reference line *with*
      --  leading spaces.

      The_File : File_Name_Type;
      Index : Nat;
      Found : Boolean;

      --  Variable to handle the ouput file:

      File_Handler : Unix_FD;


      ---------------------
      -- Fetch_New_Line --
      ---------------------

      procedure Fetch_New_Line (J : Int) is
      --  "Reads" a new line and updates the corresponding
      --  Line_Start and Line_End values.

         S_Tmp : Source_Ptr;

      begin
         S_Tmp := Line_End (J) + 1;
         while File_Buffer (J).all (S_Tmp) = ' ' loop
            S_Tmp := S_Tmp + 1;
         end loop;
         Line_Start (J) := S_Tmp;

         while File_Buffer (J).all (S_Tmp) /= LF
           and then File_Buffer (J).all (S_Tmp) /= EOF loop
            S_Tmp := S_Tmp + 1;
         end loop;
         Line_End (J) := S_Tmp;
      end Fetch_New_Line;


   ------------------------
   -- Begin of Calculate --
   ------------------------

   begin

      --  Read the recreated spec into memory.

      The_File := Next_Main_Source;
      File_Buffer (1) := Read_Source_File (The_File, True);
      Line_End (1) := File_Buffer (1)'First - 1;
      Fetch_New_Line (1);
      Ref_Line_Start := File_Buffer (1)'First;

      --  Open the provided interface file (suffix .p instead .org)

      Get_Name_String (The_File);
      Index := 1;
      while Name_Buffer (Index) /= '.' loop
         Index := Index + 1;
      end loop;
      Name_Buffer (Index + 1) := 'p';
      Name_Buffer (Index + 2) := NUL;
      File_Handler := Unix_Create_File (Name_Buffer'Address);
      if File_Handler < 0 then
         Write_String ("Cannot create provided interface file!");
         Write_Eol;
         Exit_Program (E_Fatal);
      end if;

      --  Read the required interfaces into memory.

      for I in 2 .. Number_Of_Files loop
         The_File := Next_Main_Source;
         File_Buffer (I) := Read_Source_File (The_File, True);
         Line_End (I) := File_Buffer (I)'First - 1;
         Fetch_New_Line (I);
      end loop;

      --  Start calculating.
--  CC replaced 1 by Un to avoid a GNAT BUG
      while File_Buffer (Un).all (Line_End (1)) /= EOF loop
         Found := False;
         for I in 2 .. Number_Of_Files loop
            if File_Buffer (I).all (Line_Start (I) .. Line_End (I)) =
               File_Buffer (Un).all (Line_Start (1) .. Line_End (1)) 
            then
               Found := True;
               Fetch_New_Line (I);
            end if;
         end loop;

         if Found = True then
            Unix_Write (File_Handler,
                        File_Buffer (Un).all (Ref_Line_Start)'Address,
                        Int (Line_End (1) - Ref_Line_Start) + 1);
         end if;

         Ref_Line_Start := Line_End (1) + 1;
         Fetch_New_Line (1);
      end loop;

      --  Close the provided interface file.

      Unix_Close (File_Handler);

      --  Look for incompatibilities.

      for I in 2 .. Number_Of_Files loop
         if File_Buffer (I).all (Line_End (I)) /= EOF then
            Write_Str ("Error comparing files!");
            Write_Eol;
            Exit_Program (E_Fatal);
         end if;
      end loop;
   end Calculate;


-----------------------
-- begin of Provcalc --
-----------------------

begin
   Initialize_OS_Interface (Compiler);
   Initialize_Namet;

   if Number_Of_Files < 2 then
      Write_Str ("Too less arguments!");
      Write_Eol;
      Exit_Program (E_Fatal);
   else
      Calculate;
   end if;

end Xrefprov;
