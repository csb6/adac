------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . W R I T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.29 $                             --
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

with Atree;   use Atree;
with Einfo;   use Einfo;
with Gnatvsn; use Gnatvsn;
separate (Lib)
procedure Writ is

   Info_Buffer : Str (1 .. 2 * Max_Name_Length + 64);
   --  Info_Buffer used to prepare lines of library output

   Info_Buffer_Len : Nat;
   --  Number of characters stored in Info_Buffer

   Info_Buffer_Col : Nat;
   --  Column number of next character to be written (can be different from
   --  Info_Buffer_Len because of tab characters written by Write_Info_Tab)

   With_Flags : array (File.First .. File.Last) of Boolean;
   --  Array of flags used to show which units are with'ed

   Elab_Flags : array (File.First .. File.Last) of Boolean;
   --  Array of flags used to show which units have pragma Elaborate set

   Elab_All_Flags : array (File.First .. File.Last) of Boolean;
   --  Array of flags used to show which units have pragma Elaborate All set

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Write_Info_Initiate (Key : Char);
   --  Initiates write of new line to info file, the parameter is the keyword

   procedure Write_Info_Terminate;
   --  Terminate output of info line built in Info_Buffer

   procedure Write_Info_Char (C : Char);
   pragma Inline (Write_Info_Char);
   --  Adds one character to Info_Buffer

   procedure Write_Info_Name (Name : Name_Id);
   --  Adds characters of Name to Info_Buffer

   procedure Write_Info_Str (Val : Str);
   --  Adds characters of Val to Info_Buffer surrounded by quotes

   procedure Write_Info_Tab (Col : Int);
   --  Tab out with blanks and HT's to column Col. If already at or past Col,
   --   writes a single blank, so that we do get a required field separation.

   procedure Write_Unit_Information (Unit_Num : Unit_Number_Type);
   --  Write out the library information for one unit for which code is
   --  generated (includes unit line and with lines).

   procedure Collect_Withs (Cunit : Node_Id);
   --  Collect with lines for entries in the context clause of the
   --  given compilation unit, Cunit.

   procedure Write_With_Lines;
   --  Write out with lines collected by calls to Collect_Withs

   -------------------------
   -- Write_Info_Initiate --
   -------------------------

   procedure Write_Info_Initiate (Key : Char) is
   begin
      Info_Buffer_Len := 0;
      Info_Buffer_Col := 1;
      Write_Info_Char (Key);
      Write_Info_Char (' ');
   end Write_Info_Initiate;

   --------------------------
   -- Write_Info_Terminate --
   --------------------------

   procedure Write_Info_Terminate is
   begin
      Write_Library_Info (Info_Buffer (1 .. Info_Buffer_Len));
      Info_Buffer_Len := 0;
   end Write_Info_Terminate;

   ---------------------
   -- Write_Info_Char --
   ---------------------

   procedure Write_Info_Char (C : Char) is
   begin
      Info_Buffer_Len := Info_Buffer_Len + 1;
      Info_Buffer (Info_Buffer_Len) := C;
      Info_Buffer_Col := Info_Buffer_Col + 1;
   end Write_Info_Char;

   ---------------------
   -- Write_Info_Name --
   ---------------------

   procedure Write_Info_Name (Name : Name_Id) is
   begin
      Get_Name_String (Name);
      Info_Buffer (Info_Buffer_Len + 1 .. Info_Buffer_Len + Name_Len) :=
        Name_Buffer (1 .. Name_Len);
      Info_Buffer_Len := Info_Buffer_Len + Name_Len;
      Info_Buffer_Col := Info_Buffer_Col + Name_Len;
   end Write_Info_Name;

   --------------------
   -- Write_Info_Str --
   --------------------

   procedure Write_Info_Str (Val : Str) is
   begin
      Info_Buffer (Info_Buffer_Len + 1 .. Info_Buffer_Len + Val'Length) := Val;
      Info_Buffer_Len := Info_Buffer_Len + Val'Length;
      Info_Buffer_Col := Info_Buffer_Col + Val'Length;
   end Write_Info_Str;

   --------------------
   -- Write_Info_Tab --
   --------------------

   procedure Write_Info_Tab (Col : Int) is
      Next_Tab : Nat;

   begin
      if Col <= Info_Buffer_Col then
         Write_Info_Str ("  ");
      else
         loop
            Next_Tab := 8 * ((Info_Buffer_Col - 1) / 8) + 8 + 1;
            exit when Col < Next_Tab;
            Write_Info_Char (HT);
            Info_Buffer_Col := Next_Tab;
         end loop;

         while Info_Buffer_Col < Col loop
            Write_Info_Char (' ');
         end loop;
      end if;
   end Write_Info_Tab;

   ----------------------------
   -- Write_Unit_Information --
   ----------------------------

   procedure Write_Unit_Information (Unit_Num : Unit_Number_Type) is
      Cunit : constant Node_Id := File.Table (Unit_Num).Cunit;
      Pnode : Node_Id;

   begin
      Write_Info_Initiate ('U');
      Write_Info_Name (File.Table (Unit_Num).Unit_Name);
      Write_Info_Tab (25);
      Write_Info_Name (File.Table (Unit_Num).File_Name);

      if Preelaborable (Cunit) then
         Write_Info_Str ("  PRE");
      end if;

      if Elaborate_Body_Present (Cunit) then
         Write_Info_Str ("  EB");
      end if;
      Write_Info_Terminate;

      --  Generate with lines, first those that are directly with'ed

      for I in With_Flags'range loop
         With_Flags (I) := False;
         Elab_Flags (I) := False;
         Elab_All_Flags (I) := False;
      end loop;

      Collect_Withs (Cunit);

      --  For a body, we must also check for any subunits which belong to
      --  us and which have context clauses of their own, since these with'ed
      --  units our part of our elaboration dependencies.

      if Nkind (Unit (Cunit)) in N_Unit_Body then
         for S in File.First .. File.Last loop

            --  We are only interested in subunits

            if File.Table (S).Source /= null
              and then Nkind (Unit (File.Table (S).Cunit)) = N_Subunit
            then
               Pnode := Library_Unit (File.Table (S).Cunit);

               --  Find ultimate parent of the subunit

               while Nkind (Unit (Pnode)) = N_Subunit loop
                  Pnode := Library_Unit (Pnode);
               end loop;

               --  See if it belongs to us, and if so, include it's with's

               if Pnode = Cunit then
                  Collect_Withs (File.Table (S).Cunit);
               end if;
            end if;
         end loop;
      end if;

      Write_With_Lines;
   end Write_Unit_Information;

   -------------------
   -- Collect_Withs --
   -------------------

   procedure Collect_Withs (Cunit : Node_Id) is
      Item : Node_Id;
      Unum : Unit_Number_Type;

   begin
      Item := First (Context_Items (Cunit));
      while Present (Item) loop

         if Nkind (Item) = N_With_Clause then
            Unum := Get_Cunit_Unit_Number (Library_Unit (Item));
            With_Flags (Unum) := True;

            if Elaborate_Present (Item) then
               Elab_Flags (Unum) := True;
            end if;

            if Elaborate_All_Present (Item) then
               Elab_All_Flags (Unum) := True;
            end if;
         end if;

         Item := Next (Item);
      end loop;
   end Collect_Withs;

   ----------------------
   -- Write_With_Lines --
   ----------------------

   procedure Write_With_Lines is
      With_Table : Unit_Ref_Table (1 .. Pos (File.Last - File.First + 1));
      Num_Withs : Int := 0;
      Cunit : Node_Id;
      Uname : Unit_Name_Type;
      Fname : File_Name_Type;

   begin
      --  Loop to build the with table

      for I in File.First .. File.Last loop
         if With_Flags (I) then
            Num_Withs := Num_Withs + 1;
            With_Table (Num_Withs) := I;
         end if;
      end loop;

      --  Sort and output the table

      Sort (With_Table (1 .. Num_Withs));

      for I in 1 .. Num_Withs loop
         Cunit := File.Table (With_Table (I)).Cunit;
         Uname := File.Table (With_Table (I)).Unit_Name;
         Fname := File.Table (With_Table (I)).File_Name;

         Write_Info_Initiate ('W');
         Write_Info_Name (Uname);

         --  Now we need to figure out the names of the files that contain the
         --  with'ed unit. These will usually be the files for the body, except
         --  except in the case of a package that has no body, as indicated by
         --  the Body_Required flag in the compilation unit node not being set.
         --  No names are output for a generic unit.

         if Nkind (Unit (Cunit)) not in N_Generic_Declaration then
            Write_Info_Tab (25);

            if Body_Required (Cunit)
              or else Nkind (Unit (Cunit)) = N_Subprogram_Declaration
            then
               Write_Info_Name (Get_File_Name (Get_Body_Name (Uname)));
               Write_Info_Tab (49);
               Write_Info_Name
                 (Lib_File_Name (Get_File_Name (Get_Body_Name (Uname))));
            else
               Write_Info_Name (Fname);
               Write_Info_Tab (49);
               Write_Info_Name (Lib_File_Name (Fname));
            end if;

            if Elab_Flags (With_Table (I)) then
               Write_Info_Str ("  E");
            end if;

            if Elab_All_Flags (With_Table (I)) then
               Write_Info_Str ("  EA");
            end if;
         end if;
         Write_Info_Terminate;
      end loop;
   end Write_With_Lines;

   ----------
   -- Writ --
   ----------

begin
   Create_Output_Library_Info;

   --  Output version line

   Write_Info_Initiate ('V');
   Write_Info_Char ('"');
   Write_Info_Str (Library_Version);
   Write_Info_Char ('"');
   Write_Info_Terminate;

   --  Output standard version line

   Write_Info_Initiate ('S');
   Write_Info_Char ('"');
   Write_Info_Str (Standard_Version);
   Write_Info_Char ('"');
   Write_Info_Terminate;

   --  Output main program line if this is acceptable main program

   declare
      U : constant Node_Id := Unit (File.Table (Main_Unit).Cunit);
      S : Node_Id;

   begin
      if Nkind (U) = N_Subprogram_Body then
         S := Specification (U);

         if not List_Present (Parameter_Specifications (S)) then
            if Nkind (S) = N_Procedure_Specification then
               Write_Info_Initiate ('M');
               Write_Info_Char ('P');
               Write_Info_Terminate;

            elsif Is_Integer_Type
              (Etype (Defining_Unit_Name (S)))
            then
               Write_Info_Initiate ('M');
               Write_Info_Char ('F');
               Write_Info_Terminate;
            end if;
         end if;
      end if;
   end;

   --  Loop through file table to output information for all units for which
   --  we have generated code, as marked by the Generate_Code flag.

   for Unit in File.First .. File.Last loop
      if File.Table (Unit).Generate_Code then
         Write_Info_Terminate; -- blank line
         Write_Unit_Information (Unit);
      end if;
   end loop;

   Write_Info_Terminate; -- blank line

   --  Prepare to output the source dependency lines

   declare
      Sdep_Table : Unit_Ref_Table (1 .. Pos (File.Last - File.First + 1));
      --  Keeps track of sdep entries

      Num_Sdep : Int := 0;
      --  Number of active entries in Sdep_Table

   begin
      for Unit in File.First .. File.Last loop
         if File.Table (Unit).Source /= null then
            Num_Sdep := Num_Sdep + 1;
            Sdep_Table (Num_Sdep) := Unit;
         end if;
      end loop;

      Lib.Sort (Sdep_Table (1 .. Num_Sdep));

      for I in 1 .. Num_Sdep loop
         Write_Info_Initiate ('D');
         Write_Info_Name (File.Table (Sdep_Table (I)).File_Name);
         Write_Info_Tab (25);
         Write_Info_Str (File.Table (Sdep_Table (I)).Time_Stamp);

         if File.Table (Sdep_Table (I)).Version (1) /= ' ' then
            Write_Info_Char (' ');
            Write_Info_Str (File.Table (Sdep_Table (I)).Version);
         end if;

         Write_Info_Terminate;
      end loop;
   end;

   Close_Output_Library_Info;
end Writ;
