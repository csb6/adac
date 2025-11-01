------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  A L I                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.9 $                              --
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

with Binderr; use Binderr;
with Butil;   use Butil;
with Namet;   use Namet;
with Opt;     use Opt;
with Osint;   use Osint;
with Output;  use Output;
package body ALI is

   --------------------
   -- Initialize_ALI --
   --------------------

   procedure Initialize_ALI is
   begin
      ALIs.Init;
      Unit.Init;
      Withs.Init;
      Sdep.Init;
   end Initialize_ALI;

   --------------
   -- Read_ALI --
   --------------

   procedure Read_ALI (Afile, Sfile : File_Name_Type) is
      Id : ALI_Id;
      Text : Text_Buffer_Ptr;

   begin
      --  Immediate return if ALI file already read, as indicated by a non-
      --  zero information field in the names table entry for the file. We
      --  also test for No_File (happens in the generic case), and ignore
      --  the call if Afile is No_File.

      if Afile = No_File or else Get_Name_Table_Info (Afile) /= 0 then
         return;
      end if;

      Text := Read_Library_Info (Afile, Sfile = No_File);

      if Text = null then
         Error_Msg_Name_1 := Afile;
         Error_Msg_Name_2 := Sfile;
         Error_Msg ("% not found, % must be compiled");
         Set_Name_Table_Info (Afile, Int (No_Unit_Id));

      else
         Id := Scan_ALI (Afile, Text);

         for I in ALIs.Table (Id).First_Unit .. ALIs.Table (Id).Last_Unit loop
            for J in Unit.Table (I).First_With .. Unit.Table (I).Last_With loop
               Read_ALI (Withs.Table (J).Afile, Withs.Table (J).Sfile);
            end loop;
         end loop;
      end if;
   end Read_ALI;

   --------------
   -- Scan_ALI --
   --------------

   function Scan_ALI (F : File_Name_Type; T : Text_Buffer_Ptr) return ALI_Id is
      P    : Text_Ptr := T'First;
      Line : Line_Number_Type := 1;
      Id   : ALI_Id;
      C    : Char;

      --  Local subprograms

      function At_Eol return Boolean;
      --  Test if at end of line

      function At_End_Of_Field return Boolean;
      --  Test if at end of line, or if at blank or horizontal tab

      procedure Check_At_End_Of_Field;
      --  Check if we are at end of field, fatal error if not

      procedure Checkc (C : Char);
      --  Check next character is C. If so bump past it, if not fatal error

      procedure Fatal_Error;
      --  Generate fatal error message for badly formatted ALI file

      function Getc return Char;
      --  Get next character, bumping P past the character obtained

      function Get_Name return Name_Id;
      --  Skip blanks, then scan out a name

      function Get_Stamp return Time_Stamp_Type;
      --  Skip blanks, then scan out a time stamp

      procedure Skip_Eol;
      --  Skip past end of line (fatal error if not at end of line)

      procedure Skip_Space;
      --  Skip past white space (blanks or horizontal tab)

      ------------
      -- At_Eol --
      ------------

      function At_Eol return Boolean is
      begin
         return T (P) = EOF or else T (P) = CR or else T (P) = LF;
      end At_Eol;

      ---------------------
      -- At_End_Of_Field --
      ---------------------

      function At_End_Of_Field return Boolean is
      begin
         return T (P) <= ' ';
      end At_End_Of_Field;

      ---------------------------
      -- Check_At_End_Of_Field --
      ---------------------------

      procedure Check_At_End_Of_Field is
      begin
         if not At_End_Of_Field then
            Fatal_Error;
         end if;
      end Check_At_End_Of_Field;

      ------------
      -- Checkc --
      ------------

      procedure Checkc (C : Char) is
      begin
         if T (P) = C then
            P := P + 1;
         else
            Fatal_Error;
         end if;
      end Checkc;

      -----------------
      -- Fatal_Error --
      -----------------

      procedure Fatal_Error is
         Ptr1 : Text_Ptr;
         Ptr2 : Text_Ptr;
         Col  : Int;

         procedure Wchar (C : Char) is
         begin
            if C = HT then
               loop
                  Wchar (' ');
                  exit when Col mod 8 = 0;
               end loop;

            else
               Write_Char (C);
               Col := Col + 1;
            end if;
         end Wchar;

      begin
         Write_Str ("fatal error: file ");
         Write_Name (F);
         Write_Str (" is incorrectly formatted");
         Write_Eol;

         --  Find start of line

         Ptr1 := P;

         while Ptr1 > T'First
           and then T (Ptr1 - 1) /= CR
           and then T (Ptr1 - 1) /= LF
         loop
            Ptr1 := Ptr1 - 1;
         end loop;

         Write_Int (Int (Line));
         Write_Str (". ");

         if Line < 100 then
            Write_Char (' ');
         end if;

         if Line < 10 then
            Write_Char (' ');
         end if;

         Col := 0;
         Ptr2 := Ptr1;

         while Ptr2 < T'Last
           and then T (Ptr2) /= CR
           and then T (Ptr2) /= LF
         loop
            Wchar (T (Ptr2));
            Ptr2 := Ptr2 + 1;
         end loop;

         Write_Eol;

         Write_Str ("     ");
         Col := 0;

         while Ptr1 < P loop
            if T (Ptr1) = HT then
               Wchar (HT);
            else
               Wchar (' ');
            end if;

            Ptr1 := Ptr1 + 1;
         end loop;

         Wchar ('|');
         Write_Eol;

         Exit_Program (E_Fatal);
      end Fatal_Error;

      ----------
      -- Getc --
      ----------

      function Getc return Char is
      begin
         if P = T'Last then
            return EOF;
         else
            P := P + 1;
            return T (P - 1);
         end if;
      end Getc;

      --------------
      -- Get_Name --
      --------------

      function Get_Name return Name_Id is
      begin
         Name_Len := 0;
         Skip_Space;

         if At_Eol then
            Fatal_Error;
         end if;

         loop
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := Getc;
            exit when At_End_Of_Field;
         end loop;

         return Name_Find;
      end Get_Name;

      ---------------
      -- Get_Stamp --
      ---------------

      function Get_Stamp return Time_Stamp_Type is
         T : Time_Stamp_Type;

      begin
         Skip_Space;

         if At_Eol then Fatal_Error; end if;

         for I in T'range loop
            T (I) := Getc;
         end loop;

         return T;
      end Get_Stamp;

      --------------
      -- Skip_Eol --
      --------------

      procedure Skip_Eol is
      begin
         Skip_Space;
         if not At_Eol then Fatal_Error; end if;

         --  Loop to skip past blank lines (first time through skips this EOL)

         while T (P) < ' ' and then T (P) /= EOF loop
            if T (P) = LF then
               Line := Line + 1;
            end if;

            P := P + 1;
         end loop;
      end Skip_Eol;

      ----------------
      -- Skip_Space --
      ----------------

      procedure Skip_Space is
      begin
         while T (P) = ' ' or else T (P) = HT loop
            P := P + 1;
         end loop;
      end Skip_Space;

   --------------------------------------
   -- Start of processing for Scan_ALI --
   --------------------------------------

   begin
      ALIs.Increment_Last;
      Id := ALIs.Last;
      Set_Name_Table_Info (F, Int (Id));

      ALIs.Table (Id).Afile := F;
      ALIs.Table (Id).First_Unit := No_Unit_Id;

      --  Acquire library version

      Checkc ('V');
      Checkc (' ');
      Checkc ('"');

      for I in ALIs.Table (Id).Ver'range loop
         ALIs.Table (Id).Ver (I) := Getc;
      end loop;

      Checkc ('"');
      Skip_Eol;

      --  Acquire standard version

      Checkc ('S');
      Checkc (' ');
      Checkc ('"');

      for I in ALIs.Table (Id).Std'range loop
         ALIs.Table (Id).Std (I) := Getc;
      end loop;

      Checkc ('"');
      Skip_Eol;

      --  Acquire main program line if present

      C := Getc;

      if C = 'M' then
         Checkc (' ');

         C := Getc;

         if C = 'F' then
            ALIs.Table (Id).Main_Program := Func;
         elsif C = 'P' then
            ALIs.Table (Id).Main_Program := Proc;
         else
            P := P - 1;
            Fatal_Error;
         end if;

         Skip_Eol;
         C := Getc;

      else
         ALIs.Table (Id).Main_Program := None;
      end if;

      --  Loop to acquire unit entries

      Unit_Loop : while C = 'U' loop
         Checkc (' ');
         Unit.Increment_Last;

         if ALIs.Table (Id).First_Unit = No_Unit_Id then
            ALIs.Table (Id).First_Unit := Unit.Last;
         end if;

         Unit.Table (Unit.Last).Uname          := Get_Name;
         Unit.Table (Unit.Last).My_ALI         := Id;
         Unit.Table (Unit.Last).Sfile          := Get_Name;

         Unit.Table (Unit.Last).Preelab        := False;
         Unit.Table (Unit.Last).Elaborate_Body := False;
         Unit.Table (Unit.Last).First_With     := Withs.Last + 1;

         Set_Name_Table_Info (Unit.Table (Unit.Last).Uname, Int (Unit.Last));

         --  Scan out possible PRE and EB parameters

         while not At_Eol loop
            Skip_Space;

            if Getc = 'P' then
               Checkc ('R');
               Checkc ('E');
               Check_At_End_Of_Field;
               Unit.Table (Unit.Last).Preelab := True;

            elsif Getc = 'E' then
               Checkc ('B');
               Check_At_End_Of_Field;
               Unit.Table (Unit.Last).Elaborate_Body := True;
            end if;
         end loop;

         Skip_Eol;
         C := Getc;


         --  Scan out With lines for this unit

         With_Loop : while C = 'W' loop
            Checkc (' ');
            Withs.Increment_Last;
            Withs.Table (Withs.Last).Uname         := Get_Name;
            Withs.Table (Withs.Last).Elaborate     := False;
            Withs.Table (Withs.Last).Elaborate_All := False;

            --  Generic case

            if At_Eol then
               Withs.Table (Withs.Last).Sfile := No_File;
               Withs.Table (Withs.Last).Afile := No_File;

            --  Normal case

            else
               Withs.Table (Withs.Last).Sfile := Get_Name;
               Withs.Table (Withs.Last).Afile := Get_Name;

               --  Scan out possible E and EA parameters

               while not At_Eol loop
                  Skip_Space;

                  if Getc = 'E' then
                     if At_End_Of_Field then
                        Withs.Table (Withs.Last).Elaborate := True;
                     else
                        Checkc ('A');
                        Check_At_End_Of_Field;
                        Withs.Table (Withs.Last).Elaborate_All := True;
                     end if;
                  end if;
               end loop;
            end if;

            Skip_Eol;
            C := Getc;

            Unit.Table (Unit.Last).Last_With  := Withs.Last;
         end loop With_Loop;

      end loop Unit_Loop;

      --  End loop through units for one ALI file

      ALIs.Table (Id).Last_Unit := Unit.Last;
      ALIs.Table (Id).Sfile := Unit.Table (ALIs.Table (Id).First_Unit).Sfile;

      --  Set types of the units (there can be at most 2 of them)

      if ALIs.Table (Id).First_Unit /= ALIs.Table (Id).Last_Unit then
         Unit.Table (ALIs.Table (Id).First_Unit).Utype := Is_Body;
         Unit.Table (ALIs.Table (Id).Last_Unit).Utype  := Is_Spec;

      else
         Get_Name_String (Unit.Table (Unit.Last).Uname);

         if Name_Buffer (Name_Len) = 'b' then
            Unit.Table (Unit.Last).Utype := Is_Body_Only;
         else
            Unit.Table (Unit.Last).Utype := Is_Spec_Only;
         end if;
      end if;

      --  Scan out source dependency lines for this ALI file

      ALIs.Table (Id).First_Sdep := Sdep.Last + 1;

      while C = 'D' loop
         Checkc (' ');
         Sdep.Increment_Last;
         Sdep.Table (Sdep.Last).Sfile := Get_Name;
         Sdep.Table (Sdep.Last).Stamp := Get_Stamp;

         --  Skip comments after stamp

         while not At_Eol loop
            P := P + 1;
         end loop;

         Skip_Eol;
         C := Getc;
      end loop;

      ALIs.Table (Id).Last_Sdep := Sdep.Last;

      if C /= EOF then
         Fatal_Error;
      end if;

      return Id;
   end Scan_ALI;

   ----------------------
   -- Set_Source_Table --
   ----------------------

   procedure Set_Source_Table is
      F : File_Name_Type;
      S : Source_Id;
      Stamp : Time_Stamp_Type;

   begin
      ALIs_Loop : for A in ALIs.First .. ALIs.Last loop
         Sdep_Loop : for D in
           ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep
         loop
            F := Sdep.Table (D).Sfile;

            --  If this is the first time we are seeing this source file,
            --  then make a new entry in the source table.

            if Get_Name_Table_Info (F) = 0 then
               Source.Increment_Last;
               S := Source.Last;
               Set_Name_Table_Info (F, Int (S));
               Source.Table (S).Sfile := F;

               --  In check source files mode, try to get stamp from file

               if Check_Source_Files then
                  Stamp := Source_File_Stamp (F);

                  --  If we got the stamp, then set the stamp in the source
                  --  table entry and mark it as set from the source so that
                  --  it does not get subsequently changed.

                  if Stamp (Stamp'First) /= ' ' then
                     Source.Table (S).Stamp := Stamp;
                     Source.Table (S).Source_Found := True;

                  --  If we could not find the file, then the stamp is set
                  --  from the dependency table entry (to be possibly reset
                  --  if we find a later stamp in subsequent processing)

                  else
                     Source.Table (S).Stamp := Sdep.Table (D).Stamp;
                     Source.Table (S).Source_Found := False;

                     --  In All_Sources mode, flag error of file not found

                     if All_Sources then
                        Error_Msg_Name_1 := F;
                        Error_Msg ("cannot locate %");
                     end if;
                  end if;

               --  First time for this source file, but Check_Source_Files
               --  is off, so simply initialize the stamp from the Sdep entry

               else
                  Source.Table (S).Source_Found := False;
                  Source.Table (S).Stamp := Sdep.Table (D).Stamp;
               end if;

            --  Here if this is not the first time for this source file,
            --  so that the source table entry is already constructed.

            else
               S := Source_Id (Get_Name_Table_Info (F));

               --  If stamp was set from source file don't touch it. Otherwise
               --  update the stamp if the current reference in the Sdep entry
               --  is later than the current entry in the source table.

               if not Source.Table (S).Source_Found
                 and then Later (Sdep.Table (D).Stamp, Source.Table (S).Stamp)
               then
                  Source.Table (S).Stamp := Sdep.Table (D).Stamp;
               end if;
            end if;

         end loop Sdep_Loop;

      end loop ALIs_Loop;

   end Set_Source_Table;

end ALI;
