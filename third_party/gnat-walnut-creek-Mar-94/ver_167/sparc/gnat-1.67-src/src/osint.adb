------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                O S I N T                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.41 $                             --
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
with Output;  use Output;
with Switch;  use Switch;
with Unixlib; use Unixlib;
with Sysid;   use Sysid;
with Opt;     use Opt;

with System_Environment; use System_Environment;

package body Osint is

   File_Names : array (Int range 1 .. Int (Arg_Count)) of Str_Ptr;
   --  As arguments are scanned in OS_Interface_Init, filenames are stored
   --  in this array. The string does not contain a terminating NUL.

   Number_File_Names : Int := 0;
   --  The total number of filenames found on command line and placed in
   --  File_Names

   Current_File_Name_Index : Int := 0;
   --  The index in File_Names of the last file opened by Open_Next_File.
   --  The value 0 indicates that no files have been opened yet.

   In_Binder   : Boolean;
   In_Compiler : Boolean;
   In_Make     : Boolean;
   --  Exactly one of these flags is set True to indicate which program
   --  is bound and executing with Osint, which is used by all these programs.

   Source_Time_Stamp : Time_Stamp_Type;
   --  Time stamp for current source file

   Output_FD : Unix_FD;
   --  The file descriptor for the current library info or binder output

   Next_Source_Low_Bound : Source_Ptr := 0;
   --  Value for low bound of next text buffer

   EOL : constant Character := Ascii.LF;
   --  End of line character

   Output_Filename : Str_Ptr := null;
   --  The name after the -o option

   Save_Full_Source_Name : Name_Id;
   --  Set to full name of source file read by the most recent call to
   --  Read_Source_File (result returned by Full_Source_Name)

   Save_Full_Library_Info_Name : Name_Id;
   --  Set to full name of library information file read by the most recent
   --  call to Read_Library_Info (result returned by Full_Library_Info_Name)

   -----------------------------
   -- Initialize_OS_Interface --
   -----------------------------

   procedure Initialize_OS_Interface (P : Program_Type) is
      Already_Seen : Boolean := False;

   begin
      Program := P;

      case Program is
         when Binder   => In_Binder   := True;
         when Compiler => In_Compiler := True;
         when Make     => In_Make     := True;
      end case;

      --  Set PC character set as default if operating system is MS/DOS style

      if MS_DOS then
         Identifier_Character_Set := 'p';
      end if;

      --  Loop through command line arguments, storing them for later access

      for Next_Arg in 1 .. Arg_Count - 1 loop
         declare
            Next_Argv : constant String := Arg_Value (Next_Arg).all;
            Current_Arg : Str (1 .. Nat (Next_Argv'Last));

         begin

            if Next_Argv'Length /= 0 and then Next_Argv (1) = '-' then
               Scan_Switches (Next_Argv);

            --  Not a switch, so must be a filename (if non-empty)

            elsif Next_Argv'Length /= 0 then -- Ignore empty arguments
               for I in Next_Argv'range loop
                  Current_Arg (Nat (I)) :=
                    Char'Val (Character'Pos (Next_Argv (I)));
               end loop;

               if Output_Filename_Present and not Already_Seen then

                  Already_Seen := True;
                  Output_Filename := new Str'(Current_Arg);
               else

                  Number_File_Names := Number_File_Names + 1;
                  File_Names (Number_File_Names) := new Str'(Current_Arg);
               end if;
            end if;
         end;
      end loop;

   end Initialize_OS_Interface;

   ------------------------
   -- Write_Program_Name --
   ------------------------

   procedure Write_Program_Name is
   begin
      Write_String (Arg_Value (0).all);
   end Write_Program_Name;

   -------------------------------------
   -- System_Maximum_File_Name_Length --
   -------------------------------------

   function System_Maximum_File_Name_Length return Pos is
   begin
      return Int'Last;
   end System_Maximum_File_Name_Length;

   -----------------------
   -- More_Source_Files --
   -----------------------

   function More_Source_Files return Boolean is
   begin
      pragma Assert (In_Compiler or In_Make);
      return (Current_File_Name_Index < Number_File_Names);
   end More_Source_Files;

   ----------------------
   -- Next_Main_Source --
   ----------------------

   function Next_Main_Source return File_Name_Type is
      File_Name : Str_Ptr;
      Fptr      : Int;

   begin
      pragma Assert (In_Compiler or In_Make);
      Current_File_Name_Index := Current_File_Name_Index + 1;

      --  Fatal error if no more files (should call More_Source_Files)

      pragma Assert (Current_File_Name_Index <= Number_File_Names);

      --  Otherwise return name of the file

      File_Name := File_Names (Current_File_Name_Index);
      Fptr := File_Name'First;

      for I in reverse File_Name'range loop
         if File_Name (I) = '/' then
            Fptr := I + 1;
         end if;
      end loop;

      Name_Len := File_Name'Last - Fptr + 1;

      Name_Buffer (1 .. Name_Len) := File_Name (Fptr .. File_Name'Last);
      Next_Source_Low_Bound := 0;
      return File_Name_Type (Name_Find);
   end Next_Main_Source;

   ----------------------
   -- Read_Source_File --
   ----------------------

   function Read_Source_File (Name : File_Name_Type; Fatal_Err : Boolean)
     return Source_Buffer_Ptr
   is
      Source_File_FD : Unix_FD;
      --  The file descriptor for the current source file. A negative value
      --  indicates failure to open the specified source file.

      Text : Source_Buffer_Ptr;
      --  Allocated text buffer

   begin
      --  Set full source name -- TBD???             

      Get_Name_String (Name);
      Save_Full_Source_Name := Name_Enter;

      --  Set file name (we use supplied file name with the original
      --  directory path from the main source file name)

      Get_Name_String (Name);

      declare
         File_Name  : constant Str_Ptr := File_Names (Current_File_Name_Index);
         Given_Name : Str (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);

      begin
         Name_Buffer (1 .. File_Name'Last) := File_Name.all;
         Name_Len := File_Name'Length;

         for I in reverse 1 .. Name_Len loop
            exit when Name_Buffer (I) = '/';
            Name_Len := Name_Len - 1;
         end loop;

         Name_Buffer (Name_Len + 1 .. Name_Len + Given_Name'Length) :=
           Given_Name;

         Name_Len := Name_Len + Given_Name'Length;

         --  Now open the file

         Name_Buffer (Name_Len + 1) := NUL;
         Source_File_FD := Unix_Open_Read (Name_Buffer'Address);

         if Source_File_FD < 0 then
            if Fatal_Err then
               Write_String ("Cannot open: ");
               Write_Str (File_Name.all);
               Write_Eol;
               Exit_Program (E_Fatal);
            else
               return null;
            end if;
         end if;

      end;

      --  Read data from the file

      declare
         Len : Int := Unix_File_Length (Source_File_FD);
         --  Length of source file text

         Lo : Source_Ptr := Next_Source_Low_Bound;
         --  Low bound for allocated text buffer

         Hi : Source_Ptr := Lo + Source_Ptr (Len);
         --  High bound for allocated text buffer. Note length is Len + 1
         --  which allows for extra EOF character at the end of the buffer.

         Block_Size : constant := 2**14;
         --  Block size for read

         Ptr : Source_Ptr := Lo;
         --  Next location in text buffer to fill

         Count : Int;
         --  Count of characters read

      begin
         Text := new Source_Buffer (Lo .. Hi);
         --  Note extra charater at end for EOF character

         loop
            Count :=
              Unix_Read (Source_File_FD, Text (Ptr)'Address, Block_Size);
            exit when Count < Block_Size;
            Ptr := Ptr + Block_Size;
         end loop;

         Text (Hi) := EOF;
         Next_Source_Low_Bound := Hi + 1;
      end;

      --  Read is complete, get time stamp, close file and we are done

      Source_Time_Stamp := Unix_File_Time_Stamp (Source_File_FD);
      Unixlib.Unix_Close (Source_File_FD);
      return Text;

   end Read_Source_File;

   -------------------------------
   -- Current_Source_File_Stamp --
   -------------------------------

   function Current_Source_File_Stamp return Time_Stamp_Type is
   begin
      return Source_Time_Stamp;
   end Current_Source_File_Stamp;

   ----------------------
   -- Full_Source_Name --
   ----------------------

   function Full_Source_Name return Name_Id is
   begin
      return Save_Full_Source_Name;
   end Full_Source_Name;

   -----------------------
   -- Source_File_Stamp --
   -----------------------

   function Source_File_Stamp (Name : File_Name_Type) return Time_Stamp_Type is
      Text : Source_Buffer_Ptr;

   begin
      Text := Read_Source_File (Name, False);

      if Text = null then
         return "            ";

      else
         Free (Text);
         return Source_Time_Stamp;
      end if;
   end Source_File_Stamp;

   --------------------
   -- More_Lib_Files --
   --------------------

   function More_Lib_Files return Boolean is
   begin
      pragma Assert (In_Binder);
      return (Current_File_Name_Index < Number_File_Names);
   end More_Lib_Files;

   ------------------------
   -- Next_Main_Lib_File --
   ------------------------

   function Next_Main_Lib_File return File_Name_Type is
      File_Name : Str_Ptr;
      Fptr      : Int;

   begin
      pragma Assert (In_Binder);
      Current_File_Name_Index := Current_File_Name_Index + 1;

      --  Fatal error if no more files (should call More_Lib_Files)

      pragma Assert (Current_File_Name_Index <= Number_File_Names);

      --  Otherwise return name of the file

      File_Name := File_Names (Current_File_Name_Index);
      Fptr := File_Name'First;

      for I in reverse File_Name'range loop
         if File_Name (I) = '/' then
            Fptr := I + 1;
         end if;
      end loop;

      Name_Len := File_Name'Last - Fptr + 1;

      Name_Buffer (1 .. Name_Len) := File_Name (Fptr .. File_Name'Last);
      Next_Source_Low_Bound := 0;
      return File_Name_Type (Name_Find);
   end Next_Main_Lib_File;

   -----------------------
   -- Read_Library_Info --
   -----------------------

   function Read_Library_Info (Lib_File : File_Name_Type; Fatal_Err : Boolean)
     return Text_Buffer_Ptr
   is
      Lib_FD : Unix_FD;
      --  The file descriptor for the current library file. A negative value
      --  indicates failure to open the specified source file.

      Text : Source_Buffer_Ptr;
      --  Allocated text buffer.

   begin
      --  Set full name -- TBD???                      

      Get_Name_String (Lib_File);
      Save_Full_Library_Info_Name := Name_Enter;

      --  Set file name (we use supplied file name with the original
      --  directory path from the main source file name)

      Get_Name_String (Lib_File);

      declare
         File_Name  : Str_Ptr;
         Given_Name : Str (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);

      begin
         File_Name := File_Names (Current_File_Name_Index);
         Name_Buffer (1 .. File_Name'Last) := File_Name.all;
         Name_Len := File_Name'Length;

         for I in reverse 1 .. Name_Len loop
            exit when Name_Buffer (I) = '/';
            Name_Len := Name_Len - 1;
         end loop;

         Name_Buffer (Name_Len + 1 .. Name_Len + Given_Name'Length) :=
           Given_Name;

         Name_Len := Name_Len + Given_Name'Length;

         --  Now open the file

         Name_Buffer (Name_Len + 1) := NUL;
         Lib_FD := Unix_Open_Read (Name_Buffer'Address);

         if Lib_FD < 0 then
            if Fatal_Err then
               Write_String ("Cannot open: ");
               Write_Str (File_Name.all);
               Write_Eol;
               Exit_Program (E_Fatal);
            else
               return null;
            end if;
         end if;

      end;

      --  Read data from the file

      declare
         Len : Int := Unix_File_Length (Lib_FD);
         --  Length of source file text

         Lo : Source_Ptr := 0;
         --  Low bound for allocated text buffer

         Hi : Source_Ptr := Source_Ptr (Len);
         --  High bound for allocated text buffer. Note length is Len + 1
         --  which allows for extra EOF character at the end of the buffer.

         Block_Size : constant := 2**14;
         --  Block size for read

         Ptr : Source_Ptr := Lo;
         --  Next location in text buffer to fill

         Count : Int;
         --  Count of characters read

      begin
         Text := new Source_Buffer (Lo .. Hi);
         --  Note extra charater at end for EOF character

         loop
            Count :=
              Unix_Read (Lib_FD, Text (Ptr)'Address, Block_Size);
            exit when Count < Block_Size;
            Ptr := Ptr + Block_Size;
         end loop;

         Text (Hi) := EOF;
         Next_Source_Low_Bound := Hi + 1;
      end;

      --  Read is complete, close file and we are done

      Unixlib.Unix_Close (Lib_FD);
      return Text;

   end Read_Library_Info;

   ----------------------------
   -- Full_Library_Info_Name --
   ----------------------------

   function Full_Library_Info_Name return Name_Id is
   begin
      return Save_Full_Library_Info_Name;
   end Full_Library_Info_Name;

   --------------------------------
   -- Create_Output_Library_Info --
   --------------------------------

   procedure Create_Output_Library_Info is
      File_Name : constant Str_Ptr := File_Names (Current_File_Name_Index);

      Name_Buf  : Str (File_Name'First .. File_Name'Last + 5);
      --  Buffer for name (with extra room for .ali and NUL)

   begin
      pragma Assert (In_Compiler);
      Name_Buf (File_Name'range) := File_Name.all;

      for I in reverse File_Name'range loop
         if Name_Buf (I) = '.' then
            Name_Buf (I + 1 .. I + 3) := "ali";
            Name_Buf (I + 4) := NUL;
            Output_FD := Unix_Create_File (Name_Buf'Address);

            if Output_FD < 0 then
               Write_String ("Cannot create: ");
               Write_Str (Name_Buf);
               Write_Eol;
               Exit_Program (E_Fatal);
            end if;

            return;
         end if;
      end loop;

      --  Should be impossible not to have an extension

      pragma Assert (False);
   end Create_Output_Library_Info;

   ------------------------
   -- Write_Library_Info --
   ------------------------

   procedure Write_Library_Info (Info : Str) is
   begin
      pragma Assert (In_Compiler);
      Unix_Write (Output_FD, Info'Address, Info'Length);
      Unix_Write (Output_FD, EOL'Address, 1);
   end Write_Library_Info;

   -------------------------------
   -- Close_Output_Library_Info --
   -------------------------------

   procedure Close_Output_Library_Info is
   begin
      pragma Assert (In_Compiler);
      Unix_Close (Output_FD);
   end Close_Output_Library_Info;

   -------------------
   -- Lib_File_Name --
   -------------------

   function Lib_File_Name (Source_File : File_Name_Type)
     return File_Name_Type
   is
      Fptr : Int;
      --  Pointer to location to set extension in place

   begin
      Get_Name_String (Source_File);
      Fptr := Name_Len + 1;

      for I in reverse 1 .. Name_Len loop
         if Name_Buffer (I) = '.' then
            Fptr := I;
            exit;
         end if;
      end loop;

      Name_Buffer (Fptr .. Fptr + 3) := ".ali";
      Name_Buffer (Fptr + 4) := NUL;
      Name_Len := Fptr + 3;
      return Name_Find;
   end Lib_File_Name;

   --------------------------
   -- Create_Binder_Output --
   --------------------------

   procedure Create_Binder_Output is
      File_Name : Str_Ptr;
      Name_Buf  : Str (1 .. 200);
      I : Nat;

   begin
      pragma Assert (In_Binder);

      if (Output_Filename_Present) then

         if Output_Filename /= null then

            Name_Buf (Output_Filename'range) := Output_Filename.all;
            Name_Buf (Output_Filename'Last + 1) := NUL;
         else

            Write_String ("Output filename missing after -o");
            Write_Eol;
            Exit_Program (E_Fatal);
         end if;
      else

         File_Name := File_Names (Current_File_Name_Index);
         Name_Buf (1 .. 5) := "bind_";
         Name_Buf (6 .. File_Name'Length + 5) := File_Name.all;
         Name_Buf (File_Name'Length + 6) := '.';

         I := 6;

         while Name_Buf (I) /=  '.' loop
            I := I + 1;
         end loop;

         Name_Buf (I + 1) := 'c';
         Name_Buf (I + 2) := NUL;
      end if;

      Output_FD := Unix_Create_File (Name_Buf'Address);

      if Output_FD < 0 then
         Write_String ("Cannot create: ");
         Write_Str (Name_Buf);
         Write_Eol;
         Exit_Program (E_Fatal);
      end if;

   end Create_Binder_Output;

   ------------------------
   -- Write_Binder_Info --
   ------------------------

   procedure Write_Binder_Info (Info : Str) is
   begin
      pragma Assert (In_Binder);
      Unix_Write (Output_FD, Info'Address, Info'Length);
      Unix_Write (Output_FD, EOL'Address, 1);
   end Write_Binder_Info;

   -------------------------
   -- Close_Binder_Output --
   -------------------------

   procedure Close_Binder_Output is
   begin
      pragma Assert (In_Binder);
      Unix_Close (Output_FD);
   end Close_Binder_Output;

   ---------------------
   -- Number_Of_Files --
   ---------------------

   function Number_Of_Files return Int is
   begin
      return Number_File_Names;
   end Number_Of_Files;

   ------------------------
   -- Create_Req_Output  --
   ------------------------

   procedure Create_Req_Output is
   begin
      pragma Assert (In_Compiler or In_Make);
      Output_FD := Unix_Create_File (Name_Buffer'Address);    

      if Output_FD < 0 then
         Write_String ("Cannot create REQ File :");
         Write_Str (Name_Buffer);
         Output.Write_Eol;
         Exit_Program (E_Fatal);
      end if;

   end Create_Req_Output;

   -------------------------
   -- Create_Xref_Output  --
   -------------------------

   procedure Create_Xref_Output is

         Multi_File_Xref : constant Str (1 .. 5) := "X.ref";

   begin
      pragma Assert (In_Compiler);

      --  For now, always use X.ref, since cannot reference Lib

      Name_Buffer (1 .. Multi_File_Xref'Last) := Multi_File_Xref;
      Name_Len := Multi_File_Xref'Last;
      Name_Buffer (Name_Len + 1) := NUL;

      Output_FD := Unix_Create_File (Name_Buffer'Address);    

      if Output_FD < 0 then
         Write_String ("Cannot create Xref-File !!!");
         Write_Eol;
         Exit_Program (E_Fatal);
      end if;
   end Create_Xref_Output;

   -------------------------
   -- Write_Xref_Output  --
   -------------------------

   procedure Write_Xref_Info (Info : Str; Eol : Boolean := True) is
   begin
      pragma Assert (In_Compiler);
      Unix_Write (Output_FD, Info'Address, Info'Length);

      if Eol then
         Unix_Write (Output_FD, Osint.EOL'Address, 1);
      end if;
   end Write_Xref_Info;

   -------------------------
   -- Close_Xref_Output   --
   -------------------------

   procedure Close_Xref_Output is
   begin
      pragma Assert (In_Compiler);
      Unix_Close (Output_FD);
   end Close_Xref_Output;

   ------------------
   -- Exit_Program --
   ------------------

   procedure Exit_Program (Exit_Code : Exit_Code_Type) is
   begin
      case Exit_Code is
         when E_Success    => Unix_Exit (0);
         when E_Warnings   => Unix_Exit (0);
         when E_Errors     => Unix_Exit (1);
         when E_Fatal      => Unix_Exit (2);
         when E_Abort      => Unix_Abort;
      end case;
   end Exit_program;

   ----------------
   -- Do_Compile --
   ----------------

   function Do_Compile (Source_File : File_Name_Type) return Exit_Code_Type is
   begin
      return E_Success;
   end Do_Compile;

   -------------
   -- Do_Bind --
   -------------

   function Do_Bind (Lib_File : File_Name_Type) return Exit_Code_Type is
   begin
      return E_Success;
   end Do_Bind;

end Osint;
