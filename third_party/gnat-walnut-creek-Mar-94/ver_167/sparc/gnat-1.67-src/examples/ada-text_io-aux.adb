------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                      A D A . T E X T _ I O . A U X                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.10 $                              --
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

with System; use System;
package body Ada.Text_IO.Aux is

   Max_Num_Of_Files : constant := 60;

   Line_Feed : constant Character := Ascii.Lf;  --  Character'Val (16#0A#);
   Nul       : constant Character := Ascii.Nul; --  Character'Val (16#00#);
   Page_Mark : constant Character := Ascii.Ff;  --  Character'Val (16#0C#);

   --  The term "file" here is used in the same way as in the Ada Reference
   --  Manual, that is it refers to an object of some "file_type". Otherwise
   --  "external file" is used.

   Open_Files : array (1 .. Max_Num_Of_Files) of File_Type;
   --  Used to make sure we don't open too many files and that we do not
   --  open the same file twice.

   Standard_In    : File_Type;
   Standard_Out   : File_Type;
   Standard_Err   : File_Type;

   Scanning_From_File : Boolean;
   --  Determines if characters are read from a File (True) or String (False).

   type Temp_File_Rec;
   type Link is access Temp_File_Rec;

   type Temp_File_Rec is record 
      File_Name : Pstring;
      Next      : Link;
   end record;

   Temp_Files : Link;

   ------------------------
   --  Local Subprograms --
   -------------------------

   procedure Allocate_AFCB;
   --  Determine which AFCB in the Open_Files table is available to be used
   --  for the current file.

   function Alpha (C : Character) return Boolean;
   --  Predicate to test if Character argument is an upper or lower case
   --  letter, returns True if the argument is a letter, False if not.

   function Alphanum (C : Character) return Boolean;
   --  Predicate to test if Character is an upper or lower case letter
   --  or a digit. Returns True if the arguement is a letter or a digit,
   --  False if not.

   procedure Check_Digit;
   --  Assert that the next Character is a digit otherwise raise Data_Error.

   procedure Check_Extended_Digit;
   --  Assert that the next Character is an extended digit otherwise raise
   --  Data_Error.

   procedure Check_File_Open;
   --  Check if the current file is open or not. If the file is not open,
   --  then Status_Error is raised. Otherwise control returns normally.

   procedure Check_Hash (C : Character);
   --  Determine if next Character is matching hash, raise Data_Error if not.
   --  Stores '#' in Work_String.

   procedure Check_Multiple_File_Opens;

   procedure Check_Opened_Ok;
   --  Check that an Fopen succeeded, raise Name_Error if not

   procedure Check_Status_And_Mode (C_Mode : File_Mode);
   --  If the current file is not open, then Status_Error is raised. If
   --  the file is open, then the mode is checked against the argument which
   --  is the desired mode for the operation. If it does not match, then
   --  Mode_Error is raised, otherwise control returns normally.

   procedure Check_Status_And_Mode (C_Mode1, C_Mode2 : File_Mode);
   --  If the current file is not open, then Status_Error is raised. If
   --  the file is open, then the mode is checked against the arguments which
   --  are the desired modes for the operation. If it does not match either
   --  one of them, Mode_Error is raised, otherwise control returns normally.

   procedure Close_File;
   --  Close file and deallocate the AFCB back to the pool.

   procedure Copy_Integer;
   --  This procedure copies a string with the syntax of "based_Integer" from
   --  the input to the Work_String. Underscores are allowed but not copied.

   procedure Copy_Based_Integer;
   --  This procedure copies a string with the syntax of "based_Integer" from
   --  the input to the Work_String. Underscores are allowed but not copied.

   procedure Copyc;
   --  Copy the next input Character to Work_String using WS_Index2

   function Digit (C : Character) return Boolean;
   --  Predicate if C corresponds to the digits 0 thru 9.

   function Extended_Digit (C : Character) return Boolean;
   --  Predicate if C corresponds to the digits 0 thru 9 or letters A thru F.

   function Graphic (C : Character) return Boolean;
   --  Predicate to test if the Character is an Ascii graphic letter.
   --  True if the argument is an Ascii graphic character, False otherwise.

   function Getcp return Character;
   --  Gets the next Character from the string or file being scanned according
   --  to the setting of Scanning_From_File. In string mode, WS_Index1 is
   --  updated. If no more Characters remain to be scanned, End_Error is
   --  raised.

   function Get_Char return Character;
   --  Get the next character from the current text input file. If no 
   --  character is available, End_Error is raised.

   procedure Image_Float (Item : Float; Aft, Exp : Field);
   --  Creates a string image of Item where Aft and Exp control the format
   --  according to the rules in 14.3.8. The result is placed in Work_String.

   procedure Image_Integer (Item : Integer; Base : Integer);
   --  Creates a string image of Item using the given Base and places it
   --  Work_String. If an out of range value or a bad character is
   --  encountered, Data_Error is raised.

   procedure Make_Temp_File_Name;
   --  Generate a unique file name and use it for the name of the current file.

   function Nextc return Character;
   --  Return the next Character to be read from the string file being
   --  scanned, according to the setting of Scanning_From_File. In string
   --  mode WS_Index1 is updated. If we are currently at the end of string
   --  then a line feed is returned.

   function Page_Is_Not_Terminated return Boolean;
   --  Indicates whether the current page of current file is not terminated.

   procedure Put_Blanks (N : Integer);
   --  Write N blanks to the output. There is no check for line overflow, it
   --  is assumed that the caller has already checked for this.

   procedure Put_Buffer (Width    : Integer;
                         Pad_Type : Character;
                         Length : Integer);

   procedure Put_Line1;
   --  Outputs a line feed to the current text file 

   procedure Put_Page;
   --  Write a page mark to current text file.

   procedure Load_Look_Ahead (End_Of_File_Flag : Boolean);
   --  This procedure loads the lookahead for a TEXT_IO input file, leaving
   --  CHARS set to 3 (unless the file is less than 3 bytes long), and CHAR1
   --  CHAR2 and CHAR3 containing the initial characters of the file. A special
   --  exception occurs when the standard input file is the keyboard in which
   --  case we only read 1 character because of interactive I/O except when 
   --  load_look_ahead is called in the case of END_OF_FILE where we want to 
   --  read 2 characters to check for the EOT character. The parameter to this
   --  routine end_of_file_flag is TRUE when processing for and END_OF_FILE
   --  situation and is FALSE otherwise.

   procedure Range_Error;
   --  Procedure called if scanned number is out of range.

   function Scan_Based_Int (Base : Integer) return Integer;

   function Scan_Int return Integer;
   --  This routine scans an Integer value from the string pointed by the
   --  global Integer WS_Index2. On exit WS_Index2 is updated to point to
   --  the first
   --  non-digit. The result returned is always negative. This allows the
   --  largest negative Integer value to be properly stored and converted.
   --  A value of +1 returned indicated that overflow occured.

   procedure Scan_Integer (Width : Integer; Result : out Integer);
   --  Procedure to scan an Ada Integer value and return the Integer result
   --  The parameter Width specifies the width of the field (zero means an
   --  unlimited scan). The input is from the current TEXT_IO input file.

   procedure Scan_Integer_String (Last : out Integer; Result : out Integer);
   --  Procedure to scan an ada integer value and store it in Result.
   --  The input is from the string stored in Work_String. Last is set to
   --  the count of Characters scanned.

   procedure Scan_Integer_Val (Fixed_Field : Boolean; Result : out Integer);
   --  Procedure to scan an Ada Integer value and return the Integer result.

   procedure Scan_Blanks;
   --  Routine to scan past leading blanks to find first non-blank.
   --  Leaves WS_Index1 pointing to first non-blank character.

   procedure Setup_Fixed_Field (Width : Integer);
   --  This procedure is used for numeric conversions where the field to be
   --  scanned has a fixed width (i.e. width parameter is non-zero).
   --  It acquires the field from the input file and copies it to Work_String.
   --  It returns to the caller ready to scan the data from work_string.

   function Sign (C : Character) return Boolean;
   --  Predicate indicating whether character C is '+' or '-' 

   procedure Skipc;
   --  This procedure skips the next input Character.

   procedure Test_Fixed_Field_End;
   --  this procedure is called after scanning an item from a fixed length
   --  field to ensure that only blanks remain in the field. An exception
   --  is raised if there are any unexpected non-blank Characters left in
   --  the field.

   function Upper_Case (C : Character) return Character;
   --  Converts character C to upper case if necessary

   procedure Unimplemented (Message : String) is
   begin
      Text_IO.Put (Message);
      Text_IO.Put_Line (" not implemented yet");
   end Unimplemented;

   procedure Word_Mul (A : Integer;
                       B : Integer;
                       O : out Boolean;
                       R : out Integer);
   --  Multiply with overflow check (use until trapping arithmetic works).

   procedure Word_Sub (A : Integer;
                       B : Integer;
                       O : out Boolean;
                       R : out Integer);
   --  Subtraction with overflow check (use until trapping arithmetic works)

   --
   --  Interface with system calls
   --

   procedure C_Fgetc (F : Text_IO.File_Ptr;
                      C : out Character;
                      Is_Eof : out Boolean);

   procedure Fclose (P : Text_IO.File_Ptr);
   function  Fopen (Name : String; Typ : File_Mode) return Text_IO.File_Ptr;
   procedure Fputc (F : Text_IO.File_Ptr; C : Character);

   function  Isatty (F : Text_IO.File_Ptr) return Boolean;

   function  Stdin return Text_IO.File_Ptr;
   function  Stdout return Text_IO.File_Ptr;
   function  Stderr return Text_IO.File_Ptr;

   procedure Unlink (Name : String);                     

   -----------
   -- Chars --
   -----------

   function Chars return Integer is 
   begin
      return The_File.Count;
   end Chars;

   ---------------
   -- Set_Chars --
   ---------------

   procedure Set_Chars (Val : Integer) is 
   begin
      The_File.Count := Val;
   end Set_Chars;

   -----------
   -- Char1 --
   -----------

   function Char1 return Character is 
   begin
      return The_File.Look_Ahead (1);
   end Char1;

   ---------------
   -- Set_Char1 --
   ---------------

   procedure Set_Char1 (Val : Character) is 
   begin
      The_File.Look_Ahead (1) := Val;
   end Set_Char1;

   -----------
   -- Char2 --
   -----------

   function Char2 return Character is 
   begin
      return The_File.Look_Ahead (2);
   end Char2;

   ---------------
   -- Set_Char2 --
   ---------------

   procedure Set_Char2 (Val : Character) is 
   begin
      The_File.Look_Ahead (2) := Val;
   end Set_Char2;

   -----------
   -- Char3 --
   -----------

   function Char3 return Character is 
   begin
      return The_File.Look_Ahead (3);
   end Char3;

   ---------------
   -- Set_Char3 --
   ---------------

   procedure Set_Char3 (Val : Character) is 
   begin
      The_File.Look_Ahead (3) := Val;
   end Set_Char3;

   ------------
   -- Create --
   ------------

   procedure Create (File : in out File_Type;
                     Mode : in File_Mode := Out_File;
                     Name : in String := "";
                     Form : in String := "") is
   begin
      The_File := File;
      if The_File /= null then
         raise Status_Error; --  File already open
      elsif Mode = In_File then
         raise Use_Error;    -- Unsupported file access
      end if;
      Allocate_AFCB;
      The_File.Name := new String'(Name);
      The_File.Form := new String'(Form);
      The_File.Mode := Mode;
      if Name'Length = 0 then
         Make_Temp_File_Name;
      end if;

      Check_Multiple_File_Opens;
      The_File.AFCB_In_Use := True;
      The_File.Desc := Fopen (The_File.Name.all, Out_File);
      Check_Opened_Ok;

      The_File.Page := 1;
      The_File.Line := 1;
      The_File.Col := 1;
      The_File.Line_Length := 0;
      The_File.Page_Length := 0;
      File := The_File;
   end Create;

   ----------
   -- Open --
   ----------

   procedure Open (File : in out File_Type;
                   Mode : in File_Mode;
                   Name : in String;
                   Form : in String := "") is
   begin
      The_File := File;
      if The_File /= null then
         raise Status_Error; --  File already open
      end if;

      Allocate_AFCB;
      The_File.Name := new String'(Name);
      The_File.Form := new String'(Form);
      The_File.Mode := Mode;
      if Name'Length = 0 then
         Make_Temp_File_Name;
      end if;
      Check_Multiple_File_Opens;
      The_File.AFCB_In_Use := True;

      if Mode = In_File then
         The_File.Desc := Fopen (Name, In_File);
         Check_Opened_Ok;
         Set_Chars (0);
      else 
         The_File.Desc := Fopen (Name, Out_File);
         Check_Opened_Ok;
      end if;
      The_File.Page := 1;
      The_File.Line := 1;
      The_File.Col := 1;
      The_File.Line_Length := 0;
      The_File.Page_Length := 0;
      File := The_File;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
   begin
      The_File := File;
      Check_File_Open;
      if The_File.Mode = Out_File or else The_File.Mode = Append_File then
         --  Simulate effect of NEW_PAGE unless current page is terminated 
         if Page_Is_Not_Terminated then
            if The_File.Col > 1 
              or else (The_File.Col = 1 and then The_File.Line = 1)
            then
               Put_Line1;
            end if;
            Put_Page;
         end if;
      end if;

      --  If the file being closed is one of the default files, set the default
      --  file indicator to null to indicate that the file is closed.

      if The_File = Current_In then
         Current_In := null;
      elsif The_File = Current_Out then
         Current_Out := null;
      elsif The_File = Current_Err then
         Current_Err := null;
      end if;

      --  Sever the association between the given file and its associated
      --  external file. The given file is left closed. Do not perform system
      --  closes on the standard input, output and error files.

      if The_File /= Standard_In
        and then The_File /= Standard_Out
        and then The_File /= Standard_Err
      then
         Close_File;
      end if;

      The_File := null;
      File := The_File;
   end Close;

   ------------
   -- Delete --
   ------------

   procedure Delete (File : in out File_Type) is
      File_Name_To_Delete : Pstring;
   begin
      The_File := File;
      Check_File_Open;
      File_Name_To_Delete := new String'(The_File.Name.all);
      Close (The_File);
      Unlink (File_Name_To_Delete.all);
      File := The_File;
   end Delete;

   -----------
   -- Reset --
   -----------

   procedure Reset (File : in out File_Type;
                    Mode : in File_Mode) is
   begin
      The_File := File;
      Check_File_Open;
      if (The_File = Current_In or else The_File = Current_Out)
        and then The_File.Mode /= Mode
      then
         raise Mode_Error;  --  "Cannot change mode"
      end if;
      if The_File.Mode = Out_File or else The_File.Mode = Append_File then
         --  Simulate NEW_PAGE unless current page already terminated 
         if Page_Is_Not_Terminated then
            if The_File.Col > 1 
              or else (The_File.Col = 1 and then The_File.Line = 1)
            then
               Put_Line1;
            end if;
            Put_Page;
         end if;
      end if;
      Fclose (The_File.Desc);

      if Mode = In_File then
         The_File.Desc := Fopen (The_File.Name.all, In_File);
         Check_Opened_Ok;
      else 
         The_File.Desc := Fopen (The_File.Name.all, Out_File);
         Check_Opened_Ok;
         The_File.Line_Length := 0;
         The_File.Page_Length := 0;
      end if;
      The_File.Mode := Mode;
      Set_Chars (0);
      The_File.Col := 1;
      The_File.Line := 1;
      The_File.Page := 1;
      File := The_File;
   end Reset;

   ----------
   -- Mode --
   ----------

   function Mode (File : in File_Type) return File_Mode is
   begin
      The_File := File;
      Check_File_Open;
      return The_File.Mode;
   end Mode;

   ----------
   -- Name --
   ----------

   function Name (File : in File_Type) return String is
   begin
      The_File := File;
      Check_File_Open;
      return The_File.Name.all;
   end Name;

   ----------
   -- Form --
   ----------

   function Form (File : in File_Type) return String is
   begin
      The_File := File;
      Check_File_Open;
      return The_File.Form.all;
   end Form;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (File : in File_Type) return Boolean is
   begin
      The_File := File;
      return The_File /= null;
   end Is_Open;

   ---------------
   -- Set_Input --
   ---------------

   procedure Set_Input (File : in File_Type) is
   begin
      The_File := File;
      Check_Status_And_Mode (In_File);
      Current_In := The_File;
   end Set_Input;

   ----------------
   -- Set_Output --
   ----------------

   procedure Set_Output (File : in File_Type) is
   begin
      The_File := File;
      Check_Status_And_Mode (Out_File, Append_File);
      Current_Out := The_File;
   end Set_Output;

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error (File : in File_Type) is
   begin
      The_File := File;
      Check_Status_And_Mode (Out_File, Append_File);
      Current_Err := The_File;
   end Set_Error;

   --------------------
   -- Standard_Input --
   --------------------

   function Standard_Input return File_Type is
   begin
      return Standard_In;
   end Standard_Input;

   ---------------------
   -- Standard_Output --
   ---------------------

   function Standard_Output return File_Type is
   begin
      return Standard_Out;
   end Standard_Output;

   --------------------
   -- Standard_Error --
   --------------------

   function Standard_Error return File_Type is
   begin
      return Standard_Err;
   end Standard_Error;

   -------------------
   -- Current_Input --
   -------------------

   function Current_Input return File_Type is
   begin
      return Current_In;
   end Current_Input;

   --------------------
   -- Current_Output --
   --------------------

   function Current_Output return File_Type is
   begin
      return Current_Out;
   end Current_Output;

   -------------------
   -- Current_Error --
   -------------------

   function Current_Error return File_Type is
   begin
      return Current_Err;
   end Current_Error;

   ---------------------
   -- Set_Line_Length --
   ---------------------

   procedure Set_Line_Length (File : in File_Type; To : in Count) is
   begin
      The_File := File;
      Check_Status_And_Mode (Out_File, Append_File);
      The_File.Line_Length := To;
   end Set_Line_Length;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length (File : in File_Type) return Count is
   begin
      The_File := File;
      Check_Status_And_Mode (Out_File, Append_File);
      return The_File.Line_Length;
   end Line_Length;

   ---------------------
   -- Set_Page_Length --
   ---------------------

   procedure Set_Page_Length (File : in File_Type; To : in Count) is
   begin
      The_File := File;
      Check_Status_And_Mode (Out_File, Append_File);
      The_File.Page_Length := To;
   end Set_Page_Length;

   -----------------
   -- Page_Length --
   -----------------

   function Page_Length (File : in File_Type) return Count is
   begin
      The_File := File;
      Check_Status_And_Mode (Out_File, Append_File);
      return The_File.Page_Length;
   end Page_Length;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (File : in File_Type;
                       Spacing : in Positive_Count := 1) is
   begin
      The_File := File;
      Check_Status_And_Mode (Out_File, Append_File);
      for I in 1 .. Spacing loop
         Put_Line1;
      end loop;
   end New_Line;

   ---------------
   -- Skip_Line --
   ---------------

   procedure Skip_Line (File : in File_Type;
                        Spacing : in Positive_Count := 1) is
      C : Character;
   begin
      The_File := File;
      Check_Status_And_Mode (In_File);
      for I in 1 .. Spacing loop
         loop
            Load_Look_Ahead (False);
            exit when Get_Char = Line_Feed;
         end loop;
         --  ignore page marks for standard input.
         --  if The_File = Standard_In then return; end if;

         loop
            Load_Look_Ahead (False);
            exit when Char1 /= Page_Mark;
            C := Get_Char;
         end loop;
      end loop;
   end Skip_Line;

   -----------------
   -- End_Of_Line --
   -----------------

   function End_Of_Line (File : in File_Type) return boolean is
   begin
      The_File := File; 
      Check_Status_And_Mode (In_File);
      Load_Look_Ahead (False);
      return Chars = 0 or else Char1 = Line_Feed;
   end End_Of_Line;

   --------------
   -- New_Page --
   --------------

   procedure New_Page (File : in File_Type) is
   begin
      The_File := File; 
      Check_Status_And_Mode (Out_File, Append_File);
      if The_File.Col > 1 
         or else (The_File.Col = 1 and then The_File.Line = 1) then
         Put_Line1;
      end if;
      Put_Page;
   end New_Page;

   ---------------
   -- Skip_Page --
   ---------------

   procedure Skip_Page (File : in File_Type) is
   begin
      The_File := File; 
      Check_Status_And_Mode (In_File);
      while Get_Char /= Page_Mark loop
         null;
      end loop;
   end Skip_Page;

   -----------------
   -- End_Of_Page --
   -----------------

   function End_Of_Page (File : in File_Type) return Boolean is
   begin
      The_File := File; 
      Check_Status_And_Mode (In_File);
      if Isatty (The_File.Desc) then
         return False;
      end if;
      Load_Look_Ahead (False);
      if Chars > 1 then
         return Char1 = Line_Feed and then Char2 = Page_Mark;
      elsif Chars = 1 then
         return Char1 = Line_Feed;
      else
         return True;
      end if;
   end End_Of_Page;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : in File_Type) return Boolean is
   begin
      The_File := File; 
      Check_Status_And_Mode (In_File);
      Load_Look_Ahead (True);
      if Isatty (The_File.Desc) then
         if Chars = 2 then
            return False;
         elsif Chars = 1 then
            return Char1 = Line_Feed;
         elsif Chars = 0 then
            return True;
         end if;
      else
         if Chars = 2 then
            return Char1 = Line_Feed and then Char2 = Page_Mark;
         elsif Chars = 1 then
            return Char1 = Line_Feed;
         elsif Chars = 0 then
            return True;
         else --  Chars = 3
            return False;
         end if;
      end if;
   end End_Of_File;

   -------------
   -- Set_Col --
   -------------

   procedure Set_Col (File : in File_Type; To : in Positive_Count) is
      C : Character;
   begin
      The_File := File; 
      Check_File_Open;
      if The_File.Mode = In_File then
         --  SET_COL for file of mode In_File
         Load_Look_Ahead (False);
         while The_File.Col /= To 
           or else Char1 = Line_Feed 
           or else Char1 = Page_Mark
         loop
            C := Get_Char;
         end loop;
      else
         --  SET_COL for file of mode Out_File or Append_File
         if The_File.Line_Length > 0 
           and then To > The_File.Line_Length
         then
            raise Layout_Error; --  "SET_COL past end of line"
         end if;
         if To > The_File.Col then 
            Put_Blanks (Integer (To - The_File.Col));
            The_File.Col := To;
         elsif To < The_File.Col then
            Put_Line1;
            Put_Blanks (Integer (To - 1));
            The_File.Col := To;
         end if;
      end if;
   end Set_Col;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line (File : in File_Type; To : in Positive_Count) is
      C : Character;
   begin
      The_File := File;
      Check_File_Open;
      if The_File.Mode = In_File then
         --  SET_LINE for file of mode In_File
         Load_Look_Ahead (False);
         while The_File.Line /= To
           or else Char1 = Page_Mark
         loop
            C := Get_Char;
         end loop;
      else
         --  SET_LINE for file of mode Out_File or Append_File 
         if The_File.Page_Length > 0 
           and then To > The_File.Page_Length 
         then
            raise Layout_Error;  --  "Set_Line > Page_Length"
         end if;
         if To > The_File.Line  then
            for I in 1 .. To - The_File.Line loop
               Put_Line1;
            end loop;
         elsif To < The_File.Line then
            if The_File.Col > 1 
              or else (The_File.Col = 1 and then The_File.Line = 1)
            then
               Put_Line1;
            end if;
            Put_Page;
            for I in 1 .. To - 1 loop 
               Put_Line1;
            end loop;
         end if;
      end if;
   end Set_Line;

   ---------
   -- Col --
   ---------

   function Col (File : in File_Type) return Positive_Count is
   begin
      The_File := File;
      Check_File_Open;
      if (The_File.Col > Count'Last) then
         raise Layout_Error; --  "Col > Count'Last"
      end if;
      return The_File.Col;
   end Col;

   ----------
   -- Line --
   ----------

   function Line (File : in File_Type) return Positive_Count is
   begin
      The_File := File;
      Check_File_Open;
      if (The_File.Line > Count'Last) then
         raise Layout_Error; --  "Line > Count'Last"
      end if;
      return The_File.Line;
   end Line;

   ----------
   -- Page --
   ----------

   function Page (File : in File_Type) return Positive_Count is
   begin
      The_File := File;
      Check_File_Open;
      if (The_File.Page > Count'Last) then
         raise Layout_Error; --  "Page > Count'Last"
      end if;
      return The_File.Page;
   end Page;

   ---------
   -- Get --
   ---------

   procedure Get (Item : out Character) is
   begin
      Check_Status_And_Mode (In_File);
      loop
         Item := Get_Char; 
         exit when Item /= Page_Mark and then Item /= Line_Feed;
      end loop;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (Item : in Character) is
   begin
      Check_Status_And_Mode (Out_File, Append_File);
      if The_File.Line_Length /= 0
        and then The_File.Col > The_File.Line_Length
      then
         Put_Line1;
      end if;
      Fputc (The_File.Desc, Item);
      The_File.Col := The_File.Col + 1;
   end Put;

   ---------
   -- Get --
   ---------

   procedure Get (Item : out String) is
      I : Integer := 0;
      C : Character;
   begin
      Check_Status_And_Mode (In_File);
      while I < Item'Length loop
         C := Get_Char;
         if C /= Line_Feed and then C /= Page_Mark then
            Item (Item'First + I) := C;
            I := I + 1;
         end if;
      end loop;
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (Item : in String) is
   begin
      for I in Item'range loop
         Put (Item (I));
      end loop;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (File : in File_Type; Item : in String) is
   begin
      The_File := File;
      Put (Item);
      New_Line (File, 1);
   end Put_Line;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line (File : in File_Type;
                       Item : out String;
                       Last : out Natural) is
      I_Length : Integer := Item'Length;
      Nstore : Integer := 0;
   begin
      The_File := File;
      Check_Status_And_Mode (In_File);
      loop
         Load_Look_Ahead (False);
         exit when Nstore = I_Length;
         if Char1 = Line_Feed then
            Skip_Line (File, 1);
            exit;
         end if;
         Item (Item'First + Nstore) := Get_Char;
         Nstore := Nstore + 1;
      end loop;
      Last := Item'First + Nstore - 1;
   end Get_Line;

   -------------
   -- Get_Int --
   -------------

   procedure Get_Int (Item  : out Integer;
                      Width : in Field := 0) is
   begin
      Check_Status_And_Mode (In_File);
      Scan_Integer (Width, Item);
   end Get_Int;

   -------------
   -- Put_Int --
   -------------

   procedure Put_Int (Item  : in Integer;
                      Width : in Field;
                      Base  : in Number_Base) is
   begin
      Check_Status_And_Mode (Out_File, Append_File);
      Image_Integer (Item, Base);
      Put_Buffer (Width, 'L', WS_Length);
   end Put_Int;

   -------------
   -- Get_Int --
   -------------

   procedure Get_Int (From : in String;
                      Item : out Integer;
                      Last : out Positive) is
   begin
      WS_Length := From'Length;
      for I in 0 .. WS_Length - 1 loop
         Work_String (I) := From (From'First + I);
      end loop;
      Work_String (WS_Length) := ' ';
      WS_Index1 := 0;
      Scan_Integer_String (Last, Item);
      Last := From'First + Last - 1;
   end Get_Int;

   -------------
   -- Put_Int --
   -------------

   procedure Put_Int (To   : out String;
                      Item : in Integer;
                      Base : in Number_Base) is
      To_Length : Integer := To'Length;
   begin
      Image_Integer  (Item, Base);
      if WS_Length > To_Length then
         raise Layout_Error;
      end if;
      for I in 0 .. To_Length - WS_Length - 1 loop
         To (To'First + I) := ' ';
      end loop;
      for I in To_Length - WS_Length .. To_Length - 1 loop
         To (To'First + I) := Work_String (I - To_Length + WS_Length);
      end loop;
   end Put_Int;

   ---------------
   -- Get_Float --
   ---------------

   procedure Get_Float (Item : out Float;
                        Width : in Field) is
   begin
      Check_Status_And_Mode (In_File);
      Unimplemented ("Float_IO.Get");
   end Get_Float;

   ---------------
   -- Put_Float --
   ---------------

   procedure Put_Float (Item : in Float; 
                        Fore : in Field;
                        Aft  : in Field;
                        Exp  : in Field) is

   begin
      Check_Status_And_Mode (Out_File, Append_File);
      Image_Float (Item, Aft, Exp);
      if Exp = 0 then
         Put_Buffer (Aft + Fore + 1, 'L', WS_Length + 1);
      else
         Put_Buffer (Aft + Fore + Exp + 2, 'L', WS_Length + 1);
      end if;
   end Put_Float;

   ---------------
   -- Get_Float --
   ---------------

   procedure Get_Float (From : in String;
                        Item : out Float;
                        Last : out Positive) is
   begin
      Unimplemented ("Float_IO.Get with String parameter");
   end Get_Float;

   ---------------
   -- Put_Float --
   ---------------

   procedure Put_Float (To   : out String;
                        Item : in Float;
                        Aft  : in Field;
                        Exp  : in Field) is
      To_Length : Natural := To'Length;
   begin
      Image_Float (Item, Aft, Exp);
      if WS_Length > To_Length then
         raise Layout_Error;
      end if;
      for I in 0 .. To_Length - WS_Length - 1 loop
         To (To'First + I) := ' ';
      end loop;
      for I in To_Length - WS_Length .. To_Length - 1 loop
         To (To'First + I) := Work_String (I - To_Length + WS_Length);
      end loop;
   end Put_Float;

   --------------
   -- Put_Page --
   --------------

   procedure Put_Page is
   begin
      Fputc (The_File.Desc, Page_Mark);
      The_File.Page := The_File.Page + 1;
      The_File.Line := 1;
      The_File.Col := 1;
   end Put_Page;

   ---------------
   -- Put_Line1 --
   ---------------

   procedure Put_Line1 is
   begin
      Fputc (The_File.Desc, Line_Feed);
      The_File.Col := 1;
      if The_File.Page_Length > 0 
         and The_File.Line >= The_File.Page_Length 
      then 
         Put_Page;
      else
         The_File.Line := The_File.Line + 1;
      end if;
   end Put_Line1;

   ---------------------
   -- Check_Opened_Ok --
   ---------------------

   procedure Check_Opened_Ok is
   begin
      if The_File.Desc = 0 then
         raise Name_Error; --  Error opening file due to invalid name
      end if;
   end Check_Opened_Ok;

   ---------------------
   -- Check_File_Open --
   ---------------------

   procedure Check_File_Open is
   begin
      --  There are two ways a file can appear closed. Either it is null
      --  which indicates that it was not used as an argument of an Open_Create
      --  call or it is not null but its Is_Open field is False which indicates
      --  that the file was used in an Open/Create but subsequently was closed.

      if The_File = null then
         raise Status_Error; --  File not open
      end if;
   end Check_File_Open;

   ---------------------------
   -- Check_Status_And_Mode --
   ---------------------------

   procedure Check_Status_And_Mode (C_Mode : File_Mode) is
   begin
      Check_File_Open;
      if The_File.Mode /= C_Mode then
         raise Mode_Error;
      end if;
   end Check_Status_And_Mode;

   ---------------------------
   -- Check_Status_And_Mode --
   ---------------------------

   procedure Check_Status_And_Mode (C_Mode1, C_Mode2 : File_Mode) is
   begin
      Check_File_Open;
      if The_File.Mode /= C_Mode1 and then The_File.Mode /= C_Mode2 then
         raise Mode_Error;
      end if;
   end Check_Status_And_Mode;

   -------------------
   -- Allocate_AFCB --
   --------------------

   procedure Allocate_AFCB is
      File_Num : Integer := Open_Files'First;
   begin
      --  Loop thorugh the array of AFCBs stopping at the first vacate spot
      --  that is not currently being used.
      while File_Num <= Max_Num_Of_Files
        and then Open_Files (File_Num) /= null
        and then Open_Files (File_Num).AFCB_In_Use
      loop
         File_Num := File_Num + 1;
      end loop;

      --  No vacate spots were available since too many file are open
      if File_Num > Max_Num_Of_Files then
         raise Use_Error;  --  Too many files open
      end if;

      if Open_Files (File_Num) = null then
         Open_Files (File_Num) := new AFCB;
      end if;
      The_File := Open_Files (File_Num);
   end Allocate_AFCB;

   -------------------------
   -- Make_Temp_File_Name --
   -------------------------

   procedure Make_Temp_File_Name is
      Temp_File_Name  : String (1 .. 14);
      --  The template for temporary file name creation using Mktemp.

      procedure mktemp (S : Address);
      --  mktemp creates a unique temporary file name given the address of
      --  a null terminated template.
      pragma Import (C, mktemp);

   begin
      --  Create a template string which the call to mktemp will fill in to
      --  generate unique name file name.

      Temp_File_Name (1 .. 13) := "ADATEMPXXXXXX";
      Temp_File_Name (14) := Ascii.Nul;
      mktemp (Temp_File_Name'Address);
      The_File.Name := new String'(Temp_File_Name (1 .. 13));

      --  Append the name of the temporary file to the end of the Temp_File
      --  list which will be used for deleting all the temporary files after
      --  completion of the main program.

      if Temp_Files = null then
         Temp_Files := new Temp_File_Rec'(The_File.Name, null);
      else
         Temp_Files.Next := new Temp_File_Rec'(The_File.Name, null);
      end if;
   end Make_Temp_File_Name;

   -------------------------------
   -- Check_Multiple_File_Opens --
   -------------------------------

   procedure Check_Multiple_File_Opens is
   begin
      --  Allow a several opens to read an external file, but not one open to
      --  read and another open to write a external file.
      for I in Open_Files'range loop
         if Open_Files (I) /= null and then Open_Files (I).AFCB_In_Use then
            if The_File.Name.all = Open_Files (I).Name.all
              and then (The_File.Mode /= In_File
                or else Open_Files (I).Mode /= In_File)
            then
               raise Use_Error; --  File already open
            end if;
         end if;
      end loop;
   end Check_Multiple_File_Opens;

   -----------------------------
   --  Page_Is_Not_Terminated --
   -----------------------------

   function Page_Is_Not_Terminated return Boolean is
   begin
      return not (The_File.Col = 1
        and then The_File.Line = 1
        and then The_File.Page /= 1);
   end Page_Is_Not_Terminated;

   ----------------
   -- Close_File --
   ----------------

   procedure Close_File is
      procedure Fclose (F : Text_IO.File_Ptr);
      pragma Import (C, fclose);

      File_Num : Integer := Open_Files'First;
   begin

      while File_Num <= Max_Num_Of_Files 
         and then Open_Files (File_Num) /= The_File 
      loop
         File_Num := File_Num + 1;
      end loop;

      if File_Num > Max_Num_Of_Files then
         raise Status_Error;
      end if;

      Fclose (The_File.Desc);
      The_File.AFCB_In_Use := False;

   end Close_File;

   ---------------------
   -- Load_Look_Ahead --
   ---------------------

   procedure Load_Look_Ahead (End_Of_File_Flag : Boolean) is
      C      : Character;
      Is_Eof : Boolean;
   begin
      --  Load first character of look ahead 
      if Chars = 0 then
         Set_Char2 (Nul);
         Set_Char3 (Nul);
         C_Fgetc (The_File.Desc, C, Is_Eof);
         if Is_Eof then
            Set_Char1 (Nul);
            return;
         else
            Set_Char1 (C);
            Set_Chars (1);
         end if;
      end if;

      --  In the case where reading from the keyboard do not read more than
      --  1 character unless you are processing an end_of_file test.
      if Isatty (The_File.Desc) and then not End_Of_File_Flag then 
         return;
      end if;

      --  Load second character of look ahead
      if Chars = 1 then
         Set_Char3 (Nul);
         C_Fgetc (The_File.Desc, C, Is_Eof);
         if Is_Eof then
            Set_Char2 (Nul);
            return;
         else
            Set_Char2 (C);
            Set_Chars (2);
         end if;
      end if;

      --  Leave lookahead with at most two characters loaded if standard
      --  input is the keyboard.
      if not Isatty (The_File.Desc) then
         --  Load third character of look ahead
         if Chars = 2 then 
            C_Fgetc (The_File.Desc, C, Is_Eof);
            if Is_Eof then
               Set_Char3 (Nul);
               return;
            else
               Set_Char3 (C);
               Set_Chars (3);
            end if;
         end if;
      end if;
   end Load_Look_Ahead;

   --------------
   -- Get_Char --
   --------------

   function Get_Char return Character is
      C : Character;
   begin
      Load_Look_Ahead (False);
      if Chars = 0 then
         raise End_Error;  --  End of file on TEXT_IO input
      end if;

      C := Char1;

      --  Update lookahead
      Set_Char1 (Char2);
      Set_Char2 (Char3);
      Set_Char3 (Nul);
      Set_Chars (Chars - 1);

      --  Update PAGE and LINE counters if page mark or line feed read
      if C = Page_Mark then
         The_File.Page := The_File.Page + 1;
         The_File.Line := 1;
         The_File.Col := 1;
      elsif C = Line_Feed then 
         The_File.Line := The_File.Line + 1;
         The_File.Col := 1;
      else
         The_File.Col := The_File.Col  + 1;
      end if;

      if Character'Pos (C) > 127 then
         raise Data_Error;  --  Character > 127 for TEXT_IO input"
      end if;
      return C;
   end Get_Char;

   ----------------
   -- Upper_Case --
   ----------------

   function Upper_Case (C : Character) return Character is
      V : constant Integer := 32;
   begin
      if C in 'a' .. 'z' then
         return Character'Val (Character'Pos (C) - V);
      else
         return C;
      end if;
   end Upper_Case;

   --------------
   -- Word_Sub --
   --------------

   procedure Word_Sub (A : Integer;
                       B : Integer;
                       O : out Boolean; 
                       R : out Integer) is
   begin
      R := A - B;
      O := ((A < 0 and then B > 0) or else (A > 0 and then B < 0))
           and then ((A < 0 and then R > 0) or else (A > 0 and then R < 0));
   end Word_Sub;

   --------------
   -- Word_Mul --
   --------------

   procedure Word_Mul (A : Integer; 
                       B : Integer;
                       O : out Boolean; 
                       R : out Integer) is
   begin
      if A /= 0 then
         R := A * B;
         O := (B /= R / A) or else (A = -1 and then B < 0 and then R < 0);
      else
         R := 0;
         O := False;
      end if;
   end Word_Mul;

   ----------------
   -- Put_Blanks --
   ----------------

   procedure Put_Blanks (N : Integer) is
   begin
      for I in 1 .. N loop
         Fputc (The_File.Desc, ' ');
      end loop;
   end Put_Blanks;

   ----------------
   -- Put_Buffer --
   ----------------

   procedure Put_Buffer (Width    : Integer;
                         Pad_Type : Character;
                         Length   : Integer) is
      Pad : Character := Pad_Type;
      Target_Length : Integer;
   begin
      if Length >= Width then
         Target_Length := Length;
         Pad := ' ';
      else
         Target_Length := Width;
      end if;

      --  Ensure the buffer size does not exceed the line length
      if The_File.Line_Length > 0 then
         if Count (Target_Length) > The_File.Line_Length then
            raise Layout_Error; --  "Line too big"
         end if;
      end if;
      --  New line if does not fit on current line
      if The_File.Col + Count (Target_Length) - 1 > The_File.Line_Length then
         Put_Line1;
      end if;
      --  Output data with the required padding
      if Pad = 'L' then
         Put_Blanks (Width - Length);
      end if;
      for N in 0 .. Length - 1 loop
         Fputc (The_File.Desc, Work_String (N));
      end loop;
      The_File.Col := The_File.Col + Count (Target_Length);
      if Pad = 'T' then
         Put_Blanks (Width - Length);
      end if;
   end Put_Buffer;

   -----------------
   -- Image_Float --
   -----------------

   procedure Image_Float (Item : Float; Aft, Exp : Field) is

      procedure sprintf (Target     : Address;
                         Fmt        : Address;
                         Precision  : Natural;
                         Value      : Long_Float;
                         Length_Ptr : Address);

      pragma Import (C, sprintf);

      Fmt_E     : constant String := "%.*E%n" & Ascii.NUL;
      Fmt_F     : constant String := "%.*F%n" & Ascii.NUL;
      Fmt_Ptr   : Address; 
      E_Pos     : Natural;
      Exp_Len   : Natural;
      Length    : aliased Natural;
      Precision : Natural;

   begin
      --  The value of Exp controls whether an exponent part is to appear. Use
      --  E or F format in the call to sprintf appropriately.

      if Exp = 0 then
         Fmt_Ptr := Fmt_F'Address;
      else
         Fmt_Ptr := Fmt_E'Address;
      end if;

      --  The number of digits of the fractional part is given by AFT, or is
      --  one if AFT equals zero.  [RM 14.3.8]

      if Aft = 0 then
         Precision := 1;
      else
         Precision := Aft;
      end if;

      sprintf (Target     => Work_String'Address,
               Fmt        => Fmt_Ptr,
               Precision  => Precision,
               Value      => Long_Float (Item),
               Length_Ptr => Length'Address);

      WS_Length := Length - 1;

      --  A certain amount of correction to the exponent part of the image
      --  string is necessary since the semantics of sprintf do not allow the
      --  specification of the number of digits in the exponent as Ada allows.

      --  Find the index of the 'E' in the image string.

      E_Pos := WS_Length;
      while Work_String (E_Pos) /= 'E' loop
         E_Pos := E_Pos - 1;
      end loop;

      Exp_Len := WS_Length - E_Pos;

      --  Since sprintf always use a minimum of two digits for the exponent
      --  if there is a leading zero in the exponent and Exp is specified as
      --  one it is necessary to trim the extra zero off and slide the
      --  remaining digit over one.

      if Exp = 1 then
         if Work_String (E_Pos + 2) = '0' then
            Work_String (E_Pos + 2) := Work_String (E_Pos + 3);
            WS_Length := WS_Length - 1;
         end if;

      --  Extra leading zeroes may need to be added in front of the exponent
      --  if the value of Exp is greater than the number of digits necessary
      --  to portray the tru exponent value.

      elsif Exp > Exp_Len then
         for I in reverse E_Pos + 2 .. WS_Length loop
            Work_String (I + Exp_Len - Exp) := Work_String (I);
         end loop;
         for I in E_Pos + 2 .. E_Pos + 1 + Exp - Exp_Len loop
            Work_String (I) := '0';
         end loop;
         WS_Length := WS_Length + Exp - Exp_Len;
      end if;
   end Image_Float;

   -------------------
   -- Image_Integer --
   -------------------

   procedure Image_Integer (Item : Integer; Base : Integer) is
      P, Q   : Integer;
      Digit  : Integer;
      Buffer : Integer := Item;
   begin
      P := 0;
      if Base /= 10 then
         if Base > 10 then
            Work_String (P) := '1';
            Work_String (P + 1) := Character'Val 
                                     (Character'Pos ('0') + Base - 10);
            Work_String (P + 2) := '#';
            P := P + 3;
         else
            Work_String (P) := Character'Val (Character'Pos ('0') + Base);
            Work_String (P + 1) := '#';
            P := P + 2;
         end if;
      end if;
      --  Deal with the sign. Note we work with the negative of the absolute
      --  value of the number so that we do not have to make special checks
      --  for the largest negative number ion the twos complement case. 
      if Buffer < 0 then
         Work_String (P) := '-';
         P := P + 1;
      else
         Buffer := - Buffer;
      end if;
      --  Convert value to digit string in specified base
      if Buffer = 0 then
         Work_String (P) := '0';
         P := P + 1;
      else
         Q := 15;
         while Buffer /= 0 loop
            Digit := - (Buffer rem Base);
            Buffer := Buffer / Base;
            if Digit > 9 then
               Work_String (Q) := Character'Val (Character'Pos ('A')
                                                 + Digit - 10);
            else
               Work_String (Q) := Character'Val (Character'Pos ('0')
                                                 + Digit);
            end if;
            Q := Q - 1;
         end loop;
         for I in 1 .. 15 - Q loop
            Work_String (P) := Work_String (Q + I);
            P := P + 1;
         end loop;
      end if;
      if Base /= 10 then
         Work_String (P) := '#';
         P := P + 1;
      end if;
      WS_Length := P;
   end Image_Integer;

   -----------
   -- Getcp --
   -----------

   function Getcp return Character is
      C : Character;
   begin
      if Scanning_From_File then
         return Get_Char;
      else
         if WS_Index1 > WS_Length then
            raise End_Error;
         end if;
         WS_Index1 := WS_Index1 + 1;
         return Work_String (WS_Index1);
      end if; 
   end Getcp;

   -----------
   -- Nextc --
   -----------

   function Nextc return Character is
   begin
      if Scanning_From_File then
         Load_Look_Ahead (False);
         return Char1;
      else
         if WS_Index1 < WS_Length then
            return Work_String (WS_Index1);
         else
            return Line_Feed;
         end if;
      end if;
   end Nextc;

   -----------
   -- Skipc --
   -----------

   procedure Skipc is
      C : Character;
   begin
      if Scanning_From_File then
         C := Get_Char;
      else
         WS_Index1 := WS_Index1 + 1;
      end if;
   end Skipc;

   -----------
   -- Copyc --
   -----------

   procedure Copyc is
      C : Character;
   begin
      if Scanning_From_File then
         C := Get_Char;
      else
         if WS_Index1 > WS_Length then
            raise Program_Error;
         else
            C := Work_String (WS_Index1);
            WS_Index1 := WS_Index1 + 1;
         end if;
      end if;
      Work_String (WS_Index2) := Upper_Case (C);
      WS_Index2 := WS_Index2 + 1;
   end Copyc;

   ------------------
   -- Copy_Integer --
   ------------------

   procedure Copy_Integer is
   begin
      Check_Digit;
      while Digit (Nextc) loop
         Copyc;
         if Nextc = '_' then
            Skipc;
            Check_Digit;
         end if;
      end loop;
   end Copy_Integer;

   ------------------------
   -- Copy_Based_Integer --
   ------------------------

   procedure Copy_Based_Integer is
   begin
      Check_Extended_Digit;
      while Extended_Digit (Nextc) loop
         Copyc;
         if Nextc = '_' then
            Skipc;
            Check_Extended_Digit;
         end if;
      end loop;
   end Copy_Based_Integer;

   -----------------
   -- Scan_Blanks --
   -----------------

   procedure Scan_Blanks is
      C : Character;
   begin
      if Scanning_From_File then
         loop
            Load_Look_Ahead (False);
            if Chars = 0 then
               raise End_Error;
            end if;
            C := Nextc;
            if C = ' '
              or else C = Ascii.HT
              or else C = Line_Feed
              or else C = Page_Mark
            then
               C := Getcp;
            else
               exit;
            end if;
         end loop;
      else
         while WS_Index1 <= WS_Length - 1 loop
            if Work_String (WS_Index1) = ' '
              or else Work_String (WS_Index1) = Ascii.HT
            then
               WS_Index1 := WS_Index1 + 1;
            else
               exit;
            end if;
         end loop;
      end if;
   end Scan_Blanks;

   ------------------------
   -- Setup_Fixed_Field --
   ------------------------

   procedure Setup_Fixed_Field (Width : Integer) is
      I  : Integer := 0;
   begin
      loop
         Load_Look_Ahead (False);
         if Width /= I
           and then Chars /= 0
           and then Char1 /= Page_Mark
           and then Char1 /= Line_Feed
         then
            Work_String (I) := Get_Char;
            I := I + 1;
         else
            exit;
         end if;
      end loop;
      WS_Length := I;
      Scanning_From_File := False;
      WS_Index1 := 0;
   end Setup_Fixed_Field;

   --------------------------
   -- Test_Fixed_Field_End --
   --------------------------

   procedure Test_Fixed_Field_End is
   begin
      Scan_Blanks;
      if WS_Index1 < WS_Length then
         raise Data_Error;
      end if;
   end Test_Fixed_Field_End;

   -----------
   -- Alpha --
   -----------

   function Alpha (C : Character) return Boolean is
   begin
      return C in 'A' .. 'Z' or else C in 'a' .. 'z';
   end Alpha;

   --------------
   -- Alphanum --
   --------------

   function Alphanum (C : Character) return Boolean is
   begin
      return Alpha (C) or else C in '0' .. '9';
   end Alphanum;

   -------------
   -- Graphic --
   -------------

   function Graphic (C : Character) return Boolean is
      Low : constant Integer := 32;
      High : constant Integer := 127;
   begin
      return Character'Pos (C) in Low .. High;
   end Graphic;

   -----------
   -- Digit --
   -----------

   function Digit (C : Character) return Boolean is
   begin
      return C in '0' .. '9';
   end Digit;

   --------------------
   -- Extended_Digit --
   --------------------

   function Extended_Digit (C : Character) return Boolean is
   begin
      return C in '0' .. '9' or else C in 'a' .. 'f' or else C in 'A' .. 'F';
   end Extended_Digit;

   ----------
   -- Sign --
   ----------

   function Sign (C : Character) return Boolean is
   begin
      return C = '-' or C = '+';
   end Sign;

   -----------------
   -- Check_Digit --
   -----------------

   procedure Check_Digit is
   begin
      if not (Nextc in '0' .. '9') then
         raise Data_Error;
      end if;
   end Check_Digit;

   ----------------
   -- Check_Hash --
   ----------------

   procedure Check_Hash (C : Character) is
   begin
      if Nextc /= C then
         raise Data_Error;
      end if;
      Skipc;
      Work_String (WS_Index2) := '#';
      WS_Index2 := WS_Index2 + 1;
   end Check_Hash;

   --------------------------
   -- Check_Extended_Digit --
   --------------------------

   procedure Check_Extended_Digit is
   begin
      if not Extended_Digit (Nextc) then
         raise Data_Error;
      end if;
   end Check_Extended_Digit; 

   -----------------
   -- Range_Error --
   -----------------

   procedure Range_Error is
   begin
      raise Data_Error;
   end Range_Error;

   --------------
   -- Scan_Int --
   --------------

   function Scan_Int return Integer is
      Ival : Integer := 0;
      Digit_Value : Integer;
      Overflow1, Overflow2 : Boolean;
   begin
      while WS_Index2 < WS_Length and then Digit (Work_String (WS_Index2)) loop
         Digit_Value := Character'Pos (Work_String (WS_Index2))
                        - Character'Pos ('0');
         WS_Index2 := WS_Index2 + 1;
         Word_Mul (Ival, 10, Overflow1, Ival);
         Word_Sub (Ival, Digit_Value, Overflow2, Ival);
         if Overflow1 or else Overflow2 then
            while WS_Index2 < WS_Length
              and then Digit (Work_String (WS_Index2))
            loop
               WS_Index2 := WS_Index2 + 1;
            end loop;
            return 1;
         end if;
      end loop;
      return Ival;
   end Scan_Int;

   --------------------
   -- Scan_Based_Int --
   --------------------

   function Scan_Based_Int (Base : Integer) return Integer is
   --  this routine scans a based Integer value fromt the string pointed by
   --  the global Integer WS_Index2. On exit WS_Index2 is updated to point
   --  to the
   --  first non-digit. The result returned is always negative. This allows
   --  the largest negative Integer value to be properly stored and converted.
   --  If overflow is detected, then the value +1 is returned to signal
   --  overflow.
      Ival : Integer := 0;
      Digit_Value : Integer;
      Overflow1, Overflow2 : Boolean;
   begin
      while WS_Index2 < WS_Length
        and then Extended_Digit (Work_String (WS_Index2))
      loop
         Word_Mul (Ival, Base, Overflow1, Ival);
         Digit_Value := Character'Pos (Work_String (WS_Index2))
                                       - Character'Pos ('0');
         WS_Index2 := WS_Index2 + 1;
         if Digit_Value > 9 then
            Digit_Value := Digit_Value - 7;
         end if;
         if Digit_Value >= Base then
            raise Data_Error;
         end if;
         Word_Sub (Ival, Digit_Value, Overflow2, Ival);
         if Overflow1 or else Overflow2 then
            while WS_Index2 < WS_Length
              and then Extended_Digit (Work_String (WS_Index2))
            loop
               WS_Index2 := WS_Index2 + 1;
            end loop;
            return 1;
         end if;
      end loop;
      return Ival;
   end Scan_Based_Int;

   ----------------------
   -- Scan_Integer_Val --
   ----------------------

   procedure Scan_Integer_Val (Fixed_Field : Boolean; Result : out Integer) is
      Ival : Integer;
      Sign_Val : Character;
      C : Character;
      Base : Integer;
      Based : Boolean;
      Exponent : Integer;
      Overflow : Boolean;
   begin
      --  First scan out item with the proper syntax and put it in Work_String
      WS_Index2 := 0;
      if Sign (Nextc) then
         Copyc;
      end if;
      Copy_Integer;
      C := Nextc;
      if C = '#' or else C = ':' then
         Skipc;
         Work_String (WS_Index2) := '#';
         WS_Index2 := WS_Index2 + 1;
         Copy_Based_Integer;
         Check_Hash (C);
         Based := True;
      else
         Based := False;
      end if;
      C := Nextc;
      if C = 'e' or else C = 'E' then
         Copyc;
         C := Nextc;
         if C = '+' or else C = '-' then
            Skipc;
         end if;
         Copy_Integer;
         if C = '-' then
            raise Data_Error;  --  Negative exponent in integer value
         end if;
      end if;
      if Fixed_Field then
         Test_Fixed_Field_End;
      end if;
      WS_Length := WS_Index2;
      Work_String (WS_Index2) := ' ';
      --  Now we have the Integer literal stored in Work_String
      WS_Index2 := 0;
      if Sign (Work_String (WS_Index2)) then
         Sign_Val := Work_String (WS_Index2);
         WS_Index2 := WS_Index2 + 1;
      else
         Sign_Val := '+';
      end if;
      if Based then
         Base := - Scan_Int;
         if not (Base in 2 .. 16) then
            raise Data_Error;
         end if;
         WS_Index2 := WS_Index2 + 1;
         Ival := Scan_Based_Int (Base);
         WS_Index2 := WS_Index2 + 1;
      else
         Ival := Scan_Int;
         Base := 10;
      end if;
      --  Number is in Ival (in negative form), deal with exponent.
      if Ival = 1 then
         Range_Error;
      end if;
      if Work_String (WS_Index2) = 'E' then
         WS_Index2 := WS_Index2 + 1;
         Exponent := Scan_Int;
         if Exponent < -64 or else Exponent = 1 then
            Range_Error;
         end if;
         while Exponent /= 0 loop
            Exponent := Exponent + 1;
            Word_Mul (Ival, Base, Overflow, Ival);
            if Overflow then
               Range_Error;
            end if;
         end loop;
      else
         WS_Index2 := WS_Index2 + 1;
      end if;
      if Sign_Val = '+' then
         Ival := -Ival;
         if Ival < 0 then
            Range_Error;
         end if;
      end if;
      Result := Ival;
   end Scan_Integer_Val;

   ------------------
   -- Scan_Integer --
   ------------------

   procedure Scan_Integer (Width : Integer; Result : out Integer) is
   begin
      if Width /= 0 then
         Setup_Fixed_Field (Width);
         Scan_Blanks;
         if WS_Index1 = WS_Length then
            raise Data_Error;  --  String is all blanks
         end if;
         Scan_Integer_Val (True, Result);
      else
         Scanning_From_File := True;
         Scan_Blanks;
         Scan_Integer_Val (False, Result);
      end if;
   end Scan_Integer;

   -------------------------
   -- Scan_Integer_String --
   -------------------------

   procedure Scan_Integer_String (Last : out Integer; Result : out Integer) is
   begin
      Scanning_From_File := False;
      Scan_Blanks;
      if WS_Index1 = WS_Length then
         raise End_Error;
      end if;
      Scan_Integer_Val (False, Result);
      Last := WS_Index1;
   end Scan_Integer_String;

   ---------------------------
   -- Text_IO_Finialization --
   ---------------------------

   procedure Text_IO_Finalization is
   begin
      --  Close all open files except stdin, stdout and stderr

      for I in 4 .. Open_Files'Last loop
         if Open_Files (I) /= null
           and then Open_Files (I).AFCB_In_Use
           and then Open_Files (I).Mode /= In_File
         then
            Close_File;
         end if;
      end loop;

      --  Delete temporary files upon completion of the main program

      while (Temp_Files /= null) loop
         Unlink (Temp_Files.File_Name.all);
         Temp_Files := Temp_Files.Next;
      end loop;
   end Text_IO_Finalization;

   -----------
   -- Fopen --
   -----------

   function Fopen (Name : String; Typ : File_Mode) return Text_IO.File_Ptr is
      function C_Fopen (Name, Typ : Address) return Text_IO.File_Ptr;
      pragma Import (C, C_Fopen, "fopen");

      Name1 : String (Name'First .. Name'Last + 1);
      Read_Only : String (1 .. 3) := "rt";
      Write_Only : String (1 .. 3) := "wt";

   begin 
      Name1 (Name'range) := Name;
      Name1 (Name1'Last) := Nul;
      Read_Only (3) := Nul;
      Write_Only (3) := Nul;
      if Typ = In_File then
         return C_Fopen (Name1'Address, Read_Only'Address);
      else
         return C_Fopen (Name1'Address, Write_Only'Address);
      end if;
   end Fopen;

   ------------
   -- Fclose --
   ------------

   procedure Fclose (P : Text_IO.File_Ptr) is
      procedure C_Fclose (P : Text_IO.File_Ptr);
      pragma Import (C, C_Fclose, "fclose");
   begin  
      C_Fclose (P);
   end Fclose;    

   ------------
   -- Unlink --
   ------------

   procedure Unlink (Name : String) is
      procedure C_Unlink (Name : Address);
      pragma Import (C, C_Unlink, "unlink");

      Name1 : String (Name'First .. Name'Last + 1);
   begin
      Name1 (Name'range) := Name;
      Name1 (Name1'Last) := Nul;
      C_Unlink (Name1'Address);
   end Unlink;

   ------------
   -- Isatty --
   ------------

   function Isatty (F : Text_IO.File_Ptr) return Boolean is
      function C_Isatty (I : Integer) return Boolean;
      pragma Import (C, C_Isatty, "isatty");

      function C_Fileno (F : Text_IO.File_Ptr) return Integer;
      pragma Import (C, C_Fileno, "fileno");
   begin
      return C_Isatty (C_Fileno (F));
   end Isatty;


   -------------
   -- C_Fgetc --
   -------------

   procedure C_Fgetc (F : Text_IO.File_Ptr;
                      C : out Character;
                      Is_Eof : out Boolean) is
      I : Integer;
      function Fgetc (F : Text_IO.File_Ptr) return Integer;
      pragma Import (C, Fgetc, "fgetc");
   begin
      I := Fgetc (F);
      Is_Eof := I = -1;
      if not Is_Eof then
         C := Character'Val (I);
      end if;
   end C_Fgetc;

   -------------
   -- C_Fputc --
   -------------

   procedure Fputc (F : Text_IO.File_Ptr; C : Character) is
      procedure C_Fputc (C : Character; F : Text_IO.File_Ptr);
      pragma Import (C, C_Fputc, "fputc");
   begin
      C_Fputc (C, F);
   end Fputc;

   -----------
   -- Stdin --
   ------------

   function Stdin return Text_IO.File_Ptr is
      function C_Stdin return Text_IO.File_Ptr;
      pragma Import (C, C_Stdin);
   begin
      return C_Stdin;
   end Stdin;

   ------------
   -- Stdout --
   ------------

   function Stdout return Text_IO.File_Ptr is
      function C_Stdout return Text_IO.File_Ptr;
      pragma Import (C, C_Stdout);
   begin
      return C_Stdout;
   end Stdout;

   ------------
   -- Stderr --
   ------------

   function Stderr return Text_IO.File_Ptr is
      function C_Stderr return Text_IO.File_Ptr;
      pragma Import (C, C_Stderr);
   begin
      return C_Stderr;
   end Stderr;

begin

   --  Initialization of Standard Input

   Standard_In := new AFCB'(AFCB_In_Use => True,
     Desc => Stdin,
     Name => new String'("Standard_Input"),
     Form => new String'("rt"),
     Mode => In_File,
     Col  => 1,
     Line => 1,
     Page => 1,
     Line_Length => 80,
     Page_Length => 24,
     Count => 0,
     Look_Ahead => "   ");

   --  Initialization of Standard Output

   Standard_Out := new AFCB'(AFCB_In_Use => True,
     Desc => Stdout,
     Name => new String'("Standard_Output"),
     Form => new String'("wt"),
     Mode => Out_File,
     Col  => 1,
     Line => 1,
     Page => 1,
     Line_Length => 80,
     Page_Length => 24,
     Count => 0,
     Look_Ahead => "   ");

   --  Initialization of Standard Error

   Standard_Err := new AFCB'(AFCB_In_Use => True,
     Desc => Stderr,
     Name => new String'("Standard_Error"),
     Form => new String'("wt"),
     Mode => Out_File,
     Col  => 1,
     Line => 1,
     Page => 1,
     Line_Length => 80,
     Page_Length => 24,
     Count => 0,
     Look_Ahead => "   ");

   Current_In  := Standard_In;
   Current_Out := Standard_Out;
   Current_Err := Standard_Err;

   Open_Files (Open_Files'First + 0) := Standard_In;
   Open_Files (Open_Files'First + 1) := Standard_Out;
   Open_Files (Open_Files'First + 2) := Standard_Err;

end Ada.Text_IO.Aux;


----------------------
-- REVISION HISTORY --
----------------------

--  ----------------------------
--  revision 1.8
--  date: Wed Dec 15 16:56:33 1993;  author: banner
--  (Get_Float): remove call to Check_Status_And_Mode for String version.
--  (Put_Float): remove call to Check_Status_And_Mode for String version.
--  (Put_Float): call Image_Float to do most of the work.
--  (Put_Buffer): in the last loop replace calls to Put on Character with
--   calls to Fputc instead so that Col does not get incremented too much.
--  (Image_Float): change parameter type of Item to Float.
--  (Image_Float): move code formerly in Put_Float here and generalize.
--  (Image_Float): add code to correct exponent from the result of sprintf
--   to comply with the semantics of the Exp parameter.
--  ----------------------------
--  revision 1.9
--  date: Mon Dec 20 00:08:47 1993;  author: banner
--  Add declaration for Standard_Err and an entry for it in Open_Files.
--  Add initializations for Standard_Err and Current_Err.
--  (Current_Error): Add new Ada 9X function.
--  (Standard_Error): Add new Ada 9X function.
--  (Set_Error): Add new Ada 9X function.
--  (Stderr): new function to retrieve value of stderr.
--  (Text_IO_Finalization): Adjust iteration over Open_Files to allow for
--   Standard_Err.
--  ----------------------------
--  revision 1.10
--  date: Wed Dec 22 13:23:24 1993;  author: banner
--  (Close): Do not close the file associated with Standard_Error.
--  (Text_IO_Finalization): Adjust comment to mention stderr.
--  (Nextc): correct test against WS_Length.
--  (Scan_Blanks): scan past Horizontal Tab as well as blanks in loop.
--  (Scan_Integer, Scan_Integer_String): correct test against WS_Length.
--  (Scan_Integer_Val): set WS_Length properly before rescan of Work_String.
--  (Scan_Int, Scan_Based_Int): add condition in loops which check boundary
--    of Work_String WS_Length during iteration to make sure do not go past
--    end of string.
--  ----------------------------
--  New changes after this line.  Each line starts with: "--  "
