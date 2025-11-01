------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  S C N                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.42 $                             --
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
with Csets;   use Csets;
with Debug;   use Debug;
with Errout;  use Errout;
with Limits;  use Limits;
with Lib;     use Lib;
with Namet;   use Namet;
with Opt;     use Opt;
with Scans;   use Scans;
with Sinput;  use Sinput;
with Sinfo;   use Sinfo;
with Snames;  use Snames;

package body Scn is

   Used_As_Identifier : array (Token_Type) of Boolean;
   --  Flags set True if a given keyword is used as an identifier (used to
   --  make sure that we only post an error message for incorrect use of a
   --  keyword as an identifier once for a given keyword).

   Max_Allowed_Line_Length : Source_Ptr := Max_Line_Length;
   --  Maximum allowed line length (gets reset to 79 if GNAT style checking
   --  mode is active, i.e. GNAT_Style_Check set to True).

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Double_Char_Token (C : Char) return Boolean;
   --  This function is used for double character tokens like := or <>. It
   --  checks if the character following Source (Scan_Ptr) is C, and if so
   --  bumps Scan_Ptr past the pair of characters and returns True. A space
   --  between the two characters is also recognized with an appropriate
   --  error message being issued. If C is not present, False is returned.

   procedure GNAT_Comment_Check;
   --  Called with Scan_Ptr pointing to the first minus of a comment only
   --  if GNAT_Style_Check is set on. Checks for proper formatting and
   --  positioning of comment lines.

   procedure Error_Illegal_Character;
   --  Give illegal character error, Scan_Ptr points to character. On return,
   --  Scan_Ptr is bumped past the illegal character.

   procedure Error_Long_Line;
   --  Signal error of excessively long line

   procedure Error_No_Double_Underline;
   --  Signal error of double underline character

   procedure No_HT_Allowed (S : Source_Ptr);
   --  Called if HT encountered with GNAT_Style_Check set True. Issues an
   --  appropriate error message at the given location.

   procedure Require_Following_Space;
   --  Require token to be followed by white space. Used if in GNAT style
   --  checking mode (GNAT_Style_Check switch set to True).

   procedure Require_Preceding_Space;
   --  Require token to be preceded by white space. Used if in GNAT style
   --  checking mode (GNAT_Style_Check switch set to True).

   procedure Require_Surrounding_Space;
   --  Require token to be surrounded by white space. Used if in GNAT style
   --  checking mode (GNAT_Style_Check switch set to True).

   procedure Set_Name_Char (C : Char);
   pragma Inline (Set_Name_Char);
   --  Stores next character in Namet.Name_Buffer, incrementing count of
   --  currently stored characters in Name_Len. The caller is responsible
   --  for initializing Name_Len to zero before using this routine.

   procedure Set_Start_Column;
   --  This routine is called with Scan_Ptr pointing to the first character
   --  of a line. On exit, Scan_Ptr is advanced to the first non-blank
   --  character of this line (or to the terminating format effector if the
   --  line contains no non-blank characters), and Start_Column is set to the
   --  column number of this non-blank character (zero origin).

   procedure Nlit is separate;
   --  This is the procedure for scanning out numeric literals

   procedure Slit is separate;
   --  This is the procedure for scanning out string literals

   procedure Version_Comment_Scan;
   --  Called if Version_To_Be_Found is set and a comment is encountered to
   --  look for an RCS revision sequence inside the comment. If one is found,
   --  it is recorded in the file table, and Version_To_Be_Found is reset. On
   --  entry Scan_Ptr points to the -- and it is not modified by the call.

   -----------------------
   -- Double_Char_Token --
   -----------------------

   function Double_Char_Token (C : Char) return Boolean is
   begin
      if Source (Scan_Ptr + 1) = C then
         Scan_Ptr := Scan_Ptr + 2;
         return True;

      elsif Source (Scan_Ptr + 1) = ' '
        and then Source (Scan_Ptr + 2) = C
      then
         Scan_Ptr := Scan_Ptr + 1;
         Error_Msg_S ("no space allowed here");
         Scan_Ptr := Scan_Ptr + 2;
         return True;

      else
         return False;
      end if;
   end Double_Char_Token;

   -----------------------------
   -- Error_Illegal_Character --
   -----------------------------

   procedure Error_Illegal_Character is
   begin
      Error_Msg_S ("illegal character");
      Scan_Ptr := Scan_Ptr + 1;
   end Error_Illegal_Character;

   ---------------------
   -- Error_Long_Line --
   ---------------------

   procedure Error_Long_Line is
   begin
      Error_Msg ("this line is too long", Current_Line_Start);
   end Error_Long_Line;

   -------------------------------
   -- Error_No_Double_Underline --
   -------------------------------

   procedure Error_No_Double_Underline is
   begin
      Error_Msg_S ("two consecutive underlines not permitted");
   end Error_No_Double_Underline;

   -------------------
   -- No_HT_Allowed --
   -------------------

   procedure No_HT_Allowed (S : Source_Ptr) is
   begin
      Error_Msg ("(style) horizontal tab not allowed", S);
   end No_HT_Allowed;

   -----------------------------
   -- Require_Following_Space --
   -----------------------------

   procedure Require_Following_Space is
   begin
      if Source (Scan_Ptr) > ' ' then
         Error_Msg_S ("(style) space required");
      end if;
   end Require_Following_Space;

   -----------------------------
   -- Require_Preceding_Space --
   -----------------------------

   procedure Require_Preceding_Space is
   begin
      if Token_Ptr > Source'First
        and then Source (Token_Ptr - 1) > ' '
      then
         Error_Msg_SC ("(style) space required");
      end if;
   end Require_Preceding_Space;

   -------------------------------
   -- Require_Surrounding_Space --
   -------------------------------

   procedure Require_Surrounding_Space is
   begin
      Require_Preceding_Space;
      Require_Following_Space;
   end Require_Surrounding_Space;

   -------------------
   -- Set_Name_Char --
   -------------------

   procedure Set_Name_Char (C : Char) is
   begin
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := C;
   end Set_Name_Char;

   ------------------------
   -- Initialize_Scanner --
   ------------------------

   procedure Initialize_Scanner (Unit : Unit_Number_Type) is
   begin
      --  Set up Token_Type values in Names Table entries for reserved keywords
      --  We use the Pos value of the Token_Type value. Note we are relying on
      --  the fact that Token_Type'Val (0) is not a reserved word!

      Set_Name_Table_Byte (Name_Abort,      Token_Type'Pos (Tok_Abort));
      Set_Name_Table_Byte (Name_Abs,        Token_Type'Pos (Tok_Abs));
      Set_Name_Table_Byte (Name_Abstract,   Token_Type'Pos (Tok_Abstract));
      Set_Name_Table_Byte (Name_Accept,     Token_Type'Pos (Tok_Accept));
      Set_Name_Table_Byte (Name_Access,     Token_Type'Pos (Tok_Access));
      Set_Name_Table_Byte (Name_And,        Token_Type'Pos (Tok_And));
      Set_Name_Table_Byte (Name_Aliased,    Token_Type'Pos (Tok_Aliased));
      Set_Name_Table_Byte (Name_All,        Token_Type'Pos (Tok_All));
      Set_Name_Table_Byte (Name_Array,      Token_Type'Pos (Tok_Array));
      Set_Name_Table_Byte (Name_At,         Token_Type'Pos (Tok_At));
      Set_Name_Table_Byte (Name_Begin,      Token_Type'Pos (Tok_Begin));
      Set_Name_Table_Byte (Name_Body,       Token_Type'Pos (Tok_Body));
      Set_Name_Table_Byte (Name_Case,       Token_Type'Pos (Tok_Case));
      Set_Name_Table_Byte (Name_Constant,   Token_Type'Pos (Tok_Constant));
      Set_Name_Table_Byte (Name_Declare,    Token_Type'Pos (Tok_Declare));
      Set_Name_Table_Byte (Name_Delay,      Token_Type'Pos (Tok_Delay));
      Set_Name_Table_Byte (Name_Delta,      Token_Type'Pos (Tok_Delta));
      Set_Name_Table_Byte (Name_Digits,     Token_Type'Pos (Tok_Digits));
      Set_Name_Table_Byte (Name_Do,         Token_Type'Pos (Tok_Do));
      Set_Name_Table_Byte (Name_Else,       Token_Type'Pos (Tok_Else));
      Set_Name_Table_Byte (Name_Elsif,      Token_Type'Pos (Tok_Elsif));
      Set_Name_Table_Byte (Name_End,        Token_Type'Pos (Tok_End));
      Set_Name_Table_Byte (Name_Entry,      Token_Type'Pos (Tok_Entry));
      Set_Name_Table_Byte (Name_Exception,  Token_Type'Pos (Tok_Exception));
      Set_Name_Table_Byte (Name_Exit,       Token_Type'Pos (Tok_Exit));
      Set_Name_Table_Byte (Name_For,        Token_Type'Pos (Tok_For));
      Set_Name_Table_Byte (Name_Function,   Token_Type'Pos (Tok_Function));
      Set_Name_Table_Byte (Name_Generic,    Token_Type'Pos (Tok_Generic));
      Set_Name_Table_Byte (Name_Goto,       Token_Type'Pos (Tok_Goto));
      Set_Name_Table_Byte (Name_If,         Token_Type'Pos (Tok_If));
      Set_Name_Table_Byte (Name_In,         Token_Type'Pos (Tok_In));
      Set_Name_Table_Byte (Name_Is,         Token_Type'Pos (Tok_Is));
      Set_Name_Table_Byte (Name_Limited,    Token_Type'Pos (Tok_Limited));
      Set_Name_Table_Byte (Name_Loop,       Token_Type'Pos (Tok_Loop));
      Set_Name_Table_Byte (Name_Mod,        Token_Type'Pos (Tok_Mod));
      Set_Name_Table_Byte (Name_New,        Token_Type'Pos (Tok_New));
      Set_Name_Table_Byte (Name_Not,        Token_Type'Pos (Tok_Not));
      Set_Name_Table_Byte (Name_Null,       Token_Type'Pos (Tok_Null));
      Set_Name_Table_Byte (Name_Of,         Token_Type'Pos (Tok_Of));
      Set_Name_Table_Byte (Name_Or,         Token_Type'Pos (Tok_Or));
      Set_Name_Table_Byte (Name_Others,     Token_Type'Pos (Tok_Others));
      Set_Name_Table_Byte (Name_Out,        Token_Type'Pos (Tok_Out));
      Set_Name_Table_Byte (Name_Package,    Token_Type'Pos (Tok_Package));
      Set_Name_Table_Byte (Name_Pragma,     Token_Type'Pos (Tok_Pragma));
      Set_Name_Table_Byte (Name_Private,    Token_Type'Pos (Tok_Private));
      Set_Name_Table_Byte (Name_Procedure,  Token_Type'Pos (Tok_Procedure));
      Set_Name_Table_Byte (Name_Protected,  Token_Type'Pos (Tok_Protected));
      Set_Name_Table_Byte (Name_Raise,      Token_Type'Pos (Tok_Raise));
      Set_Name_Table_Byte (Name_Range,      Token_Type'Pos (Tok_Range));
      Set_Name_Table_Byte (Name_Record,     Token_Type'Pos (Tok_Record));
      Set_Name_Table_Byte (Name_Rem,        Token_Type'Pos (Tok_Rem));
      Set_Name_Table_Byte (Name_Renames,    Token_Type'Pos (Tok_Renames));
      Set_Name_Table_Byte (Name_Requeue,    Token_Type'Pos (Tok_Requeue));
      Set_Name_Table_Byte (Name_Return,     Token_Type'Pos (Tok_Return));
      Set_Name_Table_Byte (Name_Reverse,    Token_Type'Pos (Tok_Reverse));
      Set_Name_Table_Byte (Name_Select,     Token_Type'Pos (Tok_Select));
      Set_Name_Table_Byte (Name_Separate,   Token_Type'Pos (Tok_Separate));
      Set_Name_Table_Byte (Name_Subtype,    Token_Type'Pos (Tok_Subtype));
      Set_Name_Table_Byte (Name_Tagged,     Token_Type'Pos (Tok_Tagged));
      Set_Name_Table_Byte (Name_Task,       Token_Type'Pos (Tok_Task));
      Set_Name_Table_Byte (Name_Terminate,  Token_Type'Pos (Tok_Terminate));
      Set_Name_Table_Byte (Name_Then,       Token_Type'Pos (Tok_Then));
      Set_Name_Table_Byte (Name_Type,       Token_Type'Pos (Tok_Type));
      Set_Name_Table_Byte (Name_Until,      Token_Type'Pos (Tok_Until));
      Set_Name_Table_Byte (Name_Use,        Token_Type'Pos (Tok_Use));
      Set_Name_Table_Byte (Name_When,       Token_Type'Pos (Tok_When));
      Set_Name_Table_Byte (Name_While,      Token_Type'Pos (Tok_While));
      Set_Name_Table_Byte (Name_With,       Token_Type'Pos (Tok_With));
      Set_Name_Table_Byte (Name_Xor,        Token_Type'Pos (Tok_Xor));

      --  Initialize scan control variables

      Source              := File.Table (Unit).Source;
      Source_Cache        := Source;
      Scan_Unit           := Unit;
      Scan_Ptr            := Source'First;
      Token               := No_Token;
      Token_Ptr           := Source'First;
      Current_Line_Start  := Source'First;
      Token_Node          := Empty;
      Token_Name          := No_Name;
      Version_To_Be_Found := Debug_Flag_V;
      Set_Start_Column;

      if GNAT_Style_Check then
         Max_Allowed_Line_Length := 79;
      end if;

      --  Scan initial token (note this initializes Prev_Token, Prev_Token_Ptr)

      Scan;

      --  Clear flags for reserved words used as indentifiers

      for I in Token_Type loop
         Used_As_Identifier (I) := False;
      end loop;

   end Initialize_Scanner;

   ----------
   -- Scan --
   ----------

   procedure Scan is

   begin
      Prev_Token := Token;
      Prev_Token_Ptr := Token_Ptr;
      Token_Name := Error_Name;

      --  The following loop runs more than once only if a format effector
      --  (tab, vertical tab, form  feed, line feed, carriage return) is
      --  encountered and skipped, or some error situation, such as an
      --  illegal character, is encountered.

      loop
         --  Skip past blanks, loop is opened up for speed

         while Source (Scan_Ptr) = ' ' loop

            if Source (Scan_Ptr + 1) /= ' ' then
               Scan_Ptr := Scan_Ptr + 1;
               exit;
            end if;

            if Source (Scan_Ptr + 2) /= ' ' then
               Scan_Ptr := Scan_Ptr + 2;
               exit;
            end if;

            if Source (Scan_Ptr + 3) /= ' ' then
               Scan_Ptr := Scan_Ptr + 3;
               exit;
            end if;

            if Source (Scan_Ptr + 4) /= ' ' then
               Scan_Ptr := Scan_Ptr + 4;
               exit;
            end if;

            if Source (Scan_Ptr + 5) /= ' ' then
               Scan_Ptr := Scan_Ptr + 5;
               exit;
            end if;

            if Source (Scan_Ptr + 6) /= ' ' then
               Scan_Ptr := Scan_Ptr + 6;
               exit;
            end if;

            if Source (Scan_Ptr + 7) /= ' ' then
               Scan_Ptr := Scan_Ptr + 7;
               exit;
            end if;

            Scan_Ptr := Scan_Ptr + 8;
         end loop;

         --  Switch on non-blank character encountered

         Token_Ptr := Scan_Ptr;
         case Source (Scan_Ptr) is

         --  CR ends a physical line. If it is immediately followed by a LF,
         --  then this LF is considered to be part of the same line terminator.

         when CR =>
            if Scan_Ptr - Current_Line_Start > Max_Allowed_Line_Length then
               Error_Long_Line;
            end if;

            Scan_Ptr := Scan_Ptr + 1;

            if Source (Scan_Ptr) = LF then
               Scan_Ptr := Scan_Ptr + 1;
            end if;

            Next_Line;
            Set_Start_Column;

         --  FF never terminates a physical line

         when FF =>
            Scan_Ptr := Scan_Ptr + 1;
            Set_Start_Column;

         --  LF ends a physical line. If it is immediately followed by a CR,
         --  then this CR is considered to be part of the same line terminator.

         when LF =>
            if Scan_Ptr - Current_Line_Start > Max_Allowed_Line_Length then
               Error_Long_Line;
            end if;

            Scan_Ptr := Scan_Ptr + 1;

            if Source (Scan_Ptr) = CR then
               Scan_Ptr := Scan_Ptr + 1;
            end if;

            Next_Line;
            Set_Start_Column;

         --  VT never terminates a physical line

         when VT =>
            Scan_Ptr := Scan_Ptr + 1;
            Set_Start_Column;

         --  Horizontal tab, just skip past it

         when HT =>
            if GNAT_Style_Check then No_HT_Allowed (Scan_Ptr); end if;
            Scan_Ptr := Scan_Ptr + 1;

         --  End of file character

         when EOF =>
            Token := Tok_EOF;
            return;

         --  Ampersand

         when '&' =>
            if Source (Scan_Ptr + 1) = '&' then
               Error_Msg_S ("'&'& should be `AND THEN`");
               Scan_Ptr := Scan_Ptr + 2;
               Token := Tok_And;
               return;

            else
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Ampersand;
               if GNAT_Style_Check then Require_Surrounding_Space; end if;
               return;
            end if;

         --  Asterisk (can be multiplication operator or double asterisk
         --  which is the exponentiation compound delimtier).

         when '*' =>
            if Source (Scan_Ptr + 1) = '*' then
               Scan_Ptr := Scan_Ptr + 2;
               Token := Tok_Double_Asterisk;
               return;

            else
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Asterisk;
               if GNAT_Style_Check then Require_Surrounding_Space; end if;
               return;
            end if;

         --  Colon, which can either be an isolated colon, or part of an
         --  assignment compound delimiter.

         when ':' =>
            if Double_Char_Token ('=') then
               Token := Tok_Colon_Equal;
               if GNAT_Style_Check then Require_Surrounding_Space; end if;
               return;
            else
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Colon;
               if GNAT_Style_Check then Require_Surrounding_Space; end if;
               return;
            end if;

         --  Left parenthesis

         when '(' =>
            if GNAT_Style_Check then
               if Letter_Or_Digit (Source (Scan_Ptr - 1)) then
                  Error_Msg_S ("(style) space required");
               end if;
            end if;

            Scan_Ptr := Scan_Ptr + 1;
            Token := Tok_Left_Paren;
            return;

         --  Left bracket, treated as left paren

         when '[' =>
            Error_Msg_S ("illegal character ""["", replaced by ""(""");
            Scan_Ptr := Scan_Ptr + 1;
            Token := Tok_Left_Paren;
            return;

         --  Comma

         when ',' =>

            --  In GNAT style check mode, a comma must be either the first
            --  token on a line, or be preceded by a blank. It must also 
            --  always be followed by a blank.

            if GNAT_Style_Check then

               if Source (Scan_Ptr - 1) = ' ' 
                 and then Prev_Token_Ptr >= Current_Line_Start
               then
                  Error_Msg ("(style) no space allowed", Scan_Ptr - 1);

               elsif Source (Scan_Ptr + 1) > ' ' then
                  Error_Msg ("(style) space required", Scan_Ptr + 1);
               end if;
            end if;

            Scan_Ptr := Scan_Ptr + 1;
            Token := Tok_Comma;
            return;

         --  Dot, which is either an isolated period, or part of a double
         --  dot compound delimiter sequence. We also check for the case of
         --  a digit following the period, to give a better error message.

         when '.' =>
            if Source (Scan_Ptr + 1) = '.' then
               Scan_Ptr := Scan_Ptr + 2;
               Token := Tok_Dot_Dot;
               if GNAT_Style_Check then Require_Surrounding_Space; end if;
               return;

            elsif Source (Scan_Ptr + 1) in '0' .. '9' then
               Error_Msg_S ("numeric literal cannot start with point");
               Scan_Ptr := Scan_Ptr + 1;

            else
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Dot;
               return;
            end if;

         --  Equal, which can either be an equality operator, or part of the
         --  arrow (=>) compound delimiter.

         when '=' =>
            if Double_Char_Token ('>') then
               Token := Tok_Arrow;
               if GNAT_Style_Check then Require_Surrounding_Space; end if;
               return;

            elsif Source (Scan_Ptr + 1) = '=' then
               Error_Msg_S ("== should be =");
               Scan_Ptr := Scan_Ptr + 1;
            end if;

            Scan_Ptr := Scan_Ptr + 1;
            Token := Tok_Equal;
            if GNAT_Style_Check then Require_Surrounding_Space; end if;
            return;

         --  Greater than, which can be a greater than operator, greater than
         --  or equal operator, or first character of a right label bracket.

         when '>' =>
            if Double_Char_Token ('=') then
               Token := Tok_Greater_Equal;
               if GNAT_Style_Check then Require_Surrounding_Space; end if;
               return;

            elsif Double_Char_Token ('>') then
               Token := Tok_Greater_Greater;
               return;

            else
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Greater;
               return;
            end if;

         --  Less than, which can be a less than operator, less than or equal
         --  operator, or the first character of a left label bracket, or the
         --  first character of a box (<>) compound delimiter.

         when '<' =>
            if Double_Char_Token ('=') then
               Token := Tok_Less_Equal;
               if GNAT_Style_Check then Require_Surrounding_Space; end if;
               return;

            elsif Double_Char_Token ('>') then
               Token := Tok_Box;

               if Prev_Token /= Tok_Left_Paren and then GNAT_Style_Check then
                  Require_Preceding_Space;
               end if;

               return;

            elsif Double_Char_Token ('<') then
               Token := Tok_Less_Less;
               return;

            else
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Less;
               if GNAT_Style_Check then Require_Surrounding_Space; end if;
               return;
            end if;

         --  Minus, which is either a subtraction operator, or the first
         --  character of double minus starting a comment

         when '-' =>
            if Source (Scan_Ptr + 1) = '>' then
               Error_Msg_S ("-> should be =>");
               Scan_Ptr := Scan_Ptr + 2;
               Token := Tok_Arrow;
               if GNAT_Style_Check then Require_Surrounding_Space; end if;
               return;

            elsif Source (Scan_Ptr + 1) /= '-' then
               Scan_Ptr := Scan_Ptr + 1;

               if GNAT_Style_Check
                 and then Source (Token_Ptr - 1) /= '('
               then
                  Require_Preceding_Space;
               end if;

               Token := Tok_Minus;
               return;

            --  Comment

            else -- Source (Scan_Ptr + 1) = '-' then
               if GNAT_Style_Check then
                  GNAT_Comment_Check;
               end if;

               if Version_To_Be_Found then
                  Version_Comment_Scan;
               end if;

               Scan_Ptr := Scan_Ptr + 2;

               --  Loop to scan comment (this loop runs more than once only if
               --  a horizontal tab or other non-graphic character is scanned)

               loop
                  --  Scan to non graphic character (opened up for speed)

                  loop
                     exit when Source (Scan_Ptr) not in Graphic_Character;
                     Scan_Ptr := Scan_Ptr + 1;
                     exit when Source (Scan_Ptr) not in Graphic_Character;
                     Scan_Ptr := Scan_Ptr + 1;
                     exit when Source (Scan_Ptr) not in Graphic_Character;
                     Scan_Ptr := Scan_Ptr + 1;
                     exit when Source (Scan_Ptr) not in Graphic_Character;
                     Scan_Ptr := Scan_Ptr + 1;
                     exit when Source (Scan_Ptr) not in Graphic_Character;
                     Scan_Ptr := Scan_Ptr + 1;
                  end loop;

                  --  Keep going if horizontal tab

                  if Source (Scan_Ptr) = HT then
                     if GNAT_Style_Check then
                        No_HT_Allowed (Scan_Ptr);
                     end if;

                     Scan_Ptr := Scan_Ptr + 1;

                  --  Terminate scan of comment if line terminator or EOF

                  elsif Source (Scan_Ptr) in Line_Terminator
                     or else Source (Scan_Ptr) = EOF
                  then
                     if Scan_Ptr - Current_Line_Start >
                                           Max_Allowed_Line_Length
                     then
                        Error_Long_Line;
                     end if;

                     exit;

                  --  Terminate scan of comment if end of file encountered
                  --  (embedded EOF character or real last character in file)

                  elsif Source (Scan_Ptr) = EOF then
                     exit;

                  --  Keep going if character in 80-FF range. These characters
                  --  are allowed in comments according to the approved AI.
                  --  Also allow ESC, which just got added to the AI (June 93)

                  elsif Source (Scan_Ptr) in Upper_Half_Character
                    or else Source (Scan_Ptr) = ESC
                  then
                     Scan_Ptr := Scan_Ptr + 1;

                  --  Otherwise we have an illegal comment character

                  else
                     Error_Illegal_Character;
                  end if;

               end loop;

               --  Note that we do NOT execute a return here, instead we fall
               --  through to reexecute the scan loop to look for a token.

            end if;

         --  Double quote starting a string constant

         when '"' =>
            Slit;
            return;

         --  Percent starting a string constant

         when '%' =>
            if Ada_9X then
               Error_Msg_S
                 ("obsolescent feature: use of percent for double quote?");
            end if;

            Slit;
            return;

         --  Apostrophe. This can either be the start of a character literal,
         --  or an isolated apostrophe used in a qualified expression or an
         --  attribute. We treat it as a character literal if it does not
         --  follow a right parenthesis, identifier or literal. This means
         --  that we correctly treat constructs like:

         --    A := CHARACTER'('A');

         --  which appears to be illegal according to 2.2(2) (since the rule
         --  there would seem to require separators to avoid the confusion
         --  with the character literal), but all compilers accept the above
         --  statement, and there are at least six ACVC tests that use this
         --  type of lexical sequence, expecting it to be legal, so in fact
         --  all compilers must accept this and we must too!

         when ''' =>
            Scan_Ptr := Scan_Ptr + 1;

            --  Here is where we make the test to distinguish the cases. Treat
            --  as apostrophe if previous token is an identifier, right paren
            --  or the reserved word "all" (latter case as in A.all'Address)
            --  Also treat it as apostrophe after a literal (wrong anyway, but
            --  that's probably the better choice).

            if Prev_Token = Tok_Identifier
               or else Prev_Token = Tok_Right_Paren
               or else Prev_Token = Tok_All
               or else Prev_Token in Token_Class_Literal
            then
               Token := Tok_Apostrophe;
               return;

            --  Otherwise the apostrophe starts a character literal

            else
               --  If we do not find a closing quote in the expected place then
               --  assume that we have a misguided attempt at a string literal.

               if Source (Scan_Ptr + 1) /= ''' then
                  Scan_Ptr := Scan_Ptr - 1;
                  Error_Msg_S
                    ("strings are delimited by double quote character");
                  Slit;
                  return;

               --  Otherwise we have an OK character literal

               else
                  if Source (Scan_Ptr) not in Graphic_Character and then
                     Source (Scan_Ptr) not in Upper_Half_Character
                  then
                     Error_Illegal_Character;
                  end if;

                  Token := Tok_Char_Literal;
                  Token_Node := New_Node (N_Character_Literal, Token_Ptr);
                  Set_Char_Literal_Value
                    (Token_Node, Get_Char_Code (Source (Scan_Ptr)));
                  Name_Len := 0;
                  Set_Name_Char (''');
                  Set_Name_Char (Source (Scan_Ptr));
                  Set_Name_Char (''');
                  Token_Name := Name_Find;
                  Set_Chars (Token_Node, Token_Name);
                  Scan_Ptr := Scan_Ptr + 2;
                  return;
               end if;
            end if;

         --  Right parenthesis

         when ')' =>
            Scan_Ptr := Scan_Ptr + 1;
            Token := Tok_Right_Paren;
            return;

         --  Right bracket, treated as right paren

         when ']' =>
            Error_Msg_S ("illegal character ""]"", replaced by "")""");
            Scan_Ptr := Scan_Ptr + 1;
            Token := Tok_Right_Paren;
            return;

         --  Slash (can be division operator or first character of not equal)

         when '/' =>
            if Double_Char_Token ('=') then
               Token := Tok_Not_Equal;
               if GNAT_Style_Check then Require_Surrounding_Space; end if;
               return;
            else
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Slash;
               if GNAT_Style_Check then Require_Surrounding_Space; end if;
               return;
            end if;

         --  Semicolon

         when ';' =>
            if GNAT_Style_Check then
               if Source (Scan_Ptr - 1) = ' ' then
                  Error_Msg ("(style) no space allowed", Scan_Ptr - 1);

               elsif Source (Scan_Ptr + 1) > ' ' then
                  Error_Msg ("(style) space required", Scan_Ptr + 1);
               end if;
            end if;

            Scan_Ptr := Scan_Ptr + 1;
            Token := Tok_Semicolon;
            return;

         --  Vertical bar

         when '|' =>

            if Source (Scan_Ptr + 1) = '|' then
               Error_Msg_S ("|| should be `OR ELSE`");
               Scan_Ptr := Scan_Ptr + 2;
               Token := Tok_Or;
               return;

            else
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Vertical_Bar;
               if GNAT_Style_Check then Require_Surrounding_Space; end if;
               return;
            end if;


         --  Exclamation, replacement character for vertical bar

         when '!' =>
            if Source (Scan_Ptr + 1) = '=' then
               Error_Msg_S ("'!= should be /=");
               Scan_Ptr := Scan_Ptr + 2;
               Token := Tok_Not_Equal;
               return;

            elsif Ada_9X then
               Error_Msg_S
                 ("obsolescent feature: use of exclamation for vertical bar?");
            end if;

            Scan_Ptr := Scan_Ptr + 1;
            Token := Tok_Vertical_Bar;
            return;

         --  Plus

         when '+' =>
            Scan_Ptr := Scan_Ptr + 1;

            if GNAT_Style_Check
              and then Source (Token_Ptr - 1) /= '('
            then
               Require_Preceding_Space;
            end if;

            Token := Tok_Plus;
            return;

         --  Digits starting a numeric constant

         when '0' .. '9' =>
            Nlit;

            if Letter_Or_Digit (Source (Scan_Ptr)) then
               Error_Msg
                 ("delimiter required between literal and identifier",
                                                      Scan_Ptr);
            end if;

            return;

         --  Keyword scan routines. These are separated by initial lower case
         --  letter and for each letter, the appropriate keywords are tested
         --  directly. If a keyword is scanned, then control is passed to
         --  Scan_Keyword, otherwise control passes to Scan_Identifier_n,
         --  where n is a digit which  indicates the number of characters,
         --  starting at Scan_Ptr, which are known to be letters.

         --  The assumption is that keywords are most likely to be spelled
         --  in lower case letters, and that identifiers are likely to be
         --  spelled with an initial upper case letter. If an identifier
         --  starts with a lower case letter, then everything is fine since
         --  control goes to Scan_Identifier_n as described above. If a
         --  keyword is spelled in upper case, then things are also OK, but
         --  the mechanism for recognizing the keywords is quite different
         --  and depends on the use of the Name table (see Scan_Identifier).

         --  Keywords starting with A

         when 'a' =>

            --  ABORT

            if Source (Scan_Ptr + 1) = 'b' then
               if Source (Scan_Ptr + 2) = 'o' then
                  if Source (Scan_Ptr + 3) = 'r' then
                     if Source (Scan_Ptr + 4) = 't' then
                        Token := Tok_Abort;
                        Scan_Ptr := Scan_Ptr + 5;
                        goto Scan_Keyword;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;

            --  ABSTRACT

               elsif Source (Scan_Ptr + 2) = 's' then
                  if Ada_9X and then Source (Scan_Ptr + 3) = 't' then
                     if Source (Scan_Ptr + 4) = 'r' then
                        if Source (Scan_Ptr + 5) = 'a' then
                           if Source (Scan_Ptr + 6) = 'c' then
                              if Source (Scan_Ptr + 7) = 't' then
                                 Token := Tok_Abstract;
                                 Scan_Ptr := Scan_Ptr + 8;
                                 goto Scan_Keyword;
                              else goto Scan_Identifier_7; end if;
                           else goto Scan_Identifier_6; end if;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;

            --  ABS

                  else
                     Token := Tok_Abs;
                     Scan_Ptr := Scan_Ptr + 3;
                     goto Scan_Keyword;
                  end if;
               else goto Scan_Identifier_2; end if;

            --  ACCEPT

            elsif Source (Scan_Ptr + 1) = 'c' then
               if Source (Scan_Ptr + 2) = 'c' then
                  if Source (Scan_Ptr + 3) = 'e' then
                     if Source (Scan_Ptr + 4) = 'p' then
                        if Source (Scan_Ptr + 5) = 't' then
                           Token := Tok_Accept;
                           Scan_Ptr := Scan_Ptr + 6;
                           goto Scan_Keyword;
                        else goto Scan_Identifier_5; end if;

            --  ACCESS

                     elsif Source (Scan_Ptr + 4) = 's' then
                        if Source (Scan_Ptr + 5) = 's' then
                           Token := Tok_Access;
                           Scan_Ptr := Scan_Ptr + 6;
                           goto Scan_Keyword;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  ALL

            elsif Source (Scan_Ptr + 1) = 'l' then
               if Source (Scan_Ptr + 2) = 'l' then
                  Token := Tok_All;
                  Scan_Ptr := Scan_Ptr + 3;
                  goto Scan_Keyword;

            --  ALIASED (9X only)

               elsif Ada_9X and then Source (Scan_Ptr + 2) = 'i' then
                  if Source (Scan_Ptr + 3) = 'a' then
                     if Source (Scan_Ptr + 4) = 's' then
                        if Source (Scan_Ptr + 5) = 'e' then
                           if Source (Scan_Ptr + 6) = 'd' then
                              Token := Tok_Aliased;
                              Scan_Ptr := Scan_Ptr + 7;
                              goto Scan_Keyword;
                           else goto Scan_Identifier_6; end if;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  AND

            elsif Source (Scan_Ptr + 1) = 'n' then
               if Source (Scan_Ptr + 2) = 'd' then
                  Token := Tok_And;
                  Scan_Ptr := Scan_Ptr + 3;
                  goto Scan_Keyword;
               else goto Scan_Identifier_2; end if;

            --  ARRAY

            elsif Source (Scan_Ptr + 1) = 'r' then
               if Source (Scan_Ptr + 2) = 'r' then
                  if Source (Scan_Ptr + 3) = 'a' then
                     if Source (Scan_Ptr + 4) = 'y' then
                        Token := Tok_Array;
                        Scan_Ptr := Scan_Ptr + 5;
                        goto Scan_Keyword;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  AT

            elsif Source (Scan_Ptr + 1) = 't' then
               Token := Tok_At;
               Scan_Ptr := Scan_Ptr + 2;
               goto Scan_Keyword;
            else goto Scan_Identifier_1; end if;

         --  Keywords starting with B

         when 'b' =>

            --  BEGIN

            if Source (Scan_Ptr + 1) = 'e' then
               if Source (Scan_Ptr + 2) = 'g' then
                  if Source (Scan_Ptr + 3) = 'i' then
                     if Source (Scan_Ptr + 4) = 'n' then
                        Token := Tok_Begin;
                        Scan_Ptr := Scan_Ptr + 5;
                        goto Scan_Keyword;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  BODY

            elsif Source (Scan_Ptr + 1) = 'o' then
               if Source (Scan_Ptr + 2) = 'd' then
                  if Source (Scan_Ptr + 3) = 'y' then
                     Token := Tok_Body;
                     Scan_Ptr := Scan_Ptr + 4;
                     goto Scan_Keyword;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;
            else goto Scan_Identifier_1; end if;

         --  Keywords starting with C

         when 'c' =>

            --  CASE

            if Source (Scan_Ptr + 1) = 'a' then
               if Source (Scan_Ptr + 2) = 's' then
                  if Source (Scan_Ptr + 3) = 'e' then
                     Token := Tok_Case;
                     Scan_Ptr := Scan_Ptr + 4;
                     goto Scan_Keyword;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  CONSTANT

            elsif Source (Scan_Ptr + 1) = 'o' then
               if Source (Scan_Ptr + 2) = 'n' then
                  if Source (Scan_Ptr + 3) = 's' then
                     if Source (Scan_Ptr + 4) = 't' then
                        if Source (Scan_Ptr + 5) = 'a' then
                           if Source (Scan_Ptr + 6) = 'n' then
                              if Source (Scan_Ptr + 7) = 't' then
                                 Token := Tok_Constant;
                                 Scan_Ptr := Scan_Ptr + 8;
                                 goto Scan_Keyword;
                              else goto Scan_Identifier_7; end if;
                           else goto Scan_Identifier_6; end if;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;
            else goto Scan_Identifier_1; end if;

         --  Keywords starting with D

         when 'd' =>

            --  DECLARE

            if Source (Scan_Ptr + 1) = 'e' then
               if Source (Scan_Ptr + 2) = 'c' then
                  if Source (Scan_Ptr + 3) = 'l' then
                     if Source (Scan_Ptr + 4) = 'a' then
                        if Source (Scan_Ptr + 5) = 'r' then
                           if Source (Scan_Ptr + 6) = 'e' then
                              Token := Tok_Declare;
                              Scan_Ptr := Scan_Ptr + 7;
                              goto Scan_Keyword;
                           else goto Scan_Identifier_6; end if;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;

            --  DELAY

               elsif Source (Scan_Ptr + 2) = 'l' then
                  if Source (Scan_Ptr + 3) = 'a' then
                     if Source (Scan_Ptr + 4) = 'y' then
                        Token := Tok_Delay;
                        Scan_Ptr := Scan_Ptr + 5;
                        goto Scan_Keyword;
                     else goto Scan_Identifier_4; end if;

            --  DELTA

                  elsif Source (Scan_Ptr + 3) = 't' then
                     if Source (Scan_Ptr + 4) = 'a' then
                        Token := Tok_Delta;
                        Scan_Ptr := Scan_Ptr + 5;
                        goto Scan_Keyword;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  DIGITS

            elsif Source (Scan_Ptr + 1) = 'i' then
               if Source (Scan_Ptr + 2) = 'g' then
                  if Source (Scan_Ptr + 3) = 'i' then
                     if Source (Scan_Ptr + 4) = 't' then
                        if Source (Scan_Ptr + 5) = 's' then
                           Token := Tok_Digits;
                           Scan_Ptr := Scan_Ptr + 6;
                           goto Scan_Keyword;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  DO

            elsif Source (Scan_Ptr + 1) = 'o' then
               Token := Tok_Do;
               Scan_Ptr := Scan_Ptr + 2;
               goto Scan_Keyword;
            else goto Scan_Identifier_1; end if;

         --  Keywords starting with E

         when 'e' =>

            --  ELSE

            if Source (Scan_Ptr + 1) = 'l' then
               if Source (Scan_Ptr + 2) = 's' then
                  if Source (Scan_Ptr + 3) = 'e' then
                     Token := Tok_Else;
                     Scan_Ptr := Scan_Ptr + 4;
                     goto Scan_Keyword;

            --  ELSIF

                  elsif Source (Scan_Ptr + 3) = 'i' then
                     if Source (Scan_Ptr + 4) = 'f' then
                        Token := Tok_Elsif;
                        Scan_Ptr := Scan_Ptr + 5;
                        goto Scan_Keyword;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  END

            elsif Source (Scan_Ptr + 1) = 'n' then
               if Source (Scan_Ptr + 2) = 'd' then
                  Token := Tok_End;
                  Scan_Ptr := Scan_Ptr + 3;
                  goto Scan_Keyword;

            --  ENTRY

               elsif Source (Scan_Ptr + 2) = 't' then
                  if Source (Scan_Ptr + 3) = 'r' then
                     if Source (Scan_Ptr + 4) = 'y' then
                        Token := Tok_Entry;
                        Scan_Ptr := Scan_Ptr + 5;
                        goto Scan_Keyword;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  EXCEPTION

            elsif Source (Scan_Ptr + 1) = 'x' then
               if Source (Scan_Ptr + 2) = 'c' then
                  if Source (Scan_Ptr + 3) = 'e' then
                     if Source (Scan_Ptr + 4) = 'p' then
                        if Source (Scan_Ptr + 5) = 't' then
                           if Source (Scan_Ptr + 6) = 'i' then
                              if Source (Scan_Ptr + 7) = 'o' then
                                 if Source (Scan_Ptr + 8) = 'n' then
                                    Token := Tok_Exception;
                                    Scan_Ptr := Scan_Ptr + 9;
                                    goto Scan_Keyword;
                                 else goto Scan_Identifier_8; end if;
                              else goto Scan_Identifier_7; end if;
                           else goto Scan_Identifier_6; end if;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;

            --  EXIT

               elsif Source (Scan_Ptr + 2) = 'i' then
                  if Source (Scan_Ptr + 3) = 't' then
                     Token := Tok_Exit;
                     Scan_Ptr := Scan_Ptr + 4;
                     goto Scan_Keyword;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;
            else goto Scan_Identifier_1; end if;

         --  Keywords starting with F

         when 'f' =>

            --  FOR

            if Source (Scan_Ptr + 1) = 'o' then
               if Source (Scan_Ptr + 2) = 'r' then
                  Token := Tok_For;
                  Scan_Ptr := Scan_Ptr + 3;
                  goto Scan_Keyword;
               else goto Scan_Identifier_2; end if;

            --  FUNCTION

            elsif Source (Scan_Ptr + 1) = 'u' then
               if Source (Scan_Ptr + 2) = 'n' then
                  if Source (Scan_Ptr + 3) = 'c' then
                     if Source (Scan_Ptr + 4) = 't' then
                        if Source (Scan_Ptr + 5) = 'i' then
                           if Source (Scan_Ptr + 6) = 'o' then
                              if Source (Scan_Ptr + 7) = 'n' then
                                 Token := Tok_Function;
                                 Scan_Ptr := Scan_Ptr + 8;
                                 goto Scan_Keyword;
                              else goto Scan_Identifier_7; end if;
                           else goto Scan_Identifier_6; end if;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;
            else goto Scan_Identifier_1; end if;

         --  Keywords starting with G

         when 'g' =>

            --  GENERIC

            if Source (Scan_Ptr + 1) = 'e' then
               if Source (Scan_Ptr + 2) = 'n' then
                  if Source (Scan_Ptr + 3) = 'e' then
                     if Source (Scan_Ptr + 4) = 'r' then
                        if Source (Scan_Ptr + 5) = 'i' then
                           if Source (Scan_Ptr + 6) = 'c' then
                              Token := Tok_Generic;
                              Scan_Ptr := Scan_Ptr + 7;
                              goto Scan_Keyword;
                           else goto Scan_Identifier_6; end if;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  GOTO

            elsif Source (Scan_Ptr + 1) = 'o' then
               if Source (Scan_Ptr + 2) = 't' then
                  if Source (Scan_Ptr + 3) = 'o' then
                     Token := Tok_Goto;
                     Scan_Ptr := Scan_Ptr + 4;
                     goto Scan_Keyword;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;
            else goto Scan_Identifier_1; end if;

         --  Keywords starting with I

         when 'i' =>

            --  IF

            if Source (Scan_Ptr + 1) = 'f' then
               Token := Tok_If;
               Scan_Ptr := Scan_Ptr + 2;
               goto Scan_Keyword;

            --  IN

            elsif Source (Scan_Ptr + 1) = 'n' then
               Token := Tok_In;
               Scan_Ptr := Scan_Ptr + 2;
               goto Scan_Keyword;

            --  IS

            elsif Source (Scan_Ptr + 1) = 's' then
               Token := Tok_Is;
               Scan_Ptr := Scan_Ptr + 2;
               goto Scan_Keyword;
            else goto Scan_Identifier_1; end if;

         --  Keywords starting with L

         when 'l' =>

            --  LIMITED

            if Source (Scan_Ptr + 1) = 'i' then
               if Source (Scan_Ptr + 2) = 'm' then
                  if Source (Scan_Ptr + 3) = 'i' then
                     if Source (Scan_Ptr + 4) = 't' then
                        if Source (Scan_Ptr + 5) = 'e' then
                           if Source (Scan_Ptr + 6) = 'd' then
                              Token := Tok_Limited;
                              Scan_Ptr := Scan_Ptr + 7;
                              goto Scan_Keyword;
                           else goto Scan_Identifier_6; end if;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  LOOP

            elsif Source (Scan_Ptr + 1) = 'o' then
               if Source (Scan_Ptr + 2) = 'o' then
                  if Source (Scan_Ptr + 3) = 'p' then
                     Token := Tok_Loop;
                     Scan_Ptr := Scan_Ptr + 4;
                     goto Scan_Keyword;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;
            else goto Scan_Identifier_1; end if;

         --  Keywords starting with M

         when 'm' =>

            --  MOD

            if Source (Scan_Ptr + 1) = 'o' then
               if Source (Scan_Ptr + 2) = 'd' then
                  Token := Tok_Mod;
                  Scan_Ptr := Scan_Ptr + 3;
                  goto Scan_Keyword;
               else goto Scan_Identifier_2; end if;
            else goto Scan_Identifier_1; end if;

         --  Keywords starting with N

         when 'n' =>

            --  NEW

            if Source (Scan_Ptr + 1) = 'e' then
               if Source (Scan_Ptr + 2) = 'w' then
                  Token := Tok_New;
                  Scan_Ptr := Scan_Ptr + 3;
                  goto Scan_Keyword;
               else goto Scan_Identifier_2; end if;

            --  NOT

            elsif Source (Scan_Ptr + 1) = 'o' then
               if Source (Scan_Ptr + 2) = 't' then
                  Token := Tok_Not;
                  Scan_Ptr := Scan_Ptr + 3;
                  goto Scan_Keyword;
               else goto Scan_Identifier_2; end if;

            --  NULL

            elsif Source (Scan_Ptr + 1) = 'u' then
               if Source (Scan_Ptr + 2) = 'l' then
                  if Source (Scan_Ptr + 3) = 'l' then
                     Token := Tok_Null;
                     Scan_Ptr := Scan_Ptr + 4;
                     goto Scan_Keyword;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;
            else goto Scan_Identifier_1; end if;

         --  Keywords starting with O

         when 'o' =>

            --  OF

            if Source (Scan_Ptr + 1) = 'f' then
               Token := Tok_Of;
               Scan_Ptr := Scan_Ptr + 2;
               goto Scan_Keyword;

            --  OR

            elsif Source (Scan_Ptr + 1) = 'r' then
               Token := Tok_Or;
               Scan_Ptr := Scan_Ptr + 2;
               goto Scan_Keyword;

            --  OTHERS

            elsif Source (Scan_Ptr + 1) = 't' then
               if Source (Scan_Ptr + 2) = 'h' then
                  if Source (Scan_Ptr + 3) = 'e' then
                     if Source (Scan_Ptr + 4) = 'r' then
                        if Source (Scan_Ptr + 5) = 's' then
                           Token := Tok_Others;
                           Scan_Ptr := Scan_Ptr + 6;
                           goto Scan_Keyword;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  OUT

            elsif Source (Scan_Ptr + 1) = 'u' then
               if Source (Scan_Ptr + 2) = 't' then
                  Token := Tok_Out;
                  Scan_Ptr := Scan_Ptr + 3;
                  goto Scan_Keyword;
               else goto Scan_Identifier_2; end if;
            else goto Scan_Identifier_1; end if;

         --  Keywords starting with P

         when 'p' =>

            --  PACKAGE

            if Source (Scan_Ptr + 1) = 'a' then
               if Source (Scan_Ptr + 2) = 'c' then
                  if Source (Scan_Ptr + 3) = 'k' then
                     if Source (Scan_Ptr + 4) = 'a' then
                        if Source (Scan_Ptr + 5) = 'g' then
                           if Source (Scan_Ptr + 6) = 'e' then
                              Token := Tok_Package;
                              Scan_Ptr := Scan_Ptr + 7;
                              goto Scan_Keyword;
                           else goto Scan_Identifier_6; end if;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  PRAGMA

            elsif Source (Scan_Ptr + 1) = 'r' then
               if Source (Scan_Ptr + 2) = 'a' then
                  if Source (Scan_Ptr + 3) = 'g' then
                     if Source (Scan_Ptr + 4) = 'm' then
                        if Source (Scan_Ptr + 5) = 'a' then
                           Token := Tok_Pragma;
                           Scan_Ptr := Scan_Ptr + 6;
                           goto Scan_Keyword;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;

            --  PRIVATE

               elsif Source (Scan_Ptr + 2) = 'i' then
                  if Source (Scan_Ptr + 3) = 'v' then
                     if Source (Scan_Ptr + 4) = 'a' then
                        if Source (Scan_Ptr + 5) = 't' then
                           if Source (Scan_Ptr + 6) = 'e' then
                              Token := Tok_Private;
                              Scan_Ptr := Scan_Ptr + 7;
                              goto Scan_Keyword;
                           else goto Scan_Identifier_6; end if;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;

            --  PROCEDURE

               elsif Source (Scan_Ptr + 2) = 'o' then
                  if Source (Scan_Ptr + 3) = 'c' then
                     if Source (Scan_Ptr + 4) = 'e' then
                        if Source (Scan_Ptr + 5) = 'd' then
                           if Source (Scan_Ptr + 6) = 'u' then
                              if Source (Scan_Ptr + 7) = 'r' then
                                 if Source (Scan_Ptr + 8) = 'e' then
                                    Token := Tok_Procedure;
                                    Scan_Ptr := Scan_Ptr + 9;
                                    goto Scan_Keyword;
                                 else goto Scan_Identifier_8; end if;
                              else goto Scan_Identifier_7; end if;
                           else goto Scan_Identifier_6; end if;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;

            --  PROTECTED (9X only)

                  elsif Ada_9X and then Source (Scan_Ptr + 3) = 't' then
                     if Source (Scan_Ptr + 4) = 'e' then
                        if Source (Scan_Ptr + 5) = 'c' then
                           if Source (Scan_Ptr + 6) = 't' then
                              if Source (Scan_Ptr + 7) = 'e' then
                                 if Source (Scan_Ptr + 8) = 'd' then
                                    Token := Tok_Protected;
                                    Scan_Ptr := Scan_Ptr + 9;
                                    goto Scan_Keyword;
                                 else goto Scan_Identifier_8; end if;
                              else goto Scan_Identifier_7; end if;
                           else goto Scan_Identifier_6; end if;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;
            else goto Scan_Identifier_1; end if;

         --  Keywords starting with R

         when 'r' =>

            --  RAISE

            if Source (Scan_Ptr + 1) = 'a' then
               if Source (Scan_Ptr + 2) = 'i' then
                  if Source (Scan_Ptr + 3) = 's' then
                     if Source (Scan_Ptr + 4) = 'e' then
                        Token := Tok_Raise;
                        Scan_Ptr := Scan_Ptr + 5;
                        goto Scan_Keyword;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;

            --  RANGE

               elsif Source (Scan_Ptr + 2) = 'n' then
                  if Source (Scan_Ptr + 3) = 'g' then
                     if Source (Scan_Ptr + 4) = 'e' then
                        Token := Tok_Range;
                        Scan_Ptr := Scan_Ptr + 5;
                        goto Scan_Keyword;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  RECORD

            elsif Source (Scan_Ptr + 1) = 'e' then
               if Source (Scan_Ptr + 2) = 'c' then
                  if Source (Scan_Ptr + 3) = 'o' then
                     if Source (Scan_Ptr + 4) = 'r' then
                        if Source (Scan_Ptr + 5) = 'd' then
                           Token := Tok_Record;
                           Scan_Ptr := Scan_Ptr + 6;
                           goto Scan_Keyword;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;

            --  REM

               elsif Source (Scan_Ptr + 2) = 'm' then
                  Token := Tok_Rem;
                  Scan_Ptr := Scan_Ptr + 3;
                  goto Scan_Keyword;

            --  RENAMES

               elsif Source (Scan_Ptr + 2) = 'n' then
                  if Source (Scan_Ptr + 3) = 'a' then
                     if Source (Scan_Ptr + 4) = 'm' then
                        if Source (Scan_Ptr + 5) = 'e' then
                           if Source (Scan_Ptr + 6) = 's' then
                              Token := Tok_Renames;
                              Scan_Ptr := Scan_Ptr + 7;
                              goto Scan_Keyword;
                           else goto Scan_Identifier_6; end if;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;

            --  REQUEUE (9X only)

               elsif Ada_9X and then Source (Scan_Ptr + 2) = 'q' then
                  if Source (Scan_Ptr + 3) = 'u' then
                     if Source (Scan_Ptr + 4) = 'e' then
                        if Source (Scan_Ptr + 5) = 'u' then
                           if Source (Scan_Ptr + 6) = 'e' then
                              Token := Tok_Requeue;
                              Scan_Ptr := Scan_Ptr + 7;
                              goto Scan_Keyword;
                           else goto Scan_Identifier_6; end if;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;

            --  RETURN

               elsif Source (Scan_Ptr + 2) = 't' then
                  if Source (Scan_Ptr + 3) = 'u' then
                     if Source (Scan_Ptr + 4) = 'r' then
                        if Source (Scan_Ptr + 5) = 'n' then
                           Token := Tok_Return;
                           Scan_Ptr := Scan_Ptr + 6;
                           goto Scan_Keyword;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;

            --  REVERSE

               elsif Source (Scan_Ptr + 2) = 'v' then
                  if Source (Scan_Ptr + 3) = 'e' then
                     if Source (Scan_Ptr + 4) = 'r' then
                        if Source (Scan_Ptr + 5) = 's' then
                           if Source (Scan_Ptr + 6) = 'e' then
                              Token := Tok_Reverse;
                              Scan_Ptr := Scan_Ptr + 7;
                              goto Scan_Keyword;
                           else goto Scan_Identifier_6; end if;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;
            else goto Scan_Identifier_1; end if;

         --  Keywords starting with S

         when 's' =>

            --  SELECT

            if Source (Scan_Ptr + 1) = 'e' then
               if Source (Scan_Ptr + 2) = 'l' then
                  if Source (Scan_Ptr + 3) = 'e' then
                     if Source (Scan_Ptr + 4) = 'c' then
                        if Source (Scan_Ptr + 5) = 't' then
                           Token := Tok_Select;
                           Scan_Ptr := Scan_Ptr + 6;
                           goto Scan_Keyword;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;

            --  SEPARATE

               elsif Source (Scan_Ptr + 2) = 'p' then
                  if Source (Scan_Ptr + 3) = 'a' then
                     if Source (Scan_Ptr + 4) = 'r' then
                        if Source (Scan_Ptr + 5) = 'a' then
                           if Source (Scan_Ptr + 6) = 't' then
                              if Source (Scan_Ptr + 7) = 'e' then
                                 Token := Tok_Separate;
                                 Scan_Ptr := Scan_Ptr + 8;
                                 goto Scan_Keyword;
                              else goto Scan_Identifier_7; end if;
                           else goto Scan_Identifier_6; end if;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  SUBTYPE

            elsif Source (Scan_Ptr + 1) = 'u' then
               if Source (Scan_Ptr + 2) = 'b' then
                  if Source (Scan_Ptr + 3) = 't' then
                     if Source (Scan_Ptr + 4) = 'y' then
                        if Source (Scan_Ptr + 5) = 'p' then
                           if Source (Scan_Ptr + 6) = 'e' then
                              Token := Tok_Subtype;
                              Scan_Ptr := Scan_Ptr + 7;
                              goto Scan_Keyword;
                           else goto Scan_Identifier_6; end if;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;
            else goto Scan_Identifier_1; end if;

         --  Keywords starting with T

         when 't' =>

            --  TAGGED (9X only)

            if Source (Scan_Ptr + 1) = 'a' then
               if Ada_9X and then Source (Scan_Ptr + 2) = 'g' then
                  if Source (Scan_Ptr + 3) = 'g' then
                     if Source (Scan_Ptr + 4) = 'e' then
                        if Source (Scan_Ptr + 5) = 'd' then
                           Token := Tok_Tagged;
                           Scan_Ptr := Scan_Ptr + 6;
                           goto Scan_Keyword;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;

            --  TASK

               elsif Source (Scan_Ptr + 2) = 's' then
                  if Source (Scan_Ptr + 3) = 'k' then
                     Token := Tok_Task;
                     Scan_Ptr := Scan_Ptr + 4;
                     goto Scan_Keyword;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  TERMINATE

            elsif Source (Scan_Ptr + 1) = 'e' then
               if Source (Scan_Ptr + 2) = 'r' then
                  if Source (Scan_Ptr + 3) = 'm' then
                     if Source (Scan_Ptr + 4) = 'i' then
                        if Source (Scan_Ptr + 5) = 'n' then
                           if Source (Scan_Ptr + 6) = 'a' then
                              if Source (Scan_Ptr + 7) = 't' then
                                 if Source (Scan_Ptr + 8) = 'e' then
                                    Token := Tok_Terminate;
                                    Scan_Ptr := Scan_Ptr + 9;
                                    goto Scan_Keyword;
                                 else goto Scan_Identifier_8; end if;
                              else goto Scan_Identifier_7; end if;
                           else goto Scan_Identifier_6; end if;
                        else goto Scan_Identifier_5; end if;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  THEN

            elsif Source (Scan_Ptr + 1) = 'h' then
               if Source (Scan_Ptr + 2) = 'e' then
                  if Source (Scan_Ptr + 3) = 'n' then
                     Token := Tok_Then;
                     Scan_Ptr := Scan_Ptr + 4;
                     goto Scan_Keyword;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  TYPE

            elsif Source (Scan_Ptr + 1) = 'y' then
               if Source (Scan_Ptr + 2) = 'p' then
                  if Source (Scan_Ptr + 3) = 'e' then
                     Token := Tok_Type;
                     Scan_Ptr := Scan_Ptr + 4;
                     goto Scan_Keyword;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;
            else goto Scan_Identifier_1; end if;

         --  Keywords starting with U

         when 'u' =>

            --  UNTIL (9X only)

            if Ada_9X and then Source (Scan_Ptr + 1) = 'n' then
               if Source (Scan_Ptr + 2) = 't' then
                  if Source (Scan_Ptr + 3) = 'i' then
                     if Source (Scan_Ptr + 4) = 'l' then
                        Token := Tok_Until;
                        Scan_Ptr := Scan_Ptr + 5;
                        goto Scan_Keyword;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  USE

            elsif Source (Scan_Ptr + 1) = 's' then
               if Source (Scan_Ptr + 2) = 'e' then
                  Token := Tok_Use;
                  Scan_Ptr := Scan_Ptr + 3;
                  goto Scan_Keyword;
               else goto Scan_Identifier_2; end if;
            else goto Scan_Identifier_1; end if;

         --  Keywords starting with W

         when 'w' =>

            --  WHEN

            if Source (Scan_Ptr + 1) = 'h' then
               if Source (Scan_Ptr + 2) = 'e' then
                  if Source (Scan_Ptr + 3) = 'n' then
                     Token := Tok_When;
                     Scan_Ptr := Scan_Ptr + 4;
                     goto Scan_Keyword;
                  else goto Scan_Identifier_3; end if;

            --  WHILE

               elsif Source (Scan_Ptr + 2) = 'i' then
                  if Source (Scan_Ptr + 3) = 'l' then
                     if Source (Scan_Ptr + 4) = 'e' then
                        Token := Tok_While;
                        Scan_Ptr := Scan_Ptr + 5;
                        goto Scan_Keyword;
                     else goto Scan_Identifier_4; end if;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;

            --  WITH

            elsif Source (Scan_Ptr + 1) = 'i' then
               if Source (Scan_Ptr + 2) = 't' then
                  if Source (Scan_Ptr + 3) = 'h' then
                     Token := Tok_With;
                     Scan_Ptr := Scan_Ptr + 4;
                     goto Scan_Keyword;
                  else goto Scan_Identifier_3; end if;
               else goto Scan_Identifier_2; end if;
            else goto Scan_Identifier_1; end if;

         --  Keywords starting with X

         when 'x' =>

            --  XOR

            if Source (Scan_Ptr + 1) = 'o' then
               if Source (Scan_Ptr + 2) = 'r' then
                  Token := Tok_Xor;
                  Scan_Ptr := Scan_Ptr + 3;
                  goto Scan_Keyword;
               else goto Scan_Identifier_2; end if;
            else goto Scan_Identifier_1; end if;


         --  Upper case letters and lower case letters which cannot start
         --  a keyword. We assume, for the moment that these are identifiers.
         --  For upper case letters, we may turn out to have a keyword, but
         --  we will find that out later on in the Scan_Identifier routine.

         when 'A' .. 'Z' | 'h' | 'j' | 'k' | 'q' | 'v' | 'y' | 'z' =>
            goto Scan_Identifier_1;

         --  Underline character

         when '_' =>
            Error_Msg_S ("identifier cannot start with underline");
            goto Scan_Identifier_1;

         --  Space (not possible, because we scanned past blanks)

         when ' ' =>

            null;

         --  Invalid control characters

         when NUL | SOH | STX | ETX | EOT | ENQ | ACK | BEL | BS  | SO  |
              SI  | DLE | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN |
              EM  | ESC | FS  | GS  | RS  | US  | DEL
         =>
            Error_Illegal_Character;

         --  Invalid graphic characters

         when '#' | '$' | '?' | '@' | '`' |
              '\' | '^' | '{' | '}' | '~'
         =>
            Error_Illegal_Character;

         --  Characters in top half of ASCII 8-bit chart

         when Upper_Half_Character =>

            --  Upper half characters may possibly be identifier letters but
            --  can never be digits, so Letter_Or_Digit can be used to test
            --  for a valid start of identifier character.

            if Letter_Or_Digit (Source (Scan_Ptr)) then
               goto Scan_Identifier_1;
            else
               Error_Illegal_Character;
            end if;

         --  End switch on non-blank character

         end case;

      --  End loop past format effectors. The exit from this loop is by
      --  executing a return statement following completion of token scan
      --  (control never falls out of this loop to the code which follows)

      end loop;

      --  Keyword scanning routine. Control is passed to this routine (with a
      --  goto, for maximum speed) after scanning out a sequence of letters
      --  matching a keyword. Scan_Ptr is updated past the keyword characters
      --  and Token contains the right code. If a terminator character follows
      --  then this is definitely a keyword, and control should be returned.
      --  Otherwise is is simply an identifier whose leading characters match
      --  the characters of a keyword.

      <<Scan_Keyword>>
         if not Identifier_Char (Source (Scan_Ptr)) then
            return;

         else
            Name_Len := 0;

            for I in Token_Ptr .. Scan_Ptr loop
               Set_Name_Char (Fold_Lower (Source (I)));
            end loop;

            Scan_Ptr := Scan_Ptr + 1;
            goto Scan_Identifier;
         end if;

      --  Come here after encountering either an underline character
      --  The Scan_Identifier_N routines are passed control after scanning
      --  out N letters starting at Scan_Ptr which are letters (but which
      --  do not form the name of a keyword).

      <<Scan_Identifier_8>>
         Name_Len := 0;
         Set_Name_Char (Fold_Lower (Source (Scan_Ptr)));
         Scan_Ptr := Scan_Ptr + 1;
         goto Scan_Identifier_7C;

      <<Scan_Identifier_7>>
         Name_Len := 0;

      <<Scan_Identifier_7C>>
         Set_Name_Char (Fold_Lower (Source (Scan_Ptr)));
         Scan_Ptr := Scan_Ptr + 1;
         goto Scan_Identifier_6C;

      <<Scan_Identifier_6>>
         Name_Len := 0;

      <<Scan_Identifier_6C>>
         Set_Name_Char (Fold_Lower (Source (Scan_Ptr)));
         Scan_Ptr := Scan_Ptr + 1;
         goto Scan_Identifier_5C;

      <<Scan_Identifier_5>>
         Name_Len := 0;

      <<Scan_Identifier_5C>>
         Set_Name_Char (Fold_Lower (Source (Scan_Ptr)));
         Scan_Ptr := Scan_Ptr + 1;
         goto Scan_Identifier_4C;

      <<Scan_Identifier_4>>
         Name_Len := 0;

      <<Scan_Identifier_4C>>
         Set_Name_Char (Fold_Lower (Source (Scan_Ptr)));
         Scan_Ptr := Scan_Ptr + 1;
         goto Scan_Identifier_3C;

      <<Scan_Identifier_3>>
         Name_Len := 0;

      <<Scan_Identifier_3C>>
         Set_Name_Char (Fold_Lower (Source (Scan_Ptr)));
         Scan_Ptr := Scan_Ptr + 1;
         goto Scan_Identifier_2C;

      <<Scan_Identifier_2>>
         Name_Len := 0;

      <<Scan_Identifier_2C>>
         Set_Name_Char (Fold_Lower (Source (Scan_Ptr)));
         Scan_Ptr := Scan_Ptr + 1;
         goto Scan_Identifier_1C;

      <<Scan_Identifier_1>>
         Name_Len := 0;

      <<Scan_Identifier_1C>>
         Set_Name_Char (Fold_Lower (Source (Scan_Ptr)));
         Scan_Ptr := Scan_Ptr + 1;

      --  Here we scan out an identifier, Scan_Ptr has been updated past an
      --  initial sequence of one or more letters (so an underline character
      --  can follow immediately). The loop is opened out to speed up the
      --  scanning of long identifiers.

      <<Scan_Identifier>>
         while Letter_Or_Digit (Source (Scan_Ptr)) loop
            Set_Name_Char (Fold_Lower (Source (Scan_Ptr)));
            Scan_Ptr := Scan_Ptr + 1;
         end loop;

         --  If we fall through, then we have encountered either an underline
         --  character, or an identifier terminator.

         if Source (Scan_Ptr) = '_' then
            Set_Name_Char ('_');
            Scan_Ptr := Scan_Ptr + 1;

            if Letter_Or_Digit (Source (Scan_Ptr)) then
               Set_Name_Char (Fold_Lower (Source (Scan_Ptr)));
               Scan_Ptr := Scan_Ptr + 1;
               goto Scan_Identifier;

            elsif Source (Scan_Ptr) = '_' then
               Error_No_Double_Underline;
               goto Scan_Identifier;

            elsif Ada_83 then
               Error_Msg
                 ("identifier cannot end with underline in Ada 83",
                                                 Scan_Ptr - 1);
            end if;
         end if;

         Token_Name := Name_Find;

         --  Here is where we check if it was a keyword after all (happens
         --  when a keyword is spelled with an initial upper case letter)

         if Get_Name_Table_Byte (Token_Name) /= 0
           and then (Ada_9X or else Token_Name not in Ada_9X_Reserved_Words)
         then
            if GNAT_Style_Check then
               Error_Msg_SC ("(style) incorrectly spelled keyword");
            end if;

            Token := Token_Type'Val (Get_Name_Table_Byte (Token_Name));
            return;

         --  It is an identifier after all

         else
            Token_Node := New_Node (N_Identifier, Token_Ptr);
            Set_Chars (Token_Node, Token_Name);
            Token := Tok_Identifier;
            return;
         end if;
   end Scan;

   ---------------------
   -- Scan_First_Char --
   ---------------------

   function Scan_First_Char return Source_Ptr is
      Ptr : Source_Ptr := Current_Line_Start;

   begin
      loop
         if Source (Ptr) = ' ' then
            Ptr := Ptr + 1;

         elsif Source (Ptr) = HT then
            if GNAT_Style_Check then No_HT_Allowed (Ptr); end if;
            Ptr := Ptr + 1;

         else
            return Ptr;
         end if;
      end loop;
   end Scan_First_Char;

   ------------------------------
   -- Scan_Reserved_Identifier --
   ------------------------------

   procedure Scan_Reserved_Identifier (Force_Msg : Boolean) is
      Token_Chars : constant String := Token_Type'Image (Token);

   begin

      --  We have in Token_Chars the image of the Token name, i.e. Tok_xxx.
      --  This code extracts the xxx and makes an identifier out of it.

      Name_Len := 0;

      for I in 5 .. Token_Chars'Length loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := To_Char (Token_Chars (I));
      end loop;

      Token_Name := Name_Find;

      if not Used_As_Identifier (Token) or else Force_Msg then
         Error_Msg_Name_1 := Token_Name;
         Error_Msg_SC ("reserved word* cannot be used as identifier!");
         Used_As_Identifier (Token) := True;
      end if;

      Token := Tok_Identifier;
      Token_Node := New_Node (N_Identifier, Token_Ptr);
      Set_Chars (Token_Node, Token_Name);
   end Scan_Reserved_Identifier;

   ----------------------
   -- Set_Start_Column --
   ----------------------

   --  Note: it seems at first glance a little expensive to compute this value
   --  for every source line (since it is certainly not used for all source
   --  lines). On the other hand, it doesn't take much more work to skip past
   --  the initial white space on the line counting the columns than it would
   --  to scan past the qhite space using the standard scanning circuits.

   procedure Set_Start_Column is
   begin
      Start_Column := 0;

      loop

         --  Inner loop scans past blanks as fast as possible, bumping Scan_Ptr
         --  past the blanks and adjusting Start_Column to account for them.

         loop
            if Source (Scan_Ptr) = ' ' then
               if Source (Scan_Ptr + 1) = ' ' then
                  if Source (Scan_Ptr + 2) = ' ' then
                     if Source (Scan_Ptr + 3) = ' ' then
                        if Source (Scan_Ptr + 4) = ' ' then
                           if Source (Scan_Ptr + 5) = ' ' then
                              if Source (Scan_Ptr + 6) = ' ' then
                                 Scan_Ptr := Scan_Ptr + 7;
                                 Start_Column := Start_Column + 7;
                              else
                                 Scan_Ptr := Scan_Ptr + 6;
                                 Start_Column := Start_Column + 6;
                                 exit;
                              end if;
                           else
                              Scan_Ptr := Scan_Ptr + 5;
                              Start_Column := Start_Column + 5;
                              exit;
                           end if;
                        else
                           Scan_Ptr := Scan_Ptr + 4;
                           Start_Column := Start_Column + 4;
                           exit;
                        end if;
                     else
                        Scan_Ptr := Scan_Ptr + 3;
                        Start_Column := Start_Column + 3;
                        exit;
                     end if;
                  else
                     Scan_Ptr := Scan_Ptr + 2;
                     Start_Column := Start_Column + 2;
                     exit;
                  end if;
               else
                  Scan_Ptr := Scan_Ptr + 1;
                  Start_Column := Start_Column + 1;
                  exit;
               end if;
            else
               exit;
            end if;
         end loop;

         --  Outer loop keeps going only if a horizontal tab follows

         if Source (Scan_Ptr) = HT then
            if GNAT_Style_Check then No_HT_Allowed (Scan_Ptr); end if;
            Scan_Ptr := Scan_Ptr + 1;
            Start_Column := (Start_Column / 8) * 8 + 8;
         else
            exit;
         end if;
      end loop;
   end Set_Start_Column;

   -----------------------
   -- GNAT_Column_Check --
   -----------------------

   procedure GNAT_Column_Check (P : Source_Ptr) is
   begin
      for S in Current_Line_Start .. P - 1 loop
         if Source (S) > ' ' then
            return;
         end if;
      end loop;

      if Start_Column mod 3 /= 0 then
         Error_Msg ("(style) bad column", P);
      end if;
   end GNAT_Column_Check;

   ------------------------
   -- GNAT_Comment_Check --
   ------------------------

   procedure GNAT_Comment_Check is
      DM_Found : Boolean;

   begin
      --  Can never have a non-blank character preceding the first minus

      if Scan_Ptr > Source'First and then Source (Scan_Ptr - 1) > ' ' then
         Error_Msg_S ("(style) space required");
      end if;

      for S in Current_Line_Start .. Scan_Ptr - 1 loop
         if Source (S) > ' ' then

            --  Case of a comment that is not at the start of a line. The
            --  only required check is that we cannot have a non-blank
            --  character immediately following the second minus.

            if Source (Scan_Ptr + 2) > ' ' then
               Error_Msg ("(style) space required", Scan_Ptr + 2);
            end if;

            return;
         end if;
      end loop;

      --  Comment is at start of line

      if Start_Column mod 3 /= 0 then
         Error_Msg ("(style) bad column", Scan_Ptr);
      end if;

      --  Finally, a comment that is not a box comment (defined as a comment
      --  line ending with two minus signs), there must be a second space
      --  after the second minus.

      if Source (Scan_Ptr + 2) < ' ' or else Source (Scan_Ptr + 3) = ' ' then
         return;
      end if;

      DM_Found := False;

      for S in Scan_Ptr + 2 .. Source'Last loop
         if Source (S) = EOF or else
           Source (S) in Line_Terminator
         then
            if not DM_Found then
               Error_Msg ("(style) space required", Scan_Ptr + 3);
            end if;

            return;

         elsif Source (S) = '-' and then Source (S - 1) = '-' then
            DM_Found := True;

         elsif Source (S) > ' ' then
            DM_Found := False;
         end if;
      end loop;
   end GNAT_Comment_Check;

   --------------------------
   -- Version_Comment_Scan --
   --------------------------

   procedure Version_Comment_Scan is
      P : Source_Ptr := Scan_Ptr + 2;

   begin
      loop
         if Source (P) = ' ' then
            P := P + 1;

         elsif Source (P) < ' ' and then Source (P) /= HT then
            return;

         elsif Source (P) /= '$' then
            P := P + 1;

         --  Here we have a $ sign, might be $Revision:

         else
            P := P + 1;

            --  If too near end of file, return, not found after all

            if P + 9 >= Source'Last then
               return;

            --  If not $Revision, then just keep looking (we avoid the use of
            --  a slice compare here because it was not working at one point)

            elsif     Source (P + 0) /= 'R'
              or else Source (P + 1) /= 'e'
              or else Source (P + 2) /= 'v'
              or else Source (P + 3) /= 'i'
              or else Source (P + 4) /= 's'
              or else Source (P + 5) /= 'i'
              or else Source (P + 6) /= 'o'
              or else Source (P + 7) /= 'n'
              or else Source (P + 8) /= ':'
              or else Source (P + 9) /= ' '
            then
               P := P + 1;

            --  $Revision: found, acquire version

            else
               P := P + 10;

               for I in Int range 1 .. 6 loop
                  exit when Source (P) <= ' ';
                  File.Table (Scan_Unit).Version (I) := Source (P);
                  P := P + 1;
               end loop;

               Version_To_Be_Found := False;
               return;
            end if;
         end if;
      end loop;
   end Version_Comment_Scan;
end Scn;
