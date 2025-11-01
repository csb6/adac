------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               E R R O U T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.57 $                             --
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

with Atree;    use Atree;
with Casing;   use Casing;
with Csets;    use Csets;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errcount; use Errcount;
with Excep;    use Excep;
with Exp_Util; use Exp_Util;
with Lib;      use Lib;
with Limits;   use Limits;
with Namet;    use Namet;
with Opt;      use Opt;
with Output;   use Output;
with Scans;    use Scans;
with Sinput;   use Sinput;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Uintp;    use Uintp;
with Uname;    use Uname;

package body Errout is

   Max_Msg_Length : constant := 80 + 2 * Max_Line_Length;
   --  Maximum length of error message. The addition of Max_Line_Length
   --  ensures that two insertion tokens of maximum length can be accomodated.

   Msg_Buffer : Str (1 .. Max_Msg_Length);
   --  Buffer used to prepare error messages

   Msg_Len : Int;
   --  Number of characters currently stored in the message buffer

   Flag_Unit : Unit_Number_Type;
   --  Unit number for file where error is being posted

   Is_Warning_Msg : Boolean;
   --  Set by Set_Msg_Text to indicate if current message is warning message

   Is_Fatal_Msg : Boolean;
   --  Set by Set_Msg_Text to indicate if current message is fatal message

   Is_Unconditional_Msg : Boolean;
   --  Set by Set_Msg_Text to indicate if current message is unconditional

   Cur_Msg : Error_Msg_Id;
   --  Id of most recently posted error message

   Current_Error_Unit : Unit_Number_Type;
   --  Id of current messages. Used to post file name when unit changes. This
   --  is initialized to Main_Unit at the start of a compilation, which means
   --  that no file names will be output unless there are errors in units
   --  other than the main unit.

   Manual_Quote_Mode : Boolean;
   --  Set True in manual quotation mode

   List_Pragmas_Index : Int;
   --  Index into List_Pragmas table

   List_Pragmas_Mode : Boolean;
   --  Starts True, gets set False by pragma List (Off), True by List (On)

   Suppress_Message : Boolean;
   --  A flag used to suppress certain obviously redundant messages (i.e.
   --  those referring to a node whose type is Any_Type). This suppression
   --  is effective only if All_Errors_Mode is off.

   -----------------------------------
   -- Error Message Data Structures --
   -----------------------------------

   --  The error messages are stored as a linked list of error message objects
   --  sorted into ascending order by the source location (Sloc). Each object
   --  records the text of the message and its source location.

   --  The following record type and table are used to represent error
   --  messages, with one entry in the table being allocated for each message.

   type Error_Msg_Object is record
      Text   : Name_Id;              -- Text of error message
      Next   : Error_Msg_Id;         -- Pointer to next message
      Unit   : Unit_Number_Type;     -- Unit number of source file
      Sptr   : Source_Ptr;           -- Flag pointer
      Line   : Line_Number_Type;     -- Line number
      Col    : Column_Number_Type;   -- Column number
      Warn   : Boolean;              -- True if warning message
      Fatal  : Boolean;              -- True if fatal message
      Uncond : Boolean;              -- True if unconditional message
   end record;

   package Errors is new Table (
      Component_Type => Error_Msg_Object,
      Index_Type     => Error_Msg_Id,
      Low_Bound      => 1,
      Initial        => 200,
      Increment      => 200,
      Table_Name     => "Error");

   Error_Msgs : Error_Msg_Id;
   --  The list of error messages

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Debug_Output (N : Node_Id);
   --  Called from Error_Msg_N and Error_Msg_NE to generate line of debug
   --  output giving node number (of node N) if the debug X switch is set.

   function OK_Node (N : Node_Id) return Boolean;
   --  Determines if a node is an OK node to place an error message on (return
   --  True) or if the error message should be suppressed (return False). A
   --  message is suppressed if the node already has an error posted on it,
   --  or if it refers to an Etype that has an error posted on it, or if
   --  it references an Entity that has an error posted on it.

   procedure Output_Error_Msgs (E : in out Error_Msg_Id);
   --  Output source line, error flag, and text of stored error message and
   --  all subsequent messages for the same line and unit. On return E is
   --  set to be one higher than the last message output.

   procedure Output_Line_Number (L : Line_Number_Type);
   --  Output a line number as six digits (with leading zeroes suppressed),
   --  followed by a period and a blank (note that this is 8 characters which
   --  means that tabs in the source line will not get messed up).

   procedure Output_Msg_Text (E : Error_Msg_Id);
   --  Outputs characters of text in the text of the error message E, excluding
   --  any final exclamation point, followed by a new line.

   procedure Output_Source_Line
     (L : Line_Number_Type; U : Unit_Number_Type; Errs : Boolean);
   --  Outputs text of source line L, in unit U, together with preceding line
   --  number, as described above for Output_Line_Number. The Errs parameter
   --  indicates if there are errors attached to the line, which forces
   --  listing on, even in the presence of pragma List (Off).

   procedure Set_Msg_Text (Text : Str; Flag : Source_Ptr);
   --  Add a sequence of characters to the current message. The characters may
   --  be one of the special insertion characters (see documentation in spec).
   --  Flag is the location at which the error is to be posted, which is used
   --  to determine whether or not the # insertion needs a file name. The
   --  global variables Msg_Buffer, Msg_Len, Is_Warning_Msg, Is_Fatal_Msg,
   --  and Is_Unconditional_Msg are set on return.

   procedure Set_Msg_Insertion_Column;
   --  Handle column number insertion (@ insertion character)

   procedure Set_Msg_Insertion_Name;
   --  Handle name insertion (% insertion character)

   procedure Set_Msg_Insertion_Line_Number (Flag : Source_Ptr);
   --  Handle line number insertion (# insertion character). Flag is passed as
   --  a parameter to determine whether or not to add "in file xxx" to msg.

   procedure Set_Msg_Insertion_Node;
   --  Handle node (name from node) insertion (& insertion character)

   procedure Set_Msg_Insertion_Reserved_Name;
   --  Handle insertion of reserved word name (* insertion character).

   procedure Set_Msg_Insertion_Reserved_Word (Text : Str; I : in out Int);
   --  Handle reserved word insertion (upper case letters). The Text argument
   --  is the current error message input text, and I is an index which on
   --  entry points to the first character of the reserved word, and on exit
   --  points past the last character of the reserved word.

   procedure Set_Msg_Insertion_Uint;
   --  Handle Uint insertion (^ insertion character)

   procedure Set_Msg_Insertion_Unit_Name;
   --  Handle unit name insertion ($ insertion character)

   procedure Set_Msg_Insertion_File_Name;
   --  Handle file name insertion ({ insertion character)

   procedure Set_Msg_Blank;
   --  Sets a single blank in the message if the preceding character is a
   --  non-blank character other than a left parenthesis.

   procedure Set_Msg_Char (C : Char);
   --  Add a single character to the current message. This routine does not
   --  check for special insertion characters (they are just treated as text
   --  characters if they occur).

   procedure Set_Msg_Int (Line : Int);
   --  Set the decimal representation of the argument in the error message
   --  buffer with no leading zeroes output.

   procedure Set_Msg_Name_Buffer;
   --  Output name from Name_Buffer, with surrounding quotes unless manual
   --  quotation mode is in effect.

   procedure Set_Msg_Node (Node : Node_Id);
   --  Add the sequence of characters for the name associated with the
   --  given node to the current message.

   procedure Set_Msg_Quote;
   --  Set quote if in normal quote mode, nothing if in manual quote mode

   procedure Set_Msg_Str (Text : Str);
   --  Add a sequence of characters to the current message. This routine does
   --  not check for special insertion characters (they are just treated as
   --  text characters if they occur).

   -----------------------
   -- Initialize_Errout --
   -----------------------

   procedure Initialize_Errout is
   begin
      Errors.Init;
      Error_Msgs := No_Error_Msg;
      Errors_Detected := 0;
      Warnings_Detected := 0;
      Cur_Msg := No_Error_Msg;
      Current_Error_Unit := Main_Unit;
      List_Pragmas.Init;
   end Initialize_Errout;

   ---------------------------
   -- Finalize_Error_Output --
   ---------------------------

   procedure Finalize_Error_Output is
      E : Error_Msg_Id;
      Err_Flag : Boolean;

   begin
      --  Brief Error mode

      if Brief_Output or (not Full_List and not Verbose_Mode) then
         E := Error_Msgs;
         Set_Standard_Error;

         while E /= No_Error_Msg loop
            Write_Char ('"');
            Write_Name (File.Table (Errors.Table (E).Unit).Full_File_Name);
            Write_String (""", line ");
            Write_Int (Int (Errors.Table (E).Line));
            Write_Char ('(');
            Write_Int (Int (Errors.Table (E).Col));
            Write_Str ("): ");
            Output_Msg_Text (E);
            E := Errors.Table (E).Next;
         end loop;

         Set_Standard_Output;
      end if;

      --  Full source listing case

      if Full_List then
         List_Pragmas_Index := 1;
         List_Pragmas_Mode := True;
         E := Error_Msgs;

         --  First list main source file with its error messages

         for N in 1 .. File.Table (Main_Unit).Last_Line loop
            Err_Flag :=
              E /= No_Error_Msg
                and then Errors.Table (E).Line = N
                and then Errors.Table (E).Unit = Main_Unit;
            Output_Source_Line (N, Main_Unit, Err_Flag);

            if Err_Flag then
               Output_Error_Msgs (E);
               Write_Eol;
            end if;

         end loop;

         --  Then output errors, if any, for subsidiary units

         while E /= No_Error_Msg
           and then Errors.Table (E).Unit /= Main_Unit
         loop
            Write_Eol;
            Output_Source_Line
              (Errors.Table (E).Line, Errors.Table (E).Unit, True);
            Output_Error_Msgs (E);
         end loop;
      end if;

      --  Verbose mode (error lines only with error flags)

      if Verbose_Mode and not Full_List then
         E := Error_Msgs;

         --  Loop through error lines

         while E /= No_Error_Msg loop
            Write_Eol;
            Output_Source_Line
              (Errors.Table (E).Line, Errors.Table (E).Unit, True);
            Output_Error_Msgs (E);
         end loop;
      end if;

      --  Output error summary if verbose or full list mode

      if Verbose_Mode or else Full_List then

         --  Extra blank line if error messages or source listing were output

         if Errors_Detected + Warnings_Detected > 0 or else Full_List then
            Write_Eol;
         end if;

         --  Message giving total number of lines

         Write_Str (" ");
         Write_Int (Int (File.Table (Main_Unit).Last_Line));

         if File.Table (Main_Unit).Last_Line = 1 then
            Write_Str (" line: ");
         else
            Write_Str (" lines: ");
         end if;

         --  Message giving number of errors detected. This normally goes to
         --  Standard_Output. The exception is when brief mode is not set,
         --  verbose mode (or full list mode) is set, and there are errors.
         --  In this case we send the message to standard error to make sure
         --  that *something* appears on standard error in an error situation.

         if Errors_Detected + Warnings_Detected /= 0
           and then not Brief_Output
           and then (Verbose_Mode or Full_List)
         then
            Set_Standard_Error;
         end if;

         if Errors_Detected = 0 then
            Write_String ("No errors");

         elsif Errors_Detected = 1 then
            Write_String ("1 error");

         else
            Write_Int (Errors_Detected);
            Write_String (" errors");
         end if;

         if Warnings_Detected = 1 then
            Write_String (", 1 warning");

         elsif Warnings_Detected > 1 then
            Write_String (", ");
            Write_Int (Warnings_Detected);
            Write_String (" warnings");
         end if;

         Write_Eol;
         Set_Standard_Output;
      end if;
   end Finalize_Error_Output;

   ----------------
   -- Get_Msg_Id --
   ----------------

   function Get_Msg_Id return Error_Msg_Id is
   begin
      return Cur_Msg;
   end Get_Msg_Id;

   ---------------
   -- Error_Msg --
   ---------------

   procedure Error_Msg (Msg : Str; Flag_Location : Source_Ptr) is
      Next_Msg : Error_Msg_Id;
      --  Pointer to next message at insertion point

      Prev_Msg : Error_Msg_Id;
      --  Pointer to previous message at insertion point

      Temp_Msg : Error_Msg_Id;

   begin
      Suppress_Message := False;
      Set_Msg_Text (Msg, Flag_Location);

      --  Return without doing anything if message is suppressed

      if Suppress_Message
        and not All_Errors_Mode
        and not (Msg (Msg'Last) = '!')
      then
         return;
      end if;

      --  Immediate return if warning message and warnings are suppressed

      if Is_Warning_Msg and then Warning_Mode = Suppress then
         Cur_Msg := No_Error_Msg;
         return;
      end if;

      --  Otherwise build error message object for new message

      Name_Buffer (1 .. Msg_Len) := Msg_Buffer (1 .. Msg_Len);
      Name_Len := Msg_Len;

      Errors.Increment_Last;
      Cur_Msg := Errors.Last;
      Errors.Table (Cur_Msg).Text      := Name_Enter;
      Errors.Table (Cur_Msg).Next      := No_Error_Msg;
      Errors.Table (Cur_Msg).Sptr      := Flag_Location;
      Errors.Table (Cur_Msg).Unit      := Get_Sloc_Unit_Number (Flag_Location);
      Errors.Table (Cur_Msg).Line      := Get_Line_Number (Flag_Location);
      Errors.Table (Cur_Msg).Col       := Get_Col_Number (Flag_Location);
      Errors.Table (Cur_Msg).Warn      := Is_Warning_Msg;
      Errors.Table (Cur_Msg).Fatal     := Is_Fatal_Msg;
      Errors.Table (Cur_Msg).Uncond    := Is_Unconditional_Msg;

      --  If immediate errors mode set, output error message now. Also output
      --  now if the -d1 debug flag is set (so node number message comes out
      --  just before actual error message)

      if Immediate_Errors or else Debug_Flag_1 then
         Write_Eol;
         Output_Source_Line (Errors.Table (Cur_Msg).Line,
           Errors.Table (Cur_Msg).Unit, True);
         Temp_Msg := Cur_Msg;
         Output_Error_Msgs (Temp_Msg);

      --  If not in immediate errors mode, then we insert the message in the
      --  error chain for later output by Finalize_Error_Output. The messages
      --  are sorted first by unit (main unit comes first), and within a unit
      --  by source location (earlier flag location first in the chain).

      else
         Prev_Msg := No_Error_Msg;
         Next_Msg := Error_Msgs;

         while Next_Msg /= No_Error_Msg loop
            exit when
              Errors.Table (Cur_Msg).Unit < Errors.Table (Next_Msg).Unit;

            if Errors.Table (Cur_Msg).Unit = Errors.Table (Next_Msg).Unit then
               exit when Flag_Location < Errors.Table (Next_Msg).Sptr;
            end if;

            Prev_Msg := Next_Msg;
            Next_Msg := Errors.Table (Next_Msg).Next;
         end loop;

         --  The possible insertion point for the new message is after Prev_Msg
         --  and before Next_Msg. However, there are some cases in which we do
         --  not insert the message on the grounds that it is redundant with
         --  respect to the previous message. We only consider deleting the
         --  message if it is for the same line and unit as the previous one.

         if Prev_Msg /= No_Error_Msg
           and then Errors.Table (Prev_Msg).Line =
                                             Errors.Table (Cur_Msg).Line
           and then Errors.Table (Prev_Msg).Unit =
                                             Errors.Table (Cur_Msg).Unit
         then

            --  Definitely delete a complete duplicate (i.e. one where the
            --  error text matches as well). Such duplicates are typically
            --  lexical messages from tokens that are rescanned. Note that
            --  such complete duplicates are deleted even if All_Errors
            --  mode is set on, since they can't possibly give any useful
            --  information under any circumstances.

            if Length_Of_Name (Errors.Table (Prev_Msg).Text) =
               Length_Of_Name (Errors.Table (Cur_Msg).Text)
              and then Errors.Table (Prev_Msg).Text =
                       Errors.Table (Cur_Msg).Text
            then
               return;
            end if;

            --  Remaining case is where we are parsing and we are not in
            --  all errors mode (in semantics, don't delete any messages)

            if not All_Errors_Mode and then Compiler_State = Parsing then

               --  Don't delete unconditional or fatal messages

               if not Errors.Table (Cur_Msg).Fatal
                 and then not Errors.Table (Cur_Msg).Uncond
               then

                  --  Don't delete if prev msg is warning and new msg is
                  --  an error. This is because we don't want a real error
                  --  masked by a warning. In all other cases (that is parse
                  --  errors for the same line that are not unconditional
                  --  or fatal) we do delete the message. This helps to avoid
                  --  junk extra messages from cascaded parsing errors

                  if not Errors.Table (Prev_Msg).Warn
                    or else Errors.Table (Cur_Msg).Warn
                  then

                     --  All tests passed, delete the message by simply
                     --  returning without any further processing.

                     return;
                  end if;
               end if;
            end if;
         end if;

         --  Come here if message is to be inserted in the error chain

         if Prev_Msg = No_Error_Msg then
            Error_Msgs := Cur_Msg;

         else
            Errors.Table (Prev_Msg).Next := Cur_Msg;
         end if;

         Errors.Table (Cur_Msg).Next := Next_Msg;
      end if;

      --  Bump appropriate statistics count

      if Errors.Table (Cur_Msg).Warn
        and then Warning_Mode /= Treat_As_Error
      then
         Warnings_Detected := Warnings_Detected + 1;
      else
         Errors_Detected := Errors_Detected + 1;

         --  Turn off code generation if not done already

         if Operating_Mode = Generate_Code then
            Operating_Mode := Check_Semantics;
            Expander_Active := False;
         end if;

         --  If fatal message then set fatal flag in the File.Table entry for
         --  the appropriate unit. For now, we also set this flag for all other
         --  error messages as well unless Try_Semantics is set to True.

         if Is_Fatal_Msg or not Try_Semantics then
            File.Table (Errors.Table (Cur_Msg).Unit).Fatal_Error := True;
         end if;
      end if;

      --  Terminate if max errors reached

      if Errors_Detected = Maximum_Errors then
         Write_String ("fatal error: maximum errors reached");
         Write_Eol;

         raise Unrecoverable_Error;
      end if;

   end Error_Msg;

   -----------------
   -- Error_Msg_S --
   -----------------

   procedure Error_Msg_S (Msg : Str) is
   begin
      Error_Msg (Msg, Scan_Ptr);
   end Error_Msg_S;

   ------------------
   -- Error_Msg_AP --
   ------------------

   procedure Error_Msg_AP (Msg : Str) is
      S1 : Source_Ptr;
      C  : Char;

   begin
      --  If we had saved the Scan_Ptr value after scanning the previous
      --  token, then we would have exactly the right place for putting
      --  the flag immediately at hand. However, that would add at least
      --  two instructions to a Scan call *just* to service the possibility
      --  of an Error_Msg_AP call. So instead we reconstruct that value.

      --  We have two possibilities, start with Prev_Token_Ptr and skip over
      --  the current token, which is made harder by the possibility that this
      --  token may be in error, or start with Token_Ptr and work backwards.
      --  We used to take the second approach, but it's hard because of
      --  comments, and harder still because things that look like comments
      --  can appear inside strings. So now we take the second approach.

      --  Note: in the case where there is no previous token, Prev_Token_Ptr
      --  is set to Source'First, which is a reasonable position for the
      --  error flag in this situation.

      S1 := Prev_Token_Ptr;
      C := Source (S1);

      --  If the previous token is a string literal, we need a special approach
      --  since there may be white space inside the literal and we don't want
      --  to stop on that white space.

      if Prev_Token = Tok_String_Literal then
         loop
            S1 := S1 + 1;

            if Source (S1) = C then
               S1 := S1 + 1;
               exit when Source (S1) /= C;
            elsif Source (S1) in Line_Terminator then
               exit;
            end if;
         end loop;

      --  Character literal also needs special handling

      elsif Prev_Token = Tok_Char_Literal then
         S1 := S1 + 3;

      --  Otherwise we search forward for the end of the current token, marked
      --  by a line terminator, white space, a comment symbol or if we bump
      --  into the following token (i.e. the current token)

      else
         while Source (S1) not in Line_Terminator
           and then Source (S1) /= ' ' and then Source (S1) /= HT
           and then Source (S1 .. S1 + 1) /= "--"
           and then S1 /= Token_Ptr
         loop
            S1 := S1 + 1;
         end loop;
      end if;

      --  S1 is now set to the location for the flag

      Error_Msg (Msg, S1);

   end Error_Msg_AP;

   ------------------
   -- Error_Msg_BC --
   ------------------

   procedure Error_Msg_BC (Msg : Str) is
   begin

      --  If we are at end of file, post the flag after the previous token

      if Token = Tok_EOF then
         Error_Msg_AP (Msg);

      --  If we are at start of file, post the flag at the current token

      elsif Token_Ptr = Source'First then
         Error_Msg_SC (Msg);

      --  If the character before the current token is a space or a horizontal
      --  tab, then we place the flag on this character (in the case of a tab
      --  we would really like to place it in the "last" character of the tab
      --  space, but that it too much trouble to worry about).

      elsif Source (Token_Ptr - 1) = ' '
         or else Source (Token_Ptr - 1) = HT
      then
         Error_Msg (Msg, Token_Ptr - 1);

      --  If there is no space or tab before the current token, then there is
      --  no room to place the flag before the token, so we place it on the
      --  token instead (this happens for example at the start of a line).

      else
         Error_Msg (Msg, Token_Ptr);
      end if;
   end Error_Msg_BC;

   ------------------
   -- Error_Msg_SC --
   ------------------

   procedure Error_Msg_SC (Msg : Str) is
   begin

      --  If we are at end of file, post the flag after the previous token

      if Token = Tok_EOF then
         Error_Msg_AP (Msg);

      --  For all other cases the message is posted at the current token
      --  pointer position

      else
         Error_Msg (Msg, Token_Ptr);
      end if;
   end Error_Msg_SC;

   ------------------
   -- Error_Msg_SP --
   ------------------

   procedure Error_Msg_SP (Msg : Str) is
   begin

      --  Note: in the case where there is no previous token, Prev_Token_Ptr
      --  is set to Source'First, which is a reasonable position for the
      --  error flag in this situation

      Error_Msg (Msg, Prev_Token_Ptr);
   end Error_Msg_SP;

   -----------------
   -- Error_Msg_N --
   -----------------

   procedure Error_Msg_N (Msg : Str; N : Node_Id) is
   begin
      if All_Errors_Mode
        or else Msg (Msg'Last) = '!'
        or else OK_Node (N)
      then
         Debug_Output (N);
         Error_Msg_Node_1 := N;
         Error_Msg (Msg, Sloc (N));
      end if;

      Set_Error_Posted (N, True);
   end Error_Msg_N;

   ------------------
   -- Error_Msg_NE --
   ------------------

   procedure Error_Msg_NE (Msg : Str; N : Node_Id; E : Entity_Id) is
   begin
      if All_Errors_Mode
        or else Msg (Msg'Last) = '!'
        or else OK_Node (N)
      then
         Debug_Output (N);
         Error_Msg_Node_1 := E;
         Error_Msg (Msg, Sloc (N));
      end if;

      Set_Error_Posted (N, True);
   end Error_Msg_NE;

   -----------------------
   -- Change_Error_Text --
   -----------------------

   procedure Change_Error_Text (Error_Id : Error_Msg_Id; New_Msg : Str) is
      Save_Next : Error_Msg_Id;
      Err_Id    : Error_Msg_Id := Error_Id;

   begin
      Set_Msg_Text (New_Msg, Errors.Table (Error_Id).Sptr);
      Name_Len := Msg_Len;
      Name_Buffer (1 .. Name_Len) := Msg_Buffer (1 .. Msg_Len);

      Errors.Table (Error_Id).Text  := Name_Enter;

      --  If in immediate error message mode, output modified error message now
      --  This is just a bit tricky, because we want to output just a single
      --  message, and the messages we modified is already linked in. We solve
      --  this by temporarily resetting its forward pointer to empty.

      if Immediate_Errors then
         Save_Next := Errors.Table (Error_Id).Next;
         Errors.Table (Error_Id).Next := No_Error_Msg;
         Write_Eol;
         Output_Source_Line
           (Errors.Table (Error_Id).Line, Errors.Table (Error_Id).Unit, True);
         Output_Error_Msgs (Err_Id);
         Errors.Table (Error_Id).Next := Save_Next;
      end if;
   end Change_Error_Text;

   ------------------
   -- Debug_Output --
   ------------------

   procedure Debug_Output (N : Node_Id) is
   begin
      if Debug_Flag_1 then
         Write_String ("*** following error message posted on node id = #");
         Write_Int (Int (N));
         Write_String (" ***");
         Write_Eol;
      end if;
   end Debug_Output;

   -------------
   -- OK_Node --
   -------------

   function OK_Node (N : Node_Id) return Boolean is
      K : constant Node_Kind := Nkind (N);

   begin
      if Error_Posted (N) then
         return False;

      elsif K in N_Has_Etype
        and then Present (Etype (N))
        and then Error_Posted (Etype (N))
      then
         return False;

      elsif (K in N_Op
              or else K = N_Attribute_Reference
              or else K = N_Character_Literal
              or else K = N_Expanded_Name
              or else K = N_Identifier
              or else K = N_Operator_Symbol)
        and then Present (Entity (N))
        and then Error_Posted (Entity (N))
      then
         return False;
      else
         return True;
      end if;
   end OK_Node;

   -----------------------
   -- Output_Error_Msgs --
   -----------------------

   procedure Output_Error_Msgs (E : in out Error_Msg_Id) is
      P : Source_Ptr;
      T : Error_Msg_Id;
      Flag_Num : Pos;
      Mult_Flags : Boolean := False;

   begin
      --  Figure out if we will place more than one error flag on this line

      T := E;
      while T /= No_Error_Msg
        and then Errors.Table (T).Line = Errors.Table (E).Line
        and then Errors.Table (T).Unit = Errors.Table (E).Unit
      loop
         if Errors.Table (T).Sptr > Errors.Table (E).Sptr then
            Mult_Flags := True;
         end if;

         T := Errors.Table (T).Next;
      end loop;

      --  Output the error flags. The circuit here makes sure that the tab
      --  characters in the original line are properly accounted for. The
      --  eight blanks at the start are to match the line number.

      Write_Str ("        ");
      P :=
        File.Table (Errors.Table (E).Unit).Lines_Table (Errors.Table (E).Line);
      Flag_Num := 1;

      --  Loop through error messages for this line to place flags

      T := E;
      while T /= No_Error_Msg
        and then Errors.Table (T).Line = Errors.Table (E).Line
        and then Errors.Table (T).Unit = Errors.Table (E).Unit
      loop

         --  Loop to output blanks till current flag position

         while P < Errors.Table (T).Sptr loop
            if File.Table (Errors.Table (T).Unit).Source (P) = HT then
               Write_Char (HT);
            else
               Write_Char (' ');
            end if;

            P := P + 1;
         end loop;

         --  Output flag (unless already output, this happens if more than
         --  one error message occurs at the same flag position).

         if P = Errors.Table (T).Sptr then
            if (Flag_Num = 1 and then not Mult_Flags)
              or else Flag_Num > 9
            then
               Write_Char ('|');
            else
               Write_Char (Char'Val (Char'Pos ('0') + Flag_Num));
            end if;

            P := P + 1;
         end if;

         T := Errors.Table (T).Next;
         Flag_Num := Flag_Num + 1;
      end loop;

      Write_Eol;

      --  Now output the error messages

      T := E;

      while T /= No_Error_Msg
        and then Errors.Table (T).Line = Errors.Table (E).Line
        and then Errors.Table (T).Unit = Errors.Table (E).Unit

      loop
         Write_Str ("        ");
         Output_Msg_Text (T);
         T := Errors.Table (T).Next;
      end loop;

      E := T;
   end Output_Error_Msgs;

   ------------------------
   -- Output_Line_Number --
   ------------------------

   procedure Output_Line_Number (L : Line_Number_Type) is
      D     : Int;      -- next digit
      C     : Char;     -- next character
      Z     : Boolean;  -- flag for zero suppress
      N, M  : Int;      -- temporaries

   begin
      Z := False;
      N := Int (L);
      M := 100_000;

      while M /= 0 loop
         D := Int (N / M);
         N := N rem M;
         M := M / 10;

         if D = 0 then
            if Z then
               C := '0';
            else
               C := ' ';
            end if;
         else
            Z := True;
            C := Char'VAL (D + 48);
         end if;

         Write_Char (C);
      end loop;

      Write_Str (". ");
   end Output_Line_Number;

   ---------------------
   -- Output_Msg_Text --
   ---------------------

   procedure Output_Msg_Text (E : Error_Msg_Id) is
   begin
      if Errors.Table (E).Fatal then
         Write_String ("fatal: ");

      elsif Errors.Table (E).Warn then
         Write_String ("warning: ");

      else
         if not Brief_Output then
            Write_String ("error: ");
         end if;
      end if;

      Write_Name (Errors.Table (E).Text);
      Write_Eol;
   end Output_Msg_Text;

   ------------------------
   -- Output_Source_Line --
   ------------------------

   procedure Output_Source_Line
     (L : Line_Number_Type; U : Unit_Number_Type; Errs : Boolean)
   is
      S : Source_Ptr;
      C : Char;

      Line_Number_Output : Boolean := False;
      --  Set True once line number is output

   begin
      if U /= Current_Error_Unit then
         Write_String ("--------------Error messages for source file: ");
         Write_Name (File.Table (U).Full_File_Name);
         Write_String ("--------------");
         Write_Eol;
         Current_Error_Unit := U;
      end if;

      if Errs or List_Pragmas_Mode then
         Output_Line_Number (L);
         Line_Number_Output := True;
      end if;

      S := File.Table (U).Lines_Table (L);

      loop
         C := File.Table (U).Source (S);
         exit when C = LF or else C = CR or else C = EOF;

         --  Deal with matching entry in List_Pragmas table

         if Full_List
           and then List_Pragmas_Index <= List_Pragmas.Last
           and then S = List_Pragmas.Table (List_Pragmas_Index).Ploc
         then

            case List_Pragmas.Table (List_Pragmas_Index).Ptyp is
               when Page =>
                  Write_Char (C);

                  if not Errs then     -- ignore if on line with errors
                     Write_Char (FF);  -- so that error flags get properly
                  end if;              -- listed with the error line

               when List_On =>
                  List_Pragmas_Mode := True;

                  if not Line_Number_Output then
                     Output_Line_Number (L);
                     Line_Number_Output := True;
                  end if;

                  Write_Char (C);

               when List_Off =>
                  Write_Char (C);
                  List_Pragmas_Mode := False;
            end case;

            List_Pragmas_Index := List_Pragmas_Index + 1;

         --  Normal case (no matching entry in List_Pragmas table)

         else
            if Errs or List_Pragmas_Mode then
               Write_Char (C);
            end if;
         end if;

         S := S + 1;
      end loop;

      if Line_Number_Output then
         Write_Eol;
      end if;
   end Output_Source_Line;

   ------------------
   -- Set_Msg_Text --
   ------------------

   procedure Set_Msg_Text (Text : Str; Flag : Source_Ptr) is
      C : Char;              -- Current character
      I : Int;               -- Current index;

   begin
      Manual_Quote_Mode := False;
      Is_Warning_Msg := False;
      Is_Fatal_Msg := False;
      Is_Unconditional_Msg := False;
      Msg_Len := 0;
      Flag_Unit := Get_Sloc_Unit_Number (Flag);
      I := Text'First;

      while I <= Text'Last loop
         C := Text (I);
         I := I + 1;

         --  Check for insertion character

         if C = '%' then
            Set_Msg_Insertion_Name;

         elsif C = '$' then
            Set_Msg_Insertion_Unit_Name;

         elsif C = '{' then
            Set_Msg_Insertion_File_Name;

         elsif C = '*' then
            Set_Msg_Insertion_Reserved_Name;

         elsif C = '&' then
            Set_Msg_Insertion_Node;

         elsif C = '#' then
            Set_Msg_Insertion_Line_Number (Flag);

         elsif C = '@' then
            Set_Msg_Insertion_Column;

         elsif C = '^' then
            Set_Msg_Insertion_Uint;

         elsif C = '`' then
            Manual_Quote_Mode := not Manual_Quote_Mode;
            Set_Msg_Char ('"');

         elsif C = '!' then
            Is_Unconditional_Msg := True;

         elsif C = '?' then
            Is_Warning_Msg := True;

         elsif C = '~' then
            Is_Fatal_Msg := True;

         elsif C = ''' then
            Set_Msg_Char (Text (I));
            I := I + 1;

         --  Upper case letter (start of reserved word if 2 or more)

         elsif C in 'A' .. 'Z'
           and then I <= Text'Last
           and then Text (I) in 'A' .. 'Z'
         then
            I := I - 1;
            Set_Msg_Insertion_Reserved_Word (Text, I);

         --  Normal character with no special treatment

         else
            Set_Msg_Char (C);
         end if;

      end loop;
   end Set_Msg_Text;

   ------------------------------
   -- Set_Msg_Insertion_Column --
   ------------------------------

   procedure Set_Msg_Insertion_Column is
   begin
      if RM_Column_Check then
         Set_Msg_Str (" in column ");
         Set_Msg_Int (Int (Error_Msg_Col) + 1);
      end if;
   end Set_Msg_Insertion_Column;

   ---------------------------------
   -- Set_Msg_Insertion_File_Name --
   ---------------------------------

   procedure Set_Msg_Insertion_File_Name is
   begin
      if Error_Msg_Name_1 = No_Name then
         null;

      elsif Error_Msg_Name_1 = Error_Name then
         Set_Msg_Blank;
         Set_Msg_Str ("<error>");

      else
         Set_Msg_Blank;
         Get_Name_String (Error_Msg_Name_1);
         Set_Msg_Quote;
         Set_Msg_Name_Buffer;
         Set_Msg_Quote;
      end if;

      --  The following assignment ensures that a second percent insertion
      --  character will correspond to the Error_Msg_Name_2 parameter.

      Error_Msg_Name_1 := Error_Msg_Name_2;

   end Set_Msg_Insertion_File_Name;

   -----------------------------------
   -- Set_Msg_Insertion_Line_Number --
   -----------------------------------

   procedure Set_Msg_Insertion_Line_Number (Flag : Source_Ptr) is
   begin
      Set_Msg_Int (Int (Get_Line_Number (Error_Msg_Sloc_1)));

      --  Add file name if reference is to other than the source
      --  file in which the error message is placed.

      if Error_Msg_Sloc_1 not in
        File.Table (Get_Sloc_Unit_Number (Flag)).Source'range
      then
         Set_Msg_Str (" in file ");
         Get_Name_String (File.Table
           (Get_Sloc_Unit_Number (Error_Msg_Sloc_1)).File_Name);
         Set_Msg_Quote;
         Set_Msg_Name_Buffer;
         Set_Msg_Quote;
      end if;

      --  The following assignment ensures that a second pound insertion
      --  character will correspond to the Error_Msg_Sloc_2 parameter.

      Error_Msg_Sloc_1 := Error_Msg_Sloc_2;

   end Set_Msg_Insertion_Line_Number;

   ----------------------------
   -- Set_Msg_Insertion_Name --
   ----------------------------

   procedure Set_Msg_Insertion_Name is
   begin
      if Error_Msg_Name_1 = No_Name then
         null;

      elsif Error_Msg_Name_1 = Error_Name then
         Set_Msg_Blank;
         Set_Msg_Str ("<error>");

      else
         Set_Msg_Blank;
         Get_Name_String (Error_Msg_Name_1);

         --  If operator name, just print it as is

         if Name_Buffer (1) = '"' then
            Set_Msg_Name_Buffer;

         --  Else output with surrounding quotes in proper casing mode

         else
            Set_Casing (File.Table (Flag_Unit).Identifier_Casing, Mixed_Case);
            Set_Msg_Quote;
            Set_Msg_Name_Buffer;
            Set_Msg_Quote;
         end if;
      end if;

      --  The following assignment ensures that a second percent insertion
      --  character will correspond to the Error_Msg_Name_2 parameter.

      Error_Msg_Name_1 := Error_Msg_Name_2;

   end Set_Msg_Insertion_Name;

   ----------------------------
   -- Set_Msg_Insertion_Node --
   ----------------------------

   procedure Set_Msg_Insertion_Node is
   begin
      Suppress_Message :=
        Error_Msg_Node_1 = Error
          or else Error_Msg_Node_1 = Any_Type;

      if Error_Msg_Node_1 = Empty then
         Set_Msg_Blank;
         Set_Msg_Str ("<empty>");

      elsif Error_Msg_Node_1 = Error then
         Set_Msg_Blank;
         Set_Msg_Str ("<error>");

      else
         Set_Msg_Blank;

         --  Skip quotes for operator case

         if Nkind (Error_Msg_Node_1) in N_Op then
            Set_Msg_Node (Error_Msg_Node_1);

         else
            Set_Msg_Quote;
            Set_Msg_Node (Error_Msg_Node_1);
            Set_Msg_Quote;
         end if;
      end if;

      --  The following assignment ensures that a second ampersand insertion
      --  character will correspond to the Error_Msg_Node_2 parameter.

      Error_Msg_Node_1 := Error_Msg_Node_2;

   end Set_Msg_Insertion_Node;

   -------------------------------------
   -- Set_Msg_Insertion_Reserved_Name --
   -------------------------------------

   procedure Set_Msg_Insertion_Reserved_Name is
   begin
      Set_Msg_Blank;
      Get_Name_String (Error_Msg_Name_1);
      Set_Msg_Quote;
      Set_Casing (File.Table (Flag_Unit).Keyword_Casing, All_Lower_Case);
      Set_Msg_Name_Buffer;
      Set_Msg_Quote;
   end Set_Msg_Insertion_Reserved_Name;

   -------------------------------------
   -- Set_Msg_Insertion_Reserved_Word --
   -------------------------------------

   procedure Set_Msg_Insertion_Reserved_Word (Text : Str; I : in out Int) is
   begin
      Set_Msg_Blank;
      Name_Len := 0;

      while I <= Text'Last and then Text (I) in 'A' .. 'Z' loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Text (I);
         I := I + 1;
      end loop;

      Set_Casing (File.Table (Flag_Unit).Keyword_Casing, All_Lower_Case);
      Set_Msg_Quote;
      Set_Msg_Name_Buffer;
      Set_Msg_Quote;
   end Set_Msg_Insertion_Reserved_Word;

   ----------------------------
   -- Set_Msg_Insertion_Uint --
   ----------------------------

   procedure Set_Msg_Insertion_Uint is
   begin
      Get_Name_String (UI_Image (Error_Msg_Uint_1));
      Set_Msg_Name_Buffer;

      --  The following assignment ensures that a second carret insertion
      --  character will correspond to the Error_Msg_Uint_2 parameter.

      Error_Msg_Uint_1 := Error_Msg_Uint_2;
   end Set_Msg_Insertion_Uint;

   ---------------------------------
   -- Set_Msg_Insertion_Unit_Name --
   ---------------------------------

   procedure Set_Msg_Insertion_Unit_Name is
   begin
      if Error_Msg_Unit_1 = No_Name then
         null;

      elsif Error_Msg_Unit_1 = Error_Name then
         Set_Msg_Blank;
         Set_Msg_Str ("<error>");

      else
         Get_Unit_Name_String (Error_Msg_Unit_1);
         Set_Msg_Blank;
         Set_Msg_Quote;
         Set_Msg_Name_Buffer;
         Set_Msg_Quote;
      end if;

      --  The following assignment ensures that a second percent insertion
      --  character will correspond to the Error_Msg_Name_2 parameter.

      Error_Msg_Unit_1 := Error_Msg_Unit_2;

   end Set_Msg_Insertion_Unit_Name;

   -------------------
   -- Set_Msg_Blank --
   -------------------

   procedure Set_Msg_Blank is
   begin
      if Msg_Len > 0
        and then Msg_Buffer (Msg_Len) /= ' '
        and then Msg_Buffer (Msg_Len) /= '('
        and then Msg_Buffer (Msg_Len) /= '"'
      then
         Set_Msg_Char (' ');
      end if;
   end Set_Msg_Blank;

   ------------------
   -- Set_Msg_Char --
   ------------------

   procedure Set_Msg_Char (C : Char) is
   begin

      --  The check for message buffer overflow is needed to deal with cases
      --  where insertions get too long (in particular a child unit name can
      --  be very long).

      if Msg_Len < Max_Msg_Length then
         Msg_Len := Msg_Len + 1;
         Msg_Buffer (Msg_Len) := C;
      end if;
   end Set_Msg_Char;

   -----------------
   -- Set_Msg_Int --
   -----------------

   procedure Set_Msg_Int (Line : Int) is
   begin
      if Line > 9 then
         Set_Msg_Int (Line / 10);
      end if;

      Set_Msg_Char (Char'VAL (Char'Pos ('0') + (Line rem 10)));
   end Set_Msg_Int;

   -------------------------
   -- Set_Msg_Name_Buffer --
   -------------------------

   procedure Set_Msg_Name_Buffer is
   begin
      for I in 1 .. Name_Len loop
         Set_Msg_Char (Name_Buffer (I));
      end loop;
   end Set_Msg_Name_Buffer;

   ------------------
   -- Set_Msg_Node --
   ------------------

   procedure Set_Msg_Node (Node : Node_Id) is
      S, Sptr : Source_Ptr;
      I       : Int;
      Sbuffer : Source_Buffer_Ptr;
      Ent     : Entity_Id;

   begin
      if Nkind (Node) = N_Designator then
         Set_Msg_Node (Name (Node));
         Set_Msg_Char ('.');
         Set_Msg_Node (Identifier (Node));

      elsif Nkind (Node) = N_Defining_Program_Unit_Name then
         Set_Msg_Node (Name (Node));
         Set_Msg_Char ('.');
         Set_Msg_Node (Defining_Identifier (Node));

      elsif Nkind (Node) = N_Selected_Component then
         Set_Msg_Node (Prefix (Node));
         Set_Msg_Char ('.');
         Set_Msg_Node (Selector_Name (Node));

      --  Here we have a simple identifier

      else
         Get_Name_String (Chars (Node));

         --  A special check. If we have an implicit general access type, then
         --  the insertion becomes access to "xxx" where xxx is the designated
         --  type of the implicit general access type.

         if Name_Buffer (1 .. 6) = "ityp__" then
            if Nkind (Node) = N_Identifier then
               Ent := Entity (Node);
            else
               Ent := Node;
            end if;

            if Ekind (Ent) = E_General_Access_Type then
               if Msg_Buffer (Msg_Len) = '"' then
                  Msg_Len := Msg_Len - 1;
                  Set_Msg_Str ("access to """);
               else
                  Set_Msg_Str ("access to ");
               end if;

               Get_Name_String
                 (Chars (Directly_Designated_Type (Ent)));
            end if;
         end if;

         --  Now we have to set the proper case. If we have a source location
         --  then do a check to see if the name in the source is the same name
         --  as the name in the Names table, except for possible differences
         --  in case, which is the case when we can copy from the source.

         S := Sloc (Error_Msg_Node_1);
         I := 1;

         if S /= No_Location and then S /= Standard_Location then
            Sbuffer := File.Table (Get_Sloc_Unit_Number (S)).Source;
            Sptr := S;

            while I <= Name_Len loop
               exit when Fold_Lower (Sbuffer (Sptr)) /=
                 Fold_Lower (Name_Buffer (I));
               I := I + 1;
               Sptr := Sptr + 1;
            end loop;
         end if;

         --  If we get through the loop without a mismatch, then output
         --  the name the way it is spelled in the source program

         if I > Name_Len then
            Sptr := S;

            for I in 1 .. Name_Len loop
               Name_Buffer (I) := Sbuffer (Sptr);
               Sptr := Sptr + 1;
            end loop;

         --  Otherwise we set the casing using the default identifier casing

         else
            Set_Casing (File.Table (Flag_Unit).Identifier_Casing, Mixed_Case);
         end if;

         Set_Msg_Name_Buffer;
      end if;
   end Set_Msg_Node;

   -------------------
   -- Set_Msg_Quote --
   -------------------

   procedure Set_Msg_Quote is
   begin
      if not Manual_Quote_Mode then
         Set_Msg_Char ('"');
      end if;
   end Set_Msg_Quote;

   -----------------
   -- Set_Msg_Str --
   -----------------

   procedure Set_Msg_Str (Text : Str) is
   begin
      for I in Text'range loop
         Set_Msg_Char (Text (I));
      end loop;
   end Set_Msg_Str;

end Errout;
