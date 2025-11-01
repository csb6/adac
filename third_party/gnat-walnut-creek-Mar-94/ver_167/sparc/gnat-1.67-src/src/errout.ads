------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               E R R O U T                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.31 $                             --
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

with Table;
with Types;  use Types;
package Errout is

--  This package contains the routines to output error messages. They
--  are basically system independent, however in some environments, e.g.
--  when the parser is embedded into an editor, it may be appropriate
--  to replace the implementation of this package.

   type Compiler_State_Type is (Parsing, Analyzing, Expanding);
   Compiler_State : Compiler_State_Type;
   --  Indicates current state of compilation. This is put in the Errout
   --  spec because it affects the action of the error message handling.
   --  In particular, an attempt is made to suppress cascaded error
   --  messages in Parsing mode, but not in the other modes.

   -----------------------------------
   -- Suppression of Error Messages --
   -----------------------------------

   --  In an effort to reduce the impact of redundant error messages, the
   --  error output routines in this package normally suppress certain
   --  classes of messages as follows:

   --    1.  Identical messages placed at the same point in the text. Such
   --        duplicate error message result for example from rescanning
   --        sections of the text that contain lexical errors. Only one of
   --        such a set of duplicate messages is output, and the rest are
   --        suppressed.

   --    2.  If more than one parser message is generated for a single source
   --        line, then only the first message is output, the remaining
   --        messages on the same line are suppressed.

   --    3.  If a message is posted on a node for which a message has been
   --        previously posted, then only the first message is retained. The
   --        Error_Posted flag is used to detect such multiple postings. Note
   --        that this only applies to semantic messages, since otherwise
   --        for parser messages, this would be a special case of case 2.

   --    4.  If a message is posted on a node whose Etype or Entity
   --        fields reference entities on which an error message has
   --        already been placed, as indicated by the Error_Posted flag
   --        being set on these entities, then the message is suppressed.

   --    5.  If a message attempts to insert an Error node, or a direct
   --        reference to the Any_Type node, then the message is suppressed.

   --  This normal suppression action may be overridden in cases 2-5 (but not
   --  in case 1) by setting All_Errors mode, or by setting the special
   --  unconditional message insertion character (!) at the end of the message
   --  text as described below.

   ---------------------------------------------------------
   -- Error Message Text and Message Insertion Characters --
   ---------------------------------------------------------

   --  Error message text strings are composed of lower case letters, digits
   --  and the special characters space, comma, period, colon and semicolon,
   --  apostrophe and parentheses. Special insertion characters can also
   --  appear which cause the error message circuit to modify the given
   --  string as follows:

   --    Insertion character % (Percent: insert name from Names table)
   --      The character % is replaced by the text for the name specified by
   --      the Name_Id value stored in Error_Msg_Name_1, as generated by the
   --      routine Get_Name_String stored in Error_Msg_Name_1. A blank precedes
   --      the name if it is preceded by a non-blank character other than a
   --      left parenthesis. The name is enclosed in quotes unless manual
   --      quotation mode is set. If the Name_Id is set to No_Name, then
   --      no insertion occurs; if the Name_Id is set to Error_Name, then
   --      the string <error> is inserted. A second % may appear in a single
   --      message in which case it is similarly replaced by the name which
   --      is specified by the Name_Id value stored in Error_Msg_Name_2.
   --      THe name is cased according to the current identifier casing mode.

   --    Insertion character $ (Dollar: insert unit name from Names table)
   --      The character $ is treated similarly to %, except that the name
   --      is obtained from the Unit_Name_Type value in Error_Msg_Unit_1
   --      and Error_Msg_Unit_2, as provided by Get_Unit_Name_String in
   --      package Uname.

   --    Insertion character { (Left brace: insert file name from names table)
   --      The character { is treated similarly to %, except that the
   --      name is output literally as stored in the names table without
   --      adjusting the casing.

   --    Insertion character * (Asterisk, insert reserved word name)
   --      The insertion character * is treated exactly like % except that
   --      the resulting name is cased according to the default conventions
   --      for reserved words (see package Scans).

   --    Insertion character & (Ampersand: insert name from node)
   --      The insertion character & is treated similarly to %, except that
   --      the name is taken from the Chars field of the given node, and may
   --      refer to a child unit name, or a selected component. The casing
   --      is, if possible, taken from the original source reference, which
   --      is obtained from the Sloc field of the given node or nodes. If no
   --      Sloc is available (happens e.g. for nodes in package Standard),
   --      then the default case (see Scans spec) is used. The nodes to be
   --      used are stored in Error_Msg_Node_1, Error_Msg_Node_2. No insertion
   --      occurs for the Empty node, and the Error node results in the
   --      insertion of the characters <error>.

   --    Insertion character # (Pound: insert line number reference)
   --      The character # is replaced by the decimal line number of the
   --      location containing the source position stored in Error_Msg_Sloc_1.
   --      If this location is in a different file than the error message
   --      flag location, "in file xxx" is appended to designate the file.

   --    Insertion character @ (At: insert column number reference)
   --      The character @ is replaced by null if the RM_Column_Check mode is
   --      off (False). If the switch is on (True), then @ is replaced by the
   --      text string " in column nnn" where nnn is the decimal representation
   --      of the column number stored in Error_Msg_Col plus one (the plus one
   --      is because the number is stored 0-origin and displayed 1-origin).

   --    Insertion character ^ (Carret: insert integer value)
   --      The character ^ is replaced by the decimal conversion of the Uint
   --      value stored in Error_Msg_Uint_1, with a possible leading minus.
   --      A second ^ may occur in the message, in which case it is replaced
   --      by the decimal conversion of the Uint value in Error_Msg_Uint_2.

   --    Insertion character ! (Exclamation: unconditional message)
   --      The character ! appearing as the last character of a message makes
   --      the message unconditional which means that it is output even if it
   --      would normally be suppressed. See section above for a description
   --      of the cases in which messages are normally suppressed.

   --    Insertion character ~ (Tilde: fatal error message)
   --      The character ~ appearing anywhere in a message makes the message
   --      a fatal error message. Eventually the plan is that only fatal errors
   --      will cause subsequent semantic analysis to be suppressed, but for
   --      now fatal error messages are equivalent to normal unconditional
   --      messages, since all errors cause OK to be reset in the file table.

   --    Insertion character ? (Question: warning message)
   --      The character ? appearing anywhere in a message makes the message
   --      a warning instead of a normal error message, and the text of the
   --      message will be preceded by "Warning:" instead of "Error:" The
   --      handling of warnings if further controlled by the Warning_Mode
   --      option (-w switch), see package Opt for further details.

   --    Insertion character A-Z (Upper case letter: Ada reserved word)
   --      If two or more upper case letters appear in the message, they are
   --      taken as an Ada reserved word, and are converted to the default
   --      case for reserved words (see Scans package spec). Surrounding
   --      quotes are added unless manual quotation mode is currently set.

   --    Insertion character ` (Backquote: set manual quotation mode)
   --      The backquote character always appears in pairs. Each backquote
   --      of the pair is replaced by a double quote character. In addition,
   --      Any reserved keywords, or name insertions between these backquotes
   --      are not surrounded by the usual automatic double quotes. See the
   --      section below on manual quotation mode for further details.

   --    Insertion character ' (Quote: literal character)
   --      Precedes a character which is placed literally into the message.
   --      Used to insert characters into messages that are one of the
   --      insertion characters defined here.

   -----------------------------------------------------
   -- Global Values Used for Error Message Insertions --
   -----------------------------------------------------

   --  The following global variables are essentially additional parameters
   --  passed to the error message routine for insertion sequences described
   --  above. The reason these are passed globally is that the insertion
   --  mechanism is essentially an untyped one in which the appropriate
   --  variables are set dependingon the specific insertion characters used.

   Error_Msg_Col : Column_Number_Type;
   --  Column for @ insertion character in message

   Error_Msg_Uint_1 : Uint;
   Error_Msg_Uint_2 : Uint;
   --  Uint values for ^ insertion characters in message

   Error_Msg_Sloc_1 : Source_Ptr;
   Error_Msg_Sloc_2 : Source_Ptr;
   --  Source location for # insertion character in message

   Error_Msg_Name_1 : Name_Id;
   Error_Msg_Name_2 : Name_Id;
   --  Name_Id values for % insertion characters in message

   Error_Msg_Unit_1 : Name_Id;
   Error_Msg_Unit_2 : Name_Id;
   --  Name_Id values for $ insertion characters in message

   Error_Msg_Node_1 : Node_Id;
   Error_Msg_Node_2 : Node_Id;
   --  Node_Id values for & insertion characters in message

   -----------------------------------------------------
   -- Format of Messages and Manual Quotation Control --
   -----------------------------------------------------

   --  Messages are generally all in lower case, except for inserted names
   --  and appear in one of the following three forms:

   --    fatal error: text
   --    error: text
   --    warning: text

   --  The prefixes fatal error, error and warning are supplied automatically
   --  (depending on the use of the ? and ~ insertion characters), and the
   --  call to the error message supplies the text.

   --  Reserved Ada keywords in the message are in the default keyword case
   --  (determined from the given source program), surrounded by quotation
   --  marks. This is achieved by spelling the reserved word in upper case
   --  letters, which is recognized as a request for insertion of quotation
   --  marks by the error text processor. Thus for example:

   --    Error_Msg_AP ("IS expected");

   --  would result in the output of one of the following:

   --    error: "is" expected
   --    error: "IS" expected
   --    error: "Is" expected

   --  the choice between these being made by looking at the casing convention
   --  used for keywords (actually the first compilation unit keyword) in the
   --  source file.

   --  In the case of names, the default mode for the error text processor
   --  is to surround the name by quotation marks automatically. The case
   --  used for the identifier names is taken from the source program where
   --  possible, and otherwise is the default casing convention taken from
   --  the source file usage.

   --  In some cases, better control over the placement of quote marks is
   --  required. This is achieved using manual quotation mode. In this mode,
   --  one or more insertion sequences is surrounded by backquote characters.
   --  The backquote characters are output as double quote marks, and normal
   --  automatic insertion of quotes is suppressed between the double quotes.
   --  For example:

   --    Error_Msg_AP ("`END&;` expected");

   --  generates a message like

   --    error: "end Open_Scope;" expected

   --  where the node specifying the name Open_Scope has been stored in
   --  Error_Msg_Node_1 prior to the call. The great majority of error
   --  messages operates in normal quotation mode.

   ----------------------------
   -- Message ID Definitions --
   ----------------------------

   type Error_Msg_Id is new Int;
   --  A type used to represent specific error messages. Used by the clients
   --  of this package only in the context of the Get_Error_Id and
   --  Change_Error_Text subprograms.

   No_Error_Msg : constant Error_Msg_Id := 0;
   --  A constant which is different from any value returned by Get_Error_Id.
   --  Typically used by a client to indicate absense of a saved Id value.

   function Get_Msg_Id return Error_Msg_Id;
   --  Returns the Id of the message most recently posted using one of the
   --  Error_Msg routines.

   ------------------------
   -- List Pragmas Table --
   ------------------------

   --  When a pragma Page or pragma List is encountered by the parser, an
   --  entry is made in the following table. This table is then used to
   --  control the full listing if one is being generated. Note that the
   --  reason we do the processing in the parser is so that we get proper
   --  listing control even in syntax check only mode.

   type List_Pragma_Type is (List_On, List_Off, Page);

   type List_Pragma_Record is record
      Ptyp : List_Pragma_Type;
      Ploc : Source_Ptr;
   end record;

   --  Note: Ploc points to the terminating semicolon in the List_Off and
   --  Page cases, and to the pragma keyword for List_On. In the case of
   --  a pragma List_Off, a List_On entry is also made in the table,
   --  pointing to the pragma keyword. This ensures that, as required,
   --  a List (Off) pragma is listed even in list off mode.

   package List_Pragmas is new Table (
      Component_Type => List_Pragma_Record,
      Index_Type     => Int,
      Low_Bound      => 1,
      Initial        => 50,
      Increment      => 200,
      Table_Name     => "List_Pragmas");

   ------------------------------
   -- Error Output Subprograms --
   ------------------------------

   procedure Initialize_Errout;
   --  Initializes for output of error messages. Must be called for each
   --  source file before using any of the other routines in the package.

   procedure Finalize_Error_Output;
   --  Finalize processing of error messages for one file and output message
   --  indicating the number of detected errors.

   procedure Error_Msg (Msg : Str; Flag_Location : Source_Ptr);
   --  Output a message at specified location. Can be called from the parser
   --  or the semantic analyzer.

   procedure Error_Msg_S (Msg : Str);
   --  Output a message at current scan pointer location. This routine can be
   --  called only from the parser, since it references Scan_Ptr.

   procedure Error_Msg_AP (Msg : Str);
   --  Output a message just after the previous token. This routine can be
   --  called only from the parser, since it references Prev_Token_Ptr.

   procedure Error_Msg_BC (Msg : Str);
   --  Output a message just before the current token. Note that the important
   --  difference between this and the previous routine is that the BC case
   --  posts a flag on the current line, whereas AP can post a flag at the
   --  end of the preceding line. This routine can be called only from the
   --  parser, since it references Token_Ptr.

   procedure Error_Msg_SC (Msg : Str);
   --  Output a message at the start of the current token, unless we are at
   --  the end of file, in which case we always output the message after the
   --  last real token in the file. This routine can be called only from the
   --  parser, since it references Token_Ptr.

   procedure Error_Msg_SP (Msg : Str);
   --  Output a message at the start of the previous token. This routine can
   --  be called only from the parser, since it references Prev_Token_Ptr.

   procedure Error_Msg_N (Msg : Str; N : Node_Id);
   --  Output a message at the Sloc of the given node. This routine can be
   --  called from the parser or the semantic analyzer, although the call
   --  from the latter is much more common (and is the most usual way of
   --  generating error messages from the analyzer). The message text may
   --  contain a single & insertion, which will reference the given node.

   procedure Error_Msg_NE (Msg : Str; N : Node_Id; E : Entity_Id);
   --  Output a message at the Sloc of the given node, with an insertion of
   --  the name from the given entity node. This is used by the semantic
   --  routines, where this is a common error message situation. The Msg
   --  text will contain a & as usual to mark the name insert point. This
   --  routine can be called from the parser or the analyzer.

   procedure Change_Error_Text (Error_Id : Error_Msg_Id; New_Msg : Str);
   --  The error message text of the message identified by Id is replaced by
   --  the given text. This text may contain insertion characters in the
   --  usual manner, and need not be the same length as the original text.

end Errout;
