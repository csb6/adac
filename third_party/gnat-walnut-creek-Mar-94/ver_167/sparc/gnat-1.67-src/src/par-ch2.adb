------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R . C H 2                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.20 $                             --
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

separate (Par)
package body Ch2 is

   --  Local functions, used only in this chapter

   function P_Pragma_Argument_Association return Node_Id;

   ---------------------
   -- 2.3  Identifier --
   ---------------------

   --  IDENTIFIER ::= LETTER {LETTER | DIGIT | UNDERLINE}

   --  An IDENTIFIER shall not contain two underlines in a row
   --  An IDENTIFIER shall not be a reserved word

   --  Error recovery: can raise Error_Resync (cannot return Error)

   function P_Identifier return Node_Id is
      Ident_Node : Node_Id;

   begin
      --  All set if we do indeed have an identifier

      if Token = Tok_Identifier then
         Ident_Node := Token_Node;
         Scan; -- past Identifier
         return Ident_Node;

      --  If we have a reserved identifier, manufacture an identifier with
      --  a corresponding name after posting an appropriate error message

      elsif Is_Reserved_Identifier then
         Scan_Reserved_Identifier (Force_Msg => False);
         Ident_Node := Token_Node;
         Scan; -- past the node
         return Ident_Node;

      --  Otherwise we have junk that cannot be interpreted as an identifier

      else
         T_Identifier; -- to give message
         raise Error_Resync;
      end if;
   end P_Identifier;

   --------------------------
   -- 2.4  Numeric Literal --
   --------------------------

   --  Numeric literal is returned by the scanner as either
   --  Tok_Integer_Literal or Tok_Real_Literal

   ----------------------------
   -- 2.4.1  Decimal Literal --
   ----------------------------

   --  Decimal literal is returned by the scanner as either
   --  Tok_Integer_Literal or Tok_Real_Literal

   --------------------------
   -- 2.4.1  Based Literal --
   --------------------------

   --  Based literal is returned by the scanner as either
   --  Tok_Integer_Literal or Tok_Real_Literal

   ----------------------------
   -- 2.5  Character Literal --
   ----------------------------

   --  Handled by the scanner and returned as Tok_Character_Literal

   -------------------------
   -- 2.6  String Literal --
   -------------------------

   --  Handled by the scanner and returned as Tok_Character_Literal
   --  or if the string looks like an operator as Tok_Operator_Symbol.

   ------------------
   -- 2.7  Comment --
   ------------------

   --  Handled by the scanner which simply skips past encountered comments

   -----------------
   -- 2.8  Pragma --
   -----------------

   --  PRAGMA ::= pragma IDENTIFIER
   --    [(PRAGMA_ARGUMENT_ASSOCIATION {, PRAGMA_ARGUMENT_ASSOCIATION})];

   --  The caller has checked that the initial token is PRAGMA

   --  Error recovery: cannot raise Error_Resync

   --  One special piece of processing is needed in this routine. As described
   --  in the section on "Handling semicolon used in place of IS" in module
   --  Parse, the parser detects the case of missing subprogram bodies to
   --  allow recovery from this syntactic error. Pragma INTERFACE (and, for
   --  Ada 9X, pragma IMPORT) can appear in place of the body. The parser must
   --  recognize the use of these two pragmas in this context, otherwise it
   --  will think there are missing bodies, and try to change ; to IS, when
   --  in fact the bodies ARE present, supplied by these pragmas.

   function P_Pragma return Node_Id is

      Interface_Check_Required : Boolean := False;
      --  Set True if check of pragma INTERFACE is required

      Import_Check_Required : Boolean := False;
      --  Set True if check of pragma IMPORT is required

      Arg_Count : Int := 0;
      --  Number of argument associations processed

      Pragma_Node   : Node_Id;
      Pragma_Name   : Name_Id;
      Semicolon_Loc : Source_Ptr;

   begin
      Pragma_Node := New_Node (N_Pragma, Token_Ptr);
      Scan; -- past PRAGMA
      Pragma_Name := Token_Name;
      Set_Identifier (Pragma_Node, P_Identifier);

      --  See if special INTERFACE/IMPORT check is required

      if SIS_Entry_Active then
         Interface_Check_Required := (Pragma_Name = Name_Interface);
         Import_Check_Required    := (Pragma_Name = Name_Import);
      else
         Interface_Check_Required := False;
         Import_Check_Required    := False;
      end if;

      if Token = Tok_Left_Paren then
         Set_Pragma_Argument_Associations (Pragma_Node, New_List);
         Scan; -- past (

         loop
            Arg_Count := Arg_Count + 1;

            if Arg_Count = 2
              and then (Interface_Check_Required or else Import_Check_Required)
            then

               --  Here is where we cancel the SIS active status if this pragma
               --  supplies a body for the currently active subprogram spec.

               if Token in Token_Class_Desig then
                  if Token_Name = Chars (SIS_Labl) then
                     SIS_Entry_Active := False;
                  end if;
               end if;
            end if;

            Append (P_Pragma_Argument_Association,
              Pragma_Argument_Associations (Pragma_Node));
            exit when Token /= Tok_Comma;
            Scan; -- past comma
         end loop;

         T_Right_Paren;
      end if;

      Semicolon_Loc := Token_Ptr;

      if Token /= Tok_Semicolon then
         T_Semicolon;
         Resync_Past_Semicolon;
      else
         Scan; -- past semicolon
      end if;

      if Is_Pragma_Name (Chars (Identifier (Pragma_Node))) then
         return Par.Prag (Pragma_Node, Semicolon_Loc);
      else
         Error_Msg_Node_1 := Identifier (Pragma_Node);
         Error_Msg ("unrecognized pragma&?!", Sloc (Identifier (Pragma_Node)));
         return Error;
      end if;

   exception
      when Error_Resync =>
         Resync_Past_Semicolon;
         return Error;

   end P_Pragma;

   --  This routine is called if a pragma is encountered in an inappropriate
   --  position, the pragma is scanned out and control returns to continue.

   --  The caller has checked that the initial token is pragma

   --  Error recovery: cannot raise Error_Resync

   procedure P_Pragmas_Misplaced is
   begin
      while Token = Tok_Pragma loop
         Error_Msg_SC ("pragma not allowed here");
         Discard_Junk_Node (P_Pragma);
      end loop;
   end P_Pragmas_Misplaced;

   --  This procedure is called to scan out an optional sequence of pragmas.
   --  Any pragmas found are appended to the list provided as an argument.

   --  Error recovery: Cannot raise Error_Resync

   procedure P_Pragmas_Opt (List : List_Id) is
   begin
      while Token = Tok_Pragma loop
         Append (P_Pragma, List);
      end loop;
   end P_Pragmas_Opt;

   --------------------------------------
   -- 2.8  Pragma_Argument Association --
   --------------------------------------

   --  PRAGMA_ARGUMENT_ASSOCIATION ::=
   --    [argument_IDENTIFIER =>] NAME
   --  | [argument_IDENTIFIER =>] EXPRESSION

   --  Error recovery: cannot raise Error_Resync

   function P_Pragma_Argument_Association return Node_Id is
      Scan_State      : Saved_Scan_State;
      Pragma_Arg_Node : Node_Id;
      Identifier_Node : Node_Id;

   begin
      Pragma_Arg_Node := New_Node (N_Pragma_Argument_Association, Token_Ptr);

      if Token = Tok_Identifier then
         Identifier_Node := Token_Node;
         Save_Scan_State (Scan_State); -- at Identifier
         Scan; -- past Identifier

         if Token = Tok_Arrow then
            Scan; -- past arrow
            Set_Identifier (Pragma_Arg_Node, Identifier_Node);
         else
            Restore_Scan_State (Scan_State); -- to Identifier
         end if;
      end if;

      Set_Expression (Pragma_Arg_Node, P_Expression);
      return Pragma_Arg_Node;

   end P_Pragma_Argument_Association;

end Ch2;
