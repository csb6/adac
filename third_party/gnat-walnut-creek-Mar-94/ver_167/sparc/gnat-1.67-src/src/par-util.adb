------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P A R . U T I L                              --
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

with Uintp; use Uintp;
separate (Par)
package body Util is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Set_Keyword_Name;
   --  Builds a names table entry for the current token and stores the Name_Id
   --  in Error_Msg_Name_1, ready for incorporation into an error message.

   ---------------------
   -- Bad_Spelling_Of --
   ---------------------

   function Bad_Spelling_Of (T : Token_Type) return Boolean is
      Tname : constant String := Token_Type'Image (T);
      --  Characters of token name

      S : Str (1 .. Int (Tname'Last) - 4);
      --  Characters of token name folded to lowe case, omitting TOK_ at start

      M : Str (1 .. 42) := "Incorrect spelling of keyword ????????????";
      --  Buffer used to construct error message

      P : constant := 30;
      --  Starting subscript in M for keyword name

      SL : constant Int := S'Length;
      --  Length of token name excluding TOK_ at start

   begin
      if Token /= Tok_Identifier then
         return False;
      end if;

      for I in S'range loop
         S (I) := Fold_Lower (To_Char (Tname (Integer (I) + 4)));
      end loop;

      Get_Name_String (Token_Name);

      --  Special case: if prefix matches, then assume that we have the
      --  missing space case and give appropriate error message. The scan
      --  pointer is adjusted to point just past the token in this case

      if Name_Len > SL
        and then S = Name_Buffer (1 .. SL)
      then
         Scan_Ptr := Token_Ptr + S'Length;
         Error_Msg_S ("missing space");
         Token := T;
         return True;
      end if;

      --  If first character does not match, then definitely not misspelling

      if S (1) /= Name_Buffer (1) then
         return False;
      end if;

      --  Lengths match. Execute loop to check for a single error, single
      --  transposition or exact match (we only fall through this loop if
      --  one of these three conditions is found).

      if Name_Len = SL then

         --  Loop to check for single mismatch or exact match (we only fall
         --  through this loop if one of these two conditions is met).

         for I in Int range 2 .. Name_Len - 1 loop
            if Name_Buffer (I) /= S (I) then

               exit when Name_Buffer (I + 1) = S (I + 1)
                 and then Name_Buffer (I + 2 .. Name_Len) = S (I + 2 .. SL);

               exit when Name_Buffer (I) = S (I + 1)
                 and then Name_Buffer (I + 1) = S (I)
                 and then Name_Buffer (I + 2 .. Name_Len) = S (I + 2 .. SL);

               return False;
            end if;
         end loop;

      --  Length is 1 too short. Execute loop to check for single deletion
      --  (we only fall through this loop if a single insertion is found)

      elsif Name_Len = S'Last - 1 then
         for I in Int range 2 .. Name_Len loop
            if Name_Buffer (I) /= S (I) then
               exit when Name_Buffer (I .. Name_Len) = S (I + 1 .. SL);
               return False;
            end if;
         end loop;

      --  Length is 1 too long. Execute loop to check for single insertion
      --  (we only fall through this loop if a single insertion is found)

      elsif Name_Len = S'Last + 1 then
         for I in Int range 2 .. S'Last loop
            if Name_Buffer (I) /= S (I) then
               exit when Name_Buffer (I + 1 .. Name_Len) = S (I .. SL);
               return False;
            end if;
         end loop;

      --  Length is completely wrong

      else
         return False;
      end if;

      --  Fall through to here if we have a bad spelling

      for I in 1 .. S'Last loop
         M (P + I - 1) := Fold_Upper (S (I));
      end loop;

      Error_Msg_SC (M (1 .. 29 + S'Last));
      Token := T;
      return True;

   end Bad_Spelling_Of;

   ----------------------
   -- Check_9X_Keyword --
   ----------------------

   --  On entry, the caller has checked that current token is an identifier
   --  whose name matches the name of the 9X keyword New_Tok.

   procedure Check_9X_Keyword (Token_9X, Next : Token_Type) is
      Scan_State : Saved_Scan_State;

   begin
      Save_Scan_State (Scan_State); -- at identifier/keyword
      Scan; -- past identifier/keyword

      if Token = Next then
         Restore_Scan_State (Scan_State); -- to identifier
         Error_Msg_Name_1 := Token_Name;
         Error_Msg_SC ("the keyword* cannot be used in Ada 83");
         Token := Token_9X;
      else
         Restore_Scan_State (Scan_State); -- to identifier
      end if;
   end Check_9X_Keyword;

   -----------------------------
   -- Check_Simple_Expression --
   -----------------------------

   procedure Check_Simple_Expression (E : Node_Id) is
   begin
      if Expr_Form = EF_Non_Simple then
         Error_Msg_N ("this expression must be parenthesized", E);
      end if;
   end Check_Simple_Expression;

   ---------------------------------------
   -- Check_Simple_Expression_In_Ada_83 --
   ---------------------------------------

   procedure Check_Simple_Expression_In_Ada_83 (E : Node_Id) is
   begin
      if Expr_Form = EF_Non_Simple and then Ada_83 then
         Error_Msg_N ("this expression must be parenthesized in Ada 83", E);
      end if;
   end Check_Simple_Expression_In_Ada_83;

   ------------------------
   -- Check_Subtype_Mark --
   ------------------------

   function Check_Subtype_Mark (Mark : Node_Id) return Node_Id is
   begin
      if Nkind (Mark) = N_Identifier
        or else Nkind (Mark) = N_Selected_Component
        or else (Nkind (Mark) = N_Attribute_Reference
                  and then Is_Type_Attribute_Name (Chars (Identifier (Mark))))
        or else Mark = Error


      then
         return Mark;
      else
         Error_Msg ("subtype mark expected", Sloc (Mark));
         return Error;
      end if;
   end Check_Subtype_Mark;

   -------------------
   -- Comma_Present --
   -------------------

   function Comma_Present return Boolean is
      Scan_State  : Saved_Scan_State;
      Paren_Count : Nat;

   begin

      --  First check, if a comma is present, then a comma is present!

      if Token = Tok_Comma then
         T_Comma;
         return True;

      --  If we have a right paren, then that is taken as ending the list
      --  i.e. no comma is present.

      elsif Token = Tok_Right_Paren then
         return False;

      --  If pragmas, then get rid of them and make a recursive call
      --  to process what follows these pragmas.

      elsif Token = Tok_Pragma then
         P_Pragmas_Misplaced;
         return Comma_Present;

      --  At this stage we have an error, and the goal is to decide on whether
      --  or not we should diagnose an error and report a (non-existent)
      --  comma as being present, or simply to report no comma is present

      --  If we are a semicolon, then the question is whether we have a missing
      --  right paren, or whether the semicolon should have been a comma. To
      --  guess the right answer, we scan ahead keeping track of the paren
      --  level, looking for a clue that helps us make the right decision.

      --  This approach is highly accurate in the single error case, and does
      --  not make bad mistakes in the multiple error case (indeed we can't
      --  really make a very bad decision at this point in any case).

      elsif Token = Tok_Semicolon then
         Save_Scan_State (Scan_State);
         Scan; -- past semicolon
         Paren_Count := 0;

         --  Here is the look ahead loop, Paren_Count tells us whether the
         --  token we are looking at is at the same paren level as the
         --  suspicious semicolon that we are trying to figure out.

         loop

            --  If we hit another semicolon or an end of file, and we have
            --  not seen a right paren or another comma on the way, then
            --  probably the semicolon did end the list. Indeed that is
            --  certainly the only single error correction possible here.

            if Token = Tok_Semicolon or else Token = Tok_EOF then
               Restore_Scan_State (Scan_State);
               return False;

            --  A comma at the same paren level as the semicolon is a strong
            --  indicator that the semicolon should have been a comma, indeed
            --  again this is the only possible single error correction.

            elsif Token = Tok_Comma then
               exit when Paren_Count = 0;

            --  A left paren just bumps the paren count

            elsif Token = Tok_Left_Paren then
               Paren_Count := Paren_Count + 1;

            --  A right paren that is at the same paren level as the semicolon
            --  also means that the only possible single error correction is
            --  to assume that the semicolon should have been a comma. If we
            --  are not at the same paren level, then adjust the paren level.

            elsif Token = Tok_Right_Paren then
               exit when Paren_Count = 0;
               Paren_Count := Paren_Count - 1;
            end if;

            --  Keep going, we haven't made a decision yet

            Scan;
         end loop;

         --  If we fall through the loop, it means that we found a terminating
         --  right paren or another comma. In either case it is reasonable to
         --  assume that the semicolon was really intended to be a comma.

         Restore_Scan_State (Scan_State);
         Error_Msg_SC (""";"" illegal here, replaced by "",""");
         Scan; -- past the semicolon
         return True;

      --  If we are not at semicolon or a right paren, then we base the
      --  decision on whether or not the next token can be part of an
      --  expression. If not, then decide that no comma is present (the
      --  caller will eventually generate a missing right parent message)

      elsif Token in Token_Class_Eterm then
         return False;

      --  Otherwise we assume a comma is present, even if none is present,
      --  since the next token must be part of an expression, so if we were
      --  at the end of the list, then there is more than one error present.

      else
         T_Comma; -- to give error
         return True;
      end if;
   end Comma_Present;

   -----------------------
   -- Discard_Junk_List --
   -----------------------

   procedure Discard_Junk_List (L : List_Id) is
   begin
      null;
   end Discard_Junk_List;

   -----------------------
   -- Discard_Junk_Node --
   -----------------------

   procedure Discard_Junk_Node (N : Node_Id) is
   begin
      null;
   end Discard_Junk_Node;

   ------------
   -- Ignore --
   ------------

   procedure Ignore (T : Token_Type) is
   begin
      if Token = T then
         Set_Keyword_Name;
         Error_Msg_SC ("unexpected keyword% ignored");
         Scan;
      end if;
   end Ignore;

   ----------------------------
   -- Is_Reserved_Identifier --
   ----------------------------

   function Is_Reserved_Identifier return Boolean is
      Ident_Casing : Casing_Type := File.Table (Scan_Unit).Identifier_Casing;

   begin
      if not Is_Reserved_Keyword (Token) then
         return False;

      --  If we have a reserved word, check its casing with the default
      --  identifier casing. A match is an exact equality of case styles,
      --  or a case in which the current identifier case mode is unknown.

      else
         if Determine_Token_Casing = Ident_Casing then
            return True;

         elsif Ident_Casing = Unknown then
            return True;

         else
            return False;
         end if;
      end if;
   end Is_Reserved_Identifier;

   -------------------
   -- No_Constraint --
   -------------------

   procedure No_Constraint is
   begin
      if Token in Token_Class_Consk then
         Error_Msg_SC ("constraint not allowed here");
         Discard_Junk_Node (P_Constraint_Opt);
      end if;
   end No_Constraint;

   ---------------------
   -- Pop_Scope_Stack --
   ---------------------

   procedure Pop_Scope_Stack is
   begin
      if Scope.Last = 0 then
         Error_Msg_SC ("compiler error: scope stack underflow!");
         Compiler_Abort;
      end if;

      Scope.Decrement_Last;

      if Debug_Flag_P then
         Error_Msg_Uint_1 := UI_From_Int (Scope.Last);
         Error_Msg_SC ("decrement scope stack ptr, new value = ^!");
      end if;
   end Pop_Scope_Stack;

   ----------------------
   -- Push_Scope_Stack --
   ----------------------

   procedure Push_Scope_Stack is
   begin
      Scope.Increment_Last;

      if Debug_Flag_P then
         Error_Msg_Uint_1 := UI_From_Int (Scope.Last);
         Error_Msg_SC ("increment scope stack ptr, new value = ^!");
      end if;
   end Push_Scope_Stack;

   ----------------------
   -- Separate_Present --
   ----------------------

   function Separate_Present return Boolean is
      Scan_State : Saved_Scan_State;

   begin
      if Token = Tok_Separate then
         return True;

      elsif Token /= Tok_Identifier then
         return False;

      else
         Save_Scan_State (Scan_State);
         Scan; -- past identifier

         if Token = Tok_Semicolon then
            Restore_Scan_State (Scan_State);
            return Bad_Spelling_Of (Tok_Separate);

         else
            Restore_Scan_State (Scan_State);
            return False;
         end if;
      end if;
   end Separate_Present;

   ----------------------
   -- Set_Keyword_Name --
   ----------------------

   procedure Set_Keyword_Name is
      Tname : constant String := Token_Type'Image (Token);
      --  Characters of token name (note we omit TOK_ at the start)

   begin
      Name_Len := Int (Tname'Last) - 4;

      for I in 1 .. Name_Len loop
         Name_Buffer (I) := Fold_Lower (To_Char (Tname (Integer (I) + 4)));
      end loop;

      Error_Msg_Name_1 := Name_Find;
   end Set_Keyword_Name;

   -------------------------------
   -- Token_Is_At_Start_Of_Line --
   -------------------------------

   function Token_Is_At_Start_Of_Line return Boolean is
   begin
      return Prev_Token_Ptr < Current_Line_Start;
   end Token_Is_At_Start_Of_Line;

   ---------------------
   -- Write_Node_Name --
   ---------------------

   procedure Write_Node_Name (Name_Node : Node_Id) is
   begin
      if Nkind (Name_Node) = N_Defining_Program_Unit_Name then
         Write_Node_Name (Name (Name_Node));
         Write_Char ('.');
         Write_Node_Name (Defining_Identifier (Name_Node));

      elsif Nkind (Name_Node) = N_Designator then
         Write_Node_Name (Name (Name_Node));
         Write_Char ('.');
         Write_Node_Name (Identifier (Name_Node));

      elsif Nkind (Name_Node) = N_Selected_Component then
         Write_Node_Name (Prefix (Name_Node));
         Write_Char ('.');
         Write_Node_Name (Selector_Name (Name_Node));

      else
         Write_Name (Chars (Name_Node));
      end if;
   end Write_Node_Name;

end Util;
