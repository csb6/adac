------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R . C H 4                               --
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

separate (Par)
package body Ch4 is

   --  Local functions, used only in this chapter

   function P_Aggregate_Or_Paren_Expr return Node_Id;
   function P_Allocator               return Node_Id;
   function P_Component_Association   return Node_Id;
   function P_Factor                  return Node_Id;
   function P_Primary                 return Node_Id;
   function P_Range_Or_Subtype_Mark   return Node_Id;
   function P_Relation                return Node_Id;
   function P_Simple_Expression       return Node_Id;
   function P_Term                    return Node_Id;

   function P_Binary_Adding_Operator  return Node_Kind;
   function P_Logical_Operator        return Node_Kind;
   function P_Multiplying_Operator    return Node_Kind;
   function P_Relational_Operator     return Node_Kind;
   function P_Unary_Adding_Operator   return Node_Kind;

   function No_Right_Paren (Expr : Node_Id) return Node_Id;
   --  Function to check for no right paren at end of expression, returns
   --  its argument if no right paren, else flags the paren and returns Error.

   procedure Set_Op_Name (Node : Node_Id);
   --  Procedure to set name field (Chars) in operator node

   --------------------
   -- No_Right_Paren --
   --------------------

   function No_Right_Paren (Expr : Node_Id) return Node_Id is
   begin
      if Token = Tok_Right_Paren then
         Error_Msg_SC ("unexpected right parenthesis");
         Resync_Expression;
         return Error;
      else
         return Expr;
      end if;
   end No_Right_Paren;

   ------------------
   -- Set_Op_Name --
   ------------------

   procedure Set_Op_Name (Node : Node_Id) is
      subtype Op_Node is Node_Kind range N_Op_And .. N_Op_Not;

      type Name_Of_Type is array (Op_Node) of Name_Id;
      Name_Of : Name_Of_Type := Name_Of_Type'(
         N_Op_And      => Name_Op_And,
         N_Op_And_Then => Name_Op_And_Then,
         N_Op_Or       => Name_Op_Or,
         N_Op_Or_Else  => Name_Op_Or_Else,
         N_Op_Xor      => Name_Op_Xor,
         N_Op_In       => Name_Op_In,
         N_Op_Not_In   => Name_Op_Not_In,
         N_Op_Eq       => Name_Op_Eq,
         N_Op_Ne       => Name_Op_Ne,
         N_Op_Lt       => Name_Op_Lt,
         N_Op_Le       => Name_Op_Le,
         N_Op_Gt       => Name_Op_Gt,
         N_Op_Ge       => Name_Op_Ge,
         N_Op_Add      => Name_Op_Add,
         N_Op_Subtract => Name_Op_Subtract,
         N_Op_Concat   => Name_Op_Concat,
         N_Op_Multiply => Name_Op_Multiply,
         N_Op_Divide   => Name_Op_Divide,
         N_Op_Mod      => Name_Op_Mod,
         N_Op_Rem      => Name_Op_Rem,
         N_Op_Expon    => Name_Op_Expon,
         N_Op_Plus     => Name_Op_Add,
         N_Op_Minus    => Name_Op_Subtract,
         N_Op_Abs      => Name_Op_Abs,
         N_Op_Not      => Name_Op_Not);

   begin
      Set_Chars (Node, Name_Of (Nkind (Node)));
   end Set_Op_Name;

   --------------------------
   -- Name (4.1, also 6.4) --
   --------------------------

   --  NAME ::=
   --    DIRECT_NAME        | EXPLICIT_DEREFERENCE
   --  | INDEXED_COMPONENT  | SLICE
   --  | SELECTED_COMPONENT | ATTRIBUTE
   --  | TYPE_CONVERSION    | FUNCTION_CALL
   --  | CHARACTER_LITERAL

   --  DIRECT_NAME ::= IDENTIFIER | OPERATOR_SYMBOL

   --  PREFIX ::= NAME | IMPLICIT_DEREFERENCE

   --  EXPLICIT_DEREFERENCE ::= NAME . all

   --  IMPLICIT_DEREFERENCE ::= NAME

   --  INDEXED_COMPONENT ::= PREFIX (EXPRESSION {, EXPRESSION})

   --  SLICE ::= PREFIX (DISCRETE_RANGE)

   --  SELECTED_COMPONENT ::= PREFIX . SELECTOR_NAME

   --  SELECTOR_NAME ::= IDENTIFIER | CHARACTER_LITERAL | OPERATOR_SYMBOL

   --  ATTRIBUTE_REFERENCE ::= PREFIX ' ATTRIBUTE_DESIGNATOR

   --  ATTRIBUTE_DESIGNATOR ::=
   --    IDENTIFIER [(static_EXPRESSION)]
   --  | access | delta | digits

   --  RANGE_ATTRIBUTE_REFERENCE ::= PREFIX ' RANGE_ATTRIBUTE_DESIGNATOR

   --  RANGE_ATTRIBUTE_DESIGNATOR ::= PREFIX [(static_EXPRESSION)]

   --  FUNCTION_CALL ::=
   --    function_NAME | function_PREFIX [ACTUAL_PARAMETER_PART]

   --  ACTUAL_PARAMETER_PART ::=
   --    (PARAMETER_ASSOCIATION {,PARAMETER_ASSOCIATION})

   --  PARAMETER_ASSOCIATION ::= [FORMAL_PARAMETER =>] ACTUAL_PARAMETER

   --  FORMAL_PARAMETER ::= parameter_SELECTOR_NAME

   --  ACTUAL_PARAMETER ::= EXPRESSION | variable_NAME

   --  This routine scans a name. Note that we do not consider the attributes
   --  Range, Base (unless followed by another attribute), or Class to be
   --  Names in this parser (they can only be used in subtype mark contexts).

   --  Note: syntactically a procedure call looks just like a function call,
   --  so this routine is in practice used to scan out procedure calls as well.

   --  On return, Expr_Form is set to either EF_Name or EF_Simple_Name

   --  Error recovery: can raise Error_Resync

   function P_Name return Node_Id is
      Scan_State : Saved_Scan_State;
      Name_Node  : Node_Id;

   begin
      Name_Node := P_Call_Name;

      --  Complain if we didn't get a name

      if Expr_Form = EF_Call then
         Error_Msg ("function call cannot be used as name",
                       Sloc (Name (Name_Node)));
         Expr_Form := EF_Simple_Name;
         return Error;

      --  Complain about bad apostrophe cases

      elsif Token = Tok_Apostrophe then
         Save_Scan_State (Scan_State); -- at apostrophe
         Scan; -- past apostrophe

         if Token = Tok_Range then
            Error_Msg_SP ("range attribute cannot be used as a name");
            Restore_Scan_State (Scan_State); -- to apostrophe
            raise Error_Resync;

         else -- Token = Tok_Left_Paren
            Error_Msg_SP ("qualified expression cannot be used as a name");
            raise Error_Resync;
         end if;
      end if;

      return Name_Node;
   end P_Name;

   --  This routine scans either a call or a name. These constructs have
   --  overlapping syntax, so the approach is to use this general routine to
   --  scan out any of these cases. On return, the global variable Expr_Form
   --  is set to one of the following

   --    EF_Name       For a name that could not be a call (array slice or
   --                  character literal)
   --
   --    EF_Call       For a call that could be a name (one or more named
   --                  parameters present)
   --
   --    EF_Call_Name  For all other cases, which could be either calls or
   --                  names, and the parser cannot tell the difference.

   --  Note: if on return Token = Tok_Apostrophe, then the apostrophe must be
   --  followed by either a left paren (qualified expression case), or by
   --  range (range attribute case). All other uses of apostrophe (i.e. all
   --  other attributes) are handled in this routine. In the case of the left
   --  paren, P_Call_Name checks that the preceding name is simple, and issues
   --  an error message if this is not the case.

   --  Error recovery: can raise Error_Resync

   function P_Call_Name return Node_Id is
      Scan_State     : Saved_Scan_State;
      Call_Name_Node : Node_Id;
      Prefix_Node    : Node_Id;
      Ident_Node     : Node_Id;
      Expr_Node      : Node_Id;
      Range_Node     : Node_Id;
      Arg_Node       : Node_Id;
      Arg_List       : List_Id;
      Attr_Follows   : Boolean;
      Attr_Id        : Attribute_Id;

   begin
      if Token not in Token_Class_Name then
         Error_Msg_AP ("name expected");
         raise Error_Resync;
      end if;

      --  Loop through designators in qualified name

      Call_Name_Node := Token_Node;

      loop
         Scan; -- past designator
         exit when Token /= Tok_Dot;
         Scan; -- past dot

         --  If we do not have another designator after the dot, then join
         --  the normal circuit to handle a dot extension (may be .all or
         --  character literal case). Otherwise loop back to scan the next
         --  designator.

         if Token not in Token_Class_Desig then
            goto Scan_Name_Extension_Dot;
         else
            Prefix_Node := Call_Name_Node;
            Call_Name_Node := New_Node (N_Selected_Component, Prev_Token_Ptr);
            Set_Prefix (Call_Name_Node, Prefix_Node);
            Set_Selector_Name (Call_Name_Node, Token_Node);
         end if;
      end loop;

      --  We have now scanned out a qualified designator. If the last token is
      --  an operator symbol, then we certainly do not have the Snam case, so
      --  we can just use the normal name extension check circuit

      if Prev_Token = Tok_Operator_Symbol then
         goto Scan_Name_Extension;
      end if;

      --  We have scanned out a qualified simple name, check for name extension
      --  Note that we know there is no dot here at this stage, so the only
      --  possible cases of name extension are apostrophe and left paren.

      if Token = Tok_Apostrophe then
         Save_Scan_State (Scan_State); -- at apostrophe
         Scan; -- past apostrophe

         --  If left paren, then this might be a qualified expression, but we
         --  are only in the business of scanning out names, so return with
         --  Token backed up to point to the apostrophe. The treatment for
         --  the range attribute is similar (we do not consider x'range to
         --  be a name in this grammar).

         if Token = Tok_Left_Paren or else Token = Tok_Range then
            Restore_Scan_State (Scan_State); -- to apostrophe
            Expr_Form := EF_Simple_Name;
            return Call_Name_Node;

         --  Otherwise we have the case of a name extended by an attribute

         else
            goto Scan_Name_Extension_Apostrophe;
         end if;

      --  Check case of qualified simple name extended by a left parenthesis

      elsif Token = Tok_Left_Paren then
         Scan; -- past left paren
         goto Scan_Name_Extension_Left_Paren;

      --  Otherwise the qualified simple name is not extended, so return

      else
         Expr_Form := EF_Simple_Name;
         return Call_Name_Node;
      end if;

      --  Loop scanning past name extensions. A label is used for control
      --  transfer for this loop for ease of interfacing with the finite state
      --  machine in the parenthesis scanning circuit, and also to allow for
      --  passing in control to the appropriate point from the above code.

      <<Scan_Name_Extension>>

         --  Character literal used as name cannot be extended. Also this
         --  cannot be a call, since the name for a call must be a designator.

         if Prev_Token = Tok_Char_Literal then
            Expr_Form := EF_Name;
            return Call_Name_Node;

         --  Immediate exit if no name extension (left paren, dot, apostrophe)

         elsif Token not in Token_Class_Namext then
            Expr_Form := EF_Call_Name;
            return Call_Name_Node;
         end if;

      --  Merge here when we know there is a name extension

      <<Scan_Name_Extension_OK>>

         if Token = Tok_Left_Paren then
            Scan; -- past left paren
            goto Scan_Name_Extension_Left_Paren;

         elsif Token = Tok_Apostrophe then
            Save_Scan_State (Scan_State); -- at apostrophe
            Scan; -- past apostrophe
            goto Scan_Name_Extension_Apostrophe;

         else -- Token = Tok_Dot
            Scan; -- past dot
            goto Scan_Name_Extension_Dot;
         end if;

      --  Case of name extended by dot (selection), dot is already skipped

      <<Scan_Name_Extension_Dot>>

         --  Explicit dereference case

         if Token = Tok_All then
            Prefix_Node := Call_Name_Node;
            Call_Name_Node := New_Node (N_Explicit_Dereference, Token_Ptr);
            Set_Prefix (Call_Name_Node, Prefix_Node);
            Scan; -- past ALL
            goto Scan_Name_Extension;

         --  Selected component case

         elsif Token in Token_Class_Name then
            Prefix_Node := Call_Name_Node;
            Call_Name_Node := New_Node (N_Selected_Component, Prev_Token_Ptr);
            Set_Prefix (Call_Name_Node, Prefix_Node);
            Set_Selector_Name (Call_Name_Node, Token_Node);
            Scan; -- past selector
            goto Scan_Name_Extension;

         --  Reserved identifier as selector

         elsif Is_Reserved_Identifier then
            Scan_Reserved_Identifier (Force_Msg => False);
            Prefix_Node := Call_Name_Node;
            Call_Name_Node := New_Node (N_Selected_Component, Prev_Token_Ptr);
            Set_Prefix (Call_Name_Node, Prefix_Node);
            Set_Selector_Name (Call_Name_Node, Token_Node);
            Scan; -- past identifier used as selector
            goto Scan_Name_Extension;

         --  Here if nothing legal after the dot

         else
            Error_Msg_AP ("selector expected");
            raise Error_Resync;
         end if;

      --  Here for an apostrophe as name extension. The scan position at the
      --  apostrophe has already been saved, and the apostrophe scanned out.

      <<Scan_Name_Extension_Apostrophe>>

         --  If range attribute after apostrophe, then return with Token
         --  pointing to the apostrophe. Note that in this case the prefix
         --  need not be a simple name (cases like A.all'range). Similarly
         --  if there is a left paren after the apostrophe, then we also
         --  return with Token pointing to the apostrophe (this is the
         --  qualified expression case).

         if Token = Tok_Range or else Token = Tok_Left_Paren then
            Restore_Scan_State (Scan_State); -- to apostrophe
            Expr_Form := EF_Call_Name;
            return Call_Name_Node;

         --  Here for cases where attribute designator is an identifier

         elsif Token = Tok_Identifier then

            if Is_Attribute_Name (Token_Name) then
               Ident_Node := Token_Node;
            else
               Error_Msg_N ("Unrecognized attribute&", Token_Node);
            end if;

         --  Here for case of attribute designator is not an identifier

         else
            Ident_Node := New_Node (N_Identifier, Token_Ptr);

            if Token = Tok_Delta then
               Set_Chars (Ident_Node, Name_Delta);
            elsif Token = Tok_Digits then
               Set_Chars (Ident_Node, Name_Digits);
            elsif Token = Tok_Access then
               Set_Chars (Ident_Node, Name_Access);

               if Ada_83 then
                  Error_Msg_N
                    ("`Access` attribute not allowed in Ada 83!", Token_Node);
               end if;
            else
               Error_Msg_AP ("attribute designator expected");
               raise Error_Resync;
            end if;
         end if;

         --  We come here with an OK attribute scanned, and the corresponding
         --  Attribute identifier node stored in Ident_Node.

         Prefix_Node := Call_Name_Node;
         Call_Name_Node := New_Node (N_Attribute_Reference, Prev_Token_Ptr);
         Scan; -- past attribute designator
         Set_Prefix (Call_Name_Node, Prefix_Node);
         Set_Identifier (Call_Name_Node, Ident_Node);
         Attr_Id := Get_Attribute_Id (Chars (Ident_Node));

         --  The syntax of Array attributes (FIRST, LAST, LENGTH and RANGE)
         --  includes an optional static expression that denotes a dimension.
         --  Attributes that are functions (IMAGE, POS, PRED, SUCC, VAL
         --  and VALUE) must be semantically followed by a single argument.
         --  It is simplest to regard a parenthesized expression that follows
         --  and attribute designator as being part of the designator.
         --  Subsequent semantic checks determine whether the presence of
         --  the argument is legal for that attribute.

         --  An exception occurs with attributes that return functions of two
         --  parameters ('Min, 'Max, 'Read, 'Write, 'Output). For these cases
         --  we do *not* swallow the parenthesized expressions which follow
         --  the attribute reference.

         --  The attribute ACCESS is also treated specially, because it may 
         --  be followed by an argument list, in the admidedly pathological
         --  usage: some_function'access(x, y, z), which is equivalent to
         --  calling some_function(x, y, z) directly.

         if Token = Tok_Left_Paren 
           and then Attr_Id /= Attribute_Access
           and then Attr_Id /= Attribute_Max
           and then Attr_Id /= Attribute_Min
           and then Attr_Id /= Attribute_Output
           and then Attr_Id /= Attribute_Read
           and then Attr_Id /= Attribute_Write
         then
            Scan; -- past left paren
            Set_Expression (Call_Name_Node, P_Expression);
            T_Right_Paren;
         end if;

         goto Scan_Name_Extension;

      --  Here for left parenthesis extending name (left paren skipped)

      <<Scan_Name_Extension_Left_Paren>>

         --  We now have to scan through a list of items, terminated by a
         --  right parenthesis. The scan is handled by a finite state
         --  machine. The possibilities are:

         --   (discrete_range)

         --      This is a slice. This case is handled in LP_State_Init.

         --   (expression, expression, ..)

         --      This is interpreted as an indexed component, i.e. as a
         --      case of a name which can be extended in the normal manner.
         --      This case is handled by LP_State_Name or LP_State_Expr.

         --   (..., identifier => expression , ...)

         --      If there is at least one occurence of identifier => (but
         --      none of the other cases apply), then we have a call.

         --  LP_State_Init handles the scan of the initial argument

         <<LP_State_Init>>

            --  Test for Id => case

            if Token = Tok_Identifier then
               Save_Scan_State (Scan_State); -- at Id
               Scan; -- past Id

               if Token = Tok_Arrow then
                  Restore_Scan_State (Scan_State); -- to Id
                  Arg_List := New_List;
                  goto LP_State_Call;

               else
                  Restore_Scan_State (Scan_State); -- to Id
               end if;
            end if;

            --  Here we have an expression after all

            Expr_Node := P_Expression;

            --  Check cases of discrete range for a slice

            --  First possibility: Simple_expression .. Simple_expression

            if Token = Tok_Dot_Dot then
               Check_Simple_Expression (Expr_Node);
               Range_Node := New_Node (N_Range, Token_Ptr);
               Set_Low_Bound (Range_Node, Expr_Node);
               Scan; -- past ..
               Expr_Node := P_Expression;
               Check_Simple_Expression (Expr_Node);
               Set_High_Bound (Range_Node, Expr_Node);

            --  Second possibility: Range attribute

            elsif Token = Tok_Apostrophe then
               Range_Node := P_Range_Attribute (Expr_Node);

            --  Third possibility: Type_name range Range

            elsif Token = Tok_Range then
               if Expr_Form /= EF_Simple_Name then
                  Error_Msg_SC ("subtype mark must precede RANGE");
                  raise Error_Resync;
               end if;

               Range_Node := P_Subtype_Indication (Expr_Node);

            --  Otherwise we just have an expression. It is true that we might
            --  have a subtype mark without a range constraint but this case
            --  is syntactically indistinguishable from the expression case.

            else
               Arg_List := New_List;
               goto LP_State_Expr;
            end if;

            --  Fall through here with unmistakable Discrete range scanned,
            --  which means that we definitely have the case of a slice. The
            --  Discrete range is in Range_Node.

            if Token = Tok_Comma then
               Error_Msg_SC ("slice cannot have more than one dimension");
               raise Error_Resync;

            elsif Token /= Tok_Right_Paren then
               T_Right_Paren;
               raise Error_Resync;

            else
               Scan; -- past right paren
               Prefix_Node := Call_Name_Node;
               Call_Name_Node := New_Node (N_Slice, Sloc (Prefix_Node));
               Set_Prefix (Call_Name_Node, Prefix_Node);
               Set_Discrete_Range (Call_Name_Node, Range_Node);

               --  If we have a name extension, go scan it

               if Token in Token_Class_Namext then
                  goto Scan_Name_Extension_OK;

               --  Otherwise return (a slice is a name, but is not a call)

               else
                  Expr_Form := EF_Name;
                  return Call_Name_Node;
               end if;
            end if;

         --  In LP_State_Expr, we have scanned one or more expressions, and
         --  so we have a call or an indexed component which is a name. On
         --  entry we have the expression just scanned in Expr_Node and
         --  Arg_List contains the list of expressions encountered so far

         <<LP_State_Expr>>
            Append (Expr_Node, Arg_List);

            if not Comma_Present then
               T_Right_Paren;
               Prefix_Node := Call_Name_Node;
               Call_Name_Node :=
                 New_Node (N_Indexed_Component, Sloc (Prefix_Node));
               Set_Prefix (Call_Name_Node, Prefix_Node);
               Set_Expressions (Call_Name_Node, Arg_List);
               goto Scan_Name_Extension;
            end if;

            --  Comma present (and scanned out), test for identifier => case
            --  Test for identifer => case

            if Token = Tok_Identifier then
               Save_Scan_State (Scan_State); -- at Id
               Scan; -- past Id

               if Token = Tok_Arrow then
                  Restore_Scan_State (Scan_State); -- to Id
                  goto LP_State_Call;

               --  Otherwise it's just an expression after all, so backup

               else
                  Restore_Scan_State (Scan_State); -- to Id
               end if;
            end if;

            --  Here we have an expression after all, so stay in this state

            Expr_Node := P_Expression;
            goto LP_State_Expr;

         --  LP_State_Call corresponds to the situation in which at least
         --  one instance of Id => Expression has been encountered, so we
         --  know that we do not have a name, but rather a call. We enter
         --  it with the scan pointer pointing to the next argument to scan,
         --  and Arg_List containing the list of arguments scanned so far.

         <<LP_State_Call>>

            --  Test for case of Id => Expression (named parameter)

            if Token = Tok_Identifier then
               Save_Scan_State (Scan_State); -- at Id
               Ident_Node := Token_Node;
               Scan; -- past Id

               if Token = Tok_Arrow then
                  Arg_Node :=
                    New_Node (N_Parameter_Association, Prev_Token_Ptr);
                  Set_Selector_Name (Arg_Node, Ident_Node);
                  Scan; -- past arrow
                  Set_Actual_Parameter (Arg_Node, P_Expression);
                  Append (Arg_Node, Arg_List);

                  --  If a comma follows, go back and scan next entry

                  if Comma_Present then
                     goto LP_State_Call;

                  --  Otherwise we have the end of a call

                  else
                     Prefix_Node := Call_Name_Node;
                     Call_Name_Node :=
                       New_Node (N_Function_Call, Sloc (Prefix_Node));
                     Set_Name (Call_Name_Node, Prefix_Node);
                     Set_Parameter_Associations (Call_Name_Node, Arg_List);
                     T_Right_Paren;

                     if Token in Token_Class_Namext then
                        goto Scan_Name_Extension_OK;

                     --  This is a case of a call which cannot be a name

                     else
                        Expr_Form := EF_Call;
                        return Call_Name_Node;
                     end if;
                  end if;

               --  Not named parameter: Id started an expression after all

               else
                  Restore_Scan_State (Scan_State); -- to Id
               end if;
            end if;

            --  Here if entry did not start with Id => which means that it
            --  is a positional parameter, which is not allowed, since we
            --  have seen at least one named parameter already.

            Error_Msg_SC
               ("positional parameter association " &
                 "not allowed after named one");

            Expr_Node := P_Expression;

            --  We go back to scanning out expressions, so that we do not get
            --  multiple error messages when several positional parameters
            --  follow a named parameter.

            goto LP_State_Expr;

         --  End of treatment for name extensions starting with left paren

      --  End of loop through name extensions

   end P_Call_Name;

   --  This function parses a restricted form of Names which are either
   --  designators, or designators preceded by a sequence of prefixes
   --  that are direct names.

   --  Error recovery: cannot raise Error_Resync

   function P_Function_Name return Node_Id is
      Designator_Node : Node_Id;
      Prefix_Node     : Node_Id;
      Selector_Node   : Node_Id;
      Dot_Sloc        : Source_Ptr;

   begin

      --  Prefix node is set to the gathered prefix so far, Empty means that
      --  no prefix has been scanned. This allows us to build up the result
      --  in the required right recursive manner.

      Prefix_Node := Empty;

      --  Loop through prefixes

      loop
         Designator_Node := Token_Node;

         if Token not in Token_Class_Desig then
            return P_Identifier; -- let P_Identifier issue the error message

         else -- Token in Token_Class_Desig
            Scan; -- past designator
            exit when Token /= Tok_Dot;
         end if;

         --  Here at a dot, with token just before it in Designator_Node

         if No (Prefix_Node) then
            Prefix_Node := Designator_Node;
         else
            Selector_Node := New_Node (N_Selected_Component, Dot_Sloc);
            Set_Prefix (Selector_Node, Prefix_Node);
            Set_Selector_Name (Selector_Node, Designator_Node);
            Prefix_Node := Selector_Node;
         end if;

         Dot_Sloc := Token_Ptr;
         Scan; -- past dot
      end loop;

      --  Fall out of the loop having just scanned a designator

      if No (Prefix_Node) then
         return Designator_Node;
      else
         Selector_Node := New_Node (N_Selected_Component, Dot_Sloc);
         Set_Prefix (Selector_Node, Prefix_Node);
         Set_Selector_Name (Selector_Node, Designator_Node);
         return Selector_Node;
      end if;

   exception
      when Error_Resync =>
         return Error;

   end P_Function_Name;

   --  This function parses a restricted form of Names which are either
   --  identifiers, or identifiers preceded by a sequence of prefixes
   --  that are direct names.

   --  Error recovery: cannot raise Error_Resync

   function P_Qualified_Simple_Name return Node_Id is
      Designator_Node : Node_Id;
      Prefix_Node     : Node_Id;
      Selector_Node   : Node_Id;
      Dot_Sloc        : Source_Ptr;

   begin

      --  Prefix node is set to the gathered prefix so far, Empty means that
      --  no prefix has been scanned. This allows us to build up the result
      --  in the required right recursive manner.

      Prefix_Node := Empty;

      --  Loop through prefixes

      loop
         Designator_Node := Token_Node;

         if Token = Tok_Identifier then
            Scan; -- past identifier
            exit when Token /= Tok_Dot;

         elsif Token not in Token_Class_Desig then
            return P_Identifier; -- let P_Identifier issue the error message

         else
            Scan; -- past designator

            if Token /= Tok_Dot then
               Error_Msg_SP ("identifier expected");
               return Error;
            end if;
         end if;

         --  Here at a dot, with token just before it in Designator_Node

         if No (Prefix_Node) then
            Prefix_Node := Designator_Node;
         else
            Selector_Node := New_Node (N_Selected_Component, Dot_Sloc);
            Set_Prefix (Selector_Node, Prefix_Node);
            Set_Selector_Name (Selector_Node, Designator_Node);
            Prefix_Node := Selector_Node;
         end if;

         Dot_Sloc := Token_Ptr;
         Scan; -- past dot
      end loop;

      --  Fall out of the loop having just scanned an identifier

      if No (Prefix_Node) then
         return Designator_Node;
      else
         Selector_Node := New_Node (N_Selected_Component, Dot_Sloc);
         Set_Prefix (Selector_Node, Prefix_Node);
         Set_Selector_Name (Selector_Node, Designator_Node);
         return Selector_Node;
      end if;

   exception
      when Error_Resync =>
         return Error;

   end P_Qualified_Simple_Name;

   --  This procedure differs from P_Qualified_Simple_Name only in that it
   --  raises Error_Resync if any error is encountered. It only returns after
   --  scanning a valid qualified simple name.

   --  Error recovery: can raise Error_Resync

   function P_Qualified_Simple_Name_Resync return Node_Id is
      Designator_Node : Node_Id;
      Prefix_Node     : Node_Id;
      Selector_Node   : Node_Id;
      Dot_Sloc        : Source_Ptr;

   begin
      Prefix_Node := Empty;

      --  Loop through prefixes

      loop
         Designator_Node := Token_Node;

         if Token = Tok_Identifier then
            Scan; -- past identifier
            exit when Token /= Tok_Dot;

         elsif Token not in Token_Class_Desig then
            Discard_Junk_Node (P_Identifier); -- to issue the error message
            raise Error_Resync;

         else
            Scan; -- past designator

            if Token /= Tok_Dot then
               Error_Msg_SP ("identifier expected");
               raise Error_Resync;
            end if;
         end if;

         --  Here at a dot, with token just before it in Designator_Node

         if No (Prefix_Node) then
            Prefix_Node := Designator_Node;
         else
            Selector_Node := New_Node (N_Selected_Component, Dot_Sloc);
            Set_Prefix (Selector_Node, Prefix_Node);
            Set_Selector_Name (Selector_Node, Designator_Node);
            Prefix_Node := Selector_Node;
         end if;

         Dot_Sloc := Token_Ptr;
         Scan; -- past period
      end loop;

      --  Fall out of the loop having just scanned an identifier

      if No (Prefix_Node) then
         return Designator_Node;
      else
         Selector_Node := New_Node (N_Selected_Component, Dot_Sloc);
         Set_Prefix (Selector_Node, Prefix_Node);
         Set_Selector_Name (Selector_Node, Designator_Node);
         return Selector_Node;
      end if;

   end P_Qualified_Simple_Name_Resync;

   ----------------------
   -- 4.1  Direct Name --
   ----------------------

   --  Parsed by P_Call_Name (4.1)

   -----------------
   -- 4.1  Prefix --
   -----------------

   --  Parsed by P_Call_Name (4.1)

   -------------------------------
   -- 4.1  Explicit Dereference --
   -------------------------------

   --  Parsed by P_Call_Name (4.1)

   -------------------------------
   -- 4.1  Implicit Dereference --
   -------------------------------

   --  Parsed by P_Call_Name (4.1)

   ------------------------------
   -- 4.1.1  Indexed Component --
   ------------------------------

   --  Parsed by P_Call_Name (4.1)

   ------------------
   -- 4.1.2  Slice --
   ------------------

   --  Parsed by P_Call_Name (4.1)

   -------------------------------
   -- 4.1.3  Selected Component --
   -------------------------------

   --  Parsed by P_Call_Name (4.1)

   --------------------------
   -- 4.1.3  Selector Name --
   --------------------------

   --  Parsed by P_Call_Name (4.1)

   --------------------------------
   -- 4.1.4  Attribute Reference --
   --------------------------------

   --  Parsed by P_Call_Name (4.1)

   ---------------------------------
   -- 4.1.4  Attribute Designator --
   ---------------------------------

   --  Parsed by P_Call_Name (4.1) or P_Range_Attribute (4.1.4)

   --------------------------------------
   -- 4.1.4  Range Attribute Reference --
   --------------------------------------

   --  RANGE_ATTRIBUTE_REFERENCE ::= PREFIX ' RANGE_ATTRIBUTE_DESIGNATOR

   --  RANGE_ATTRIBUTE_DESIGNATOR ::= PREFIX [(static_EXPRESSION)]

   --  In the grammar, a RANGE attribute is simply a name, but its use is
   --  highly restricted, so in the parser, we do not regard it as a name.
   --  Instead, P_Call_Name returns without scanning the 'RANGE part of
   --  the attribute, and the caller uses the following function to construct
   --  a range attribute in places where it is appropriate.

   --  Note that RANGE here is treated essentially as an identifier,
   --  rather than a reserved word.

   --  The caller has parsed the prefix, i.e. a name, and Token points to 
   --  the apostrophe. The token after the apostrophe is known to be RANGE
   --  at this point. The prefix node becomes the prefix of the attribute.

   --  Error_Recovery: Cannot raise Error_Resync

   function P_Range_Attribute (Prefix_Node : Node_Id) return Node_Id is
      Attr_Node  : Node_Id;
      Range_Node : Node_Id;

   begin
      Attr_Node := New_Node (N_Attribute_Reference, Token_Ptr);
      Set_Prefix (Attr_Node, Prefix_Node);
      Scan; -- past apostrophe
      Range_Node := New_Node (N_Identifier, Token_Ptr);
      Set_Chars (Range_Node, Name_Range);
      Set_Identifier (Attr_Node, Range_Node);
      Scan; -- past RANGE

      if Token = Tok_Left_Paren then
         Scan; -- past left paren
         Set_Expression (Attr_Node, P_Expression);
         T_Right_Paren;
      end if;

      return Attr_Node;
   end P_Range_Attribute;

   ---------------------------------------
   -- 4.1.4  Range Attribute Designator --
   ---------------------------------------

   --  Parsed by P_Range_Attribute (4.1.4)

   --------------------
   -- 4.3  Aggregate --
   --------------------

   --  Parsed by P_Aggregate_Or_Paren_Expr (4.3), except in the case where
   --  an aggregate is known to be required (code statement, extension
   --  aggregate), in which cases this routine performs the necessary check.

   --  Error recovery: can raise Error_Resync

   function P_Aggregate return Node_Id is
      Aggregate_Node : Node_Id;
      Aggregate_Sloc : Source_Ptr;
      Expr_Node      : Node_Id;

   begin
      Aggregate_Sloc := Token_Ptr;
      Aggregate_Node := P_Aggregate_Or_Paren_Expr;

      if Nkind (Aggregate_Node) /= N_Aggregate and then
         Nkind (Aggregate_Node) /= N_Extension_Aggregate
      then
         Error_Msg ("aggregate may not have single positional component",
                       Aggregate_Sloc);
         Expr_Node := Aggregate_Node;
         Aggregate_Node := New_Node (N_Aggregate, Aggregate_Sloc);
         Set_Expressions (Aggregate_Node, New_List);
         Append (Expr_Node, Expressions (Aggregate_Node));
      end if;

      return Aggregate_Node;
   end P_Aggregate;

   ------------------------------------------------------------
   -- 4.3  Aggregate or Parenthesized Expresssion (also 4.4) --
   ------------------------------------------------------------

   --  This procedure parses out either an aggregate or a parenthesized
   --  expression (these two constructs are closely related, since a
   --  parenthesized expression looks like an aggregate with a single
   --  positional component).

   --  AGGREGATE ::=
   --    RECORD_AGGREGATE | EXTENSION_AGGREGATE | ARRAY_AGGREGATE

   --  RECORD_AGGREGATE ::= (RECORD_COMPONENT_ASSOCIATION_LIST)

   --  RECORD_COMPONENT_ASSOCIATION_LIST ::=
   --     RECORD_COMPONENT_ASSOCIATION {, RECORD_COMPONENT_ASSOCIATION}
   --   | NULL RECORD

   --  EXTENSION_AGGREGATE ::=
   --    (EXPRESSION with RECORD_COMPONENT_ASSOCIATION_LIST)

   --  ARRAY_AGGREGATE ::=
   --    POSITIONAL_ARRAY_AGGREGATE | NAMED_ARRAY_AGGREGATE

   --  POSITIONAL_ARRAY_AGGREGATE ::=
   --    (EXPRESSION, EXPRESSION {, EXPRESSION})
   --  | (EXPRESSION {, EXPRESSION}, OTHERS => EXPRESSION)

   --  NAMED_ARRAY_AGGREGATE ::=
   --    (ARRAY_COMPONENT_ASSOCIATION {, ARRAY_COMPONENT_ASSOCIATION})

   --  PRIMARY ::= (EXPRESSION);

   --  Error recovery: can raise Error_Resync

   function P_Aggregate_Or_Paren_Expr return Node_Id is
      Aggregate_Node : Node_Id;
      Expr_List      : List_Id;
      Assoc_List     : List_Id;
      Expr_Node      : Node_Id;
      Pexpr_Node     : Node_Id;
      Assoc_Node     : Node_Id;
      Lparen_Sloc    : Source_Ptr;
      Scan_State     : Saved_Scan_State;

   begin
      Lparen_Sloc := Token_Ptr;
      T_Left_Paren;

      --  Note: the mechanism used here of rescanning the initial expression
      --  is distinctly unpleasant, but it saves a lot of fiddling in scanning
      --  out the discrete choice list. Perhaps we should fix things later?

      --  Deal with expression and extension aggregate cases first

      if Token /= Tok_Others then
         Save_Scan_State (Scan_State); -- at start of expression

         --  Deal with (NULL RECORD) case

         if Token = Tok_Null then
            Scan; -- past NULL

            if Token = Tok_Record then
               Aggregate_Node := New_Node (N_Aggregate, Lparen_Sloc);
               Set_Null_Record_Present (Aggregate_Node, True);
               Scan; -- past RECORD
               T_Right_Paren;
               return Aggregate_Node;
            else
               Restore_Scan_State (Scan_State); -- to NULL that must be expr
            end if;
         end if;

         Expr_Node := P_Expression;

         --  Extension aggregate case

         if Token = Tok_With then
            Aggregate_Node := New_Node (N_Extension_Aggregate, Lparen_Sloc);
            Set_Expression (Aggregate_Node, Expr_Node);
            Scan; -- past WITH

            --  Deal with WITH NULL RECORD case

            if Token = Tok_Null then
               Save_Scan_State (Scan_State); -- at NULL
               Scan; -- past NULL

               if Token = Tok_Record then
                  Scan; -- past RECORD
                  Set_Null_Record_Present (Aggregate_Node, True);
                  T_Right_Paren;
                  return Aggregate_Node;

               else
                  Restore_Scan_State (Scan_State); -- to NULL that must be expr
               end if;
            end if;

            --  Otherwise we have a component association list

            Assoc_List := New_List_1 (P_Component_Association);

            while Comma_Present loop
               Append (P_Component_Association, Assoc_List);
            end loop;

            Set_Component_Associations (Aggregate_Node, Assoc_List);
            T_Right_Paren;
            return Aggregate_Node;

         --  Expression case

         elsif Token = Tok_Right_Paren or else Token in Token_Class_Eterm then
            if Expr_Node /= Error then
               if Parens (Expr_Node) then
                  Pexpr_Node :=
                     New_Node (N_Parenthesized_Expression, Lparen_Sloc);
                  Set_Expression (Pexpr_Node, Expr_Node);
                  Expr_Node := Pexpr_Node;
               else
                  Set_Parens (Expr_Node, True);
               end if;
            end if;

            T_Right_Paren; -- past right paren (error message if none)
            return Expr_Node;

         end if;
      end if;

      --  Come here if the only possibility left is a record/array aggregate

      Expr_List  := No_List; -- don't set yet, maybe all named entries
      Assoc_List := No_List; -- don't set yet, maybe all positional entries
      Aggregate_Node := New_Node (N_Aggregate, Lparen_Sloc);

      --  This loop scans through component associations. On entry to the
      --  loop, an expression has been scanned at the start of the current
      --  association unless the current token is OTHERS.

      loop
         --  Deal with others association first. This is a named association

         if Token = Tok_Others then
            if Assoc_List = No_List then Assoc_List := New_List; end if;
            Append (P_Component_Association, Assoc_List);

         --  Here an expression has been scanned out, deal with positional case

         elsif Token = Tok_Comma or else Token = Tok_Right_Paren then
            if Assoc_List /= No_List then
               Error_Msg_BC
                  ("""=>"" expected (positional association cannot follow " &
                   "named association");
            end if;

            if Expr_List = No_List then Expr_List := New_List; end if;
            Append (Expr_Node, Expr_List);

         --  Improper use of WITH

         elsif Token = Tok_With then
            Error_Msg_SC ("WITH must be preceded by single expression in " &
                             "extension aggregate");
            raise Error_Resync;

         --  Here we definitely have a named association

         else
            Restore_Scan_State (Scan_State); -- to start of expression
            if Assoc_List = No_List then Assoc_List := New_List; end if;
            Append (P_Component_Association, Assoc_List);
         end if;

         exit when not Comma_Present;

         if Token /= Tok_Others then
            Save_Scan_State (Scan_State); -- at start of expression
            Expr_Node := P_Expression;
         end if;
      end loop;

      --  All component associations (positional and named) have been scanned

      T_Right_Paren;
      Set_Expressions (Aggregate_Node, Expr_List);
      Set_Component_Associations (Aggregate_Node, Assoc_List);
      return Aggregate_Node;
   end P_Aggregate_Or_Paren_Expr;

   --------------------------------
   -- 4.3  Component Association --
   --------------------------------

   --  RECORD_COMPONENT_ASSOCIATION ::=
   --    component_SELECTOR_NAME {| component_SELECTOR_NAME} => EXPRESSION
   --  | others => EXPRESSION

   --  ARRAY_COMPONENT_ASSOCIATION ::=
   --    DISCRETE_CHOICE_LIST => EXPRESSION

   --  Note: An others choice can also be handled by this routine

   --  Error recovery: can raise Error_Resync

   function P_Component_Association return Node_Id is
      Assoc_Node : Node_Id;

   begin
      Assoc_Node := New_Node (N_Component_Association, Token_Ptr);
      Set_Choices (Assoc_Node, P_Discrete_Choice_List);
      Set_Sloc (Assoc_Node, Token_Ptr);
      TF_Arrow;
      Set_Expression (Assoc_Node, P_Expression);
      return Assoc_Node;
   end P_Component_Association;

   -----------------------------
   -- 4.3.1  Record Aggregate --
   -----------------------------

   --  Parsed by P_Aggregate_Or_Paren_Expr (4.3)

   -----------------------------------------
   -- 4.3.1  Record Component Association --
   -----------------------------------------

   --  Parsed by P_Component_Association (4.3)

   --------------------------------
   -- 4.3.2  Extension Aggregate --
   --------------------------------

   --  Parsed by P_Aggregate_Or_Paren_Expr (4.3)

   ----------------------------------------
   -- 4.3.2  Extension_Part_Subaggregate --
   ----------------------------------------

   --  Parsed by P_Aggregate_Or_Paren_Expr (4.3)

   ----------------------------
   -- 4.3.3  Array Aggregate --
   ----------------------------

   --  Parsed by P_Aggregate_Or_Paren_Expr (4.3)

   ---------------------------------------
   -- 4.3.3  Positional Array Aggregate --
   ---------------------------------------

   --  Parsed by P_Aggregate_Or_Paren_Expr (4.3)

   ----------------------------------
   -- 4.3.3  Named Array Aggregate --
   ----------------------------------

   --  Parsed by P_Aggregate_Or_Paren_Expr (4.3)

   ----------------------------------------
   -- 4.3.3  Array Component Association --
   ----------------------------------------

   --  Parsed by P_Component_Association (4.3)

   ---------------------
   -- 4.4  Expression --
   ---------------------

   --  EXPRESSION ::=
   --    RELATION {and RELATION} | RELATION {and then RELATION}
   --  | RELATION {or RELATION}  | RELATION {or else RELATION}
   --  | RELATION {xor RELATION}

   --  On return, Expr_Form indicates the categorization of the expression

   --  Note: if Token = Tok_Apostrophe on return, then Expr_Form is set to
   --  EF_Simple_Name and the following token is RANGE (range attribute case).

   --  Error recovery: cannot raise Error_Resync

   function P_Expression return Node_Id is
      Logical_Op      : Node_Kind;
      Prev_Logical_Op : Node_Kind;
      Op_Location     : Source_Ptr;
      Node1           : Node_Id;
      Node2           : Node_Id;

   begin
      Node1 := P_Relation;

      if Token in Token_Class_Logop then
         Prev_Logical_Op := N_Empty;

         loop
            Op_Location := Token_Ptr;
            Logical_Op := P_Logical_Operator;

            if Prev_Logical_Op /= N_Empty and then
               Logical_Op /= Prev_Logical_Op
            then
               Error_Msg
                 ("mixed logical operators in expression", Op_Location);
               Prev_Logical_Op := N_Empty;
            else
               Prev_Logical_Op := Logical_Op;
            end if;

            Node2 := Node1;
            Node1 := New_Node (Logical_Op, Op_Location);
            Set_Left_Opnd (Node1, Node2);
            Set_Right_Opnd (Node1, P_Relation);
            Set_Op_Name (Node1);
            exit when Token not in Token_Class_Logop;
         end loop;

         Expr_Form := EF_Non_Simple;
      end if;

      return Node1;
   end P_Expression;

   ---------------------------------------------------
   -- 4.4  Expression with no Right Paren Following --
   ---------------------------------------------------

   --  This function is identical to the normal P_Expression, except that it
   --  checks that the expression scan did not stop on a right paren. It is
   --  called in all contexts where a right parenthesis cannot legitimately
   --  follow an expression.

   function P_Expression_No_Right_Paren return Node_Id is
   begin
      return No_Right_Paren (P_Expression);
   end P_Expression_No_Right_Paren;

   -------------------
   -- 4.4  Relation --
   -------------------

   --  RELATION ::=
   --    SIMPLE_EXPRESSION [RELATIONAL_OPERATOR SIMPLE_EXPRESSION]
   --  | SIMPLE_EXPRESSION [not] in RANGE
   --  | SIMPLE_EXPRESSION [not] in SUBTYPE_MARK

   --  On return, Expr_Form indicates the categorization of the expression

   --  Note: if Token = Tok_Apostrophe on return, then Expr_Form is set to
   --  EF_Simple_Name and the following token is RANGE (range attribute case).

   --  Error recovery: cannot raise Error_Resync. If an error occurs within an
   --  expression, then tokens are scanned until either a non-expression token,
   --  a right paren (not matched by a left paren) or a comma, is encountered.

   function P_Relation return Node_Id is
      Node1, Node2 : Node_Id;

   begin
      Node1 := P_Simple_Expression;

      if Token not in Token_Class_Relop then
         return Node1;

      else

      --  Here we have a relational operator following. If so then scan it
      --  out. Note that the assignment symbol := is treated as a relational
      --  operator to improve the error recovery when it is misused for =.

         Node2 := New_Node (P_Relational_Operator, Token_Ptr);
         Set_Left_Opnd (Node2, Node1);
         Set_Op_Name (Node2);

         if Prev_Token = Tok_In then
            Set_Right_Opnd (Node2, P_Range_Or_Subtype_Mark);
         else
            Set_Right_Opnd (Node2, P_Simple_Expression);
         end if;

         Expr_Form := EF_Non_Simple;

         if Token in Token_Class_Relop then
            Error_Msg_SC ("unexpected relational operator");
            raise Error_Resync;
         end if;

         return Node2;
      end if;

   --  If any error occurs, then scan to the next expression terminator symbol
   --  or comma or right paren at the outer (i.e. current) parentheses level.
   --  The flags are set to indicate a normal simple expression.

   exception
      when Error_Resync =>
         Resync_Expression;
         Expr_Form := EF_Simple;
         return Error;
   end P_Relation;

   -----------------------------------
   -- P_Range_Or_Subtype_Mark (4.4) --
   -----------------------------------

   --  This routine scans out the range or subtype mark that forms the right
   --  operand of a membership test. The reason that we include this code in
   --  chapter 4 is that it is only called from Ch4, and also we need to be
   --  able to call the Simple_Expression routine in this context.

   function P_Range_Or_Subtype_Mark return Node_Id is
      Expr_Node  : Node_Id;
      Range_Node : Node_Id;

   begin
      Expr_Node := P_Simple_Expression;

      --  Simple_Expression .. Simple_Expression as range

      if Token = Tok_Dot_Dot then
         Range_Node := New_Node (N_Range, Token_Ptr);
         Set_Low_Bound (Range_Node, Expr_Node);
         Scan; -- past ..
         Set_High_Bound (Range_Node, P_Simple_Expression);
         return Range_Node;

      --  If no double dot, could be range attribute

      elsif Token = Tok_Apostrophe then
         return P_Range_Attribute (Expr_Node);

      --  Case of subtype mark (optionally qualified simple name or an
      --  attribute whose prefix is an optionally qualifed simple name)

      elsif Expr_Form = EF_Simple_Name
        or else Nkind (Expr_Node) = N_Attribute_Reference
      then
         --  Check for error of range constraint after a subtype mark

         if Token = Tok_Range then
            Error_Msg_SC
              ("range constraint not allowed in membership test");
            Scan; -- past RANGE
            raise Error_Resync;

         --  Check for error of DIGITS or DELTA after a subtype mark

         elsif Token = Tok_Digits or else Token = Tok_Delta then
            Error_Msg_SC
               ("accuracy definition not allowed in membership test");
            Scan; -- past DIGITS or DELTA
            raise Error_Resync;

         elsif Token = Tok_Apostrophe then
            return P_Subtype_Mark_Attribute (Expr_Node);

         else
            return Expr_Node;
         end if;

      --  For other cases (call, non-simple name, qualified expression), we
      --  assume that we have a missing .. (as reasonable an error as any!)

      else
         T_Dot_Dot;  -- give .. expected message
         raise Error_Resync;
      end if;
   end P_Range_Or_Subtype_Mark;

   ----------------------------
   -- 4.4  Simple Expression --
   ----------------------------

   --  SIMPLE_EXPRESSION ::=
   --    [UNARY_ADDING_OPERATOR] TERM {BINARY_ADDING_OPERATOR TERM}

   --  On return, Expr_Form indicates the categorization of the expression

   --  Note: if Token = Tok_Apostrophe on return, then Expr_Form is set to
   --  EF_Simple_Name and the following token is RANGE (range attribute case).

   --  Error recovery: cannot raise Error_Resync. If an error occurs within an
   --  expression, then tokens are scanned until either a non-expression token,
   --  a right paren (not matched by a left paren) or a comma, is encountered.

   --  Note: P_Simple_Expression is called only internally by higher level
   --  expression routines. In cases in the grammar where a simple expression
   --  is required, the approach is to scan an expression, and then post an
   --  appropriate error message if the expression obtained is not simple. This
   --  gives better error recovery and treatment.

   function P_Simple_Expression return Node_Id is
      Scan_State : Saved_Scan_State;
      Node1, Node2 : Node_Id;

   begin
      --  Check for cases starting with a name. There are two reasons for
      --  special casing. First speed things up by catching a common case
      --  without going through several routine layers. Second the caller must
      --  be informed via Expr_Form when the simple expression is a name.

      if Token in Token_Class_Name then
         Node1 := P_Call_Name;

         --  Deal with apostrophe cases

         if Token = Tok_Apostrophe then
            Save_Scan_State (Scan_State); -- at apostrophe
            Scan; -- past apostrophe

            --  If qualified expression, scan it out and fall through

            if Token = Tok_Left_Paren then
               Node1 := P_Qualified_Expression (Node1);
               Expr_Form := EF_Simple;

            --  If range attribute, then we return with Token pointing to the
            --  apostrophe. Note: avoid the normal error check on exit. We
            --  know that the expression really is complete in this case!

            else -- Token = Tok_Range then
               Restore_Scan_State (Scan_State); -- to apostrophe
               Expr_Form := EF_Simple_Name;
               return Node1;
            end if;
         end if;

         --  If an expression terminator follows, the previous processing
         --  completely scanned out the expression (a common case), and
         --  left Expr_Form set appropriately for returning to our caller.

         if Token in Token_Class_Sterm then
            null;

         --  If we do not have an expression terminator, then complete the
         --  scan of a simple expression. This code duplicates the code
         --  found in P_Term and P_Factor.

         else
            if Token = Tok_Double_Asterisk then
               Node2 := New_Node (N_Op_Expon, Token_Ptr);
               Scan; -- past **
               Set_Left_Opnd (Node2, Node1);
               Set_Right_Opnd (Node2, P_Primary);
               Set_Op_Name (Node2);
               Node1 := Node2;
            end if;

            loop
               exit when Token not in Token_Class_Mulop;
               Node2 := New_Node (P_Multiplying_Operator, Token_Ptr);
               Scan; -- past operator
               Set_Left_Opnd (Node2, Node1);
               Set_Right_Opnd (Node2, P_Factor);
               Set_Op_Name (Node2);
               Node1 := Node2;
            end loop;

            loop
               exit when Token not in Token_Class_Binary_Addop;
               Node2 := New_Node (P_Binary_Adding_Operator, Token_Ptr);
               Scan; -- past operator
               Set_Left_Opnd (Node2, Node1);
               Set_Right_Opnd (Node2, P_Term);
               Set_Op_Name (Node2);
               Node1 := Node2;
            end loop;

            Expr_Form := EF_Simple;
         end if;

      --  Cases where simple expression does not start with a name

      else

         --  Scan initial sign and initial Term

         if Token in Token_Class_Unary_Addop then
            Node1 := New_Node (P_Unary_Adding_Operator, Token_Ptr);
            Scan; -- past operator
            Set_Right_Opnd (Node1, P_Term);
            Set_Op_Name (Node1);
         else
            Node1 := P_Term;
         end if;

         --  Scan out sequence of terms separated by binary adding operators

         loop
            exit when Token not in Token_Class_Binary_Addop;
            Node2 := New_Node (P_Binary_Adding_Operator, Token_Ptr);
            Scan; -- past operator
            Set_Left_Opnd (Node2, Node1);
            Set_Right_Opnd (Node2, P_Term);
            Set_Op_Name (Node2);
            Node1 := Node2;
         end loop;

         --  All done, we clearly do not have name or numeric literal so this
         --  is a case of a simple expression which is some other possibility.

         Expr_Form := EF_Simple;
      end if;

      --  Special test to improve error recovery: If the current token is not
      --  the first token on a line (as determined by checking the previous
      --  token position with the start of the current line), then we insist
      --  that we have an appropriate terminating token. Consider the
      --  following two examples:

      --   1)  if A nad B then ...

      --   2)  A := B
      --       C := D

      --  In the first example, we would like to issue a binary operator
      --  expected message and resynchronize to the then. In the second
      --  example, we do not want to issue a binary operator message, so
      --  that instead we will get the missing semicolon message. This
      --  distinction is of course a heuristic which does not always work,
      --  but in practice it is quite effective.

      --  Note: the one case in which we do not go through this circuit is
      --  when we have scanned a range attribute and want to return with
      --  Token pointing to the apostrophe. The apostrophe is not normally
      --  an expression terminator, and is not in Token_Class_Sterm, but
      --  in this special case we know that the expression is complete.

      <<Simple_Expression_Exit_Error_Check>>

         if not Token_Is_At_Start_Of_Line
            and then Token not in Token_Class_Sterm
         then
            Error_Msg_AP ("binary operator expected");
            raise Error_Resync;
         else
            return Node1;
         end if;

   --  If any error occurs, then scan to next expression terminator symbol
   --  or comma, right paren or vertical bar at the outer (i.e. current) paren
   --  level. Expr_Form is set to indicate a normal simple expression.

   exception
      when Error_Resync =>
         Resync_Expression;
         Expr_Form := EF_Simple;
         return Error;

   end P_Simple_Expression;

   ---------------
   -- 4.4  Term --
   ---------------

   --  TERM ::= FACTOR {MULTIPLYING_OPERATOR FACTOR}

   --  Error recovery: can raise Error_Resync

   function P_Term return Node_Id is
      Node1, Node2 : Node_Id;

   begin
      Node1 := P_Factor;

      loop
         exit when Token not in Token_Class_Mulop;
         Node2 := New_Node (P_Multiplying_Operator, Token_Ptr);
         Scan; -- past operator
         Set_Left_Opnd (Node2, Node1);
         Set_Right_Opnd (Node2, P_Factor);
         Set_Op_Name (Node2);
         Node1 := Node2;
      end loop;

      return Node1;
   end P_Term;

   -----------------
   -- 4.4  Factor --
   -----------------

   --  FACTOR ::= PRIMARY [** PRIMARY] | abs PRIMARY | not PRIMARY

   --  Error recovery: can raise Error_Resync

   function P_Factor return Node_Id is
      Node1 : Node_Id;
      Node2 : Node_Id;

   begin
      if Token = Tok_Abs then
         Node1 := New_Node (N_Op_Abs, Token_Ptr);
         Scan; -- past ABS
         Set_Right_Opnd (Node1, P_Primary);
         Set_Op_Name (Node1);
         return Node1;

      elsif Token = Tok_Not then
         Node1 := New_Node (N_Op_Not, Token_Ptr);
         Scan; -- past NOT
         Set_Right_Opnd (Node1, P_Primary);
         Set_Op_Name (Node1);
         return Node1;

      else
         Node1 := P_Primary;

         if Token = Tok_Double_Asterisk then
            Node2 := New_Node (N_Op_Expon, Token_Ptr);
            Scan; -- past **
            Set_Left_Opnd (Node2, Node1);
            Set_Right_Opnd (Node2, P_Primary);
            Set_Op_Name (Node2);
            return Node2;
         else
            return Node1;
         end if;
      end if;
   end P_Factor;

   ------------------
   -- 4.4  Primary --
   ------------------

   --  PRIMARY ::=
   --    NUMERIC_LITERAL  | null
   --  | STRING_LITERAL   | AGGREGATE
   --  | NAME             | QUALIFIED_EXPRESSION
   --  | ALLOCATOR        | (EXPRESSION)

   --  Error recovery: can raise Error_Resync

   function P_Primary return Node_Id is
      Scan_State   : Saved_Scan_State;
      Node1, Node2 : Node_Id;

   begin

      --  The loop runs more than once only if misplaced pragmas are found

      loop
         case Token is

            --  Name token can start a name, call or qualified expression, all
            --  of which are acceptable possibilities for primary. Note also
            --  that string literal is included in name (as operator symbol)
            --  and type conversion is included in name (as indexed component).

            when Tok_Char_Literal | Tok_Operator_Symbol | Tok_Identifier =>
               Node1 := P_Call_Name;

               --  All done unless apostrophe follows

               if Token /= Tok_Apostrophe then
                  return Node1;

               --  Apostrophe following means that we have either just parsed
               --  the subtype mark of a qualified expression, or the prefix
               --  or a range attribute.

               else -- Token = Tok_Apostrophe
                  Save_Scan_State (Scan_State); -- at apostrophe
                  Scan; -- past apostrophe

                  --  If range attribute, then we have a case where this cannot
                  --  appear, since the only legitimate case (where the scanned
                  --  expression is a qualified simple name) is handled at the
                  --  Simple_Expression level. This case corresponds to a usage
                  --  such as 3 + A'Range, which is always illegal.

                  if Token = Tok_Range then
                     Error_Msg_SP ("range attribute not allowed here");
                     Restore_Scan_State (Scan_State); -- to apostrophe
                     Node1 := P_Range_Attribute (Node1);
                     return Error;

                  --  If left paren, then we have a qualified expression. Note
                  --  that P_Call_Name guarantees that in this case, where
                  --  Token = Tok_Apostrophe on return, the only two possible
                  --  tokens following the apostrophe are left paren and
                  --  RANGE, so we know we have a left paren here.

                  else -- Token = Tok_Left_Paren
                     return P_Qualified_Expression (Node1);

                  end if;
               end if;

            --  Numeric or string literal

            when Tok_Integer_Literal |
                 Tok_Real_Literal    |
                 Tok_String_Literal  =>

               Node1 := Token_Node;
               Scan; -- past number
               return Node1;

            --  Left paren, starts aggregate or parenthesized expression

            when Tok_Left_Paren =>
               return P_Aggregate_Or_Paren_Expr;

            --  Allocator

            when Tok_New =>
               return P_Allocator;

            --  Null

            when Tok_Null =>
               Scan; -- past NULL
               return New_Node (N_Null, Prev_Token_Ptr);

            --  Pragma, not allowed here, so just skip past it

            when Tok_Pragma =>
               P_Pragmas_Misplaced;

            --  Anything else is illegal as the first token of a primary, but
            --  we test for a reserved identifier so that it is treated nicely

            when others =>
               if Is_Reserved_Identifier then
                  return P_Identifier;
               else
                  Error_Msg_AP ("missing operand");
                  raise Error_Resync;
               end if;

         end case;
      end loop;
   end P_Primary;

   ---------------------------
   -- 4.5  Logical Operator --
   ---------------------------

   --  LOGICAL_OPERATOR  ::=  AND | OR | XOR

   --  Note: AND THEN and OR ELSE are also treated as logical operators
   --  by the parser (even though they are not operators semantically)

   --  The value returned is the appropriate Node_Kind code for the operator
   --  On return, Token points to the token following the scanned operator.

   --  The caller has checked that the first token is a legitimate logical
   --  operator token (i.e. is either XOR, AND, OR).

   --  Error recovery: cannot raise Error_Resync

   function P_Logical_Operator return Node_Kind is
   begin
      if Token = Tok_And then
         Scan; -- past AND

         if Token = Tok_Then then
            Scan; -- past THEN
            return N_Op_And_Then;
         else
            return N_Op_And;
         end if;

      elsif Token = Tok_Or then
         Scan; -- past OR

         if Token = Tok_Else then
            Scan; -- past ELSE
            return N_Op_Or_Else;
         else
            return N_Op_Or;
         end if;

      else -- Token = Tok_Xor
         Scan; -- past XOR
         return N_Op_Xor;
      end if;
   end P_Logical_Operator;

   ------------------------------
   -- 4.5  Relational Operator --
   ------------------------------

   --  RELATIONAL_OPERATOR ::= = | /= | < | <= | > | >=

   --  The value returned is the appropriate Node_Kind code for the operator.
   --  On return, Token points to the operator token, NOT past it.

   --  The caller has checked that the first token is a legitimate relational
   --  operator token (i.e. is one of the operator tokens listed above).

   --  Error recovery: cannot raise Error_Resync

   function P_Relational_Operator return Node_Kind is
      Op_Kind : Node_Kind;
      Relop_Node : constant array (Token_Class_Relop) of Node_Kind :=
        (Tok_Less           => N_Op_Lt,
         Tok_Equal          => N_Op_Eq,
         Tok_Greater        => N_Op_Gt,
         Tok_Not_Equal      => N_Op_Ne,
         Tok_Greater_Equal  => N_Op_Ge,
         Tok_Less_Equal     => N_Op_Le,
         Tok_In             => N_Op_In,
         Tok_Not            => N_Op_Not_In,
         Tok_Box            => N_Op_Ne);

   begin
      if Token = Tok_Box then
         Error_Msg_SC ("<> should be /=");
      end if;

      Op_Kind := Relop_Node (Token);
      Scan; -- past operator token

      if Prev_Token = Tok_Not then
         T_In;
      end if;

      return Op_Kind;
   end P_Relational_Operator;

   ---------------------------------
   -- 4.5  Binary Adding Operator --
   ---------------------------------

   --  BINARY_ADDING_OPERATOR ::= + | - | &

   --  The value returned is the appropriate Node_Kind code for the operator.
   --  On return, Token points to the operator token (NOT past it).

   --  The caller has checked that the first token is a legitimate adding
   --  operator token (i.e. is one of the operator tokens listed above).

   --  Error recovery: cannot raise Error_Resync

   function P_Binary_Adding_Operator return Node_Kind is
      Addop_Node : constant array (Token_Class_Binary_Addop) of Node_Kind :=
        (Tok_Ampersand      => N_Op_Concat,
         Tok_Minus          => N_Op_Subtract,
         Tok_Plus           => N_Op_Add);
   begin
      --  This is where we make sure we have a space after the binary
      --  operator. We could not do this in the scanner, because we did
      --  not know if a plus or minus was a binary or unary operator.

      if GNAT_Style_Check and then Source (Scan_Ptr) > ' ' then
         Error_Msg_S ("(style) space required");
      end if;

      return Addop_Node (Token);
   end P_Binary_Adding_Operator;

   --------------------------------
   -- 4.5  Unary Adding Operator --
   --------------------------------

   --  UNARY_ADDING_OPERATOR ::= + | -

   --  The value returned is the appropriate Node_Kind code for the operator.
   --  On return, Token points to the operator token (NOT past it).

   --  The caller has checked that the first token is a legitimate adding
   --  operator token (i.e. is one of the operator tokens listed above).

   --  Error recovery: cannot raise Error_Resync

   function P_Unary_Adding_Operator return Node_Kind is
      Addop_Node : constant array (Token_Class_Unary_Addop) of Node_Kind :=
        (Tok_Minus          => N_Op_Minus,
         Tok_Plus           => N_Op_Plus);
   begin
      return Addop_Node (Token);
   end P_Unary_Adding_Operator;

   -------------------------------
   -- 4.5  Multiplying Operator --
   -------------------------------

   --  MULTIPLYING_OPERATOR ::= * | / | MOD | REM

   --  The value returned is the appropriate Node_Kind code for the operator.
   --  On return, Token points to the operator token (NOT past it).

   --  The caller has checked that the first token is a legitimate multiplying
   --  operator token (i.e. is one of the operator tokens listed above).

   --  Error recovery: cannot raise Error_Resync

   function P_Multiplying_Operator return Node_Kind is
      Mulop_Node : constant array (Token_Class_Mulop) of Node_Kind :=
        (Tok_Asterisk       => N_Op_Multiply,
         Tok_Mod            => N_Op_Mod,
         Tok_Rem            => N_Op_Rem,
         Tok_Slash          => N_Op_Divide);
   begin
      return Mulop_Node (Token);
   end P_Multiplying_Operator;

   --------------------------------------
   -- 4.5  Highest Precedence Operator --
   --------------------------------------

   --  Parsed by P_Factor (4.4)

   --  Note: this rule is not in fact used by the grammar at any point!

   --------------------------
   -- 4.6  Type Conversion --
   --------------------------

   --  Parsed by P_Primary as a Name (4.1)

   -------------------------------
   -- 4.7  Qualified Expression --
   -------------------------------

   --  QUALIFIED_EXPRESSION ::=
   --    SUBTYPE_MARK ' (EXPRESSION) | SUBTYPE_MARK ' AGGREGATE

   --  The caller has scanned the name which is the Subtype_Mark parameter
   --  and scanned past the single quote following the subtype mark. The
   --  caller has not checked that this name is in fact appropriate for
   --  a subtype mark name (i.e. it is a selected component or identifier).

   --  Error_Recovery: cannot raise Error_Resync

   function  P_Qualified_Expression (Subtype_Mark : Node_Id) return Node_Id is
      Qual_Node : Node_Id;

   begin
      Qual_Node := New_Node (N_Qualified_Expression, Prev_Token_Ptr);
      Set_Subtype_Mark (Qual_Node, Check_Subtype_Mark (Subtype_Mark));
      Set_Expression (Qual_Node, P_Aggregate_Or_Paren_Expr);
      return Qual_Node;
   end P_Qualified_Expression;

   --------------------
   -- 4.8  Allocator --
   --------------------

   --  ALLOCATOR ::=
   --   new SUBTYPE_INDICATION | new QUALIFIED_EXPRESSION

   --  The caller has checked that the initial token is NEW

   --  Error recovery: can raise Error_Resync

   function P_Allocator return Node_Id is
      Alloc_Node  : Node_Id;
      Type_Node   : Node_Id;

   begin
      Alloc_Node := New_Node (N_Allocator, Token_Ptr);
      T_New;
      Type_Node := P_Subtype_Mark_Resync;

      if Token = Tok_Apostrophe then
         Scan; -- past apostrophe
         Set_Expression (Alloc_Node, P_Qualified_Expression (Type_Node));
      else
         Set_Expression (Alloc_Node, P_Subtype_Indication (Type_Node));
      end if;

      return Alloc_Node;
   end P_Allocator;

end Ch4;
