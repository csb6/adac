------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ E V A L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.34 $                             --
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

with Atree;     use Atree;
with Comperr;   use Comperr;
with Debug;     use Debug;
with Debug_Sem; use Debug_Sem;
with Einfo;     use Einfo;
with Errout;    use Errout;
with Excep;     use Excep;
with Namet;     use Namet;
with Output;    use Output;
with Sem_Util;  use Sem_Util;
with Sinfo;     use Sinfo;
with Sinput;    use Sinput;
with Snames;    use Snames;
with Stand;     use Stand;
with Stringt;   use Stringt;
with Tbuild;    use Tbuild;
with Uintp;     use Uintp;

package body Sem_Eval is

   ----------------
   -- Local Data --
   ----------------

   Pragma_Preelaborable_Enabled : Boolean := False;
   --  This flag is used to enable checking for pragma Preelaborate, but this
   --  circuit is not enabled yet, so there is no way for this to get set True

   Recursive_Call_To_Eval : Boolean := False;
   --  Eval is not supposed to call itself recursively, directly or indirectly.
   --  A pragma Assert is used to check for such a recursive call. 

   Is_Static_Result : Boolean;
   --  Global flag to record if current expression is static. Note that it is
   --  OK for this to be a global precisely because Eval is non-recursive.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Can_Fold_Attribute (N : Node_Id) return Boolean;
   --  Determine if Attribute is foldable. Only attributes of static subtypes
   --  are static. All others are dealt with in subsequent compiler phases.

   function Can_Fold_Expr (N : Node_Id) return Boolean;
   --  Determine if an expression can be folded (but don't do folding yet)

   procedure Check_Int_Literal (N : Node_Id);
   --  Check that the N_Integer_Literal node N has a value that is in range
   --  of the type. If not, a warning message is raised.

   procedure Check_Preevaluate (N : Node_Id);
   --  If the pragma Preelaborate switch is set, this subprogram checks for
   --  a violation of preelaborate rules and generates an appropriate message

   procedure Create_Str_Value_Node (N : Node_Id; Str_Id : String_Id);
   --  Create an N_String_Literal node for the result of compile time
   --  evaluation of the "&" operation on string literals, character literals
   --  or constants. Use of Rewrite_Substitute_Tree ensures that the original
   --  expression tree can be retrieved.

   procedure Eval_Expr (N : Node_Id);
   --  Perform evaluation of operators or attributes. Does not do any
   --  bounds checking for the results of the arithmetic operations.

   procedure Fold_Expr (N : Node_Id);
   --  Do constant folding on an expression for which Can_Fold_Expr is true

   procedure Fold_String (N : Node_Id);
   --  Used to fold a string value (concatenation or string/character literal)

   function Fold_Uint (N : Node_Id) return Uint;
   --  Compute the result of a foldable expression of either an Integer
   --  or Enumeration type, returning the result as a universal integer.

   function Fold_Uint_Attribute (N : Node_Id) return Uint;
   --  Compute result of a foldable attribute, returning the result as a
   --  universal integer.

   function Is_Foldable_Subtype (Typ : Entity_Id) return Boolean;
   --  Determine if a subtype is foldable

   function Test (Cond : Boolean) return Uint;
   --  This function simply returns the appropriate Boolean'Pos value
   --  corresponding to the value of Cond as a universal integer. It is
   --  used for producing the result of the static evaluation of the
   --  logical operators

   --------------------------
   -- Is_Static_Expression --
   --------------------------

   --  If the expression was previously evaluated the Is_Static flag has
   --  been properly set and just needs to be queried. Otherwise attempt to
   --  evaluate the expression now and check the result of the evaluation.

   function Is_Static_Expression (N : Node_Id) return Boolean is
   begin
      if not Is_Evaluated (N) then
         Static_Evaluation (N);
      end if;

      return Is_Static (N);
   end Is_Static_Expression;

   -----------------------
   -- Is_Static_Subtype --
   -----------------------

   --  A static subtype is either a scalar base type, other than a generic
   --  formal type; or a scalar subtype formed by imposing on a static
   --  subtype either a static range constraint, or a floating or fixed
   --  point constraint whose range constraint, if any, is static. [LRM 4.9]

   function Is_Static_Subtype (Typ : Entity_Id) return Boolean is
      Base_T : Entity_Id := Base_Type (Typ);

   begin
      if Is_Generic_Type (Base_T)
        or else not Is_Scalar_Type (Base_T)
      then
         return False;

      elsif Base_T = Typ then
         return True;

      else
         Static_Evaluation (Type_Low_Bound (Typ));
         Static_Evaluation (Type_High_Bound (Typ));
         return Is_Static_Subtype (Base_T)
           and then Is_Static (Type_Low_Bound (Typ))
           and then Is_Static (Type_High_Bound (Typ));
      end if;
   end Is_Static_Subtype;

   -------------------------
   -- Is_Foldable_Subtype --
   -------------------------

   function Is_Foldable_Subtype (Typ : Entity_Id) return Boolean is
      Base_T : Entity_Id := Base_Type (Typ);

   begin
      if Is_Generic_Type (Base_T) or
         not Is_Scalar_Type (Base_T)
      then
         return False;

      elsif Base_T = Typ then
         return True;

      else
         return Is_Foldable_Subtype (Base_T)
           and then Can_Fold_Expr (Type_Low_Bound (Typ))
           and then Can_Fold_Expr (Type_High_Bound (Typ));
      end if;
   end Is_Foldable_Subtype;

   -----------------------
   -- Static_Evaluation --
   -----------------------

   --  This procedure is called to perform compile time evaluation of an
   --  expression. The Is_Evaluated flag is first checked to see if the
   --  expression was previously evaluated. Note that there are two types
   --  of expressions that can be evaluated at compile time, those that the
   --  Ada definition considers static and the more general category of
   --  expressions that do not need run-time support. For example the
   --  expression (True and then True) can be evaluated at compile time
   --  but is not considered static by Ada 83 since "and then" is not an
   --  operator (Ada 9X considers this static). All expressions that can
   --  be evaluated without run-time support are folded by this procedure.

   --  The result of the evaluation creates only one additional node which
   --  holds the result of the evaluation. In the case where folding has
   --  been sucessful the resulting node is one of the following:
   --  N_Integer_Literal, N_Identifier, N_String_Literal.

   --  Rewrite_Substitute_Tree is used to replace the original expression
   --  with the node containing the result of the evaluation. The original
   --  expression tree can still be retrieved (for purposes of conformance
   --  checking, source recreation, and ASIS compliance) by using the
   --  function Original_Node.

   --  If the expression evaluation shows that the expression would raise
   --  Constraint_Error during run-time (e.g. divide by zero), a special
   --  N_Expression_Actions node is created to raise Constraint_Error
   --  (see Create_Constraint_Error).

   --  Identifiers representing user-defined constants which are static are
   --  not replaced by other nodes but are just marked as being static.

   --  The Is_Static flag on the result node is set to True only if it is
   --  Ada static. So the N_Identifier node created for "True and then True",
   --  (in Ada 83 mode) has Is_Static set to False.

   --  If the Is_Evaluation flag is False, static evaluation is attempted
   --  and the Is_Static flag is then set appropriately.

   --  Evaluation works in a two pass manner. The first pass determines
   --  whether the expression can be folded, while the second pass actually
   --  computes the result. If the root of the tree cannot be folded an
   --  attempt is then made to fold the left and right operands of the
   --  expression and in case of failure their descendents in turn.

   --  For the arithmetic results no overflow checking is done here (i.e.
   --  the exact result is stored as a Uint). Overflow checking will be
   --  performed as part of the expander pass.

   --  An expression of a scalar type is said to be static if and only if
   --  every primary is one of those listed in (a) through (h) below, every
   --  operator denotes a predefined operator, and the evaluation of the
   --  expression delivers a value (that is, it does not raise an exception):
   --  [LRM 4.9]

   procedure Static_Evaluation (N : Node_Id) is
      Def_Id : Entity_Id;

      procedure Debug_Set_Recursion_Flag (B : Boolean) is
      begin
         Recursive_Call_To_Eval := B;
      end Debug_Set_Recursion_Flag;

   --  Start of processing for Static_Evaluation

   begin
      pragma Debug (Debug_A_Entry ("evaluating ", N));

      --  If already evaluated, then the subexpression has already been folded
      --  if folding was possible, and Is_Static (N) is appropriately set.

      if Is_Evaluated (N) then
         pragma Debug
           (Debug_A_Exit ("evaluating ", N, "  (already evaluated)"));
         return;
      end if;

      --  Debugging code to make sure there is no recursive call to this
      --  procedure (which should never occur, because we handle the required
      --  recursion directly in the code).

      pragma Assert (not Recursive_Call_To_Eval);
      pragma Debug (Debug_Set_Recursion_Flag (True));

      --  Different cases of static expressions

      case Nkind (N) is
         when N_Identifier | N_Expanded_Name =>
            Def_Id := Entity (N);

            --  An enumeration literal (including a character literal)

            if Ekind (Def_Id) = E_Enumeration_Literal then
               Set_Is_Static (N, True);

            --  A constant explicitly declared by a constant declaration with
            --  a static subtype, and initialized with a static expression.

            elsif Ekind (Def_Id) = E_Constant and then
               Is_Scalar_Type (Etype (Def_Id)) and then

               --  The expression value given for a constant is evaluated
               --  when the object declaration is processed. So here we
               --  can just test its Is_Static field.

               Is_Static (Constant_Value (Def_Id))

            then
               Set_Is_Static (N, True);

            else
               Set_Is_Static (N, False);
            end if;

         --  A numeric literal

         when N_Integer_Literal =>
            Check_Int_Literal (N);
            Set_Is_Static (N, True);

         when N_Real_Literal =>
            Set_Is_Static (N, True);

         --  A string literal

         when N_String_Literal =>
            Set_Is_Static (N, False);

         --  An enumeration literal (including a character literal)

         when N_Character_Literal =>
            Set_Is_Static (N, True);

         --  A function call whose function name is an operator symbol
         --  that denotes a predefined operator, including a function
         --  name that is an expanded name; each actual parameter must
         --  also be a static expression.

         when N_Op | N_Attribute_Reference =>
            Eval_Expr (N);

         when N_Range =>
            Eval_Expr (Low_Bound (N));
            Eval_Expr (High_Bound (N));

         when others =>
            null;
      end case;

      pragma Debug (Debug_Set_Recursion_Flag (False));
      pragma Debug (Debug_A_Exit ("evaluating ", N, "  (done)"));

      --  Final step is to mark node as evaluated

      Set_Is_Evaluated (N, True);                               
   end Static_Evaluation;

   ---------------
   -- Eval_Expr --
   ---------------

   procedure Eval_Expr (N : Node_Id) is
      Left  : Node_Id;
      Right : Node_Id;

   begin
      if Nkind (N) in N_Entity_Name and then
         Ekind (Entity (N)) = E_Enumeration_Literal then
         Set_Is_Static (N, True);

      elsif Nkind (N) = N_Integer_Literal then
         Set_Is_Static (N, True);
         Check_Int_Literal (N);

      elsif Nkind (N) in N_Unary_Op then
         if Can_Fold_Expr (Right_Opnd (N)) then
            Fold_Expr (N);
         end if;

      elsif Nkind (N) = N_Op_Concat then
         if Can_Fold_Expr (Left_Opnd (N))
           and then Can_Fold_Expr (Right_Opnd (N))
         then
            Fold_Expr (N);
         else
            Error_Msg_N ("dynamic string concatenation not implemented", N);
         end if;

      elsif Nkind (N) in N_Binary_Op then
         Left  := Left_Opnd (N);
         Right := Right_Opnd (N);

         if Can_Fold_Expr (Left) then
            if Can_Fold_Expr (Right) then
               Fold_Expr (N);
            else
               Fold_Expr (Left);
            end if;

         elsif Can_Fold_Expr (Right) then
            Fold_Expr (Right);
         end if;

      elsif Nkind (N) = N_Attribute_Reference then
         if Can_Fold_Expr (N) then
            Fold_Expr (N);
         end if;
      end if;

   end Eval_Expr;

   -------------------
   -- Can_Fold_Expr --
   -------------------

   function Can_Fold_Expr (N : Node_Id) return Boolean is
   begin
      if Is_Generic_Type (Etype (N)) then
         return false;
      end if;

      case Nkind (N) is

         when N_Binary_Op =>
            return Can_Fold_Expr (Left_Opnd (N)) and then
                   Can_Fold_Expr (Right_Opnd (N));

         when N_Unary_Op =>
            return Can_Fold_Expr (Right_Opnd (N));

         when N_Identifier | N_Expanded_Name =>
            declare
               Def_Id : Entity_Id := Entity (N);

            begin
               --  An enumeration literal

               if Ekind (Def_Id) = E_Enumeration_Literal then
                  return True;

               --  A user defined static constant

               elsif Ekind (Def_Id) = E_Constant then
                  return Is_Scalar_Type (Etype (Def_Id)) and then
                         Can_Fold_Expr (Constant_Value (Def_Id));

               --  A static type or subtype

               elsif Is_Type (Def_Id) then
                  return Is_Foldable_Subtype (Def_Id);

               else
                  return False;
               end if;
            end;

         --  An integer literal that was either in the source or created as
         --  a result of static evaluation.

         when N_Integer_Literal =>
            return True;

         --  A string literal that was either in the source or created as a
         --  result of static evaluation or a character literal.

         when N_String_Literal | N_Character_Literal =>
            return True;

         when N_Attribute_Reference =>
            return Can_Fold_Attribute (N);

         when others =>
            return False;

      end case;
   end Can_Fold_Expr;

   ---------------
   -- Fold_Expr --
   ---------------

   procedure Fold_Expr (N : Node_Id) is
      Kind : Node_Kind;
      Lit  : Entity_Id;
      Pos  : Int;
      Val  : Uint;

   begin
      Is_Static_Result := True;
      Kind := Nkind (N);

      if Kind = N_Identifier
        or else Kind = N_Expanded_Name
        or else Kind = N_Character_Literal
      then
         return;

      elsif Kind = N_Integer_Literal then
         Check_Int_Literal (N);
         return;

      --  Attempt to constant fold concatenation between combinations of
      --  string_literal, character_literals and static user defined
      --  constants. One pass (Can_Fold_Expr) determines whether the whole
      --  subtree can be folded and if so, the second pass (Fold_String)
      --  actually allocates an entry in the strings table and enters the
      --  new resulting string. No intermediate nodes are created.

      elsif Kind = N_Op_Concat then
         Start_String;
         Fold_String (N);
         Create_Str_Value_Node (N, End_String);

      elsif Is_Integer_Type (Etype (N)) then

         --  Substitute an N_Integer_Literal node for the result of the
         --  compile time evaluation of an operation on an integer literal or
         --  constant (i.e. results of arithmetic operators or attributes).

         Val := Fold_Uint (N);
         Set_Is_Static (N, Is_Static_Result);
         Rewrite_Int_Literal (N, Val);
         Check_Int_Literal (N);

      elsif Is_Enumeration_Type (Etype (N)) then

         --  Substitute an N_Identifier or N_Character_Literal node for
         --  the result of the compile time evaluation of an operation on
         --  an enumeration literal or constant (i.e. results of logical
         --  operators or attributes).

         --  In the case where the literal is of type Character, there needs
         --  to be some special handling since there is no explicit chain of
         --  literals to search. Instead, a N_Character_Literal node is
         --  created with the appropriate Char_Code and Chars fields.

         if Base_Type (Etype (N)) = Standard_Character then
            Pos := UI_To_Int (Fold_Uint (N));
            Name_Buffer (1) := ''';
            Name_Buffer (2) := Char'Val (Pos);
            Name_Buffer (3) := ''';
            Name_Len := 3;
            Lit := New_Node (N_Character_Literal, Sloc (N));
            Set_Chars (Lit, Name_Find);
            Set_Char_Literal_Value (Lit, Char_Code (Pos));
            Set_Etype (Lit, Etype (N));
            Set_Is_Evaluated (Lit, True);
            Set_Is_Static (Lit, Is_Static_Result);
            Rewrite_Substitute_Tree (N, Lit);

         else
            --  Interate through the literals list until the one in the
            --  desired position in the chain is found. Note that since
            --  this offset is relative to the original enumeration type
            --  start at the first literal of the base type.

            Pos := UI_To_Int (Fold_Uint (N)) - 1;
            Lit := First_Literal (Base_Type (Etype (N)));

            for I in 0 .. Pos loop
               Lit := Next_Literal (Lit);
            end loop;

            Set_Is_Static (N, Is_Static_Result);
            Rewrite_Enum_Literal (N, Lit);
         end if;
      end if;

   exception
      when Compile_Time_Constraint_Error =>

      --  Handled when evaluation of an expression indicates a constraint
      --  violation (e.g. division by zero). A special N_Expression_Actions
      --  node is inserted which consists of a raise statement for
      --  Constraint_Error. A warning message has already been issued at
      --  the point of the expression which violated the constraint.

      Create_Raise_Expression (N, Standard_Entity (S_Constraint_Error));
   end Fold_Expr;

   -----------------------
   -- Check_Int_Literal --
   -----------------------

   procedure Check_Int_Literal (N : Node_Id) is
      Typ : constant Entity_Id := Etype (N);

   begin
      --  For now, only worry about signed types

      if Ekind (Typ) in Integer_Kind
        and then Typ /= Any_Type
        and then Typ /= Universal_Integer
        and then Nkind (Type_Low_Bound (Typ)) = N_Integer_Literal
        and then Nkind (Type_High_Bound (Typ)) = N_Integer_Literal
      then
         if UI_Lt (Intval (N), Intval (Type_Low_Bound (Typ)))
           or else UI_Gt (Intval (N), Intval (Type_High_Bound (Typ)))
         then
            Error_Msg_NE ("?literal out of range of type&", N, Typ);
         end if;
      end if;
   end Check_Int_Literal;

   --------------
   -- Fold_Uint --
   --------------

   function Fold_Uint (N : Node_Id) return Uint is
      Def_Id    : Entity_Id;
      Kind      : Node_Kind;
      Left_Int  : Uint;
      Right_Int : Uint;

   begin
      Kind := Nkind (N);

      if Kind in N_Binary_Op then
         Left_Int := Fold_Uint (Left_Opnd (N));

         if Kind = N_Op_In then
            Def_Id := Entity (Right_Opnd (N));
            return
              Test (UI_Le (Expr_Value (Type_Low_Bound (Def_Id)), Left_Int)
                and then UI_Ge (Expr_Value (Type_High_Bound (Def_Id)),
                                Left_Int));

         elsif Kind = N_Op_Not_In then
            Def_Id := Entity (Right_Opnd (N));

            return
              Test (UI_Gt (Expr_Value (Type_Low_Bound (Def_Id)), Left_Int)
              or else UI_Lt (Expr_Value (Type_High_Bound (Def_Id)), Left_Int));

         else
            Right_Int := Fold_Uint (Right_Opnd (N));
         end if;

      elsif Kind in N_Unary_Op then
         Right_Int := Fold_Uint (Right_Opnd (N));
      end if;

      case Kind is

         --  An integer literal that was either in the source or created as
         --  a result of static evaluation. An enumeration literal that was
         --  either in the source or created as a result of static evaluation.

         when N_Attribute_Reference =>
            return Fold_Uint_Attribute (N);

         when N_Concat_Multiple =>
            Compiler_Abort;

         when N_Identifier        | N_Expanded_Name |
              N_Integer_Literal   | N_Character_Literal =>
            return Expr_Value (N);

         when N_Op_And =>
            return Test (not (UI_Is_Zero (Left_Int) or else
                        UI_Is_Zero (Right_Int)));

         when N_Op_And_Then =>
            Is_Static_Result := False;
            Check_Preevaluate (N);
            return Test (not (UI_Is_Zero (Left_Int) or else
                              UI_Is_Zero (Right_Int)));

         when N_Op_Or =>
            return Test (not (UI_Is_Zero (Left_Int) and then
                              UI_Is_Zero (Right_Int)));

         when N_Op_Or_Else =>
            Is_Static_Result := False;
            Check_Preevaluate (N);
            return Test (not (UI_Is_Zero (Left_Int) and then
                              UI_Is_Zero (Right_Int)));

         when N_Op_Xor =>
            return Test (UI_Eq (Left_Int, Right_Int));

         when N_Op_Eq =>
            return Test (UI_Eq (Left_Int, Right_Int));

         when N_Op_Ne =>
            return Test (UI_Ne (Left_Int, Right_Int));

         when N_Op_Lt =>
            return Test (UI_Lt (Left_Int, Right_Int));

         when N_Op_Le =>
            return Test (UI_Le (Left_Int, Right_Int));

         when N_Op_Gt =>
            return Test (UI_Gt (Left_Int, Right_Int));

         when N_Op_Ge =>
            return Test (UI_Ge (Left_Int, Right_Int));

         when N_Op_Add =>
            return UI_Sum (Left_Int, Right_Int);

         when N_Op_Subtract =>
            return UI_Difference (Left_Int, Right_Int);

         --  Concatenation is already handled separately by Fold_String
         --  so it would be an internal error if we reached this point.

         when N_Op_Concat =>
            Compiler_Abort;

         when N_Op_Multiply =>
            return UI_Product (Left_Int, Right_Int);

         when N_Op_Divide =>

            --  The exception Constraint_Error is raised by integer division,
            --  rem and mod if the right operand is zero.

            if UI_Is_Zero (Right_Int) then
               Raise_Warning (N, Standard_Entity (S_Constraint_Error),
                              "division by zero?!");
               raise Compile_Time_Constraint_Error;
            else
               return UI_Quotient (Left_Int, Right_Int);
            end if;

         when N_Op_Mod =>

            --  The exception Constraint_Error is raised by integer division,
            --  rem and mod if the right operand is zero. [LRM 4.5.4]

            if UI_Is_Zero (Right_Int) then
               Raise_Warning (N, Standard_Entity (S_Constraint_Error),
                              "mod with zero right operand?!");
               raise Compile_Time_Constraint_Error;
            else
               return UI_Mod (Left_Int, Right_Int);
            end if;

         when N_Op_Rem =>

            --  The exception Constraint_Error is raised by integer division,
            --  rem and mod if the right operand is zero. [LRM 4.5.4]

            if UI_Is_Zero (Right_Int) then
               Raise_Warning (N, Standard_Entity (S_Constraint_Error),
                              "rem with zero right operand?!");
               raise Compile_Time_Constraint_Error;

            else
               return UI_Rem (Left_Int, Right_Int);
            end if;

         when N_Op_Expon =>

            --  Exponentiation of an integer raises the exception
            --  Constraint_Error for a negative exponent. [LRM 4.5.6]

            if UI_Is_Negative (Right_Int) then
               Raise_Warning (N, Standard_Entity (S_Constraint_Error),
                              "negative exponent for integer base?!");
               raise Compile_Time_Constraint_Error;

            else
               return UI_Exponentiate (Left_Int, Right_Int);
            end if;

         when N_Op_Plus =>

            --  The "+" can be ignored if it is used as a unary operator

            return Right_Int;

         when N_Op_Minus =>
            return UI_Negate (Right_Int);

         when N_Op_Abs =>
            return UI_Abs (Right_Int);

         when N_Op_Not =>
            return Test (not UI_Is_Zero (Right_Int));

         when N_Parenthesized_Expression  =>
            return Right_Int;

         when others =>
            Compiler_Abort;

      end case;

   end Fold_Uint;

   -----------------
   -- Fold_String --
   -----------------

   procedure Fold_String (N : Node_Id) is
      Operation : Node_Kind := Nkind (N);

   begin
      if Operation = N_Op_Concat then
         Fold_String (Left_Opnd (N));
         Fold_String (Right_Opnd (N));

      elsif Operation = N_String_Literal then
         for I in 1 .. String_Length (Strval (N)) loop
            Store_String_Char (Get_String_Char (Strval (N), I));
         end loop;

      elsif Operation = N_Character_Literal then
         Store_String_Char (Char_Literal_Value (N));

      --  User defined constants that are static appear here in the category
      --  of N_Entity_Name. Fold_String is only called if all the arguments
      --  of the concatenation are static.

      elsif Operation in N_Entity_Name then
         Store_String_Char (Char_Code (UI_To_Int (Expr_Value (N))));

      else
         null;
      end if;
   end Fold_String;

   ------------------------
   -- Can_Fold_Attribute --
   ------------------------

   function Can_Fold_Attribute (N : Node_Id) return Boolean is
      Id   : Attribute_Id := Get_Attribute_Id (Chars (Identifier (N)));
      P    : Node_Id := Prefix (N);
      E    : Node_Id := Expression (N);
      Typ  : Entity_Id;

   begin
      case Id is
         when Attribute_First |
              Attribute_Last  |
              Attribute_Pos   |
              Attribute_Pred  |
              Attribute_Size  |
              Attribute_Succ  |
              Attribute_Val
                             => null;

         when others => return False;
      end case;

      if Etype (P) = Any_Type or else
         (Present (E) and then Etype (E) = Any_Type)
      then
         return False;
      end if;

      if Nkind (P) in N_Entity_Name then
         Typ := Entity (P);

      --  The attribute 'BASE must be treated specially.

      elsif Nkind (P) = N_Attribute_Reference
        and then Attribute_Base = Get_Attribute_Id (Chars (Identifier (P)))
      then
         Typ := Etype (P);

      else
         return False;
      end if;

      if not Is_Type (Typ) or else not Is_Foldable_Subtype (Typ) then
         return False;
      elsif Present (E) and then not Can_Fold_Expr (E) then
         return False;
      else
         return True;
      end if;

   end Can_Fold_Attribute;

   -------------------------
   -- Fold_Uint_Attribute --
   -------------------------

   function Fold_Uint_Attribute (N : Node_Id) return Uint is
      Id        : Attribute_Id := Get_Attribute_Id (Chars (Identifier (N)));
      P         : Node_Id := Prefix (N);
      E         : Node_Id := Expression (N);
      Expr_Uint : Uint;
      Typ       : Entity_Id;

   begin
      if Nkind (P) in N_Entity_Name then
         Typ := Entity (P);

      --  The attribute 'BASE must be treated specially.

      elsif (Nkind (P) = N_Attribute_Reference
        and then Attribute_Base = Get_Attribute_Id (Chars (Identifier (P))))
      then
         Typ := Etype (P);
      end if;

      if Present (E) then
         Expr_Uint := Fold_Uint (E);
      end if;

      --  Remaining processing depends on particular attribute

      case Id is

         --  Processing for 'First attribute. For any scalar type T or for
         --  any subtype T of a scalar type, T'First yields the upper bound of
         --  T. The value of this attribute has the same type as T. [LRM 3.5]

         when Attribute_First =>
            return Expr_Value (Type_Low_Bound (Typ));

         --  Processing for 'Last attribute. For any scalar type T or for
         --  any subtype T of a scalar type, T'First yields the upper bound of
         --  T. The value of this attribute has the same type as T. [LRM 3.5]

         when Attribute_Last =>
            return Expr_Value (Type_High_Bound (Typ));

         --  Pos attribute. The result is the position number of the value
         --  of the parameter. [LRM 3.5.5]

         when Attribute_Pos =>
            return Expr_Uint;

         --  For T'PRED (X) The parameter X must be a value of the base type
         --  of T. The result type is the base type of T. The result is the
         --  value whose position number is one less than that of X. The
         --  exception CONSTRAINT_ERROR is raised if X equals T'BASE'FIRST.

         when Attribute_Pred =>
            if UI_Eq (Expr_Uint,
                      Expr_Value (Type_Low_Bound (Base_Type (Typ))))
            then
               Raise_Warning (N, Standard_Entity (S_Constraint_Error),
                              "Pred of type'First?!");
               raise Compile_Time_Constraint_Error;

            else
               return UI_Difference (Expr_Uint, Uint_1);
            end if;

         --  For T'SUCC (X) The parameter X must be a value of the base type
         --  of T. The result type is the base type of T. The result is the
         --  value whose position number is one greater than that of X. The
         --  exception CONSTRAINT_ERROR is raised if X equals T'BASE'LAST.

         when Attribute_Succ =>
            if UI_Eq (Expr_Uint,
                      Expr_Value (Type_High_Bound (Base_Type (Typ)))) then
               Raise_Warning (N, Standard_Entity (S_Constraint_Error),
                              "Succ of type'Last?!");
               raise Compile_Time_Constraint_Error;
            else
               return UI_Sum (Expr_Uint, Uint_1);
            end if;


         --  For T'VAL (X) This attribute is a special function with a single
         --  parameter which  can be of any integer type. The result type is
         --  the base type of T. The result is the value whose position number
         --  is the universal_integer value corresponding to X. The exception
         --  CONSTRAINT_ERROR is raised if the universal_integer value
         --  corresponding to X is not in the range T'POS(T'BASE'FIRST)
         --  ..  T'POS(T'BASE'LAST).

         when Attribute_Val =>
            if UI_Lt (Expr_Uint,
                      Expr_Value (Type_Low_Bound (Base_Type (Typ)))) or else
               UI_Gt (Expr_Uint,
                      Expr_Value (Type_High_Bound (Base_Type (Typ))))
            then
               Raise_Warning (N, Standard_Entity (S_Constraint_Error),
                              "pos out of range?!");
               raise Compile_Time_Constraint_Error;
            else
               return Expr_Uint;
            end if;

         --  Size attribute just returns the size (note that we could only
         --  be called for size applied to an appropriate type to yield
         --  a static type, see Can_Fold_Attribute function)

         when Attribute_Size =>
            return Esize (Typ);

         --  Remaining attributes cannot be folded

         when Attribute_Access                       => null;
         when Attribute_Address                      => null;
         when Attribute_Adjacent                     => null;
         when Attribute_Aft                          => null;
         when Attribute_Alignment                    => null;
         when Attribute_Base                         => null;
         when Attribute_Bit_Order                    => null;
         when Attribute_Body_Version                 => null;
         when Attribute_Callable                     => null;
         when Attribute_Caller                       => null;
         when Attribute_Ceiling                      => null;
         when Attribute_Class                        => null;
         when Attribute_Component_Size               => null;
         when Attribute_Compose                      => null;
         when Attribute_Constrained                  => null;
         when Attribute_Copy_Sign                    => null;
         when Attribute_Count                        => null;
         when Attribute_Delta                        => null;
         when Attribute_Denorm                       => null;
         when Attribute_Digits                       => null;
         when Attribute_Emax                         => null;
         when Attribute_Epsilon                      => null;
         when Attribute_Exponent                     => null;
         when Attribute_External_Tag                 => null;
         when Attribute_First_Bit                    => null;
         when Attribute_Floor                        => null;
         when Attribute_Fore                         => null;
         when Attribute_Fraction                     => null;
         when Attribute_Identity                     => null;
         when Attribute_Image                        => null;
         when Attribute_Input                        => null;
         when Attribute_Large                        => null;
         when Attribute_Last_Bit                     => null;
         when Attribute_Leading_Part                 => null;
         when Attribute_Length                       => null;
         when Attribute_Machine                      => null;
         when Attribute_Machine_Emax                 => null;
         when Attribute_Machine_Emin                 => null;
         when Attribute_Machine_Mantissa             => null;
         when Attribute_Machine_Overflows            => null;
         when Attribute_Machine_Radix                => null;
         when Attribute_Machine_Rounds               => null;
         when Attribute_Mantissa                     => null;
         when Attribute_Max                          => null;
         when Attribute_Max_Size_In_Storage_Elements => null;
         when Attribute_Min                          => null;
         when Attribute_Model                        => null;
         when Attribute_Model_Emax                   => null;
         when Attribute_Model_Emin                   => null;
         when Attribute_Model_Epsilon                => null;
         when Attribute_Model_Large                  => null;
         when Attribute_Model_Mantissa               => null;
         when Attribute_Model_Small                  => null;
         when Attribute_Output                       => null;
         when Attribute_Position                     => null;
         when Attribute_Range                        => null;
         when Attribute_Read                         => null;
         when Attribute_Remainder                    => null;
         when Attribute_Round                        => null;
         when Attribute_Rounding                     => null;
         when Attribute_Safe_Emax                    => null;
         when Attribute_Safe_First                   => null;
         when Attribute_Safe_Large                   => null;
         when Attribute_Safe_Last                    => null;
         when Attribute_Safe_Small                   => null;
         when Attribute_Scale                        => null;
         when Attribute_Signed_Zeros                 => null;
         when Attribute_Small                        => null;
         when Attribute_Standard_Access              => null;
         when Attribute_Storage_Pool                 => null;
         when Attribute_Storage_Size                 => null;
         when Attribute_Tag                          => null;
         when Attribute_Terminated                   => null;
         when Attribute_Truncation                   => null;
         when Attribute_Unbiased_Rounding            => null;
         when Attribute_Unchecked_Access             => null;
         when Attribute_Valid                        => null;
         when Attribute_Value                        => null;
         when Attribute_Version                      => null;
         when Attribute_Wide_Image                   => null;
         when Attribute_Wide_Value                   => null;
         when Attribute_Width                        => null;
         when Attribute_Write                        => null;
      end case;
   end Fold_Uint_Attribute;

   ---------------------------
   -- Create_Str_Value_Node --
   ---------------------------

   procedure Create_Str_Value_Node (N : Node_Id; Str_Id : String_Id) is
      Str_Literal_Node : Node_Id;
      Subtype_Id : Entity_Id;

   begin
      Str_Literal_Node := New_Node (N_String_Literal, Sloc (N));
      Set_Strval (Str_Literal_Node, Str_Id);
      Set_Is_Evaluated (Str_Literal_Node, True);
      Set_Is_Static (Str_Literal_Node, False);

      --  Create a special subtype for the N_String_Literal node which
      --  becomes its Etype.

      Subtype_Id := 
        New_Implicit_Type (Sloc (N), Scope_Id => Scope (Etype (N)));
      Set_Ekind (Subtype_Id, E_String_Literal_Subtype);
      Set_Etype (Subtype_Id, Etype (N));
      Set_Component_Type (Subtype_Id, Component_Type (Etype (N)));
      Set_String_Literal_Length 
        (Subtype_Id, UI_From_Int (String_Length (Str_Id)));
      Set_Etype (Str_Literal_Node, Subtype_Id);
      Rewrite_Substitute_Tree (N, Str_Literal_Node);
   end Create_Str_Value_Node;

   -----------------------
   -- Check_Preevaluate --
   -----------------------

   procedure Check_Preevaluate (N : Node_Id) is
   begin
      if Pragma_Preelaborable_Enabled then
         Error_Msg_N ("Evaluation of this expression is not preevaluable", N);
      end if;
   end Check_Preevaluate;

   ----------
   -- Test --
   ----------

   function Test (Cond : Boolean) return Uint is
   begin
      if Cond then
         return Uint_1;
      else
         return Uint_0;
      end if;
   end Test;

   ----------------
   -- Expr_Value --
   ----------------

   function Expr_Value (N : Node_Id) return Uint is
      Def_Id : Entity_Id;
      Kind   : Node_Kind;

   begin
      Kind := Nkind (N);

      if Kind = N_Identifier or else Kind = N_Expanded_Name then
         Def_Id := Entity (N);

         --  An enumeration literal that was either in the source or
         --  created as a result of static evaluation.

         if Ekind (Def_Id) = E_Enumeration_Literal then
            return Enumeration_Pos (Def_Id);

         --  A user defined static constant

         elsif Ekind (Def_Id) = E_Constant then
            return Expr_Value (Constant_Value (Def_Id));
         end if;

      --  An integer literal that was either in the source or created as
      --  a result of static evaluation.

      elsif Kind = N_Integer_Literal then
         return Intval (N);

      elsif Kind = N_Character_Literal then

         --  Since Character literals of type Standard.Character don't
         --  have any defining character literals built for them, they
         --  do not have their Entity set, so just use their Char
         --  code. Otherwise for user-defined character literals use
         --  their Pos value as usual.

         if No (Entity (N)) then
            return UI_From_Int (Int (Char_Literal_Value (N)));
         else
            return Enumeration_Pos (Entity (N));
         end if;

      else
         Compiler_Abort;
      end if;

   end Expr_Value;

end Sem_Eval;
