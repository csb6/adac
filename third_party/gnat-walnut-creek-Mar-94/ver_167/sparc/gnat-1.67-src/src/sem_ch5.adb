------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 5                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.92 $                             --
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
with Comperr;  use Comperr;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Disp; use Sem_Disp;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sem_Type; use Sem_Type;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Uintp;    use Uintp;

package body Sem_Ch5 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Elsif_Parts      (L : List_Id);
   procedure Analyze_Iteration_Scheme (N : Node_Id);

   ------------------------
   -- Analyze_Statements --
   ------------------------

   procedure Analyze_Statements (L : List_Id) is
      S : Node_Id;

   begin
      --  The labels declared in the statement list are reachable from
      --  statements in the list.

      S := First (L);

      while Present (S) loop
         if Nkind (S) = N_Label then
            Find_Name (Identifier (S));

            if Ekind (Entity (Identifier (S))) /= E_Label then
               Error_Msg_N
                 ("label& conflicts with outer declaration ", Identifier (S));
            else
               Set_Reachable (Entity (Identifier (S)));
            end if;
         end if;

         S := Next (S);

         --  Here is where we reset the data structures for overload resolution
         --  since no overload data is required to be kept between statements.
         --  Note that an expression actions node analyzes the actions as a
         --  sequence of declarations, not as a sequence of statements, so
         --  there is no problem in that case.

         Init_Interp;
      end loop;

      --  Perform semantic analysis on all statements

      S := First (L);

      while Present (S) loop
         if Nkind (S) /= N_Label then
            Analyze (S);

            --  Insert all the N_Implicit_Type nodes representing implicit
            --  defining ids generated for the statement just analyzed (S).
            --  Statements which may generate implicit types include the loop
            --  statement and allocators. (See comments in New_Implicit_Type
            --  for details.)

            if Is_Non_Empty_List (Implicit_Type_List) then
               Insert_List_Before (S, Implicit_Type_List);
               Implicit_Type_List := New_List;
            end if;
         end if;

         S := Next (S);
      end loop;

      --  Make labels unreachable. Visibility is not sufficient, because
      --  labels in one if-branch for example are not reachable from the
      --  other branch, even though their declarations are in the enclosing
      --  declarative part.

      S := First (L);

      while Present (S) loop
         if Nkind (S) = N_Label then
            Set_Reachable (Entity (Identifier (S)), False);
         end if;
         S := Next (S);
      end loop;
   end Analyze_Statements;

   ------------------------
   -- Analyze_Assignment --
   ------------------------

   --  For now, no overloaded left-hand side

   procedure Analyze_Assignment (N : Node_Id) is
      Lhs : constant Node_Id := Name (N);
      Rhs : constant Node_Id := Expression (N);
      T1, T2 : Entity_Id;

   begin
      Analyze (Lhs);
      T1 := Etype (Lhs);

      --  In the most general case, both Lhs and Rhs can be overloaded, and we
      --  must compute the intersection of the possible types on each side.
      --  (TBSL).

      if Is_Overloaded (Lhs) then
         Unimplemented (N, "overloaded lhs in assignment");
      end if;

      Analyze (Rhs);
      Resolve_Subexpr (Lhs, T1);
      Resolve_Complete_Context (Rhs, T1);
      T2 := Etype (Rhs);

      if Covers (T1, T2) then
         null;
      else
         Error_Msg_Name_2 := Chars (Etype (Rhs));
         Error_Msg_NE
           ("incompatible types on assignment:& :=&", N, Etype (Lhs));
      end if;

      if T1 = Any_Type or else T2 = Any_Type then
         return;
      end if;

      if not Is_Variable (Lhs)
        and then not Assignment_OK (N)
      then
         Error_Msg_N
           ("left hand side of assignment must be a variable", Lhs);

      elsif Is_Limited_Type (T1)
        and then not Assignment_OK (N)
      then
         Error_Msg_N
           ("left hand of assignment must not be limited type", Lhs);
      end if;

      if Is_Class_Type (T1) and then Is_Tag_Indeterminate (Rhs) then
         Propagate_Tag (Lhs, Rhs);
      end if;

      Apply_Range_Check (Rhs, Etype (Lhs));
   end Analyze_Assignment;

   --------------------------
   -- Analyze_If_Statement --
   --------------------------

   procedure Analyze_If_Statement (N : Node_Id) is
   begin
      Analyze (Condition (N));
      Resolve_Complete_Context (Condition (N), Standard_Boolean);

      --  insert Implicit types generated by the condition before the 
      --  If statement 

      if Is_Non_Empty_List (Implicit_Type_List) then
         Insert_List_Before (N, Implicit_Type_List);
         Implicit_Type_List := New_List;
      end if;

      Analyze_Statements (Then_Statements (N));

      if List_Present (Elsif_Parts (N)) then
         Analyze_Elsif_Parts (Elsif_Parts (N));
      end if;

      if List_Present (Else_Statements (N)) then
         Analyze_Statements (Else_Statements (N));
      end if;
   end Analyze_If_Statement;

   -------------------------
   -- Analyze_Elsif_Parts --
   -------------------------

   procedure Analyze_Elsif_Parts (L : List_Id) is
      E : Node_Id := First (L);
      N : Node_Id := List_Parent (L);

   begin
      while Present (E) loop
         Analyze (Condition (E));
         Resolve_Complete_Context (Condition (E), Standard_Boolean);

         --  Insert Implicit types generated by the condition before the
         --  if statement

         if Is_Non_Empty_List (Implicit_Type_List) then
            Insert_List_Before (N, Implicit_Type_List);
            Implicit_Type_List := New_List;
         end if;

         Analyze_Statements (Then_Statements (E));
         E := Next (E);
      end loop;
   end Analyze_Elsif_Parts;

   ----------------------------
   -- Analyze_Case_Statement --
   ----------------------------

   procedure Analyze_Case_Statement (N : Node_Id) is
      Alt            : Node_Id;
      Case_Table     : Case_Table_Type (1 .. Number_Of_Case_Choices (N));
      Choice         : Node_Id;
      Choice_Count   : Nat := 0;
      E              : Entity_Id;
      Exp            : Node_Id;
      Exp_Base_Type  : Entity_Id;
      Exp_Type       : Entity_Id;
      Hi             : Node_Id;
      Invalid_Case   : Boolean := False;
      Kind           : Node_Kind;
      Lo             : Node_Id;
      Others_Present : Boolean := False;

      procedure Check_Choice (Lo, Hi : Node_Id; Position : Node_Id);
      --  Check_Choice checks whether the given bounds of a choice are static.
      --  If not a message is issued, otherwise the bounds are entered into
      --  the case table. The simple expressions and discrete ranges given as
      --  choices in a case statement must be static. [LRM 5.4]

      procedure Check_Choice (Lo, Hi : Node_Id; Position : Node_Id) is
      begin
         if not
           (Is_Static_Expression (Lo) and then Is_Static_Expression (Hi))
         then
            Error_Msg_N
              ("choice given in case statement is not static", Position);
            Invalid_Case := True;

         else
            Choice_Count := Choice_Count + 1;
            Case_Table (Choice_Count).Choice_Lo := Lo;
            Case_Table (Choice_Count).Choice_Hi := Hi;
            Case_Table (Choice_Count).Choice_Node := Position;
         end if;
      end Check_Choice;

   --  Start of processing for Analyze_Case_Statement

   begin

      --  Check that the case expression is of a discrete type and that its
      --  range is static, and find the length of the range.

      Exp := Expression (N);
      Analyze (Exp);
      Exp_Type := Etype (Exp);
      Resolve_Subexpr (Exp, Exp_Type);

      --  Insert Implicit types generated by the condition before the
      --  case statement 

      if Is_Non_Empty_List (Implicit_Type_List) then
         Insert_List_Before (N, Implicit_Type_List);
         Implicit_Type_List := New_List;
      end if;

      if Exp_Type = Universal_Integer then        -- literal expression
         Exp_Base_Type := Standard_Integer;       -- force INTEGER
      else
         Exp_Base_Type := Base_Type (Exp_Type);
      end if;

      --  The expression must be of a discrete type which must be determinable
      --  independently of the context in which the expression occurs, but
      --  using the fact that the expression must be of a discrete type.
      --  Moreover, the type this expression must not be a generic formal type.

      if not Is_Discrete_Type (Exp_Base_Type) then
         Error_Msg_N ("case expression not of discrete type", Exp);
         return;

      elsif Is_Generic_Type (Exp_Base_Type) then
         Error_Msg_N ("case expression cannot be of a generic type", Exp);
         return;
      end if;

      --  The simple expressions and discrete ranges given as choices in a case
      --  statement must be static. [LRM 5.4]

      --  Now check each of the case choices against exp_base_type,

      Alt := First (Alternatives (N));

      while Present (Alt) loop
         Choice := First (Discrete_Choices (Alt));

         while Present (Choice) loop

            --  Type check the choice and ensure that it is static,
            --  that it is in the range for the expression subtype, and
            --  that it appears no more than once as a value possibility.

            Analyze (Choice);
            Kind := Nkind (Choice);

            if Kind = N_Range then
               Resolve_Complete_Context (Choice, Exp_Base_Type);
               Check_Choice (Low_Bound (Choice), High_Bound (Choice), Choice);

            elsif (Kind = N_Identifier or else Kind = N_Selected_Component)
              and then Is_Type (Entity (Choice))
            then
               E := Entity (Choice);
               Lo := Type_Low_Bound (E);
               Hi := Type_High_Bound (E);
               Check_Choice (Lo, Hi, Choice);

            elsif Kind = N_Subtype_Indication then
               Compiler_Abort;        -- for now

            --  The choice others is only allowed for the last alternative and
            --  as its only choice; it stands for all values (possibly none)
            --  not given in the choices of previous statement alternatives.

            elsif Kind = N_Others_Choice then
               if not (Choice = First (Discrete_Choices (Alt))
                        and then Choice = Last (Discrete_Choices (Alt))
                        and then Alt = Last (Alternatives (N)))
               then
                  Error_Msg_N
                    ("the choice OTHERS must appear alone and last", Choice);
                  return;
               end if;

               Others_Present := True;

            --  Only other possibility is an expression

            else
               Resolve_Complete_Context (Choice, Exp_Base_Type);
               Check_Choice (Choice, Choice, Choice);
            end if;

            Choice := Next (Choice);
         end loop;

         Analyze_Statements (Statements (Alt));
         Alt := Next (Alt);
      end loop;

      if not Invalid_Case and then Case_Table'Length > 0 then
         Check_Case_Choices (Case_Table, N, Exp_Type, Others_Present);
      end if;

   end Analyze_Case_Statement;

   -----------------------------
   -- Number_Of_Case_Choices  --
   -----------------------------

   function Number_Of_Case_Choices (N : Node_Id) return Nat is
      Alt_or_Var : Node_Id;
      Choice     : Node_Id;
      Count      : Nat := 0;

   begin
      --  The iteration uses different access functions depending on whether
      --  it is processing a case statement or a variant part here.

      if Nkind (N) = N_Case_Statement then
         Alt_or_Var := First (Alternatives (N));

      else -- N_Variant_Part
         Alt_or_Var := First (Variants (N));
      end if;

      while Present (Alt_or_Var) loop
         Choice := First (Discrete_Choices (Alt_or_Var));

         while Present (Choice) loop
            if Nkind (Choice) /= N_Others_Choice then
               Count := Count + 1;
            end if;

            Choice := Next (Choice);
         end loop;

         Alt_or_Var := Next (Alt_or_Var);
      end loop;

      return Count;
   end Number_Of_Case_Choices;

   ---------------------
   -- Sort_Case_Table --
   ---------------------

   procedure Sort_Case_Table (Case_Table : in out Case_Table_Type) is

      L : Int := Case_Table'First;
      U : Int := Case_Table'Last;
      I : Int;
      J : Int;
      T : Case_Bounds;

   begin
      I := L;

      while I /= U loop
         T := Case_Table (I + 1);
         J := I + 1;

         while J /= L
           and then UI_Gt (Expr_Value (Case_Table (J - 1).Choice_Lo),
                           Expr_Value (T.Choice_Lo))
         loop
            Case_Table (J) := Case_Table (J - 1);
            J := J - 1;
         end loop;

         Case_Table (J) := T;
         I := I + 1;
      end loop;
   end Sort_Case_Table;

   ------------------------
   -- Check_Case_Choices --
   ------------------------

   procedure Check_Case_Choices (Case_Table     : in out Case_Table_Type;
                                 N              : Node_Id;
                                 Choice_Type    : Entity_Id;
                                 Others_Present : Boolean) is
      Choice      : Node_Id;
      First_Msg   : Boolean := True;
      Exp_Lo      : Node_Id;
      Exp_Hi      : Node_Id;
      Hi          : Uint;
      Lo          : Uint;
      Msg_Sloc    : Source_Ptr;
      Previous_Hi : Uint;

      function Image_Of (Value : Uint) return Node_Id;
      --  Returns the Node_Id for the enumeration literal corresponding to the
      --  position given by Value, within the enumeration type Choice_Type.

      procedure Issue_Msg (Value1, Value2 : Uint);
      --  Issue an error message indicating that there are missing choices,
      --  followed by the image of the missing choices themselves which lie
      --  between Value1 and Value2 exclusive. If there is more than one
      --  choice missing print the first and last of the range. Since this
      --  can be called several times for the same case statement or variant
      --  part, make sure to print the error message itself only once per
      --  case statement or variant part.

      --------------
      -- Image_Of --
      --------------

      function Image_Of (Value : Uint) return Node_Id is
         Lit : Entity_Id;

      begin
         --  Iterate through the literals list "Value" number of times
         --  until the desired literal is reached and return its Name_Id.
         --  Note: since Value is an offset given in terms of the original
         --  enumeration type use the base type.

         Lit := First_Literal (Base_Type (Choice_Type));

         for I in 1 .. UI_To_Int (Value) loop
            Lit := Next_Literal (Lit);
         end loop;

         return Lit;
      end Image_Of;

      ---------------
      -- Issue_Msg --
      ---------------

      procedure Issue_Msg (Value1, Value2 : Uint) is
      begin
         if First_Msg then
            Msg_Sloc := Sloc (N);
            First_Msg := False;
         end if;

         if UI_Eq (UI_Difference (Value2, Value1), Uint_1) then

            --  There is only one choice value that is missing between
            --  Value1 and Value2, so print only that value.

            if Is_Integer_Type (Choice_Type) then
               Error_Msg_Uint_1 := UI_Sum (Value1, Uint_1);
               Error_Msg ("missing case value: ^!", Msg_Sloc);

            elsif Base_Type (Choice_Type) = Standard_Character
              or else Root_Type (Choice_Type) = Standard_Character
            then
               Error_Msg_Uint_1 := UI_Sum (Value1, Uint_1);
               Error_Msg ("missing case value: Character''Pos (^)!", Msg_Sloc);
            else
               Error_Msg_Node_1 := Image_Of (UI_Sum (Value1, Uint_1));
               Error_Msg ("missing case value: &!", Msg_Sloc);
            end if;

         else
            --  There is more that one choice value that is missing between
            --  Value1 and Value2, so print the range of values between them.

            if Is_Integer_Type (Choice_Type) then
               Error_Msg_Uint_1 := UI_Sum (Value1, Uint_1);
               Error_Msg_Uint_2 := UI_Difference (Value2, Uint_1);
               Error_Msg ("missing case values: ^ .. ^!", Msg_Sloc);

            elsif Base_Type (Choice_Type) = Standard_Character
              or else Root_Type (Choice_Type) = Standard_Character
            then
               Error_Msg_Uint_1 := UI_Sum (Value1, Uint_1);
               Error_Msg_Uint_2 := UI_Difference (Value2, Uint_1);
               Error_Msg ("missing case values: Character''Pos (^) .. " &
                          "Character''Pos (^)!", Msg_Sloc);
            else
               Error_Msg_Node_1 := Image_Of (UI_Sum (Value1, Uint_1));
               Error_Msg_Node_2 := Image_Of (UI_Difference (Value2, Uint_1));
               Error_Msg ("missing case values: & .. &!", Msg_Sloc);
            end if;
         end if;
      end Issue_Msg;

   --  Start processing for Check_Case_Choices

   begin
      Sort_Case_Table (Case_Table);

      --  If the subtype of the discriminant is static, then each value of this
      --  subtype must be represented once and only once in the set of choices
      --  of the variant part, and no other value is allowed. Otherwise, each
      --  value of the (base) type of the discriminant must be represented once
      --  and only once in the set of choices. [LRM 3.7.3]

      --  If the expression is the name of an object whose subtype is static,
      --  then each value of this subtype must be represented once and only
      --  once in the set of choices of the case statement, and no other value
      --  is allowed. Otherwise, for other forms of expression, each value
      --  of the (base) type of the expression must be represented once and
      --  only once in the set of choices, and no other value is allowed.

      if Is_Static_Subtype (Choice_Type) then
         Exp_Lo := Type_Low_Bound (Choice_Type);
         Exp_Hi := Type_High_Bound (Choice_Type);
      else
         Exp_Lo := Type_Low_Bound (Base_Type (Choice_Type));
         Exp_Hi := Type_High_Bound (Base_Type (Choice_Type));
      end if;

      Lo := Expr_Value (Case_Table (Case_Table'First).Choice_Lo);
      Hi := Expr_Value (Case_Table (Case_Table'First).Choice_Hi);
      Previous_Hi := Expr_Value (Case_Table (Case_Table'First).Choice_Hi);

      if not Others_Present and then UI_Lt (Expr_Value (Exp_Lo), Lo)  then
         Issue_Msg (Expr_Value (Exp_Lo), Lo);
      end if;

      for I in Case_Table'First + 1 .. Case_Table'Last loop
         Lo := Expr_Value (Case_Table (I).Choice_Lo);
         Hi := Expr_Value (Case_Table (I).Choice_Hi);
         Choice := Case_Table (I).Choice_Node;

         if UI_Le (Lo, Previous_Hi) then
            Error_Msg_Sloc_1 := Sloc (Case_Table (I - 1).Choice_Node);
            Error_Msg_N
              ("choice value duplicates that given on line #", Choice);

         elsif not Others_Present
           and then UI_Ne (Lo, UI_Sum (Previous_Hi, Uint_1))
         then
            Issue_Msg (Previous_Hi, Lo);
         end if;

         Previous_Hi := Hi;
      end loop;

      if not Others_Present and then UI_Gt (Expr_Value (Exp_Hi), Hi) then
         Issue_Msg (Hi, UI_Sum (Expr_Value (Exp_Hi), Uint_1));
      end if;
   end Check_Case_Choices;

   -----------------
   -- Is_Variable --
   -----------------

   function Is_Variable (N : Node_Id) return Boolean is

      function Is_Variable_Prefix (N : Node_Id) return Boolean;
      --   Prefixes can involve implicit dereferences

      function Is_Variable_Prefix (N : Node_Id) return Boolean is
      begin
         return Is_Variable (N) or else Is_Access_Type (Etype (N));
      end Is_Variable_Prefix;

   begin
      if Is_Name (N) then
         declare
            K : Entity_Kind := Ekind (Entity (N));

         begin
            return K = E_Variable
              or else  K = E_Component
              or else  K = E_Out_Parameter
              or else  K = E_In_Out_Parameter
              or else  K = E_Generic_In_Out_Parameter;
         end;

      else
         case Nkind (N) is
            when N_Indexed_Component | N_Slice =>
               return Is_Variable_Prefix (Prefix (N));

            when N_Selected_Component =>
               return Is_Variable_Prefix (Prefix (N))
               and then Is_Variable (Selector_Name (N));

            when N_Expanded_Name =>
               return Is_Variable (Selector_Name (N));

            when N_Explicit_Dereference =>

               --  Still need to check whether the type is access constant

               return Is_Name (Prefix (N))
                 or else Is_Variable (Prefix (N))
                 or else Is_Access_Type (Etype (Prefix (N)))
                 or else (Nkind (Prefix (N)) = N_Function_Call
                            and then Is_Access_Type (Etype
                                         (Entity (Name (Prefix (N))))));

            when N_Type_Conversion =>
               return Is_Variable (Expression (N));

            when others =>  return False;
         end case;
      end if;
   end Is_Variable;

   ------------------------------
   -- Analyze_Iteration_Scheme --
   ------------------------------

   procedure Analyze_Iteration_Scheme (N : Node_Id) is
   begin

      --  For an infinite loop, there is no iteration scheme

      if No (N) then
         return;
      end if;

      --  For WHILE loop, verify that the condition is a Boolean expression

      if Present (Condition (N)) then
         Analyze (Condition (N));
         Resolve_Complete_Context (Condition (N), Any_Boolean);

      --  Else we have a FOR loop

      else
         declare
            L : constant Node_Id := Loop_Parameter_Specification (N);
            I : constant Node_Id := Defining_Identifier (L);
            D : constant Node_Id := Discrete_Subtype_Definition (L);

         begin
            Analyze (D);
            Make_Index (D);
            Enter_Name (I);
            Set_Ekind (I, E_Loop_Parameter);
            Set_Etype (I, Etype (D));
         end;
      end if;
   end Analyze_Iteration_Scheme;

   ----------------------------
   -- Analyze_Loop_Statement --
   ----------------------------

   procedure Analyze_Loop_Statement (N : Node_Id) is
      Id : Node_Id := Identifier (N);

   begin
      if Present (Id) then

         --  Make name visible, e.g. for use in exit statements

         Find_Name (Id);
         Id := Entity (Id);

         --  Check that the loop id does not clash with another use of the id.

         if Ekind (Id) /= E_Label then
            Error_Msg_N
              ("loop name conflicts with outer declaration", Identifier (N));
         end if;

         Set_Ekind (Id, E_Loop);

      else
         Id := New_Internal_Entity (E_Loop, Current_Scope, Sloc (N), "loop");
         Set_Etype (Id,  Standard_Void_Type);
      end if;

      New_Scope (Id);
      Analyze_Iteration_Scheme (Iteration_Scheme (N));

      --  Insert any N_Implicit_Type nodes generated by the Iteration Scheme.
      --  These nodes are inserted before the loop statement.

      if Is_Non_Empty_List (Implicit_Type_List) then
         Insert_List_Before (N, Implicit_Type_List);
         Implicit_Type_List := New_List;
      end if;

      Analyze_Statements (Statements (N));
      End_Scope;
   end Analyze_Loop_Statement;

   ----------------------------
   -- Analyze_Exit_Statement --
   ----------------------------

   --  If the exit includes a name, it must be the name of a currently open
   --  loop. Otherwise there must be an innermost open loop on the stack,
   --  to which the statement implicitly refers.

   procedure Analyze_Exit_Statement (N : Node_Id) is
      Target   : Node_Id := Name (N);
      Scope_Id : Entity_Id;
      U_Name   : Entity_Id;
      Kind     : Entity_Kind;

   begin
      if Present (Target) then
         Find_Name (Target);
         U_Name := Entity (Target);

         if not In_Open_Scopes (U_Name) or else Ekind (U_Name) /= E_Loop then
            Error_Msg_N ("invalid loop name in exit statement", N);
            return;
         else
            Set_Has_Exit (U_Name);
         end if;
      end if;

      for I in reverse 0 .. Scope_Stack.Last loop
         Scope_Id := Scope_Stack.Table (I).Entity;
         Kind := Ekind (Scope_Id);

         if Kind = E_Loop  and (No (Target) or Scope_Id = U_Name) then
            exit;

         elsif Kind = E_Block or else Kind = E_Loop then
            null;

         else
            Error_Msg_N
              ("cannot exit from program unit or accept statement", N);
            exit;
         end if;
      end loop;

      --  Verify that if present the condition is a Boolean expression.

      if Present (Condition (N)) then
         Analyze (Condition (N));
         Resolve_Complete_Context (Condition (N), Any_Boolean);
      end if;
   end Analyze_Exit_Statement;

   -----------------------------
   -- Analyze_Block_Statement --
   -----------------------------

   procedure Analyze_Block_Statement (N : Node_Id) is
      Decls : constant List_Id := Declarations (N);
      Id    : Node_Id;

   begin
      Id := Identifier (N);

      if Present (Id) then
         Find_Name (Id);
         Id := Entity (Id);
         Set_Ekind (Id, E_Block);
      else
         Id := New_Internal_Entity (E_Block, Current_Scope, Sloc (N), "block");
      end if;

      Set_Etype (Id, Standard_Void_Type);
      New_Scope (Id);

      if List_Present (Decls) then
         Analyze_Declarations (Decls);
      end if;

      Analyze (Handled_Statement_Sequence (N));
      End_Use (Decls);
      End_Scope;
   end Analyze_Block_Statement;

   ------------------------------
   -- Analyze_Return_Statement --
   ------------------------------

   procedure Analyze_Return_Statement (N : Node_Id) is
      Scope_Id : Entity_Id;
      Kind     : Entity_Kind;

   begin
      --  Find subprogram or accept statement enclosing the return statement

      for I in reverse 0 .. Scope_Stack.Last loop
         Scope_Id := Scope_Stack.Table (I).Entity;
         exit when Ekind (Scope_Id) /= E_Block
           and then Ekind (Scope_Id) /= E_Loop;
      end loop;

      Kind := Ekind (Scope_Id);

      if Present (Expression (N)) then
         if Kind = E_Function or Kind = E_Generic_Function then
            Analyze (Expression (N));
            Resolve_Complete_Context (Expression (N), Etype (Scope_Id));
         else
            Error_Msg_N
              ("Procedure or accept statement cannot return value", N);
         end if;

      else
         if Kind = E_Function or Kind = E_Generic_Function then
            Error_Msg_N ("missing expression in return from function", N);
         end if;
      end if;
   end Analyze_Return_Statement;

   ----------------------------
   -- Analyze_Goto_Statement --
   ----------------------------

   procedure Analyze_Goto_Statement (N : Node_Id) is
      Label       : constant Node_Id := Name (N);
      Scope_Id    : Entity_Id;
      Label_Scope : Entity_Id;

   begin
      Find_Name (Label);

      if Ekind (Entity (Label)) /= E_Label then
         Error_Msg_N ("target of goto statement must be a label", Label);
      elsif not Reachable (Entity (Label)) then
         Error_Msg_N ("target of goto statement is not reachable", Label);
      end if;

      Label_Scope := Scope (Entity (Label));

      for I in reverse 0 .. Scope_Stack.Last loop
         Scope_Id := Scope_Stack.Table (I).Entity;
         exit when (Label_Scope = Scope_Id)
           or else (Ekind (Scope_Id) /= E_Block
                     and then Ekind (Scope_Id) /= E_Loop);
      end loop;

      if Scope_Id /= Label_Scope then
         Error_Msg_N
           ("cannot exit from program unit or accept statement", N);
      end if;
   end Analyze_Goto_Statement;

   ----------------------------------------
   -- Analyze_Implicit_Label_Declaration --
   ----------------------------------------

   --  An implicit label declaration is generated in the innermost
   --  enclosing declarative part. This is done for labels as well as
   --  block and loop names.

   --  TBSL: verify that names are unique over the innermost program
   --  unit (i.e. different blocks cannot have labels with the same name.

   procedure Analyze_Implicit_Label_Declaration (N : Node_Id) is
      Id : Node_Id := Defining_Identifier (N);

   begin
      Enter_Name (Id);
      Set_Ekind (Id, E_Label);
      Set_Etype (Id, Standard_Void_Type);
   end Analyze_Implicit_Label_Declaration;

end Sem_Ch5;
