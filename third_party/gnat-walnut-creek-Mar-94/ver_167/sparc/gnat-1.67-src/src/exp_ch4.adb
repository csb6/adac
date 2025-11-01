------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 4                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.36 $                             --
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
with Exp_Ch9;  use Exp_Ch9;
with Exp_Util; use Exp_Util;
with Nmake;    use Nmake;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Ch4 is

   ------------------------
   --  Local Subprograms --
   ------------------------

   procedure Expand_Comparison_Operator (N : Node_Id);
   --  This routine handles expansion of the comparison operators (N_Op_Lt,
   --  N_Op_Le, N_Op_Gt, N_Op_Ge). Since the code is basicallly similar with
   --  the addition of some outer

   function Make_Array_Comparison_Op
     (Typ : Entity_Id; Loc : Source_Ptr; Equal : Boolean) return Node_Id;
   --  Comparisons between arrays are expanded in line. This function
   --  produces the body of the implementation of (a > b), or (a >= b), when 
   --  a and b are one-dimensional arrays of some discrete type. The original
   --  node is then expanded into the appropriate call to this function. 

   function Make_Boolean_Array_Op (N : Node_Id) return Node_Id;
   --  Boolean operations on boolean arrays are expanded in line. This
   --  function produce the body for (a and b), (a or b), or (a xor b).

   function Tagged_Membership (N : Node_Id) return Node_Id;
   --  Construct the expression corresponding to the tagged membership test.
   --  Deals with a second operand being (or not) a class-wide type.

   --------------------------------
   -- Expand_Comparison_Operator --
   --------------------------------

   --  Expansion is only required in the case of array types. The form of
   --  the expansion is:

   --     [body for greater_nn; boolean_expression]

   --  The body is built by Make_Array_Comparison_Op, and the form of the
   --  Boolean expression depends on the operator involved.

   procedure Expand_Comparison_Operator (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Op1 : Node_Id    := Left_Opnd (N);
      Op2 : Node_Id    := Right_Opnd (N);
      Typ : constant Node_Id    := Etype (Op1);

      Result    : Node_Id;
      Expr      : Node_Id;
      Func_Body : Node_Id;
      Func_Name : Entity_Id;

   begin
      if Is_Array_Type (Typ) then

         --  For <= the Boolean expression is
         --    greater__nn (op2, op1, true)

         if Chars (N) = Name_Op_Le then
            Func_Body := Make_Array_Comparison_Op (Typ, Loc, True);
            Op1  := Right_Opnd (N);
            Op2  := Left_Opnd  (N);

         --  For < the Boolean expression is
         --    greater__nn (op2, op1)

         elsif Chars (N) = Name_Op_Lt then
            Func_Body := Make_Array_Comparison_Op (Typ, Loc, False);
            Op1  := Right_Opnd (N);
            Op2  := Left_Opnd  (N);

         --  For >= the Boolean expression is
         --    op1 = op2 or else greater__nn (op1, op2)

         elsif Chars (N) = Name_Op_Ge then
            Func_Body := Make_Array_Comparison_Op (Typ, Loc, True);

         --  For > the Boolean expression is
         --    greater__nn (op1, op2)

         elsif Chars (N) = Name_Op_Gt then
            Func_Body := Make_Array_Comparison_Op (Typ, Loc, False);
         else
            return;
         end if;

         Func_Name := Defining_Unit_Name (Specification (Func_Body));
         Expr := Make_Function_Call (Loc,
            Name => New_Reference_To (Func_Name, Loc),
            Parameter_Associations => New_List_2 (Op1, Op2));

         Result := Make_Expression_Actions (Loc,
            Actions => New_List_1 (Func_Body),
            Expression => Expr);

         Analyze (Result);
         Resolve_Subexpr (Result, Standard_Boolean);

         Rewrite_Substitute_Tree (N, Result);
      end if;
   end Expand_Comparison_Operator;

   ------------------------
   -- Expand_N_Allocator --
   ------------------------

   --  If the allocator is for a type which requires initialization, and
   --  there is no initial value (i.e. the operand is a subtype indication
   --  rather than a qualifed expression), then we must generate a call to
   --  the initialization routine. This is done using an expression actions
   --  node:
   --
   --     [Temp : constant ptr_T := new (T); Init (Temp.all,...); Temp]
   --
   --  Here ptr_T is the pointer type for the allocator, and T is the
   --  subtype of the allocator. A special case arises if the designated
   --  type of the access type is a task or contains tasks. In this case
   --  the call to Init (Temp.all ...) is replaced by code that ensures
   --  that the tasks get activated (see Exp_Ch9.Build_Task_Allocate_Block
   --  for details). In addition, if the type T is a task T, then the first
   --  argument to Init must be converted to the task record type.

   procedure Expand_N_Allocator (N : Node_Id) is
   begin                                    
      --  Nothing to do if we have a qualified expression (i.e. the allocator
      --  has an initial value, so the initialization routine is not needed)

      if Nkind (Expression (N)) = N_Qualified_Expression then
         null;

      --  Else this is the case that may need an initialization routine

      else
         declare
            PtrT  : constant Entity_Id  := Etype (N);
            T     : constant Entity_Id  := Entity (Expression (N));
            Init  : constant Entity_Id  := Base_Init_Proc (T);
            Loc   : constant Source_Ptr := Sloc (N);
            Temp  : Entity_Id;
            Arg1  : Node_Id;
            Args  : List_Id;
            Discr : Elmt_Id;
            Node  : Node_Id := N;
            Eact  : Node_Id;

         begin
            --  Nothing to do if no initialization routine required

            if No (Init) then
               null;

            --  Else we have the case that definitely needs a call

            else
               Temp :=
                 Make_Defining_Identifier (Loc, New_Internal_Name ("temp"));

               --  Construct argument list for the initialization routine call

               Arg1 :=
                 Make_Explicit_Dereference (Loc,
                   Prefix => New_Reference_To (Temp, Loc));

               --  The initialization procedure expects a specific type.   
               --  if the context is access to classwide,  indicate that   
               --  the object being allocated has the right specific type. 

               if Is_Class_Type (Designated_Type (PtrT)) then
                  Arg1 :=
                    Make_Type_Conversion (Loc,
                      Subtype_Mark => New_Reference_To (T,  Loc),
                      Expression => Arg1);
               end if;

               --  If designated type is a task type, then the first argument
               --  in the Init routine has to be unchecked converted to the
               --  corresponding record type, since that's what Init expects.

               if Is_Task_Type (T) then
                  Arg1 := 
                    Make_Unchecked_Type_Conversion (Loc,
                      Subtype_Mark => 
                        New_Reference_To (Task_Value_Type (T), Loc),
                      Expression => Arg1);
               end if;

               Args := New_List_1 (Arg1);

               --  For the task case, pass the Master_Id of the access type
               --  as the value of the _Master parameter, and _Chain as the
               --  value of the _Chain parameter (_Chain will be defined as
               --  part of the generated code for the allocator).

               if Has_Tasks (T) then
                  Append_To (Args, New_Reference_To (Master_Id (PtrT), Loc));
                  Append_To (Args, Make_Identifier (Loc, Name_uChain));
               end if;

               --  Add discriminants if discriminated type

               if Has_Discriminants (T) then
                  Discr := First_Elmt (Discriminant_Constraint (T));

                  while Discr /= No_Elmt loop
                     Append (New_Copy (Id_Of (Discr)), Args);
                     Discr := Next_Elmt (Discr);
                  end loop;
               end if;

               --  We set the allocator as analyzed so that when we analyze the
               --  expression actions node, we do not get an unwanted recursive
               --  expansion of the allocator expression.

               Set_Analyzed (N, True);

               --  Now we can rewrite the allocator. First see if it is
               --  already in an expression actions node, which will often
               --  be the case, because this is how we handle the case of
               --  discriminants being present. If so, we can just modify
               --  that expression actions node that is there, otherwise
               --  we must create an expression actions node.

               Eact := Parent (N);

               if Nkind (Eact) = N_Expression_Actions
                 and then Expression (Eact) = N
               then
                  null;

               else
                  Rewrite_Substitute_Tree (N,
                    Make_Expression_Actions (Loc,
                      Actions    => New_List,
                      Expression => New_Copy (N)));

                  Eact := N;
                  Node := Expression (N);
               end if;

               --  Now we modify the expression actions node as follows

               --    input:   [... ; new T]

               --    output:  [... ;
               --              Temp : constant ptr_T := new (T);
               --              Init (Temp.all, ...);
               --              Temp]

               --  Here ptr_T is the pointer type for the allocator, and T
               --  is the subtype of the allocator.

               Append_To (Actions (Eact),
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Constant_Present    => True,
                   Object_Definition   => New_Reference_To (PtrT, Loc),
                   Expression          => New_Copy (Node)));

               --  Case of designated type is task or contains task

               if Has_Tasks (T) then
                  Build_Task_Allocate_Block (Actions (Eact), N, Args);

               else
                  Append_To (Actions (Eact),
                    Make_Procedure_Call_Statement (Loc,
                      Name => New_Reference_To (Init, Loc),
                      Parameter_Associations => Args));
               end if;

               Set_Expression (Eact, New_Reference_To (Temp, Loc));

               Analyze (Eact);
            end if;
         end;
      end if;
   end Expand_N_Allocator;

   -----------------------------
   -- Expand_Boolean_Operator --
   -----------------------------

   procedure Expand_Boolean_Operator (N : Node_Id) is
      Loc       : constant Source_Ptr := Sloc (N);
      Typ       : constant Entity_Id  := Etype (N);
      Result    : Node_Id;
      Func_Body : Node_Id;
      Func_Name : Entity_Id;

   begin
      if Is_Array_Type (Typ) then
         Func_Body := Make_Boolean_Array_Op (N);
         Func_Name := Defining_Unit_Name (Specification (Func_Body));

         Result := Make_Expression_Actions (Loc,
            Actions => New_List_1 (Func_Body),
            Expression => 
              Make_Function_Call (Loc,
                 Name => New_Reference_To (Func_Name, Loc),
                 Parameter_Associations => 
                   New_List_2 (Left_Opnd (N), Right_Opnd (N))));

         Analyze (Result);
         Resolve_Subexpr (Result, Typ);

         Rewrite_Substitute_Tree (N, Result);
      end if;
   end Expand_Boolean_Operator;

   ---------------------
   -- Expand_N_Op_And --
   ---------------------

   --  This is really just a renaming of Expand_Boolean_Operator ???

   procedure Expand_N_Op_And (N : Node_Id) is
   begin
      Expand_Boolean_Operator (N);
   end Expand_N_Op_And;

   --------------------
   -- Expand_N_Op_Eq --
   --------------------

   procedure Expand_N_Op_Eq (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Lhs     : constant Node_Id    := Left_Opnd (N);
      Rhs     : constant Node_Id    := Right_Opnd (N);
      Typ     : Entity_Id  := Etype (Lhs);
      Eq_Prim : Entity_Id;

   begin

      if Ekind (Typ) = E_Private_Type then
         Typ := Full_Declaration (Typ);
      end if;

      if  Is_Array_Type (Typ) then 

         if Is_Scalar_Type (Component_Type (Typ)) then
            --  This case is left to Gigi optimization purposes
            null;

         else
            Rewrite_Substitute_Tree (N,
              Expand_Array_Equality
                (Loc, Typ, New_Copy (Lhs), New_Copy (Rhs)));

            Analyze (N);
            Resolve_Subexpr (N, Standard_Boolean);
         end if;

      elsif Is_Record_Type (Typ) then

         if Has_Discriminants (Typ) 
           and then Present (Variant_Part (Component_List (
                               Type_Definition (Parent (Base_Type (Typ))))))
         then

            --  ???
            --  in this case a function has to be expanded and called using
            --  the same model as for initialization procedures  (use of 
            --  the case statement in the record definition).
            --  It has to be dealt with as a apecial case because in the
            --  simple case (record without variant part), we prefer to 
            --  generate a big expression which will be optimized by the 
            --  back-end. 

            Unimplemented (N, "?complex equality of discriminated records");

         else
            declare 
               L : Node_Id := New_Copy (Lhs);
               R : Node_Id := New_Copy (Rhs);

            begin
               --  ??? L and R should be marked evaluate_once because the 
               --  equality is expanded into a bunch of component equailities  
               --  but in this case this flag seems to cause big trouble in
               --  the generatyed code, so it is commented out till further
               --  investigation.
               --  Set_Evaluate_Once (L, True);
               --  Set_Evaluate_Once (R, True);

               Rewrite_Substitute_Tree (N, 
                 Expand_Record_Equality (Loc, Typ, L, R));
               Analyze (N);
               Resolve_Subexpr (N, Standard_Boolean);
            end;
         end if;
      end if;
   end Expand_N_Op_Eq;

   -----------------------
   -- Expand_N_Op_Expon --
   -----------------------

   procedure Expand_N_Op_Expon (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Typ   : constant Entity_Id  := Etype (N);
      Btyp  : constant Entity_Id  := Root_Type (Typ);
      Max   : constant Uint       := Uint_4;
      Min   : constant Uint       := Uint_Minus_4;
      Base  : constant Node_Id    := New_Copy (Left_Opnd (N));
      Exp   : constant Node_Id    := New_Copy (Right_Opnd (N));
      Expv  : Uint;
      Xnode : Node_Id;
      Temp  : Node_Id;
      Rent  : RE_Id;

   begin
      --  Evaluate our arguments, so that we can test for static case

      Static_Evaluation (Base);
      Static_Evaluation (Exp);

      --  Do nothing if both our operands are static. The result will be
      --  folded at a higher level (or at least it jolly well ought to be)

      if Is_Static (Base) and then Is_Static (Exp) then
         return;
      end if;

      --  Test for case of literal right argument

      if Nkind (Exp) = N_Integer_Literal then
         Expv := Intval (Exp);

         if (Ekind (Typ) in Float_Kind
               and then UI_Ge (Expv, Min)
               and then UI_Le (Expv, Max))
           or else
            (Ekind (Typ) in Integer_Kind
               and then UI_Ge (Expv, Uint_0)
               and then UI_Le (Expv, Max))
         then
            Expv := UI_Abs (Expv);

            --  X ** 0 = 1 (or 1.0)

            if Expv = Uint_0 then
               if Ekind (Typ) in Integer_Kind then
                  Xnode := Make_Integer_Literal (Loc, Intval => Uint_1);
               else
                  Xnode :=
                    Make_Real_Literal (Loc,
                      Numerator   => Uint_1,
                      Denominator => Uint_1);
               end if;

            --  X ** 1 = X

            elsif Expv = Uint_1 then
               Xnode := Base;

            --  X ** 2 = X * X

            elsif Expv = Uint_2 then
               Set_Evaluate_Once (Base, True);
               Xnode :=
                 Make_Op_Multiply (Loc,
                   Left_Opnd  => Base,
                   Right_Opnd => Base);

            --  X ** 3 = (X * X) * X

            elsif Expv = Uint_3 then
               Set_Evaluate_Once (Base, True);
               Xnode :=
                 Make_Op_Multiply (Loc,
                   Left_Opnd =>
                     Make_Op_Multiply (Loc,
                       Left_Opnd  => Base,
                       Right_Opnd => Base,
                       Parens     => True),

                   Right_Opnd  => Base);

            --  X ** 4 = {temp : constant base'type := base * base} temp * temp

            elsif Expv = Uint_4 then
               Set_Evaluate_Once (Base, True);
               Temp := 
                 Make_Defining_Identifier (Loc, New_Internal_Name ("tmp"));
               Xnode :=
                 Make_Expression_Actions (Loc,
                   Actions => New_List_1 (
                     Make_Object_Declaration (Loc,
                       Defining_Identifier => Temp,
                       Constant_Present    => True,
                       Object_Definition   => New_Reference_To (Typ, Loc),
                       Expression =>
                         Make_Op_Multiply (Loc,
                           Left_Opnd  => Base,
                           Right_Opnd => Base))),
                   Expression =>
                     Make_Op_Multiply (Loc,
                           Left_Opnd  => New_Reference_To (Temp, Loc),
                           Right_Opnd => New_Reference_To (Temp, Loc)));
            end if;

            --  For non-negative case, we are all set

            if not UI_Is_Negative (Intval (Exp)) then
               Rewrite_Substitute_Tree (N, Xnode);

            --  For negative cases, take reciprocal (base must be real)

            else
               Set_Parens (Xnode, True);
               Rewrite_Substitute_Tree (N,
                 Make_Op_Divide (Loc,
                   Left_Opnd   =>
                     Make_Real_Literal (Loc,
                       Numerator   => Uint_1,
                       Denominator => Uint_1),
                   Right_Opnd  => Xnode));
            end if;

            Analyze (N);
            Resolve_Subexpr (N, Typ);
            return;

         --  Don't fold cases of large literal exponents, and also don't fold
         --  cases of integer bases with negative literal exponents.

         end if;

      --  Don't fold cases where exponent is not integer literal

      end if;

      --  Fall through if exponentiation must be done using a runtime routine

      if Btyp = Standard_Integer then
         Rent := RE_Xp_I;
      elsif Btyp = Standard_Short_Integer then
         Rent := RE_Xp_SI;
      elsif Btyp = Standard_Short_Short_Integer then
         Rent := RE_Xp_SSI;
      elsif Btyp = Standard_Long_Integer then
         Rent := RE_Xp_LI;
      elsif Btyp = Standard_Long_Long_Integer then
         Rent := RE_Xp_LLI;
      elsif Btyp = Standard_Float then
         Rent := RE_Xp_F;
      elsif Btyp = Standard_Short_Float then
         Rent := RE_Xp_SF;
      elsif Btyp = Standard_Long_Float then
         Rent := RE_Xp_LF;
      elsif Btyp = Standard_Long_Long_Float then
         Rent := RE_Xp_LLF;
      else
         Compiler_Error;
         Write_Entity_Info (Typ,  "Typ ->");
         Write_Entity_Info (Btyp, "Btyp ->");
         Compiler_Abort;
      end if;

      --  If we are in base type, we can call runtime routine directly

      if Typ = Btyp then
         Rewrite_Substitute_Tree (N,
           Make_Function_Call (Loc,
             Name => New_Reference_To (RTE (Rent), Loc),
             Parameter_Associations => New_List_2 (Base, Exp)));

      --  Otherwise we have to introduce conversions

      else
         Rewrite_Substitute_Tree (N,
           Make_Type_Conversion (Loc,
             Subtype_Mark => New_Reference_To (Typ, Loc),
             Expression   =>
               Make_Function_Call (Loc,
                 Name => New_Reference_To (RTE (Rent), Loc),
                 Parameter_Associations => New_List_2 (
                   Make_Type_Conversion (Loc,
                     Subtype_Mark => New_Reference_To (Btyp, Loc),
                     Expression   => Base),
                   Exp))));
      end if;

      Analyze (N);
      Resolve_Subexpr (N, Typ);
      return;

   end Expand_N_Op_Expon;

   --------------------
   -- Expand_N_Op_Ge --
   --------------------

   --  This should be a body renaming if this worked ???

   procedure Expand_N_Op_Ge (N : Node_Id) is
   begin
      Expand_Comparison_Operator (N);
   end Expand_N_Op_Ge;

   --------------------
   -- Expand_N_Op_Gt --
   --------------------

   --  This should be a body renaming if this worked ???

   procedure Expand_N_Op_Gt (N : Node_Id) is
   begin
      Expand_Comparison_Operator (N);
   end Expand_N_Op_Gt;

   --------------------
   -- Expand_N_Op_In --
   --------------------

   --  Expansion is only required for the tagged case. See specification of
   --  Tagged_Membership function for details of required transformation.

   procedure Expand_N_Op_In (N : Node_Id) is
      Typ : constant Entity_Id := Etype (N);

   begin
      if Is_Tagged_Type (Etype (Etype (Right_Opnd (N)))) then
         Rewrite_Substitute_Tree (N, Tagged_Membership (N));
         Analyze (N);
         Resolve_Subexpr (N, Typ);
      end if;
   end Expand_N_Op_In;

   --------------------
   -- Expand_N_Op_Le --
   --------------------

   --  This should be a body renaming if this worked ???

   procedure Expand_N_Op_Le (N : Node_Id) is
   begin
      Expand_Comparison_Operator (N);
   end Expand_N_Op_Le;

   --------------------
   -- Expand_N_Op_Lt --
   --------------------

   --  This should be a body renaming if this worked ???

   procedure Expand_N_Op_Lt (N : Node_Id) is
   begin
      Expand_Comparison_Operator (N);
   end Expand_N_Op_Lt;

   ---------------------
   -- Expand_N_Op_Not --
   ---------------------

   procedure Expand_N_Op_Not (N : Node_Id) is
      --  If the argument of negation is a boolean array type, expand 
      --  in line the following:

      --  function not_ (a: arr) is 
      --    b : arr;
      --  begin
      --    for i in a'range loop
      --       b (i) := not a(i);
      --    end loop;
      --    return b;
      --  end not_;

      Loc : Source_Ptr := Sloc (N);

      A : Entity_Id := Make_Defining_Identifier (Loc, New_Internal_Name ("A"));
      B : Entity_Id := Make_Defining_Identifier (Loc, New_Internal_Name ("B"));
      I : Entity_Id := Make_Defining_Identifier (Loc, New_Internal_Name ("I"));
      A_I : Node_Id;
      B_I : Node_Id;

      Typ : constant Entity_Id := Etype (N);
      Func_Name      : Entity_Id;
      Func_Body      : Node_Id;
      Loop_Statement : Node_Id;
      Result         : Node_Id;

   begin
      if not Is_Array_Type (Typ) then
         return;
      end if;

      A_I := 
        Make_Indexed_Component (Loc,
          Prefix => Make_Identifier (Loc, Chars (A)),
            Expressions => New_List_1 (
              Make_Identifier (Loc, Chars (I))));

      B_I := 
        Make_Indexed_Component (Loc,
          Prefix => Make_Identifier (Loc, Chars (B)),
            Expressions => New_List_1 (
              Make_Identifier (Loc, Chars (I))));

      Loop_Statement :=
        Make_Loop_Statement (Loc,
          Identifier => Empty,

          Iteration_Scheme =>
            Make_Iteration_Scheme (Loc,
              Loop_Parameter_Specification =>
                Make_Loop_Parameter_Specification (Loc,
                  Defining_Identifier => I,
                  Discrete_Subtype_Definition =>
                    Make_Attribute_Reference (Loc,
                      Prefix => Make_Identifier (Loc, Chars (A)),
                      Identifier => Make_Identifier (Loc, Name_Range)))),

          Statements => New_List_1 (
            Make_Assignment_Statement (Loc,
              Name => B_I, 
              Expression => 
                Make_Op_Not (Loc,
                  Right_Opnd => A_I))));


      Func_Name := 
              Make_Defining_Identifier (Loc,  New_Internal_Name ("Not"));

      Func_Body :=
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name => Func_Name,
              Parameter_Specifications => New_List_1 (
                Make_Parameter_Specification (Loc,
                  Defining_Identifier => A,
                  Parameter_Type => New_Reference_To (Typ, Loc))),
              Subtype_Mark => New_Reference_To (Typ,  Loc)),

          Declarations => New_List_1 (
            Make_Object_Declaration (Loc,
              Defining_Identifier => B,
              Object_Definition   => New_Reference_To (Typ, Loc))),

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List_2 (
                Loop_Statement,
                Make_Return_Statement (Loc,
                  Expression =>
                    Make_Identifier (Loc, Chars (B))))));

      Result := Make_Expression_Actions (Loc,
         Actions => New_List_1 (Func_Body),
         Expression => 
           Make_Function_Call (Loc,
              Name => New_Reference_To (Func_Name, Loc),
              Parameter_Associations => 
                New_List_1 (Right_Opnd (N))));

      Analyze (Result);
      Resolve_Subexpr (Result, Typ);

      Rewrite_Substitute_Tree (N, Result);

   end Expand_N_Op_Not;

   ------------------------
   -- Expand_N_Op_Not_In --
   ------------------------

   --  Expansion is only required for the tagged case. See specification of
   --  Tagged_Membership function for details of required transformation.

   procedure Expand_N_Op_Not_In (N : Node_Id) is
      Typ : constant Entity_Id := Etype (N);

   begin
      if Is_Tagged_Type (Etype (Etype (Right_Opnd (N)))) then
         Rewrite_Substitute_Tree (N,
           Make_Op_Not (Sloc (N), Tagged_Membership (N)));
         Analyze (N);
         Resolve_Subexpr (N, Typ);
      end if;
   end Expand_N_Op_Not_In;

   --------------------
   -- Expand_N_Op_Or --
   --------------------

   procedure Expand_N_Op_Or (N : Node_Id) is
      --  renames Expand_Boolean_Operator;
   begin
      Expand_Boolean_Operator (N);
   end Expand_N_Op_Or;

   ---------------------
   -- Expand_N_Op_Xor --
   ---------------------

   procedure Expand_N_Op_Xor (N : Node_Id) is
      --  renames Expand_Boolean_Operator;
   begin
      Expand_Boolean_Operator (N);
   end Expand_N_Op_Xor;

   ---------------------------------------
   -- Expand_N_Parenthesized_Expression --
   ---------------------------------------

   --  We remove these from the tree, since they make tests higher up in
   --  the tree in expand more complicated, and it is easier if we just
   --  get rid of them, since they serve no code generation purpose.

   procedure Expand_N_Parenthesized_Expression (N : Node_Id) is
   begin
      Rewrite_Substitute_Tree (N, Expression (N));
   end Expand_N_Parenthesized_Expression;

   --------------------
   -- Expand_N_Slice --
   --------------------

   --  Build an implicit subtype declaration to represent the type delivered
   --  by the slice. This subtype has an Ekind of E_Slice_Subtype, which is
   --  a special kind of type used just for this purpose. Logically, what is
   --  needed is a full array subtype declaration, but that would take a lot
   --  of nodes. On the other hand if we don't provide any kind of subtype
   --  for the slice, Gigi gets really confused. The compromise of building
   --  a special kind of very economical subtype declaration node, and then
   --  putting a bit of specialized code in Gigi to deal with this special
   --  declaration meets the need with minimum overhead.

   --  The procesing consists of building this subtype and then resetting the
   --  Etype of the slice node to have this type.

   procedure Expand_N_Slice (N : Node_Id) is
      Impl_Subtype : Entity_Id;

   begin
      --  First we build a defining occurrence for the "slice subtype"

      Impl_Subtype := New_Implicit_Type (Sloc (N));
      Set_Ekind (Impl_Subtype, E_Slice_Subtype); 
      Set_Component_Type (Impl_Subtype, Component_Type (Etype (N)));
      Set_Slice_Range (Impl_Subtype, Discrete_Range (N));
      Set_Etype (Impl_Subtype, Etype (N));

      --  The Etype of the existing Slice node is reset to this anymous
      --  subtype. This node will be marked as Analyzed when we return and
      --  nothing else needs to be done to it.

      Set_Etype (N, Impl_Subtype);
   end Expand_N_Slice;

   ------------------------------
   -- Make_Array_Comparison_Op --
   ------------------------------
   --  This is a hand-coded expansion of the following generic function:

   --  generic
   --    type elem is  (<>);
   --    type index is (<>);
   --    type a is array (index range <>) of elem;
   --
   --  function greater__nn (X : a; Y: a) return boolean is
   --    J : index :=Y'first;
   --
   --  begin
   --    if X'length = 0 then 
   --       return false;
   --
   --    elsif Y'length = 0 then
   --       return true;
   --
   --    else
   --      for I in X'range loop
   --        if X (I) = Y (J) then
   --          if J = Y'last then
   --            exit;
   --          else
   --            J := index'succ (J);
   --          end if;
   --
   --        else 
   --           return X (I) > Y (J);
   --        end if;
   --      end loop;
   --
   --      return X'length > Y'length;
   --    end if;
   --  end greater__nn;

   --  If the flag Equal is true, the procedure generates the body for
   --  >= instead. This only affects the last return statement.

   --  Note that since we are essentially doing this expansion by hand, we
   --  do not need to generate an actual or formal generic part, just the
   --  instantiated function itself.

   function Make_Array_Comparison_Op
     (Typ : Entity_Id; Loc : Source_Ptr; Equal : Boolean) return Node_Id
   is
      X : constant Entity_Id := Make_Defining_Identifier (Loc, Name_X);
      Y : constant Entity_Id := Make_Defining_Identifier (Loc, Name_Y);
      I : constant Entity_Id := Make_Defining_Identifier (Loc, Name_I);
      J : constant Entity_Id := Make_Defining_Identifier (Loc, Name_J);

      Index : constant Entity_Id := Base_Type (Etype (First_Index (Typ)));

      Loop_Statement : Node_Id;
      Loop_Body      : Node_Id;
      If_Stat        : Node_Id;
      Inner_If       : Node_Id;
      Final_Expr     : Node_Id;
      Func_Body      : Node_Id;
      Func_Name      : Entity_Id;
      Formals        : List_Id;
      Length1        : Node_Id;
      Length2        : Node_Id;

   begin
      --  if J = Y'last then
      --     exit;
      --  else
      --     J := index'succ (J);
      --  end if;

      Inner_If :=
        Make_If_Statement (Loc,
          Condition =>
            Make_Op_Eq (Loc,
              Left_Opnd =>
                Make_Identifier (Loc, Chars (J)),
              Right_Opnd =>
                Make_Attribute_Reference (Loc,
                  Prefix => Make_Identifier (Loc, Chars (Y)),
                  Identifier => Make_Identifier (Loc, Name_Last))),

          Then_Statements => New_List_1 (
                Make_Exit_Statement (Loc)),

          Else_Statements =>
            New_List_1 (
              Make_Assignment_Statement (Loc,
                Name => Make_Identifier (Loc, Chars (J)),
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix => New_Reference_To (Index, Loc),
                    Identifier => Make_Identifier (Loc, Name_Succ),
                    Expression => Make_Identifier (Loc, Chars (J))))));

      --  if X (I) = Y (J) then
      --     if ... end if;
      --  else
      --     return X (I) > Y (J);
      --  end if;

      Loop_Body :=
        Make_If_Statement (Loc,
          Condition =>
            Make_Op_Eq (Loc,
              Left_Opnd =>
                Make_Indexed_Component (Loc,
                  Prefix => Make_Identifier (Loc, Chars (X)),
                  Expressions => New_List_1 (
                    Make_Identifier (Loc, Chars (I)))),
              Right_Opnd =>
                Make_Indexed_Component (Loc,
                  Prefix => Make_Identifier (Loc, Chars (Y)),
                  Expressions => New_List_1 (
                    Make_Identifier (Loc, Chars (J))))),

          Then_Statements => New_List_1 (Inner_If),

          Else_Statements =>
            New_List_1 ( Make_Return_Statement (Loc,
              Expression =>
                Make_Op_Gt (Loc,
                  Left_Opnd =>
                    Make_Indexed_Component (Loc,
                      Prefix => Make_Identifier (Loc, Chars (X)),
                      Expressions => New_List_1 (
                        Make_Identifier (Loc, Chars (I)))),

                  Right_Opnd =>
                    Make_Indexed_Component (Loc,
                      Prefix => Make_Identifier (Loc, Chars (Y)),
                      Expressions => New_List_1 (
                        Make_Identifier (Loc, Chars (J))))))));

      --  for I in X'range loop
      --     if ... end if;
      --  end loop;

      Loop_Statement :=
        Make_Loop_Statement (Loc,
          Identifier => Empty,

          Iteration_Scheme =>
            Make_Iteration_Scheme (Loc,
              Loop_Parameter_Specification =>
                Make_Loop_Parameter_Specification (Loc,
                  Defining_Identifier => I,
                  Discrete_Subtype_Definition =>
                    Make_Attribute_Reference (Loc,
                      Prefix => Make_Identifier (Loc, Chars (X)),
                      Identifier => Make_Identifier (Loc, Name_Range)))),

          Statements => New_List_1 (Loop_Body));

      --    if X'length = 0 then
      --       return false;
      --    elsif Y'length = 0 then
      --       return true;
      --    else
      --      for ... loop ... end loop;
      --      return X'length > Y'length;
      --    --  return X'length >= Y'length to implement >=.
      --    end if;

      Length1 := Make_Attribute_Reference (Loc,
          Prefix => Make_Identifier (Loc, Chars (X)),
          Identifier => Make_Identifier (Loc, Name_Length));

      Length2 := Make_Attribute_Reference (Loc,
          Prefix => Make_Identifier (Loc, Chars (Y)),
          Identifier => Make_Identifier (Loc, Name_Length));

      if Equal then
         Final_Expr := Make_Op_Ge (Loc,
            Left_Opnd =>  Length1,
            Right_Opnd => Length2);
      else
         Final_Expr := Make_Op_Gt (Loc,
            Left_Opnd =>  Length1,
            Right_Opnd => Length2);
      end if;

      If_Stat :=
        Make_If_Statement (Loc,
          Condition =>
            Make_Op_Eq (Loc,
              Left_Opnd =>
                Make_Attribute_Reference (Loc,
                  Prefix => Make_Identifier (Loc, Chars (X)),
                  Identifier => Make_Identifier (Loc, Name_Length)),
              Right_Opnd =>
                Make_Integer_Literal (Loc, Uint_0)),

          Then_Statements =>
            New_List_1 (
              Make_Return_Statement (Loc,
                Expression => New_Reference_To (Standard_False, Loc))),

          Elsif_Parts => New_List_1 (
            Make_If_Statement (Loc,
              Condition =>
                Make_Op_Eq (Loc,
                  Left_Opnd =>
                    Make_Attribute_Reference (Loc,
                      Prefix => Make_Identifier (Loc, Chars (Y)),
                      Identifier => Make_Identifier (Loc, Name_Length)),
                  Right_Opnd =>
                    Make_Integer_Literal (Loc, Uint_0)),

              Then_Statements =>
                New_List_1 (
                  Make_Return_Statement (Loc,
                     Expression => New_Reference_To (Standard_True, Loc))))),

          Else_Statements => New_List_2 (
            Loop_Statement, 
            Make_Return_Statement (Loc,
              Expression => Final_Expr)));


      --  (X : a; Y: a)

      Formals := New_List_2 (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => X,
          Parameter_Type => New_Reference_To (Typ, Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier => Y,
          Parameter_Type => New_Reference_To (Typ, Loc)));

      --  function greater (...) return boolean is
      --    J : index :=Y'first;
      --  begin
      --    if ... end if;
      --  end greater;

      if Equal then 
         Func_Name := 
              Make_Defining_Identifier (Loc,  New_Internal_Name ("Ge"));
      else
         Func_Name := 
              Make_Defining_Identifier (Loc,  New_Internal_Name ("Gt"));
      end if;

      Func_Body :=
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name => Func_Name,
              Parameter_Specifications => Formals,
              Subtype_Mark => New_Reference_To (Standard_Boolean,  Loc)),

          Declarations => New_List_1 (
            Make_Object_Declaration (Loc,
              Defining_Identifier => J,
              Object_Definition   => New_Reference_To (Index, Loc),
              Expression =>
                Make_Attribute_Reference (Loc,
                  Prefix => Make_Identifier (Loc, Chars (Y)),
                  Identifier => Make_Identifier (Loc, Name_First)))),

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List_1 (If_Stat)));

      return Func_Body;

   end Make_Array_Comparison_Op;

   ---------------------------
   -- Make_Boolean_Array_Op --
   ---------------------------

   --  For logical operations on boolean arrays, expand in line the
   --  following, replacing 'and' with 'or' or 'xor' where needed:

   --    function and___nn (a : arr; b: arr) is
   --       c : arr;
   --    begin
   --       for i in a'range loop
   --          c (i) := a(i) and b(i);
   --       end loop;
   --       return c;
   --    end and___nn;

   function Make_Boolean_Array_Op (N : Node_Id) return Node_Id is

      Loc : Source_Ptr := Sloc (N);
      Typ : Entity_Id := Etype (Left_Opnd (N));

      A : Entity_Id := Make_Defining_Identifier (Loc, New_Internal_Name ("A"));
      B : Entity_Id := Make_Defining_Identifier (Loc, New_Internal_Name ("B"));
      C : Entity_Id := Make_Defining_Identifier (Loc, New_Internal_Name ("C"));
      I : Entity_Id := Make_Defining_Identifier (Loc, New_Internal_Name ("I"));
      A_I : Node_Id;
      B_I : Node_Id;
      C_I : Node_Id;
      Op  : Node_Id;

      Formals        : List_Id;
      Func_Name      : Entity_Id;
      Func_Body      : Node_Id;
      Loop_Statement : Node_Id;

   begin
      A_I := 
        Make_Indexed_Component (Loc,
          Prefix => Make_Identifier (Loc, Chars (A)),
            Expressions => New_List_1 (
              Make_Identifier (Loc, Chars (I))));

      B_I := 
        Make_Indexed_Component (Loc,
          Prefix => Make_Identifier (Loc, Chars (B)),
            Expressions => New_List_1 (
              Make_Identifier (Loc, Chars (I))));

      if Nkind (N) = N_Op_And then
         Op := Make_Op_And (Loc, Left_Opnd  => A_I, Right_Opnd => B_I);
      elsif Nkind (N) = N_Op_Or then
         Op := Make_Op_Or (Loc, Left_Opnd  => A_I, Right_Opnd => B_I);
      else 
         Op := Make_Op_Xor (Loc, Left_Opnd  => A_I, Right_Opnd => B_I);
      end if;

      Loop_Statement :=
        Make_Loop_Statement (Loc,
          Identifier => Empty,

          Iteration_Scheme =>
            Make_Iteration_Scheme (Loc,
              Loop_Parameter_Specification =>
                Make_Loop_Parameter_Specification (Loc,
                  Defining_Identifier => I,
                  Discrete_Subtype_Definition =>
                    Make_Attribute_Reference (Loc,
                      Prefix => Make_Identifier (Loc, Chars (A)),
                      Identifier => Make_Identifier (Loc, Name_Range)))),

          Statements => New_List_1 (
            Make_Assignment_Statement (Loc,
              Name => 
                Make_Indexed_Component (Loc,
                  Prefix => Make_Identifier (Loc, Chars (C)),
                    Expressions => New_List_1 (
                      Make_Identifier (Loc, Chars (I)))),
              Expression => Op)));

      Formals := New_List_2 (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => A,
          Parameter_Type => New_Reference_To (Typ, Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier => B,
          Parameter_Type => New_Reference_To (Typ, Loc)));

      if Nkind (N) = N_Op_And then 
         Func_Name := 
           Make_Defining_Identifier (Loc,  New_Internal_Name ("And"));

      elsif Nkind (N) = N_Op_Or then
         Func_Name := 
           Make_Defining_Identifier (Loc,  New_Internal_Name ("Or"));

      else
         Func_Name := 
           Make_Defining_Identifier (Loc,  New_Internal_Name ("Xor"));
      end if;

      Func_Body :=
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name => Func_Name,
              Parameter_Specifications => Formals,
              Subtype_Mark => New_Reference_To (Typ,  Loc)),

          Declarations => New_List_1 (
            Make_Object_Declaration (Loc,
              Defining_Identifier => C,
              Object_Definition   => New_Reference_To (Typ, Loc))),

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List_2 (
                Loop_Statement,
                Make_Return_Statement (Loc,
                  Expression =>
                    Make_Identifier (Loc, Chars (C))))));

      return Func_Body;

   end Make_Boolean_Array_Op;

   ------------------------
   --  Tagged_Membership --
   ------------------------

   --  There are two different cases to consider depending on whether
   --  the right operand is a class-wide type or not. If not we just
   --  compare the actual tag of the left expr to the target type tag:
   --
   --     Left_Expr.Tag = Tag_Of (Right_Type);
   --
   --  If it is a class-wide type, it is more complex. We use the table of
   --  ancestors accessed by the "Tags" field of the Dispatch table. We have
   --  to ensure that the inheritance depth of the operand if greater or equal
   --  than the target types's and that they are on the inheritance path :
   --
   --  <action>
   --    N : Integer := Left_Expr.Tag.all.Inheritance_Depth -
   --                     Tag_Of (Right_Type).all.Inheritance_Depth;
   --  <expression>
   --    (N >= 0)
   --      and then Left_Expr.Tag.all.Tags.all (N) = Tag_Of (Right_Type)
   --
   --  the real expressions are a bit more complicated due to type conversions

   function Tagged_Membership (N : Node_Id) return Node_Id is
      Left       : constant Node_Id   := Left_Opnd  (N);
      Right      : constant Node_Id   := Right_Opnd (N);
      Sloc_N     : constant Source_Ptr := Sloc (N);

      Left_Type  : Entity_Id;
      Right_Type : Entity_Id;
      Var_N      : Node_Id;

   begin
      Left_Type  := Etype (Left);
      Right_Type := Etype (Right);

      if Is_Class_Type (Left_Type) then
         Left_Type := Etype (Left_Type); 
      end if;

      if not Is_Class_Type (Right_Type) then

         --  Left_Type (Left)._Tag = 
         --    System.Tag (Access_Disp_Table (Right_Type));

         return
           Make_Op_Eq (Sloc_N,
             Left_Opnd =>
               Make_Selected_Component (Sloc_N,
                 Prefix => 
                   Make_Unchecked_Type_Conversion (Sloc_N,
                     Subtype_Mark => New_Reference_To (Left_Type, Sloc_N),
                     Expression => New_Copy (Left)),
                 Selector_Name => 
                   New_Reference_To (Tag_Component (Left_Type), Sloc_N)),
             Right_Opnd =>
               Make_Unchecked_Type_Conversion (Sloc_N,
                 Subtype_Mark => New_Reference_To (RTE (RE_Tag), Sloc_N),
                 Expression =>
                   New_Reference_To (
                     Access_Disp_Table (Right_Type), Sloc_N)));

      else
         --  Replace N by expression-actions
         --
         --    <actions>
         --    N : Integer :=
         --      Acc_Dt (Left_Type(Left)._Tag).all.Inheritance_Depth
         --        - Access_Disp_Table (Right_Type).all.Inheritance_Depth;

         Var_N := Make_Defining_Identifier (Sloc_N,  New_Internal_Name ("n"));

         --  Use the root type of the class

         Right_Type := Etype (Right_Type);

         return
           Make_Expression_Actions (Sloc_N,
             Actions => New_List_1 (
               Make_Object_Declaration (Sloc_N,
                 Defining_Identifier => Var_N,

                 Object_Definition   =>
                   New_Reference_To (Standard_Integer, Sloc_N),

                 Expression =>
                   Make_Op_Subtract (Sloc_N,
                     Left_Opnd =>
                       Make_Selected_Component (Sloc_N,
                         Prefix =>  Make_DT_Access (Sloc_N, Left, Left_Type),
                         Selector_Name => 
                           Make_DT_Component (Sloc_N, Left_Type, 1)),

                     Right_Opnd =>
                       Make_Selected_Component (Sloc_N,
                         Prefix =>
                           Make_Explicit_Dereference (Sloc_N,
                             Prefix =>
                               Make_Identifier (Sloc_N,
                                 Chars (Access_Disp_Table (Right_Type)))),

                       Selector_Name => 
                         Make_DT_Component (Sloc_N, Right_Type, 1))))),

         --  (N >= 0) 
         --  and then
         --    (Acc_Dt (Left_Type (Left).__Tag).all.Tags.all (N) 
         --     = System.Tag (Access_Disp_Table (Right_Type)))

             Expression =>
               Make_Op_And_Then (Sloc_N,
                 Left_Opnd =>
                   Make_Op_Ge (Sloc_N,
                     Left_Opnd => New_Reference_To (Var_N, Sloc_N),
                     Right_Opnd => Make_Integer_Literal (Sloc_N, Uint_0)),

                 Right_Opnd =>
                   Make_Op_Eq (Sloc_N,
                     Left_Opnd =>
                       Make_Indexed_Component (Sloc_N,
                         Prefix =>
                           Make_Explicit_Dereference (Sloc_N,
                             Prefix =>  
                               Make_Selected_Component (Sloc_N,
                                 Prefix => 
                                   Make_DT_Access (Sloc_N, Left, Left_Type),
                                 Selector_Name => 
                                   Make_DT_Component (Sloc_N, Left_Type, 2))),
                         Expressions => 
                           New_List_1 (New_Reference_To (Var_N, Sloc_N))),

                     Right_Opnd =>
                       Make_Unchecked_Type_Conversion (Sloc_N,
                         Subtype_Mark => 
                           New_Reference_To (RTE (RE_Tag), Sloc_N),
                         Expression =>
                           New_Reference_To (
                             Access_Disp_Table (Right_Type), Sloc_N)))));
      end if;
   end Tagged_Membership;

end Exp_Ch4;
