------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 4                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.164 $                            --
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
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Util; use Exp_Util;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Output;   use Output;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Util; use Sem_Util;
with Sem_Type; use Sem_Type;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Tbuild;   use Tbuild;

package body Sem_Ch4 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Expanded_Operator (N : Node_Id; Op_Id : Entity_Id);
   --  Analyze a call of the form P."+", etc. where the prefix of the call
   --  is an expanded name whose selector is an operator name, and one
   --  possible interpretation of the name is a predefined operator.

   procedure Analyze_Overloaded_Indexed_Component (N : Node_Id);
   --  If the prefix of an indexed component is overloaded, the proper
   --  interpretation is selected by the index types and the context.

   procedure Analyze_Overloaded_Selected_Component (N : Node_Id);
   --  If the prefix of a selected_component is overloaded, the proper
   --  interpretation that yields a record type with the proper selector
   --  name must be selected.

   procedure Analyze_User_Defined_Binary_Op (N : Node_Id;
                                             Op_Id : Entity_Id);
   --  A user-defined operator is resolved like a function, but instead of
   --  a list of actuals it is presented with the left and right operands
   --  of an operator node.

   procedure Analyze_User_Defined_Unary_Op (N : Node_Id;
                                             Op_Id : Entity_Id);

   procedure Insert_Explicit_Dereference (N : Node_Id);
   --  In a context that requires a composite or subprogram type and
   --  where a prefix is an access type, insert an explicit dereference.

   procedure Analyze_One_Call (N : Node_Id; Nam : Entity_Id; Report : Boolean);
   --  Check one interpretation of an overloaded subprogram name
   --  for compatibility with the types of the actuals in a call.
   --  If there is a single interpretation which does not match,
   --  report error if Report is set to True.

   procedure Find_Arithmetic_Types (L, R  : Node_Id;
                                    Op_Id : Entity_Id;
                                    N     : Node_Id);
   --  L and R are the operands of an arithmetic operator. Find
   --  consistent pairs of interpretations for L and R that have a
   --  numeric type consistent with the semantics of the operator.

   procedure Find_Comparison_Types (L, R  : Node_Id;
                                    Op_Id : Entity_Id;
                                    N     : Node_Id);
   --  L and R are operands of a comparison operator. Find consistent
   --  pairs of interpretations for L and R.

   procedure Find_Concatenation_Types (L, R  : Node_Id;
                                       Op_Id : Entity_Id;
                                       N     : Node_Id);
   --  for the four varieties of concatenation.

   procedure Find_Equality_Types (L, R  : Node_Id;
                                  Op_Id : Entity_Id;
                                  N     : Node_Id);
   --  Ditto for equality operators.

   procedure Find_Boolean_Types (L, R  : Node_Id;
                                 Op_Id : Entity_Id;
                                 N     : Node_Id);
   --  Ditto for binary logical operations.

   procedure Find_Negation_Types (R : Node_Id; Op_Id : Entity_Id; N : Node_Id);
   --  negation operator.

   procedure Find_Unary_Types (R : Node_Id; Op_Id : Entity_Id; N : Node_Id);
   --  Unary arithmetic types: plus, minus, abs.

   procedure Check_Arithmetic_Pair (T1, T2  : Entity_Id;
                                    Op_Id   : Entity_Id;
                                    N       : Node_Id);
   --  Subsidiary procedure to Find_Arithmetic_Types. T1 and T2 are valid
   --  types for left and right operand. Determine whether they constitute
   --  a valid pair for the given operator, and record the corresponding
   --  interpretation of the operator node.

   procedure Operator_Check (N : Node_Id);
   --  Verify that an operator has received some valid interpretation.
   --  If none was found, determine whether a use clause would make the
   --  operation legal.

   procedure Rewrite_Operator_As_Call (N : Node_Id; Nam : Entity_Id);
   --  If an operator node resolves to a call to a user-defined operator,
   --  rewrite the node as a function call.

   ------------------------------
   -- Rewrite_Operator_As_Call --
   ------------------------------

   procedure Rewrite_Operator_As_Call (N : Node_Id; Nam : Entity_Id) is
      L, R : Node_Id;
      Actuals :  List_Id := New_List;

   begin
      if Nkind (N) in  N_Binary_Op then
         Append (Left_Opnd (N), Actuals);
      end if;

      Append (Right_Opnd (N), Actuals);

      Change_Node (N, N_Function_Call);
      Set_Etype   (N, Etype (Nam));
      Set_Name    (N, New_Occurrence_Of (Nam, Sloc (N)));
      Set_Parameter_Associations (N, Actuals);
   end Rewrite_Operator_As_Call;

   -----------------------
   -- Analyze_Aggregate --
   -----------------------

   --  For the moment, all Analyze_Aggregate does is to set the Etype, since
   --  all the work is done in Resolve_Aggregate. This is a bit peculiar, and
   --  should be fixed later to work like the other Analyze/Resolve pairs.

   procedure Analyze_Aggregate (N : Node_Id) is
   begin
      Set_Etype (N, Any_Composite);
   end Analyze_Aggregate;

   -----------------------
   -- Analyze_Allocator --
   -----------------------

   procedure Analyze_Allocator (N : Node_Id) is
      E        : constant Node_Id := Expression (N);
      Acc_Type : Entity_Id;
      Type_Id  : Entity_Id;
      Loc      : constant Source_Ptr := Sloc (N);

   begin
      if Nkind (E) = N_Qualified_Expression then
         Acc_Type := New_Implicit_Type (Loc);
         Set_Ekind (Acc_Type, E_Allocator_Type);
         Set_Etype (Acc_Type, Acc_Type);
         Find_Type (Subtype_Mark (E));
         Type_Id := Entity (Subtype_Mark (E));
         Set_Directly_Designated_Type (Acc_Type, Type_Id);

         Analyze (Expression (E));

         --  When allocator's form is : new T'Class'(Exp)
         --   * an Equivalent type if generated (Expand_Class_Subtype)
         --   * the node is expanded into 
         --         Acc_Type (new Equiv_Type'(Equiv_Type (Exp)))

         if Is_Class_Type (Type_Id) then
            Expand_Class_Allocator (N, Acc_Type, 
              New_Class_Subtype (N, Type_Id));
            return;
         end if;

      else
         declare
            Desig_Type     : Entity_Id;
            Subtype_Decl   : Node_Id;
            Def_Id         : Entity_Id;
            New_Alloc      : Node_Id;

         begin
            --  If the allocator includes a N_Subtype_Indication then a
            --  constraint is present, otherwise the node is a subtype mark.
            --  Introduce an explicit subtype declaration into the tree
            --  defining some anonymous subtype and rewrite the allocator to
            --  use this subtype rather than the subtype indication.

            --  Change the allocator node to an N_Expression_Actions, where
            --  the actions just consist of the subtype declaration just
            --  introduced and the expression is the rewritten allocator.
            --  Analyze is then called on the rewritten node for the analysis.

            --  It is important to introduce the explicit subtype declaration
            --  so that the bounds of the subtype indication are attached to
            --  the tree in case the allocator is inside a generic unit.

            if Nkind (E) = N_Subtype_Indication then
               Subtype_Decl := New_Node (N_Subtype_Declaration, Loc);
               Def_Id :=
                 Make_Defining_Identifier (Loc, 
                   Chars => New_Internal_Name ("subtype"));
               Set_Defining_Identifier (Subtype_Decl, Def_Id);
               Set_Subtype_Indication (Subtype_Decl, E);

               New_Alloc := New_Node (N_Allocator, Loc);
               Set_Expression (New_Alloc,
                               Make_Identifier (Loc, Chars (Def_Id)));

               Rewrite_Substitute_Tree (N,
                 Make_Expression_Actions (Loc,
                   Actions    => New_List_1 (Subtype_Decl),
                   Expression => New_Alloc));

               Analyze (N);
               return;
            else
               Desig_Type := Process_Subtype (E);
               Acc_Type := New_Implicit_Type (Sloc (N));
               Set_Ekind (Acc_Type, E_Allocator_Type);
               Set_Etype (Acc_Type, Acc_Type);
               Set_Directly_Designated_Type (Acc_Type, Desig_Type);
            end if;
         end;
      end if;

      Set_Etype (N, Acc_Type);
   end Analyze_Allocator;

   ---------------------------
   -- Analyze_Arithmetic_Op --
   ---------------------------

   procedure Analyze_Arithmetic_Op (N : Node_Id) is
      Index : Interp_Index;
      It : Interp;
      L  : constant Node_Id := Left_Opnd (N);
      R  : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id;

   begin
      Set_Etype (N, Any_Type);
      Analyze_Expression (L);
      Analyze_Expression (R);
      Op_Id := Get_Name_Entity_Id (Chars (N));

      while Present (Op_Id) loop

         if Ekind (Op_Id) = E_Operator
           and then Present (Next_Entity (First_Entity (Op_Id)))
         then
            Find_Arithmetic_Types (L, R, Op_Id, N);
         else
            Analyze_User_Defined_Binary_Op (N, Op_Id);
         end if;

         Op_Id := Homonym (Op_Id);
      end loop;

      Operator_Check (N);
   end Analyze_Arithmetic_Op;

   ------------------
   -- Analyze_Call --
   ------------------

   --  Function, procedure, and entry calls are checked here. E is the prefix
   --  of the call (which may be overloaded). The actuals have been analyzed
   --  and may themselves be overloaded. On exit from this procedure, the node
   --  N may have zero, one or more interpretations. In the first case an error
   --  message is produced. In the last case, the node is flagged as overloaded
   --  and the interpretations are collected in All_Interp.

   --  If the prefix is an Access_To_Subprogram, it cannot be overloaded, but
   --  the type-checking is similar to that of other calls.

   procedure Analyze_Call (N : Node_Id) is
      Actuals : constant List_Id := Parameter_Associations (N);
      E       : constant Entity_Id := Name (N);
      I       : Interp_Index;
      It      : Interp;
      Nam     : Entity_Id;
      Found   : Boolean := False;

   begin
      --  Initialize the type of the result of the call to
      --  Universal type, to prevent cascaded errors.

      Set_Etype (N, Any_Type);

      if not Is_Overloaded (E) then

         --  Only one interpretation to check

         if Ekind (Etype (E)) = E_Subprogram_Type then
            Nam := Etype (E);

         elsif Is_Access_Type (Etype (E))
           and then Ekind (Designated_Type (Etype (E))) = E_Subprogram_Type
         then
            Nam := Designated_Type (Etype (E));
            Insert_Explicit_Dereference (E);

         else
            Nam := Entity (E);

            if not Is_Overloadable (Nam) then
               Error_Msg_N ("invalid prefix in call", E);
               return;
            end if;
         end if;

         Analyze_One_Call (N, Nam, True);

      else
         Get_First_Interp (E, I, It);

         while Present (It.Nam) loop
            Nam := It.Nam;

            Analyze_One_Call (N, Nam, False);
            Get_Next_Interp (I, It);
         end loop;

         if Etype (N) = Any_Type then

            --  None of the interpretations is compatible with the actuals

            Error_Msg_N ("invalid parameter list in call", E);

         elsif not Is_Overloaded (N) then

            --  Resolution yields a single interpretation. Verify that
            --  is has the proper capitalization.

            Set_Entity_With_Style_Check (E, Entity (E));
         end if;

         End_Interp_List;
      end if;
   end Analyze_Call;

   ---------------------------
   -- Analyze_Comparison_Op --
   ---------------------------

   procedure Analyze_Comparison_Op (N : Node_Id) is
      L : constant Node_Id := Left_Opnd (N);
      R : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id;

   begin
      Set_Etype (N, Any_Type);
      Analyze_Expression (L);
      Analyze_Expression (R);
      Op_Id := Get_Name_Entity_Id (Chars (N));

      while Present (Op_Id) loop

         if Ekind (Op_Id) = E_Operator then
            Find_Comparison_Types (L, R, Op_Id, N);
         else
            Analyze_User_Defined_Binary_Op (N, Op_Id);
         end if;

         Op_Id := Homonym (Op_Id);
      end loop;

      Operator_Check (N);
   end Analyze_Comparison_Op;

   ---------------------------
   -- Analyze_Concatenation --
   ---------------------------

   --  If the only one-dimensional array type in scope is String,
   --  this is the resulting type of the operation. Otherwise there
   --  will be a concatenation operation defined for each user-defined
   --  one-dimensional array.

   procedure Analyze_Concatenation (N : Node_Id) is
      L : constant Node_Id := Left_Opnd (N);
      R : constant Node_Id := Right_Opnd (N);
      Op_Id   : Entity_Id;

   begin
      Set_Etype (N, Any_Type);
      Analyze_Expression (L);
      Analyze_Expression (R);

      Op_Id  := Get_Name_Entity_Id (Name_Op_Concat);
      while Present (Op_Id) loop
         if Ekind (Op_Id) = E_Operator then
            Find_Concatenation_Types (L, R, Op_Id, N);

         else
            Analyze_User_Defined_Binary_Op (N, Op_Id);
         end if;
         Op_Id := Homonym (Op_Id);
      end loop;

      Operator_Check (N);
   end Analyze_Concatenation;

   ------------------------
   -- Analyze_Conversion --
   ------------------------

   procedure Analyze_Conversion (N : Node_Id) is
      Expr : constant Node_Id := Expression (N);
      T    : Entity_Id;

   begin
      Find_Type (Subtype_Mark (N));
      T := Entity (Subtype_Mark (N));
      Set_Etype (N, T);
      Analyze_Expression (Expr);

      --  if it is a tagged type, that's a view conversion

      if Is_Name (Expr) and then Is_Tagged_Type (T) then 
         Set_Unchecked_Conversion (N, True);
      end if;

      if Nkind (Expr) = N_Aggregate
         or else Nkind (Expr) = N_String_Literal
         or else (Nkind (Expr) = N_Attribute_Reference
           and then 
             (Chars (Identifier (Expr))  = Name_Access
             or else Chars (Identifier (Expr))  = Name_Unchecked_Access))
      then
         Error_Msg_N
           ("argument of conversion cannot be aggregate, literal or access", 
                   N);
      end if;
   end Analyze_Conversion;

   -------------------------
   -- Analyze_Equality_Op --
   -------------------------

   procedure Analyze_Equality_Op (N : Node_Id) is
      L : constant Node_Id := Left_Opnd (N);
      R : constant Node_Id := Right_Opnd (N);
      Op_Id  : Entity_Id;
      Neg    : Node_Id;
      Eq     : Node_Id;

   begin
      if Chars (N) = Name_Op_Ne then
         Eq  := Make_Op_Eq  (Sloc (N), L, R);
         Neg := Make_Op_Not (Sloc (N), Eq);
         Analyze (Eq);
         Set_Etype (Neg, Standard_Boolean);

         --  Find predefined negation operation.
         Op_Id := Current_Entity (Neg);
         while Ekind (Op_Id) /= E_Operator loop
            Op_Id := Homonym (Op_Id);
         end loop;
         Set_Entity (Neg, Op_Id);
         Rewrite_Substitute_Tree (N, Neg);

      else 
         Set_Etype (N, Any_Type);
         Analyze_Expression (L);
         Analyze_Expression (R);
         Op_Id := Get_Name_Entity_Id (Chars (N));

         while Present (Op_Id) loop

            if Ekind (Op_Id) = E_Operator then
               Find_Equality_Types (L, R, Op_Id, N);
            else
               Analyze_User_Defined_Binary_Op (N, Op_Id);
            end if;

            Op_Id := Homonym (Op_Id);
         end loop;
         Operator_Check (N);
      end if;
   end Analyze_Equality_Op;

   ----------------------------------
   -- Analyze_Explicit_Dereference --
   ----------------------------------

   procedure Analyze_Explicit_Dereference (N : Node_Id) is
      P  : constant Node_Id := Prefix (N);
      T  : Entity_Id;
      I  : Interp_Index;
      It : Interp;

   begin
      Analyze_Expression (P);
      Set_Etype (N, Any_Type);

      if not Is_Overloaded (P) then
         T := Etype (P);

         --  Return if error in prefix

         if T = Any_Type then
            return;

         elsif not Is_Access_Type (T) then
            Error_Msg_N
              ("access type required in prefix of explicit dereference", P);
         else
            Set_Etype (N, Designated_Type (T));
         end if;

      else
         New_Interps (N);
         Get_First_Interp (P, I, It);

         while Present (It.Nam) loop
            T := It.Typ;

            if Is_Access_Type (T) then
               Add_One_Interp (N, Designated_Type (T), Designated_Type (T));
            end if;

            Get_Next_Interp (I, It);
         end loop;

         if Etype (N) = Any_Type then

            --  No access type was found for prefix

            Error_Msg_N
                ("access type required in prefix of explicit dereference", P);
            Set_Etype (N, Any_Type);
         end if;
      end if;
   end Analyze_Explicit_Dereference;

   --------------------------------
   -- Analyze_Expression_Actions --
   --------------------------------

   procedure Analyze_Expression_Actions (N : Node_Id) is
   begin
      Analyze_Declarations (Actions (N));
      Analyze (Expression (N));
      Set_Etype (N, Etype (Expression (N)));

      --  All implicit subtypes that were generated by the expression part
      --  should be become attached to the actions part so they will be
      --  processed before the expression.

      if Is_Non_Empty_List (Implicit_Type_List) then
         Append_List (Implicit_Type_List, Actions (N));
         Implicit_Type_List := New_List;
      end if;
   end Analyze_Expression_Actions;

   -------------------------------
   -- Analyze_Indexed_Component --
   -------------------------------

   procedure Analyze_Indexed_Component (N : Node_Id) is
      Name  : Node_Id := Prefix  (N);
      Expr  : Node_Id := First (Expressions (N));
      Array_Type : Entity_Id;
      Index      : Node_Id;

   begin
      if Is_Overloaded (Name) then
         Analyze_Overloaded_Indexed_Component (N);
      else
         Array_Type := Etype (Name);

         --  Prefix must be appropriate for an array type. Dereference if
         --  prefix is an access type.

         if Is_Access_Type (Array_Type) then
            Array_Type := Designated_Type (Array_Type);

            --  Make_Explicit_Dereference (Name); -- TBSL

         end if;

         if not Is_Array_Type (Array_Type) then
            Error_Msg_N ("array type required in indexed component", Name);
            Set_Etype (N, Any_Type);
            return;
         end if;

         Index := First_Index (Array_Type);

         while Present (Index) and then Present (Expr) loop
            if Has_Compatible_Type (Expr, Etype (Index)) then
               null;

            else
               Set_Etype (N, Any_Type);
               Error_Msg_NE
                 ("invalid index type. Expect &", Expr, Etype (Index));
               return;
            end if;

            Index := Next_Index (Index);
            Expr  := Next (Expr);
         end loop;

         Set_Etype (N, Component_Type (Array_Type));

         if No (Index) and No (Expr) then
            null;

         else
            Error_Msg_N (
              "incorrect number of indices in indexed component", N);
         end if;
      end if;
   end Analyze_Indexed_Component;

   ------------------------
   -- Analyze_Logical_Op --
   ------------------------

   procedure Analyze_Logical_Op (N : Node_Id) is
      L : constant Node_Id := Left_Opnd (N);
      R : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id;

   begin
      Set_Etype (N, Any_Type);
      Analyze_Expression (L);
      Analyze_Expression (R);
      Op_Id := Get_Name_Entity_Id (Chars (N));

      while Present (Op_Id) loop
         if Ekind (Op_Id) = E_Operator then
            Find_Boolean_Types (L, R, Op_Id, N);
         else
            Analyze_User_Defined_Binary_Op (N, Op_Id);
         end if;

         Op_Id := Homonym (Op_Id);
      end loop;

      Operator_Check (N);
   end Analyze_Logical_Op;

   ---------------------------
   -- Analyze_Membership_Op --
   ---------------------------

   procedure Analyze_Membership_Op (N : Node_Id) is
      L : constant Node_Id := Left_Opnd (N);
      R : constant Node_Id := Right_Opnd (N);
      Typ : Entity_Id;

   begin
      Analyze_Expression (L);
      if Nkind (R) = N_Range or else
         (Nkind (R) = N_Attribute_Reference and then
           Get_Attribute_Id (Chars (Identifier (R))) = Attribute_Range)
      then
         Analyze (R);
      else
         --  If not a range, it can only be a subtype-mark.
         Find_Type (R);
      end if;

      Typ := Intersect_Types (L, R);
      Set_Etype (N, Standard_Boolean);
   end Analyze_Membership_Op;

   ----------------------
   -- Analyze_Negation --
   ----------------------

   procedure Analyze_Negation (N : Node_Id) is
      R  : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id;

   begin
      Set_Etype (N, Any_Type);
      Analyze_Expression (R);
      Op_Id := Get_Name_Entity_Id (Chars (N));

      while Present (Op_Id) loop
         if Ekind (Op_Id) = E_Operator then
            Find_Negation_Types (R, Op_Id, N);

         else
            Analyze_User_Defined_Unary_Op (N, Op_Id);
         end if;

         Op_Id := Homonym (Op_Id);
      end loop;

      Operator_Check (N);
   end Analyze_Negation;

   -------------------
   --  Analyze_Null --
   -------------------

   procedure Analyze_Null (N : Node_Id) is
   begin
      Set_Etype (N, Any_Access);
   end Analyze_Null;

   -----------------------------------------
   --  Analyze_N_Parenthesized_Expression --
   -----------------------------------------

   procedure Analyze_N_Parenthesized_Expression (N : Node_Id) is
   begin
      Analyze_Expression (Expression (N));
      Set_Etype (N, Etype (Expression (N)));
   end Analyze_N_Parenthesized_Expression;

   ------------------------
   -- Analyze_Expression --
   ------------------------

   procedure Analyze_Expression (N : Node_Id) is
      Nam : Node_Id;
   begin
      Analyze (N);
      if Is_Name (N) and then Is_Overloadable (Entity (N))
        and then (Ekind (Entity (N)) /= E_Enumeration_Literal
                    or else Is_Overloaded (N))
      then
         Nam := New_Copy (N);

         --  If overloaded, overload set belongs to new copy.

         if Is_Overloaded (N) then
            for Index in 0 .. Interp_Map.Last loop
               if Interp_Map.Table (Index).Node = N then
                  Interp_Map.Table (Index).Node := Nam;
                  exit;
               end if;
            end loop;
         end if;

         --  Change node to parameterless function call 
         Change_Node (N, N_Function_Call);
         Set_Name (N, Nam);
         Set_Sloc (N, Sloc (Nam));
         Analyze_Call (N);
      end if;

   end Analyze_Expression;

   ----------------------------------
   -- Analyze_Qualified_Expression --
   ----------------------------------

   procedure Analyze_Qualified_Expression (N : Node_Id) is
      Mark : constant Entity_Id := Subtype_Mark (N);
      T : Entity_Id;

   begin
      Set_Etype (N, Any_Type);
      Find_Type (Mark);
      T := Entity (Mark);

      if T = Any_Type then
         return;
      end if;

      if Is_Incomplete_Type (T) then
         Error_Msg_N ("premature use of incomplete type&", T);
      end if;

      Analyze_Expression (Expression (N));
      Set_Etype  (N, T);
   end Analyze_Qualified_Expression;

   -------------------
   -- Analyze_Range --
   -------------------

   procedure Analyze_Range (N : Node_Id) is
      L : Node_Id := Low_Bound  (N);
      H : Node_Id := High_Bound (N);
      Typ : Entity_Id;

   begin
      Analyze_Expression (L);
      Analyze_Expression (H);
      Typ := Intersect_Types (L, H);

      if Typ /= Any_Type and then not Is_Discrete_Type (Typ) then
         Error_Msg_N ("type& in range is not discrete type", N);
         Set_Etype (N, Any_Type);
      else
         Set_Etype (N, Typ);
      end if;
   end Analyze_Range;

   -----------------------
   -- Analyze_Reference --
   -----------------------

   procedure Analyze_Reference (N : Node_Id) is
   begin
      null; -- TBD ???
   end Analyze_Reference;

   --------------------------------
   -- Analyze_Selected_Component --
   --------------------------------

   --  Prefix is a record type or a task or protected type. In the
   --  later case, the selector must denote a visible entry.

   procedure Analyze_Selected_Component (N : Node_Id) is

      Comp : Entity_Id;
      Name : constant Node_Id := Prefix (N);
      Sel  : constant Node_Id := Selector_Name (N);
      Prefix_Type : Entity_Id;
      Expr_Type   : Entity_Id;

   begin
      Set_Etype (N, Any_Type);

      if Is_Overloaded (Name) then
         Analyze_Overloaded_Selected_Component (N);
         return;

      elsif Etype (Name) = Any_Type then
         Set_Entity (Sel, Any_Id);
         Set_Etype (Sel, Any_Type);
         return;

      else
         Prefix_Type := Etype (Name);
      end if;

      if Is_Access_Type (Prefix_Type) then
         Prefix_Type := Designated_Type (Prefix_Type);
      end if;

      Comp := First_Entity (Prefix_Type);

      if Is_Record_Type (Prefix_Type) then

         --  Find component with given name

         while Present (Comp) loop

            if Chars (Comp) = Chars (Sel) then
               Set_Entity_With_Style_Check (Sel, Comp);
               Set_Etype (Sel, Etype (Comp));
               Set_Etype (N,   Etype (Comp));
               return;
            end if;

            Comp := Next_Entity (Comp);
         end loop;

      else
         --  Prefix is concurrent type. Find visible operation with given name

         while Present (Comp)
           and then Comp /= First_Private_Entity (Prefix_Type)
         loop
            if Chars (Comp) = Chars (Sel) then
               Set_Entity_With_Style_Check (Sel, Comp);
               Set_Etype (Sel, Etype (Comp));
               Set_Etype (N,   Etype (Comp));

               if Present (Homonym (Comp)) then
                  Collect_Interps (N);
               end if;

               return;
            end if;

            Comp := Next_Entity (Comp);
         end loop;
      end if;

      --  If we exit, the component is not defined in the type.

      Error_Msg_NE ("undefined selector for type&", N, Prefix_Type);
      Set_Entity (Sel, Any_Id);
      Set_Etype (Sel, Any_Type);
   end Analyze_Selected_Component;

   ---------------------------
   -- Analyze_Short_Circuit --
   ---------------------------

   procedure Analyze_Short_Circuit (N : Node_Id) is
      L : constant Node_Id := Left_Opnd (N);
      R : constant Node_Id := Right_Opnd (N);
      Typ : Entity_Id;

   begin
      Analyze_Expression (L);
      Analyze_Expression (R);
      Typ := Intersect_Types (L, R);

      Set_Etype (N, Standard_Boolean);

      if Is_Boolean_Type (Typ) or else Typ = Any_Type then
         null;
      else
         Error_Msg_N ("expect boolean arguments for operator ", N);
      end if;
   end Analyze_Short_Circuit;

   ----------------------
   -- Analyze_Unary_Op --
   ----------------------

   procedure Analyze_Unary_Op (N : Node_Id) is
      R  : constant Node_Id := Right_Opnd (N);
      Op_Id : Entity_Id;

   begin
      Set_Etype (N, Any_Type);
      Analyze_Expression (R);

      Op_Id := Get_Name_Entity_Id (Chars (N));
      while Present (Op_Id) loop

         if Ekind (Op_Id) = E_Operator then
            if No (Next_Entity (First_Entity (Op_Id))) then
               Find_Unary_Types (R, Op_Id,  N);
            end if;

         else
            Analyze_User_Defined_Unary_Op (N, Op_Id);
         end if;

         Op_Id := Homonym (Op_Id);
      end loop;

      Operator_Check (N);
   end Analyze_Unary_Op;

   ------------------------------------------
   -- Analyze_Overloaded_Indexed_Component --
   ------------------------------------------

   procedure Analyze_Overloaded_Indexed_Component (N : Node_Id) is
      Name  : constant Node_Id := Prefix (N);
      Expr  : Node_Id;
      I     : Interp_Index;
      It    : Interp;
      Typ   : Entity_Id;
      Index : Node_Id;
      Found : Boolean;

   begin
      Set_Etype (N, Any_Type);
      New_Interps (N);
      Get_First_Interp (Name, I,  It);

      while Present (It.Nam) loop
         Typ := It.Typ;

         if Is_Access_Type (Typ) then
            Typ := Designated_Type (Typ);
         end if;

         if Is_Array_Type (Typ) then

            --  Got a candidate: verify that index types are compatible

            Index := First_Index (Typ);
            Found := True;

            Expr := First (Expressions (N));

            while Present (Index) and then Present (Expr) loop
               if Has_Compatible_Type (Expr, Etype (Index)) then
                  null;
               else
                  Found := False;
                  Remove_Interp (I);
                  exit;
               end if;

               Index := Next_Index (Index);
               Expr  := Next (Expr);
            end loop;

            if Found and then No (Index) and then No (Expr) then
               Add_One_Interp (N,
                  Etype (Component_Type (Typ)), Etype (Component_Type (Typ)));
            end if;
         end if;

         Get_Next_Interp (I, It);
      end loop;

      if Etype (N) = Any_Type then
         Error_Msg_N ("no legal interpetation for indexed component", N);
         Set_Is_Overloaded (N, False);
      end if;

      End_Interp_List;
   end Analyze_Overloaded_Indexed_Component;

   -------------------------------------------
   -- Analyze_Overloaded_Selected_Component --
   -------------------------------------------

   procedure Analyze_Overloaded_Selected_Component (N : Node_Id) is
   begin
      Unimplemented (N,  "overloaded prefix in  selected component");
   end Analyze_Overloaded_Selected_Component;

   ---------------------------------
   -- Insert_Explicit_Dereference --
   ---------------------------------

   procedure Insert_Explicit_Dereference (N : Node_Id) is
   begin
      Rewrite_Substitute_Tree (N,
        Make_Explicit_Dereference (Sloc (N), Prefix => New_Copy (N)));
      Set_Etype (N, Designated_Type (Etype (Prefix (N))));
   end Insert_Explicit_Dereference;

   ---------------------------
   -- Find_Comparison_Types --
   ---------------------------

   procedure Find_Comparison_Types (L, R  : Node_Id;
                                    Op_Id : Entity_Id;
                                    N     : Node_Id) is
      Index : Interp_Index;
      It    : Interp;
      Found : Boolean := False;

      procedure Try_One_Interp (T1 : Entity_Id) is
         --  The context of the operator plays no role in resolving the
         --  arguments,  so that if there is more than one interpretation
         --  of the operands that is compatible with comparison, 
      begin
         if Valid_Comparison_Arg (T1)
           and then Has_Compatible_Type (R, T1)
         then
            if Found then
               Error_Msg_N ("ambiguous operands for comparison",  N);
               Set_Etype (L, Any_Type);
            else
               Found := True;
               Set_Etype (L, T1);
               Add_One_Interp (N, Op_Id, Standard_Boolean);
            end if;
         end if;
      end Try_One_Interp;

   begin
      if not Is_Overloaded (L) then
         Try_One_Interp (Etype (L));

      else
         Get_First_Interp (L, Index, It);

         while Present (It.Typ) loop
            Try_One_Interp (It.Typ);
            Get_Next_Interp (Index, It);
         end loop;
      end if;
   end Find_Comparison_Types;

   ------------------------------
   -- Find_Concatenation_Types --
   ------------------------------

   procedure Find_Concatenation_Types (L, R  : Node_Id;
                                       Op_Id : Entity_Id;
                                       N     : Node_Id) is
      Op_Type : Entity_Id := Etype (Op_Id);
   begin
      if (Has_Compatible_Type (L, Op_Type)
             or else Has_Compatible_Type (L, Component_Type (Op_Type)))
        and then
         (Has_Compatible_Type (R, Op_Type)
             or else Has_Compatible_Type (R, Component_Type (Op_Type)))
      then
         Add_One_Interp (N, Op_Id, Op_Type);
      end if;
   end Find_Concatenation_Types;

   -------------------------
   -- Find_Equality_Types --
   -------------------------

   procedure Find_Equality_Types (L, R  : Node_Id;
                                  Op_Id : Entity_Id;
                                  N     : Node_Id) is
      Index : Interp_Index;
      It : Interp;
      Found : Boolean := False;

      procedure Try_One_Interp (T1 : Entity_Id) is
         --  The context of the operator plays no role in resolving the
         --  arguments,  so that if there is more than one interpretation
         --  of the operands that is compatible with equality, the construct
         --  is ambiguous and an error can be emitted now.
      begin
         if not Is_Limited_Type (T1)
           and then Has_Compatible_Type (R, T1)
         then
            if Found then
               Error_Msg_N ("ambiguous operands for equality",  N);
               Set_Etype (L, Any_Type);
            else
               Found := True;
               Set_Etype (L, T1);
               Add_One_Interp (N, Op_Id, Standard_Boolean);
            end if;
         end if;
      end Try_One_Interp;
   begin
      if not Is_Overloaded (L) then
         Try_One_Interp (Etype (L));

      else
         Get_First_Interp (L, Index, It);

         while Present (It.Typ) loop
            Try_One_Interp (It.Typ);
            Get_Next_Interp (Index, It);
         end loop;
      end if;
   end Find_Equality_Types;

   -------------------------
   --  Find_Boolean_Types --
   -------------------------

   procedure Find_Boolean_Types (L, R  : Node_Id;
                                 Op_Id : Entity_Id;
                                 N     : Node_Id) is
      Index : Interp_Index;
      It : Interp;

   begin
      if not Is_Overloaded (L) then
         if Valid_Boolean_Arg (Etype (L))
           and then Has_Compatible_Type (R, Etype (L))
         then
            Add_One_Interp (N, Op_Id, Etype (L));
         end if;

      else
         Get_First_Interp (L, Index, It);

         while Present (It.Typ) loop
            if Valid_Boolean_Arg (It.Typ)
              and then Has_Compatible_Type (R, It.Typ)
            then
               Add_One_Interp (N, Op_Id, It.Typ);
            end if;

            Get_Next_Interp (Index, It);
         end loop;
      end if;
   end Find_Boolean_Types;

   -------------------------
   -- Find_Negation_Types --
   -------------------------

   procedure Find_Negation_Types (R : Node_Id; Op_Id : Entity_Id; N : Node_Id)
   is
      Index : Interp_Index;
      It : Interp;
   begin
      if not Is_Overloaded (R) then

         if Valid_Boolean_Arg (Etype (R)) then
            Add_One_Interp (N, Op_Id, Etype (R));
         end if;

      else
         Get_First_Interp (R, Index, It);

         while Present (It.Typ) loop
            if Valid_Boolean_Arg (It.Typ) then
               Add_One_Interp (N, Op_Id, It.Typ);
            end if;

            Get_Next_Interp (Index, It);
         end loop;
      end if;
   end Find_Negation_Types;

   ----------------------
   -- Find_Unary_Types --
   ----------------------

   procedure Find_Unary_Types (R : Node_Id; Op_Id : Entity_Id; N : Node_Id)
   is
      Index : Interp_Index;
      It : Interp;
   begin

      if not Is_Overloaded (R) then

         if Is_Numeric_Type (Etype (R)) then
            Add_One_Interp (N, Op_Id, Etype (R));
         end if;

      else
         Get_First_Interp (R, Index, It);

         while Present (It.Typ) loop
            if Is_Numeric_Type (It.Typ) then
               Add_One_Interp (N, Op_Id, It.Typ);
            end if;

            Get_Next_Interp (Index, It);
         end loop;
      end if;
   end Find_Unary_Types;

   ---------------------------
   -- Find_Arithmetic_Types --
   ---------------------------

   procedure Find_Arithmetic_Types (L, R  : Node_Id;
                                    Op_Id : Entity_Id;
                                    N     : Node_Id) is
      Index1, Index2 : Interp_Index;
      It1, It2 : Interp;

      procedure Check_Right_Argument (T : Entity_Id) is
      begin
         if not Is_Overloaded (R) then
            Check_Arithmetic_Pair (T, Etype (R), Op_Id,  N);
         else
            Get_First_Interp (R, Index2, It2);

            while Present (It2.Typ) loop
               Check_Arithmetic_Pair (T, It2.Typ, Op_Id, N);
               Get_Next_Interp (Index2, It2);
            end loop;
         end if;
      end Check_Right_Argument;

   begin
      if not Is_Overloaded (L) then
         Check_Right_Argument (Etype (L));

      else
         Get_First_Interp (L, Index1, It1);

         while Present (It1.Typ) loop
            Check_Right_Argument (It1.Typ);
            Get_Next_Interp (Index1, It1);
         end loop;
      end if;

   end Find_Arithmetic_Types;

   ---------------------------
   -- Check_Arithmetic_Pair --
   ---------------------------

   procedure Check_Arithmetic_Pair (T1, T2  : Entity_Id;
                                    Op_Id   : Entity_Id;
                                    N       : Node_Id) is

      Op_Name : Name_Id := Chars (Op_Id);

      function Specific_Type (T1, T2 : Entity_Id) return Entity_Id is
      begin
         if T1 = Universal_Integer or else T1 = Universal_Real then
            return Base_Type (T2);
         else
            return Base_Type (T1);
         end if;
      end Specific_Type;

   begin
      if Op_Name = Name_Op_Add or else Op_Name = Name_Op_Subtract then
         if Is_Numeric_Type (T1)
           and then (Covers (T1, T2) or else Covers (T2, T1))
         then
            Add_One_Interp (N, Op_Id, Specific_Type (T1, T2));
         end if;

      elsif Op_Name = Name_Op_Multiply or else Op_Name = Name_Op_Divide then
         if Is_Numeric_Type (T1)
           and then (Covers (T1, T2) or else Covers (T2, T1))
         then
            Add_One_Interp (N, Op_Id, Specific_Type (T1, T2));

         elsif (Is_Fixed_Type (T1) or else T1 = Universal_Real)
           and then Is_Integer_Type (T2)
         then
            Add_One_Interp (N, Op_Id, T1);

         elsif (Is_Fixed_Type (T2) or else T2 = Universal_Real)
           and then Is_Integer_Type (T1)
           and then Nkind (N) = N_Op_Multiply
         then
            Add_One_Interp (N, Op_Id, T1);
         end if;

      elsif (Op_Name = Name_Op_Mod
        or else Op_Name = Name_Op_Rem)
      then
         if Is_Integer_Type (T1)
           and then (Covers (T1, T2) or else Covers (T2, T1)) then
            Add_One_Interp (N, Op_Id, Specific_Type (T1, T2));
         end if;

      elsif Op_Name = Name_Op_Expon then
         if Is_Numeric_Type (T1)
           and then Is_Integer_Type (T2) then
            Add_One_Interp (N, Op_Id, Base_Type (T1));
         end if;

      end if;
   end Check_Arithmetic_Pair;

   ------------------------------------
   --  Analyze_User_Defined_Unary_Op --
   ------------------------------------

   procedure Analyze_User_Defined_Unary_Op (N : Node_Id;
                                             Op_Id : Entity_Id) is
      F : Entity_Id := First_Formal (Op_Id);

   begin
      --  Verify that Op_Id is a visible unary function

      if Ekind (Op_Id) = E_Function
        and then No (Next_Formal (F))
        and then (Is_Directly_Visible (Op_Id)
                        or else Is_Use_Visible (Op_Id))
        and then Has_Compatible_Type (Right_Opnd (N), Etype (F))
      then
         Add_One_Interp (N, Op_Id, Etype (Op_Id));
      end if;
   end Analyze_User_Defined_Unary_Op;

   -------------------------------------
   --  Analyze_User_Defined_Binary_Op --
   -------------------------------------

   procedure Analyze_User_Defined_Binary_Op (N : Node_Id;
                                             Op_Id : Entity_Id) is
      F1 : Entity_Id := First_Formal (Op_Id);
      F2 : Entity_Id := Next_Formal (F1);

   begin
      --  Verify that Op_Id is a visible binary function

      if Ekind (Op_Id) = E_Function
        and then Present (F2)
        and then (Is_Directly_Visible (Op_Id)
                        or else Is_Use_Visible (Op_Id))
        and then Has_Compatible_Type (Left_Opnd (N), Etype (F1))
        and then Has_Compatible_Type (Right_Opnd (N), Etype (F2))
      then
         Add_One_Interp (N, Op_Id, Etype (Op_Id));

         if Debug_Flag_E then
            Write_Str ("user defined operator ");
            Write_Name (Chars (Op_Id));
            Write_Str (" on node ");
            Write_Int (Int (N));
            Write_Eol;
         end if;
      end if;
   end Analyze_User_Defined_Binary_Op;

   ----------------------
   -- Analyze_One_Call --
   ----------------------

   procedure Analyze_One_Call (N : Node_Id; Nam : Entity_Id; Report : Boolean)
   is
      Actuals : constant List_Id := Parameter_Associations (N);
      Formal  : Entity_Id;
      Actual  : Node_Id;

   begin
      if not Normalize_Actuals (N, Nam, Report) then

         --  Mismatch in number or names of parameters

         if Debug_Flag_E then
            Write_Str (" normalization fails in call ");
            Write_Int (Int (N));
            Write_Str (" with subprogram ");
            Write_Int (Int (Nam));
            Write_Eol;
         end if;

      elsif not List_Present (Actuals) then

         --  If Normalize succeeds, then there are default parameters for
         --  all formals.

         Add_One_Interp (N, Nam, Etype (Nam));

         --  Set the entity pointer,  unless it is an indirect call, in
         --  which case the prefix is an expression without a unique name.

         if not Is_Type (Nam) then
            Set_Entity (Name (N), Nam);
         end if;

         if Debug_Flag_E and not Report then
            Write_Str (" Overloaded call ");
            Write_Int (Int (N));
            Write_Str (" compatible with ");
            Write_Int (Int (Nam));
            Write_Eol;
         end if;

      elsif Ekind (Nam) = E_Operator then

         --  This can occur when the prefix of the call is an expanded name
         --  whose selector is an operator name.

         Analyze_Expanded_Operator (N, Nam);

      else
         --  Normalize_Actuals has chained the named associations in the
         --  correct order of the formals.

         Actual := First_Actual (N);
         Formal := First_Formal (Nam);

         while Present (Actual) and then Present (Formal) loop
            if (Nkind (Parent (Actual)) /= N_Parameter_Association
              or else Chars (Selector_Name (Parent (Actual))) = Chars (Formal))
            then
               if Has_Compatible_Type (Actual, Etype (Formal)) then
                  Actual := Next_Actual (Actual);
                  Formal := Next_Formal (Formal);

               else
                  if Debug_Flag_E then
                     Write_Str (" type checking fails in call ");
                     Write_Int (Int (N));
                     Write_Str (" with formal ");
                     Write_Int (Int (Formal));
                     Write_Str (" in subprogram ");
                     Write_Int (Int (Nam));
                     Write_Eol;
                  end if;

                  if Report then
                     Error_Msg_Node_2 := Etype (Actual);
                     Error_Msg_NE
                       ("type& expected, found&", Actual, Etype (Formal));
                  end if;

                  return;
               end if;

            else
               --  Normalize_Actuals has verified that a default value exists
               --  for this formal. Current actual names a subsequent formal.

               Formal := Next_Formal (Formal);
            end if;
         end loop;

         --  On exit, all actuals match.

         Add_One_Interp (N, Nam, Etype (Nam));
         if not Is_Type (Nam) then
            Set_Entity (Name (N), Nam);
         end if;

         if Debug_Flag_E and not Report then
            Write_Str (" Overloaded call ");
            Write_Int (Int (N));
            Write_Str (" compatible with ");
            Write_Int (Int (Nam));
            Write_Eol;
         end if;

      end if;
   end Analyze_One_Call;

   --------------------------------
   --  Analyze_Expanded_Operator --
   --------------------------------

   procedure Analyze_Expanded_Operator (N : Node_Id; Op_Id : Entity_Id) is
      E       : Node_Id := Name (N);
      Op_Name : Name_Id := Chars (Selector_Name (E));
      Act1    : Node_Id := First_Actual (N);
      Act2    : Node_Id := Next_Actual (Act1);

   begin
      if Present (Act2) then

         --  Maybe binary operators

         if Present (Next_Actual (Act2)) then

            --  Too many actuals for an operator

            return;

         elsif Op_Name = Name_Op_Add
           or else Op_Name = Name_Op_Subtract
           or else Op_Name = Name_Op_Multiply
           or else Op_Name = Name_Op_Divide
           or else Op_Name = Name_Op_Mod
           or else Op_Name = Name_Op_Rem
           or else Op_Name = Name_Op_Expon
         then
            Find_Arithmetic_Types (Act1, Act2, Op_Id, N);

         elsif Op_Name =  Name_Op_And
           or else Op_Name = Name_Op_Or
           or else Op_Name = Name_Op_Xor
         then
            Find_Boolean_Types (Act1, Act2, Op_Id, N);

         elsif Op_Name = Name_Op_Lt
           or else Op_Name = Name_Op_Le
           or else Op_Name = Name_Op_Gt
           or else Op_Name = Name_Op_Ge
           or else Op_Name = Name_Op_Eq
           or else Op_Name = Name_Op_Ne
         then
            Find_Comparison_Types (Act1, Act2, Op_Id,  N);

         elsif Op_Name = Name_Op_Concat then
            Find_Concatenation_Types (Act1, Act2, Op_Id, N);
         end if;

      else
         --  Unary operators

         if Op_Name = Name_Op_Subtract
           or else Op_Name = Name_Op_Add
           or else Op_Name = Name_Op_Abs
         then
            Find_Unary_Types (Act1, Op_Id, N);

         elsif Op_Name = Name_Op_Not then
            Find_Negation_Types (Act1, Op_Id, N);

         end if;
      end if;
   end Analyze_Expanded_Operator;

   --------------------
   -- Operator_Check --
   --------------------

   procedure Operator_Check (N : Node_Id) is
   begin
      if Etype (N) = Any_Type then

         --  Looks bad, but don't complain if either operand has no type,
         --  since that simply means that we have a propagated error.

         if Etype (Right_Opnd (N)) = Any_Type
           or else (Nkind (N) in N_Binary_Op
                      and then Etype (Left_Opnd (N)) = Any_Type)
         then
            null;
         else
            Error_Msg_N ("invalid operand types for operator&", N);
         end if;
      end if;

      --  TBSL: Indicate whether a use clause would make operator visible.

   end Operator_Check;

end Sem_Ch4;
