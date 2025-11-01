------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ R E S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.67 $                             --
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
with Expand;
with Nmake;     use Nmake;
with Opt;       use Opt;
with Output;    use Output;
with Rtsfind;   use Rtsfind;
with Sem;       use Sem;
with Sem_Attr;  use Sem_Attr;
with Sem_Ch3;   use Sem_Ch3;
with Sem_Ch4;   use Sem_Ch4;
with Sem_Ch5;   use Sem_Ch5;
with Sem_Disp;  use Sem_Disp;
with Sem_Eval;  use Sem_Eval;
with Sem_Util;  use Sem_Util;
with Sem_Type;  use Sem_Type;
with Sinput;    use Sinput;
with Stand;     use Stand;
with Sinfo;     use Sinfo;
with Snames;    use Snames;
with Stringt;   use Stringt;
with Tbuild;    use Tbuild;
with Uintp;     use Uintp;

package body Sem_Res is

   -----------------------
   -- Local Subprograms --
   -----------------------

   --  Second pass (top-down) type checking and overload resolution procedures
   --  Typ is the type required by context. These procedures propagate the
   --  type information recursively to the descendants of N. If the node
   --  is not overloaded, its Etype is established in the first pass. If
   --  overloaded,  the Resolve routines set the correct type. For arith.
   --  operators, the Etype is the base type of the context.

   --  Note that Resolve_Attribute is separated off in Sem_Attr

   procedure Resolve_Aggregate                (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Allocator                (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Arithmetic_Op            (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Array_Aggregate          (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Call                     (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Character_Literal        (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Comparison_Op            (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Conversion               (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Equality_Op              (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Explicit_Dereference     (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Extension_Aggregate      (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Expression_Actions       (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Identifier               (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Indexed_Component        (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Integer_Literal          (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Logical_Op               (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Membership_Op            (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Null                     (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Operator_Symbol          (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Op_Concat                (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Op_Expon                 (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Op_Not                   (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Parenthesized_Expression (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Qualified_Expression     (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Range                    (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Record_Aggregate         (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Real_Literal             (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Reference                (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Selected_Component       (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Slice                    (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Short_Circuit            (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_String_Literal           (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Unary_Op                 (N : Node_Id; Typ : Entity_Id);

   procedure Rewrite_Operator_As_Call (N : Node_Id; Nam : Entity_Id);
   --  If an operator node resolves to a call to a user-defined operator,
   --  rewrite the node as a function call.

   procedure Make_Call_Into_Operator (N : Node_Id);
   --  Inverse transformation: if an operator is given in functional notation,
   --  then after resolving the node, transform into an operator node, so
   --  that operands are resolved properly. Recall that predefined operators
   --  do not have a full signature and special resolution rules apply.

   function Valid_Conversion (N : Node_Id) return Boolean; 
   --  Verify legality rules given in 4.6 (8-23)

   ------------------------------
   -- Resolve_Complete_Context --
   ------------------------------

   procedure Resolve_Complete_Context (N : Node_Id; Typ : Entity_Id) is
   begin
      Resolve_Subexpr (N, Typ);

      --  Note: we used to reset the overload data structures at this point
      --  with a call to Init_Interp, but that's wrong, because in the case
      --  of expression actions, it is possible for complete contexts to be
      --  properly nested in a recursive sense, and destroying the overloading
      --  structures after completing analysis of the inner complete context
      --  would sabotage overload resolution for the outer context. Now we
      --  clear the data structures between statement and declaration
      --  boundaries (in the case of declarations, being careful to do it
      --  only if we are not processing expression actions).

      --  Now we do static evaluation on the result

      Static_Evaluation (N);
   end Resolve_Complete_Context;

   ---------------------
   -- Resolve_Subexpr --
   ---------------------

   procedure Resolve_Subexpr (N : Node_Id; Typ : Entity_Id) is
      I                : Interp_Index;
      It               : Interp;
      Found            : Boolean := False;
      Nam              : Node_Id;
      Seen             : Entity_Id;
      Ctx_Type         : Entity_Id := Typ;
      Expr_Type        : Entity_Id;

   begin
      pragma Debug (Debug_A_Entry ("resolving  ", N));

      --  Return if already analyzed

      if Analyzed (N) then
         pragma Debug
           (Debug_A_Exit ("resolving  ", N, "  (done, already analyzed)"));
         return;

      --  Return if type = Any_Type (previous error encountered)

      elsif Etype (N) = Any_Type then
         pragma Debug
           (Debug_A_Exit ("resolving  ", N, "  (done, Etype = Any_Type)"));
         return;
      end if;

      --  First deal with a parameterless function call, where the node must
      --  be rebuilt to be a function call (just looked like a name till now)

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

         --  Change node to parameterless function call (note that the 
         --  Parameter_Associations associations field is left set to Empty, 
         --  its normal default value since there are no parameters)

         Change_Node (N, N_Function_Call);
         Set_Name (N, Nam);
         Set_Sloc (N, Sloc (Nam));
         Analyze_Call (N);
      end if;

      --  If not overloaded, then we know the type, and all that needs doing
      --  is to check that this type is compatible with the context.

      if not Is_Overloaded (N) then
         Found := Covers (Typ, Etype (N));
         Expr_Type := Etype (N);

      --  In the overloaded case, we must select the interpretation that
      --  is compatible with the context (i.e. the type passed to Resolve)

      else
         Get_First_Interp (N, I, It);

         while Present (It.Typ) loop
            if Covers (Typ, It.Typ) then
               if Found then

                  --  Could be a tag-indeterminate call which resolves
                  --  statically to the operation on the root of the class.
                  --  Keep the interpretation that is closest to the root type.

                  if Is_Dispatching_Operation (It.Nam) and then
                     Root_Type (Find_Dispatching_Type (It.Nam)) =
                     Root_Type (Find_Dispatching_Type (Seen))
                  then
                     declare
                        T1 : Entity_Id := Find_Dispatching_Type (It.Nam);
                        T2 : Entity_Id := Find_Dispatching_Type (Seen);
                        R  : Entity_Id := Root_Type (T1);

                     begin
                        while T1 /= R and then T2 /= R loop
                           T1 := Etype (T1);
                           T2 := Etype (T2);
                        end loop;

                        if T1 = R then 
                           Seen := It.Nam;
                        end if;
                     end;

                  else
                     --  More than one interpretation. Use preference
                     --  rules,  and check operator visibility and hiding.

                     Error_Msg_Sloc_1 := Sloc (Seen);
                     Seen := Disambiguate (It.Nam, Seen, Typ);  

                     if Seen = Any_Id then
                        Error_Msg_Sloc_2 := Sloc (It.Nam);
                        Error_Msg_N (
                          " ambiguity: homographs declared at lines #, and #",
                          N);
                        exit;
                     end if;
                  end if;

               else
                  --  This is the first interpretation.
                  Found := True;
                  Seen  := It.Nam;
                  Expr_Type := It.Typ;
               end if;

               if Nkind (N) not in N_Op 
                 and then Nkind (N) /= N_Explicit_Dereference
                 and then Nkind (N) /= N_Attribute_Reference
               then
                  Set_Etype (Name (N), Expr_Type);

                  --  If procedure or function, set entity pointer for prefix

                  if Nkind (N) = N_Procedure_Call_Statement
                    or else Nkind (N) = N_Function_Call
                  then
                     Set_Entity (Name (N), Seen);
                  end if;

               elsif Nkind (N) in N_Op then
                  --  For operators, set only the name. Each resolve routine
                  --  sets the type.

                  Set_Entity (N, Seen);
               else
                  --  Dereferences, indexed expressions, selected components
                  --  or attributes with overloaded prefixes.
                  null;
               end if;
            end if;

            Get_Next_Interp (I, It);
         end loop;
      end if;

      --  At this stage Found indicates whether or not an acceptable
      --  interpretation exists. If not, then we have an error, except
      --  that if the context is Any_Type as a result of some other error,
      --  then we suppress the error report.

      if not Found then
         if Typ /= Any_Type then

            --  If type we are looking for is Void, then this is the
            --  procedure call case, and the error is simply that what
            --  we gave is not a procedure name (we think of procedure
            --  calls as expressions with types internally, but the user
            --  doesn't think of them this way!

            if Typ = Standard_Void_Type then
               Error_Msg_N ("expect procedure name in procedure call", N);

            --  Otherwise we do have a subexpression with the wrong type

            else

               --  Check for the case of an allocator which uses an access
               --  type instead of the designated type. This is a common
               --  error and we would like a nice error message

               if Nkind (N) = N_Allocator
                 and then Ekind (Typ) in Access_Kind
                 and then Ekind (Etype (N)) in Access_Kind
                 and then Designated_Type (Etype (N)) = Typ
               then

                  --  This is the special case, and we post the error on
                  --  the operand of the allocator, complaining that we
                  --  expected the designated type of the allocator

                  Error_Msg_Node_2 := Etype (Expression (N));
                  Error_Msg_NE ("type& expected, found&",
                    Expression (N), Designated_Type (Typ));

               --  Case of Class-wide types

               elsif Is_Class_Type (Typ) then

                  Error_Msg_NE ("expression not in `&''Class`", 
                                N, Etype (Typ));

               --  Normal case of looking for Typ, found Etype (N)

               else
                  Error_Msg_Node_2 := Etype (N);
                  Error_Msg_NE ("type& expected, found&", N, Typ);
               end if;
            end if;
         end if;

         --  Make believe it was right, to avoid cascaded errors.

         Set_Etype (N, Typ);
         pragma Debug
           (Debug_A_Exit ("resolving  ", N, " (done, resolution failed)"));
         Set_Analyzed (N, True);
         return;

      --  Here we have an acceptable interpretation for the context

      else
         --  A user-defined operator is tranformed into a function call
         --  at this point, so that further processing knows that operators
         --  are really operators (i.e. are predefined operators)

         if Nkind (N) in N_Op
           and then Present (Entity (N))
           and then Ekind (Entity (N)) /= E_Operator
         then
            Rewrite_Operator_As_Call (N, Entity (N));
         end if;

         --  Propagate type information and normalize tree for various
         --  predefined operations. If the context only imposes a class of
         --  types,  rather than a specific type, propagate the actual type
         --  downward.

         if Typ = Any_Integer or else Typ = Any_Boolean then
            Ctx_Type := Expr_Type;
         end if;

         case N_Subexpr'(Nkind (N)) is

            when N_Aggregate => Resolve_Aggregate                (N, Ctx_Type);

            when N_Allocator => Resolve_Allocator                (N, Ctx_Type);

            when N_Attribute_Reference
                             => Resolve_Attribute                (N, Ctx_Type);

            when N_Character_Literal
                             => Resolve_Character_Literal        (N, Ctx_Type);

            when N_Concat_Multiple => Compiler_Error;

            when N_Extension_Aggregate
                             => Resolve_Extension_Aggregate      (N, Ctx_Type);

            when N_Explicit_Dereference
                             => Resolve_Explicit_Dereference     (N, Ctx_Type);

            when N_Expression_Actions
                             => Resolve_Expression_Actions       (N, Ctx_Type);

            when N_Function_Call | N_Procedure_Call_Statement
                             => Resolve_Call                     (N, Ctx_Type);

            when N_Identifier | N_Expanded_Name
                             => Resolve_Identifier               (N, Ctx_Type);

            when N_Indexed_Component
                             => Resolve_Indexed_Component        (N, Ctx_Type);

            when N_Integer_Literal
                             => Resolve_Integer_Literal          (N, Ctx_Type);

            when N_Null      => Resolve_Null                     (N, Ctx_Type);

            when N_Op_And_Then | N_Op_Or_Else
                             => Resolve_Short_Circuit            (N, Ctx_Type);

            when N_Op_And | N_Op_Or | N_Op_Xor
                             => Resolve_Logical_Op               (N, Ctx_Type);

            when N_Op_In | N_Op_Not_In
                             => Resolve_Membership_Op            (N, Ctx_Type);

            when N_Op_Eq | N_Op_Ne
                             => Resolve_Equality_Op              (N, Ctx_Type);

            when N_Op_Lt | N_Op_Le | N_Op_Gt | N_Op_Ge
                             => Resolve_Comparison_Op            (N, Ctx_Type);

            when N_Op_Not    => Resolve_Op_Not                   (N, Ctx_Type);

            when N_Op_Add    | N_Op_Subtract | N_Op_Multiply |
                 N_Op_Divide | N_Op_Mod      | N_Op_Rem

                             => Resolve_Arithmetic_Op            (N, Ctx_Type);

            when N_Op_Concat => Resolve_Op_Concat                (N, Ctx_Type);

            when N_Op_Expon  => Resolve_Op_Expon                 (N, Ctx_Type);

            when N_Op_Plus | N_Op_Minus  | N_Op_Abs
                             => Resolve_Unary_Op                 (N, Ctx_Type);

            when N_Operator_Symbol
                             => Resolve_Operator_Symbol          (N, Ctx_Type);

            when N_Parenthesized_Expression
                             => Resolve_Parenthesized_Expression (N, Ctx_Type);

            when N_Qualified_Expression
                             => Resolve_Qualified_Expression     (N, Ctx_Type);

            when N_Range     => Resolve_Range                    (N, Ctx_Type);

            when N_Real_Literal
                             => Resolve_Real_Literal             (N, Ctx_Type);

            when N_Reference => Resolve_Reference                (N, Ctx_Type);

            when N_Selected_Component
                             => Resolve_Selected_Component       (N, Ctx_Type);

            when N_Slice     => Resolve_Slice                    (N, Ctx_Type);

            when N_String_Literal
                             => Resolve_String_Literal           (N, Ctx_Type);

            when N_Type_Conversion
                             => Resolve_Conversion               (N, Ctx_Type);

         end case;

         --  Now that the resolution of the type of the node is complete,
         --  and we did not detect an error, we can expand this node and
         --  mark the analysis of the node as completed.

         pragma Debug (Debug_A_Exit ("resolving  ", N, "  (done)"));
         Expand (N);
         Set_Analyzed (N, True);
      end if;

   end Resolve_Subexpr;

   -----------------------
   -- Resolve_Aggregate --
   -----------------------

   procedure Resolve_Aggregate (N : Node_Id; Typ : Entity_Id) is
   begin
      if Is_Array_Type (Typ) then
         Resolve_Array_Aggregate  (N, Typ);

      elsif Is_Record_Type (Typ) then

         --  If Typ is a derived type, we look for the Root_Type of Typ because
         --  the tree of the record, with all the components and specially
         --  the ones in variant cases, is not copied and linked to Typ.

         if Is_Derived_Type (Typ) then
            Resolve_Record_Aggregate (N, Root_Type (Typ));

         else
            Resolve_Record_Aggregate (N, Typ);
         end if;

      else
         Error_Msg_N ("illegal context for aggregate", N);
      end if;

      --  If the node N_Aggregate has been rewritten and replaced by an
      --  Expression_Actions node then we do not want to override the Etype
      --  of this node. So, only when the original N_Aggregate node has been
      --  rewritten with another N_Aggregate node, we set the Etype to be Typ.

      if Nkind (N) = N_Aggregate then
         Set_Etype (N, Typ);
      end if;

   end Resolve_Aggregate;

   -----------------------
   -- Resolve_Allocator --
   -----------------------

   procedure Resolve_Allocator (N : Node_Id; Typ : Entity_Id) is
      E : constant Node_Id := Expression (N);

   begin
      --  Replace general access with specific type

      if Ekind (Etype (N)) = E_Allocator_Type then
         Set_Etype (N, Typ);
      end if;

      if Is_Abstract (Typ) then
         Error_Msg_N ("type of allocator cannot be abstract",  N);
      end if;

      --  For qualified expression, resolve the expression using the
      --  given subtype (nothing to do for type mark, subtype indication)

      if Nkind (E) = N_Qualified_Expression then
         Resolve_Subexpr (Expression (E), Entity (Subtype_Mark (E)));
      end if;
   end Resolve_Allocator;

   ---------------------------
   -- Resolve_Arithmetic_Op --
   ---------------------------

   --  Used for resolving all arithmetic operators except exponentiation

   procedure Resolve_Arithmetic_Op (N : Node_Id; Typ : Entity_Id) is
      L : Node_Id := Left_Opnd (N);
      R : Node_Id := Right_Opnd (N);
      T : Entity_Id;

      B_Typ : constant Entity_Id := Base_Type (Typ);
      --  We do the resolution using the base type, because intermediate values
      --  in expressions always are of the base type, not a subtype of it.

   begin
      Resolve_Subexpr (L, B_Typ);
      Resolve_Subexpr (R, B_Typ);

      --  If one of the arguments was resolved to a non-universal type.
      --  label the result of the operation itself with the same type.

      T := Intersect_Types (L, R);
      Set_Etype (N, Base_Type (T));
   end Resolve_Arithmetic_Op;

   -----------------------------
   -- Resolve_Array_Aggregate --
   -----------------------------

   --  This procedure is the top-level procedure of the resolution
   --  of array aggregates.

   --  First, it calls the procedure Resolve_Multi_Array_Aggregate which does
   --  the semantic analysis on the aggregate. Some flags are modified by this
   --  procedure to determine which algorithm for the expansion of the
   --  aggregate will be called. There are two algorithms :

   --  The first one represented by the function Rewrite_Array_Aggregate is
   --  called only in very special cases : the array type should be constrained
   --  the indices of the array should be static, in the case of a named
   --  aggregate the choices should be all static and when there is an Others
   --  statements the number of values which are represented by Others should
   --  be less then a threshold fixed to 1024 values.

   --  The original tree of the aggregate is replaced by an other aggregate
   --  which contains only positional associations :

   --    (1, 2, 3, 4, 5)        => (1, 2, 3, 4, 5);
   --    (1, 2, 3, others => 4) => (1, 2, 3, 4, 4);
   --    (2 .. 3 | 5 => 1, 1 | 4 => 2)  => (2, 1, 1, 2, 1);
   --    (2 .. 3 | 5 => 1, others => 2) => (2, 1, 1, 2, 1);

   --  The second algorithm used in all other cases builds an expression
   --  actions node containing the declaration of a temporary array whose type
   --  is the type of the aggregate and containing the necessary loops or/and
   --  assignments to fill the temporary with the values specified in the
   --  aggregate. The expression of the node expression actions is the
   --  temporary filled previously. The declaration of the temporary is not
   --  necessary in all cases but it's simpler to create one each time.
   --  the original node is replaced by this expression actions node :

   --    (M .. N => Val) => {<actions>
   --                           Tmp : Array_Type;
   --                           for I in M .. N loop
   --                              Tmp (I) := Val;
   --                           end loop;
   --                        <expression>
   --                           Tmp;}

   --  This algorithm is used specially when the bound of the indices of the
   --  array are dynamic or when the array is an unconstrained array.
   --  When the semantic analysis has been able to determine that a run time
   --  Constraint Error will be raised then the aggregate is modified by :
   --  in the first algorithm, we build a node expression actions containing
   --  raise constraint_error for its actions and a correct aggregate (needed
   --  by the back end) whose values are arbitrary (in fact the first
   --  expression of the original aggregate is choosen) fills the expression
   --  field.

   --    (1, 2, 4, 5) => {<actions> raise Constraint_Error;
   --                     <expression> (1, 1, 1, 1, 1);}

   --  in the second algorithm, we build a node expression actions containing
   --  a declaration of a temporary and the statememt raise constraint error
   --  for its actions and the temporary for its expression.

   --    (1 .. 2 => 1, 3 .. 6 => 2) => {<actions>
   --                                       Tmp : Array_Type;
   --                                       raise Constraint_Error;
   --                                    <expression>
   --                                       Tmp;}

   --  The expansion of the aggregate is called only if we are in a code
   --  generating phase.

   procedure Resolve_Array_Aggregate (N : Node_Id; Typ : Entity_Id) is
      Sloc_N : Source_Ptr := Sloc (N);

      Actions_List : List_Id := New_List;

      New_Aggr     : Node_Id := New_Node (N_Aggregate, Sloc_N);
      Expr_Actions : Node_Id;
      Action       : Node_Id;
      Tmp          : Node_Id;

      Index  : Entity_Id;

      Max_Val : Uint := UI_From_Int (1024);

      Error_Found   : Boolean := False;
      Full_Others   : Boolean := False;
      Static_Choices : Boolean := True;
      Raise_C       : Boolean := False;
      Static_Indices : Boolean := True;
      Others_In_Threshold : Boolean := True;

      procedure Resolve_Multi_Array_Aggregate (N : Node_Id;
                                               Typ, Ind : Entity_Id);
      --  This procedure does the semantic checking on the aggregate.
      --  First, the procedure deals with a named aggregate and specially if
      --  there is an Others statement, then with a positional aggregate.
      --  At the end, some run time checks are done to warn the user that
      --  a Constraint Error will be raised at the execution.

      --  This procedure is recursive in the case of multi-dimensional
      --  aggregates.

      --  The N node always represents a node N_Aggregate, the Typ entity
      --  represents the type of the entire aggregate and the Ind entity
      --  represents the index correspoonding to the aggregate or subaggregate
      --  passed in the parameter N.

      function Rewrite_Array_Aggregate (N : Node_Id; Typ, Ind : Entity_Id)
                 return Node_Id;
      --  This procedure builds a new node N_Aggregate which contains only
      --  positional components.

      --  This procedure is called only if the indices of the array type are
      --  static and if all the choices, in the case of a named aggregate,
      --  are static and in the case of an aggregate with an Others statement
      --  if the number of values covered by Others is less than the threshold.
      --  The tree transformation is the following :

      --     type Ary is array (1..6) of integer;

      --     for a positional aggregate :
      --        A : Ary := (1, 2, 3, 4, others => 5);
      --        => A : Ary := (1, 2, 3, 4, 5, 5);

      --     for a named aggregate :
      --        A : Ary := (2..3 | 6 => 1, 4 => 4, Others => 0);
      --        => A : Ary := (0, 1, 1, 0, 4, 0, 1);

      --  If a constraint Error has been detected during the semantic
      --  analysis, then the aggregate created is a positional aggregate
      --  whose values are the value given to the first association of
      --  the examined aggregate.

      --  This procedure is called recursively in the case of a
      --  multi-dimentional aggregate so that each subaggregate becomes
      --  a positonal subaggregate.

      --  This procedure is called in Resolve_Array_Aggregate after the
      --  semantic analysis of the aggregate.

      --  The N node is always a node N_Aggregate which represents the
      --  aggregate or subaggregate to be transformed. The Typ entity
      --  represents the type of the aggregate. The Ind entity represents
      --  the index corresponding to the aggregate or subaggregate.
      --  The node_Id returned is a node N_Aggregate corresponding to the
      --  transformation of the aggregate or subaggregate.

      function Build_Loop (N : Node_Id; Ind : Entity_Id; L : List_Id)
                 return List_Id;
      --  This procedure builds a list which contains the loops and assignments
      --  that are needed for the expansion of the aggregate. It's part
      --  of the second algorithm of expansion of the aggregate.

      --  First, we look if there's an Others statement in the aggregate which
      --  is represented by the node N. If it's the case, we build the code
      --  corresponding to :

      --     for I in Index_Low_Bound .. Index_High_Bound loop

      --  If we deal with a multi dimensional aggregate, we call recursively
      --  Build_Loop on the expression represented by Others. Else, we build
      --  the code corresponding to the assignment of the expression :

      --     Tmp (List_Of_Indices, L) := Expression;

      --  Then, we deal with the other named associations if they exist :
      --  if the choice of the named association is a range choice then we
      --  build a loop corresponding to the code :

      --     for I in Choice_Low_Bound .. Choice_High_Bound loop

      --  If the aggregate is multi dimensional we call recursively Build_Loop
      --  else we create the code correponding to the assignment :

      --     Tmp (List_Of_Indices_Known, I) := Choice_Expression;

      --  If the choice is a simple choice : if the aggregate is a multi
      --  dimensionla aggregtae we call recursively Build_loop else we create
      --  the code corresponding to the assignment :

      --     Tmp (List_Of_Indices_Know, Choice) := Choice_Expression;

      --  If the aggregate contains positional associations instead of named
      --  associations, we need to generate two sort of code considering the
      --  case of the index Ind corresponding to the aggregate N is an
      --  enumeration type or not. It this is the case, the code created is:

      --     X : Index := Index'First;

      --  For each positional association, an assignement is created and the
      --  value of X is updated to the next enumeration type in the range
      --  Index'first .. Index'last. The code created is:

      --     X := X'succ;
      --     Tmp (List_Of_Indices_Known, X) := Positional;

      --  If the index is not an enumeration type then the code is simpler:

      --     Tmp (List_Of_Indices_Known, Index'First+Position) := Positional;

      --  where Position represents the position (- 1) of the positional
      --  association in the aggregate (the position of the first positional
      --  is zero). the value of Index'first + Position is computed when
      --  analyze is called on this subtree.

      --  The N node correspond to ther aggregate or subaggregate that needs
      --  to be expanded, Ind corresponds to the index in the array of this
      --  subaggregate and L corresponds to the list of known identifiers
      --  at this level of the aggregate : this is necessary for the case
      --  of created nested loops to create the correct list of indices in
      --  the built assignments. In the description given here, it corresponds
      --  to List_Of_Indices_Known for the code created.

      function Fill_Index (N : Node_Id; Ind : Entity_Id) return List_Id;
      --  This function is used only in case of an unconstrained array type
      --  for the aggregate. In this precise case we need to create a temporary
      --  whose type is the correct constrained type : So the purpose of this
      --  function is to create the code necessary to constrained the array by
      --  generating the range needed for each index knowing the number of
      --  associations existing in the aggregate. We remind the reader that in
      --  this precise case there is no Others statement possible.

      --  First we deal with trhe case of named associations by looking for
      --  the low and high bound of the choices existing in the aggregate and
      --  we generate the code that will be added to the list of indices of
      --  the array type of the temporary:

      --     Choice_Low_Bound .. Choice_High_Bound

      --  In the case where the choices are not static then we are sure that
      --  there's only one association so we just copy its bounds.
      --  For positional associations the bounds are compute by taking
      --  Index'first and calculating the numebr of associations. The code
      --  created is:

      --     Index_Low_Bound .. Index_Low_Bound + Nb_positional - 1

      --  If the type of the indexx is an enumeration type the code created
      --  is different : we need to know which enumeration literal will be
      --  the high bound so we create the code:

      --     Index_Low_Bound ..
      --       Enum'Val (Enum'Pos (Index_Low_Bound) + Nb_Positional - 1)

      --  The N node represents the aggregate examined and Ind the index
      --  corresponding to this N aggregate.

      --  This function is recursive for multi dimensional aggregate.

      procedure Resolve_Multi_Array_Aggregate (N : Node_Id;
                                               Typ, Ind : Entity_Id) is
         Posit_List : constant List_Id := Expressions (N);
         Assoc_List : constant List_Id := Component_Associations (N);

         Posit  : Node_Id;
         Assoc  : Node_Id;
         Choice : Node_Id;
         Lb_Ch  : Node_Id;
         Hb_Ch  : Node_Id;

         Ind_Lo : Entity_Id;
         Ind_Hi : Entity_Id;

         Nb_Posit   : Uint := Uint_0;
         Nb_Values_Choices : Uint := Uint_0;
         Ind_Lo_Val : Uint;
         Ind_Hi_Val : Uint;
         Agg_Lo_Val : Uint;
         Agg_Hi_Val : Uint;

         Static_Index  : Boolean := False;
         Others_Passed : Boolean := False;
         Others_Only   : Boolean := True;
         Others_Alone  : Boolean := True;

         Nb_Choices : Nat := 0;
         --  Variable indicating the number of choices in the component
         --  associations in the current aggregate or sub-aggregate.
         --  Used to instanciate a table in Get_Choices_Bounds.

         procedure Complete_Component (N : Node_Id; Typ, Ind : Entity_Id);
         --  The N node represents a N_Aggregate node or a node corresponding
         --  to the value given of the array component. The Typ entity is
         --  the type of the aggregate and the Ind entity is the index
         --  corresponding to the N subaggregate. Ind is Empty when the N node
         --  represents a value given to a component of the array.

         --  First, Call Analyze on the N node.

         --  If Ind is not empty, we still deal with the same
         --  multi-dimensional aggregate, so we call recursively the
         --  procedure Resolve_Multi_Array_Aggregate.

         --  Else, we have reached the component and we call
         --  Resolve_Complete_Context.

         procedure Get_Choices_Bounds (A : List_Id; L, H : out Uint);
         --  This procedure examines the A list of component associations
         --  of the aggregate being examined.

         --  The different choices of the association are stored in a table
         --  with the value corresponding to the low and high bound of the
         --  choice (this value are the same when that's not a range).

         --  Then, this table is sorted. We examine the resulting table
         --  looking for missing or duplicated choices.

         --  The nodes L and H are set respectively to the low and high
         --  bound of the values represented in the choices od the aggregate.
         --  This procedure is part of the processing of the analysis which
         --  looks for constraint errors at run time. This procedure is
         --  called only if the component associations does not have an Others
         --  choice and all the choices are static.

         procedure Complete_Component (N : Node_Id; Typ, Ind : Entity_Id) is
         begin
            Analyze (N);
            if Present (Ind) then
               if Nkind (N) /= N_Aggregate then
                  Error_Msg_N ("invalid expression for component" &
                               "of multidimensional aggregate", N);
                  Error_Found := True;

               else
                  Resolve_Multi_Array_Aggregate (N, Typ, Ind);
               end if;

            else
               Resolve_Complete_Context (N, Component_Type (Typ));
            end if;
         end Complete_Component;

         procedure Get_Choices_Bounds (A : List_Id; L, H : out Uint) is
            Assoc  : Node_Id := First (A);
            Choice : Node_Id;

            Table : Case_Table_Type (1 .. Nb_Choices);

            I : Nat := 1;

         begin
            while Present (Assoc) loop
               Choice := First (Choices (Assoc));

               while Present (Choice) and then I <= Nb_Choices loop
                  if Nkind (Choice) = N_Range then
                     Table (I).Choice_Lo := Low_Bound (Choice);
                     Table (I).Choice_Hi := High_Bound (Choice);

                  else
                     Table (I).Choice_Lo := Choice;
                     Table (I).Choice_Hi := Choice;
                  end if;

                  Table (I).Choice_Node := Expression (Assoc);
                  Choice := Next (Choice);
                  I := I + 1;
               end loop;

               Assoc := Next (Assoc);
            end loop;

            Sort_Case_Table (Table);

            for I in Table'First .. Table'Last - 1 loop
               if UI_Ge (Expr_Value (Table (I).Choice_Hi),
                         Expr_Value (Table (I + 1).Choice_Lo))
               then
                  Error_Msg_N ("a choice is not allowed to be specified" &
                     " more than once", Table (I).Choice_Hi);
                  Error_Found := True;

               elsif UI_Gt (UI_Difference (
                              Expr_Value (Table (I + 1).Choice_Lo),
                              Expr_Value (Table (I).Choice_Hi)),
                            Uint_1)
               then
                  Error_Msg_N ("missing association in the aggregate", N);
                  Error_Found := True;
               end if;
               exit when Error_Found;
            end loop;

            L := Expr_Value (Table (Table'First).Choice_Lo);
            H := Expr_Value (Table (Table'Last).Choice_Hi);
         end Get_Choices_Bounds;

      --  Start of processing for Resolve_Multi_Array_Aggregate

      begin

         --  Look for the low and the high bound of the index and
         --  if they are static, store these values.

         Get_Index_Bounds (Ind, Ind_Lo, Ind_Hi);

         if Is_Static_Expression (Ind_Lo)
           and then Is_Static_Expression (Ind_Hi)
         then
            Static_Index := True;
            Ind_Lo_Val := Expr_Value (Ind_Lo);
            Ind_Hi_Val := Expr_Value (Ind_Hi);
         else
            Static_Indices := False;
         end if;

         --  Named choices in component associations

         if List_Present (Assoc_List) then
            Assoc := First (Assoc_List);

            BCL : while Present (Assoc) loop
               if Others_Passed then
                  Error_Msg_N
                     ("OTHERS must be the last aggregate component", Assoc);
                  Error_Found := True;
                  exit BCL;
               end if;

               Choice := First (Choices (Assoc));
               Others_Alone := True;

               while Present (Choice) loop
                  Analyze (Choice);

                  --  Others statement

                  if Nkind (Choice) = N_Others_Choice then
                     if not Others_Alone then
                        Error_Msg_N ("OTHERS must appear as a single choice",
                                    Choice);
                        Error_Found := True;
                        exit BCL;

                     elsif not Is_Constrained (Typ) then
                        Error_Msg_N
                           ("OTHERS choice is not allowed in this context",
                           Choice);
                        Error_Found := True;
                        exit BCL;

                     elsif Ada_83
                        and then not Static_Index
                        and then (not Others_Only or else Present (Posit))
                     then
                        Error_Msg_N ("OTHERS choice is not allowed in this" &
                           " context (4.3.2 Ada 83)", Choice);
                        Error_Found := True;
                        exit BCL;

                     else
                        Others_Passed := True;
                     end if;

                  else

                     --  Range choice in component association

                     --  Look for the bounds, call resolve and look if they
                     --  are static, compute the number of values covered
                     --  by the range for the check of the number of values
                     --  covered by others

                     if Nkind (Choice) = N_Range then
                        Lb_Ch := Low_Bound (Choice);
                        Hb_Ch := High_Bound (Choice);
                        Resolve_Subexpr (Lb_Ch, Etype (Ind));
                        Resolve_Subexpr (Hb_Ch, Etype (Ind));

                        if Is_Static_Expression (Lb_Ch)
                          and then Is_Static_Expression (Hb_Ch)
                        then
                           if UI_Gt (Expr_Value (Lb_Ch),
                                     Expr_Value (Hb_Ch))
                           then
                              if not Others_Only then
                                 Error_Msg_N ("a null range in an array" &
                                    " aggregate must be the only choice",
                                    Choice);
                                 Error_Found := True;
                                 exit BCL;
                              end if;
                           else
                              Nb_Values_Choices :=
                                  UI_Sum (Nb_Values_Choices,
                                          UI_Difference (
                                             UI_Sum (Expr_Value (Hb_Ch),
                                                     Uint_1),
                                             Expr_Value (Lb_Ch)));
                           end if;
                        else
                           if not Others_Only then
                              Error_Msg_N ("non static choice in array" &
                                 " aggregate must be the only choice",
                                 Choice);
                              Error_Found := True;
                              exit BCL;
                           else
                              Static_Choices := False;
                           end if;
                        end if;

                     --  Simple choice in component association

                     --  Call resolve and look if it's static, add one to
                     --  the number of values covered by the choices.

                     else
                        Resolve_Subexpr (Choice, Etype (Ind));

                        if not Is_Static_Expression (Choice) then
                           if not Others_Only then
                              Error_Msg_N ("non static choice in array" &
                                 " aggregate must be the only choice",
                                 Choice);
                              Error_Found := True;
                              exit BCL;

                           else
                              Static_Choices := False;
                           end if;
                        else
                           Nb_Values_Choices :=
                             UI_Sum (Nb_Values_Choices, Uint_1);
                        end if;
                     end if;

                     Others_Only := False;
                     Others_Alone := False;
                     Nb_Choices := Nb_Choices + 1;
                  end if;

                  Choice := Next (Choice);
               end loop;

               Complete_Component (Expression (Assoc), Typ, Next_Index (Ind));
               Assoc := Next (Assoc);
            end loop BCL;

         end if;

         --  Positional associations

         --  Calculate the number of positional associations

         if not Error_Found and then List_Present (Posit_List) then
            Posit := First (Posit_List);

            while Present (Posit) loop
               Complete_Component (Posit, Typ, Next_Index (Ind));
               Nb_Posit := UI_Sum (Nb_Posit, Uint_1);
               Posit := Next (Posit);
            end loop;
         end if;

         --  Check that either positional or named associations
         --  are present in the aggregate, apart from OTHERS.

         if not Error_Found
           and then List_Present (Posit_List)
           and then not Others_Only
         then
            Error_Msg_N ("apart from OTHERS, associations must be all " &
                         "positional or all named", N);
            Error_Found := True;

         --  Check the valid context of an OTHERS choice (Ada 83)

         elsif not Error_Found
           and then Ada_83
           and then Others_Passed
           and then not Others_Only
           and then not Full_Others
         then
            Error_Msg_N ("invalid context for OTHERS and named " &
               "associations (4.3.2 (6) Ada 83)", N);
            Error_Found := True;

         end if;

         --  Named associations

         --  Check if we are able to determine now that Constraint Error will
         --  be raised a run time.

         if not Error_Found
           and then Static_Choices
           and then not List_Present (Posit_List)
           and then not Others_Passed
         then

            Get_Choices_Bounds (Assoc_List, Agg_Lo_Val, Agg_Hi_Val);

            if not Error_Found
              and then Static_Index
              and then UI_Gt (Agg_Hi_Val, Agg_Lo_Val)
            then

               --  Check if the range is out of bounds considering sliding

               if (UI_Lt (Agg_Lo_Val, Ind_Lo_Val)
                   or else UI_Lt (Ind_Hi_Val, Agg_Hi_Val))
                 and then UI_Gt (Ind_Hi_Val, Ind_Lo_Val)
                 and then Full_Others

               then
                  Raise_C := True;
                  Raise_Warning (N, Standard_Entity (S_Constraint_Error),
                     "index values in aggregate " &
                     "do not belong to index subtype?!");
               end if;

               if Is_Constrained (Typ) then
                  if UI_Gt (Ind_Hi_Val, Ind_Lo_Val) then

                     --  Check if the size of the choice range is less or
                     --  greater than the index range.

                     if UI_Lt (UI_Difference (Ind_Hi_Val, Ind_Lo_Val),
                               UI_Difference (Agg_Hi_Val, Agg_Lo_Val))
                     then
                        Raise_C := True;
                        Raise_Warning (N, Standard_Entity (S_Constraint_Error),
                          "too few index values in aggregate?!");

                     elsif UI_Gt (UI_Difference (Ind_Hi_Val, Ind_Lo_Val),
                                  UI_Difference (Agg_Hi_Val, Agg_Lo_Val))
                     then
                        Raise_C := True;
                        Raise_Warning (N, Standard_Entity (S_Constraint_Error),
                          "too many index values in aggregate?!");
                     end if;

                  --  Check if the choice range is not null and the index
                  --  range is.

                  elsif UI_Lt (Ind_Hi_Val, Ind_Lo_Val) then
                     Raise_C := True;
                     Raise_Warning (N, Standard_Entity (S_Constraint_Error),
                        "index subtype range in aggregate is null?!");
                  end if;
               end if;
            end if;
         end if;

         --  Positional associations

         if not Error_Found
           and then List_Present (Posit_List)
           and then Static_Index
           and then not Is_Constrained (Typ)
           and then not Others_Passed
           and then UI_Ne (Nb_Posit,
                           UI_Difference (UI_Sum (Ind_Hi_Val, Uint_1),
                                          Ind_Lo_Val))
         then
            Raise_C := True;
            Raise_Warning (N, Standard_Entity (S_Constraint_Error),
               "index bounds out of range in aggregate?!");
         end if;

         --  Compute the number of values covered by an Others statemnt
         --  if present in the aggregate examined and set the flag that
         --  will determinate the expansion algorithm to use.

         if not Error_Found
           and then List_Present (Posit_List)
           and then Others_Passed
           and then Static_Index
           and then UI_Gt (UI_Difference (
                             UI_Difference (UI_Sum (Ind_Hi_Val, Uint_1),
                                            Ind_Lo_Val),
                             Nb_Posit),
                           Max_Val)
         then
            Others_In_Threshold := False;
         end if;

         if not Error_Found
           and then not List_Present (Posit_List)
           and then Others_Passed
           and then Static_Index
           and then Static_Choices
           and then UI_Gt (UI_Difference (
                             UI_Difference (UI_Sum (Ind_Hi_Val, Uint_1),
                                            Ind_Lo_Val),
                             Nb_Values_Choices),
                           Max_Val)
         then
            Others_In_Threshold := False;
         end if;

      end Resolve_Multi_Array_Aggregate;

      function Rewrite_Array_Aggregate (N : Node_Id; Typ, Ind : Entity_Id)
                 return Node_Id is
         Expr_List  : constant List_Id := Expressions (N);
         Assoc_List : constant List_Id := Component_Associations (N);

         New_Expr_List : List_Id := New_List;

         Expr_Comp : Node_Id;
         Expr      : Node_Id;
         Assoc     : Node_Id;
         Choice    : Node_Id;
         New_Aggr  : Node_Id;

         Next_Ind  : constant Entity_Id := Next_Index (Ind);

         Ind_Lo : Entity_Id;
         Ind_Hi : Entity_Id;
         Lo_Rng : Entity_Id;
         Hi_Rng : Entity_Id;
         N_Expr : Entity_Id;

         Lo_Rng_Val : Uint;
         Hi_Rng_Val : Uint;

         Nb_Pos : Nat := 0;

         procedure Append_N_Copy (Lo, Hi : Node_Id;
                                  L : List_Id;
                                  V : Node_Id;
                                  Offset : Nat);
         --  Append N times a new copy of the V node in the L list.
         --  N is the number of values covered by the range Lo + Offset .. Hi.

         function Find_Expr (N, Lo : Node_Id; L : List_Id) return Node_Id;
         --  search in L list the node corresponding by its position
         --  to the node N in the list of index values and returns this node.

         procedure Append_N_Copy (Lo, Hi : Node_Id;
                                  L : List_Id;
                                  V : Node_Id;
                                  Offset : Nat) is
            Lo_Val : Uint := Expr_Value (Lo);
            Hi_Val : Uint := Expr_Value (Hi);

         begin
            for i in UI_To_Int (Lo_Val) + Offset .. UI_To_Int (Hi_Val) loop
               Append (New_Copy (V), L);
            end loop;
         end Append_N_Copy;

         function Find_Expr (N, Lo : Node_Id; L : List_Id) return Node_Id is
            Expr : Node_Id := First (L);

            Lo_Val : Uint := Expr_Value (Lo);
            N_Val  : Uint := Expr_Value (N);

         begin
            while UI_Lt (Lo_Val, N_Val) loop
               Expr := Next (Expr);
               Lo_Val := UI_Sum (Lo_Val, Uint_1);
            end loop;

            return Expr;
         end Find_Expr;

      --  Processing for Rewrite_Array_Aggregate

      begin

         --  If a Constraint Error has been detected during the semantic
         --  analysis, then build an N_Aggregate node containing the correct
         --  number of positional associations. The value given to all
         --  associations is the value of the first association in the
         --  examined aggregate. If we deal with a multi-dimensional aggregate
         --  call recursively Rewrite_Array_Aggregate.

         if Raise_C then
            Get_Index_Bounds (Ind, Ind_Lo, Ind_Hi);

            if List_Present (Expr_List) then

               --  look for the first positional association

               Expr := First (Expr_List);

               if Present (Next_Ind) then
                  Expr := Rewrite_Array_Aggregate (Expr, Typ, Next_Ind);
               end if;

            else

               --  Look for the first expression of the first named association

               Expr := Expression (First (Assoc_List));

               if Present (Next_Ind) then
                  Expr := Rewrite_Array_Aggregate (Expr, Typ, Next_Ind);
               end if;
            end if;

            --  Fill the new positional aggregate with Expr.

            Append_N_Copy (Ind_Lo, Ind_Hi, New_Expr_List, Expr, 0);

         --  The aggregate examined is correct. First deal with position
         --  aggregate with an Others statement.

         elsif List_Present (Expr_List) then

            --  Copy all the positional associations in the new list of
            --  positional associations.

            Expr := First (Expr_List);

            while Present (Expr) loop

               if Present (Next_Ind) then
                  Append (Rewrite_Array_Aggregate (Expr, Typ, Next_Ind),
                          New_Expr_List);
               else
                  Append (New_Copy (Expr), New_Expr_List);
               end if;

               Nb_Pos := Nb_Pos + 1;
               Expr := Next (Expr);
            end loop;

            --  Append at the end of the new positional associations the
            --  missing positional values represented by Others.

            if List_Present (Assoc_List) then
               Expr_Comp := Expression (First (Assoc_List));

               if Present (Next_Ind) then
                  Expr_Comp :=
                    Rewrite_Array_Aggregate (Expr_Comp, Typ, Next_Ind);
               end if;

               Get_Index_Bounds (Ind, Ind_Lo, Ind_Hi);
               Append_N_Copy (Ind_Lo, Ind_Hi, New_Expr_List, Expr_Comp,
                              Nb_Pos);
            end if;

         else
            --  Named associations

            --  First, build the New_Expr_List with the component passed to
            --  Others and if Others is not present the the list will be
            --  built with the first component passed to the aggregate.

            Get_Index_Bounds (Ind, Ind_Lo, Ind_Hi);
            Assoc := First (Assoc_List);

            while Present (Assoc) loop
               Choice := First (Choices (Assoc));
               Expr_Comp := Expression (Assoc);

               if Nkind (Choice) = N_Others_Choice then
                  if Present (Next_Ind) then
                     Expr_Comp :=
                       Rewrite_Array_Aggregate (Expr_Comp, Typ, Next_Ind);
                  end if;

                  Append_N_Copy (Ind_Lo, Ind_Hi, New_Expr_List, Expr_Comp, 0);
               end if;

               Assoc := Next (Assoc);
            end loop;

            if Is_Empty_List (New_Expr_List) then
               Append_N_Copy (Ind_Lo, Ind_Hi, New_Expr_List,
                              Expression (First (Assoc_List)), 0);
            end if;

            --  Now we place the component values given in the aggregate
            --  in the New_Expr_List at their right position.

            Assoc := First (Assoc_List);

            while Present (Assoc) loop
               Choice := First (Choices (Assoc));

               while Present (Choice) loop
                  if Nkind (Choice) = N_Others_Choice then
                     null;

                  elsif Nkind (Choice) = N_Range then
                     Lo_Rng := Low_Bound (Choice);
                     Hi_Rng := High_Bound (Choice);
                     Lo_Rng_Val := Expr_Value (Lo_Rng);
                     Hi_Rng_Val := Expr_Value (Hi_Rng);
                     Expr := Find_Expr (Lo_Rng, Ind_Lo, New_Expr_List);
                     Expr_Comp := Expression (Assoc);

                     if Present (Next_Ind) then
                        Expr_Comp :=
                          Rewrite_Array_Aggregate (Expr_Comp, Typ, Next_Ind);
                     end if;

                     while UI_Lt (Lo_Rng_Val, Hi_Rng_Val) loop
                        Insert_After (Expr, New_Copy (Expr_Comp));
                        N_Expr := Next (Expr);
                        Remove (Expr);
                        Expr := Next (N_Expr);
                        Lo_Rng_Val := UI_Sum (Lo_Rng_Val, Uint_1);
                     end loop;

                     Insert_After (Expr, New_Copy (Expr_Comp));
                     Remove (Expr);

                  else
                     Expr := Find_Expr (Choice, Ind_Lo, New_Expr_List);
                     Expr_Comp := Expression (Assoc);

                     if Present (Next_Ind) then
                        Expr_Comp :=
                          Rewrite_Array_Aggregate (Expr_Comp, Typ, Next_Ind);
                     end if;

                     Insert_After (Expr, New_Copy (Expr_Comp));
                     Remove (Expr);
                  end if;

                  Choice := Next (Choice);
               end loop;

               Assoc := Next (Assoc);
            end loop;
         end if;

         New_Aggr := Make_Aggregate (Sloc (N), Expressions => New_Expr_List);
         Set_Etype (New_Aggr, Any_Composite);
         return New_Aggr;
      end Rewrite_Array_Aggregate;

      function Build_Loop (N : Node_Id; Ind : Entity_Id; L : List_Id)
                 return List_Id is
         Posit_List : constant List_Id := Expressions (N);
         Assoc_List : constant List_Id := Component_Associations (N);

         Assg_Stat_List : List_Id;
         New_Iden_List  : List_Id := New_List_Copy (L);
         R              : List_Id := New_List;

         Posit     : Node_Id;
         Assoc     : Node_Id;
         Choice    : Node_Id;
         Iden      : Node_Id;
         Iter_Sche : Node_Id;
         Loop_Stat : Node_Id;

         Next_Ind : Entity_Id := Next_Index (Ind);
         Ind_Lo   : Entity_Id;
         Ind_Hi   : Entity_Id;

         Nb_Comp : Uint;

      begin

         Get_Index_Bounds (Ind, Ind_Lo, Ind_Hi);

         --  First deal with OTHERS in the aggregate

         if List_Present (Assoc_List) then
            Assoc := First (Assoc_List);

            while Present (Assoc) loop
               Choice := First (Choices (Assoc));

               while Present (Choice) loop
                  if Nkind (Choice) = N_Others_Choice then

                     --  Create a new identifier list by copying the list of
                     --  identifiers visible at this point and adding a new
                     --  identifier corresponding to the defining identifier
                     --  just created. This list will be used as the indexed
                     --  component of the assignment concerning the temporary.

                     Iden :=
                       Make_Defining_Identifier (Sloc_N,
                         Chars => New_Internal_Name ("i"));
                     New_Iden_List := New_List_Copy (L);
                     Append_To (New_Iden_List,
                       Make_Identifier (Sloc_N, Chars => Chars (Iden)));

                     --  Create the code : for I__xxx in Ind_Lo .. Ind_Hi loop

                     Iter_Sche :=
                       Make_Iteration_Scheme (Sloc_N,
                         Loop_Parameter_Specification =>
                           Make_Loop_Parameter_Specification (Sloc_N,
                             Defining_Identifier => Iden,
                             Discrete_Subtype_Definition =>
                               Make_Range (Sloc_N,
                                 Low_Bound  => New_Copy (Ind_Lo),
                                 High_Bound => New_Copy (Ind_Hi))));

                     --  deal with a multi-dimensional aggregate or create the
                     --  code : Tmp__xyz (New_Iden_List) := Expression (Assoc);

                     if Present (Next_Ind) then
                        Assg_Stat_List := Build_Loop (Expression (Assoc),
                                                      Next_Ind,
                                                      New_Iden_List);

                     else
                        Assg_Stat_List :=
                          New_List_1 (
                            Make_Assignment_Statement (Sloc_N,
                              Name =>
                                Make_Indexed_Component (Sloc_N,
                                  Prefix =>
                                    Make_Identifier (Sloc_N, Chars (Tmp)),
                                  Expressions => New_Iden_List),
                              Expression => Expression (Assoc)));
                     end if;

                     Loop_Stat :=
                       Make_Loop_Statement (Sloc_N,
                         Identifier       => Empty,
                         Iteration_Scheme => Iter_Sche,
                         Statements       => Assg_Stat_List);
                     Append (Loop_Stat, R);
                  end if;

                  Choice := Next (Choice);
               end loop;

               Assoc := Next (Assoc);
            end loop;

            --  Named associations different from Others

            Assoc := First (Assoc_List);

            while Present (Assoc) loop
               Choice := First (Choices (Assoc));

               while Present (Choice) loop

                  if Nkind (Choice) = N_Others_Choice then
                     null;

                  --  range choice

                  elsif Nkind (Choice) = N_Range then

                     --  We need to build a loop that will instanciate
                     --  the temporary for the values covering the range
                     --  of the choice. So, we need a new defining identifier.
                     --  The code created here is similar to the code created
                     --  in the Others case.

                     Iden :=
                       Make_Defining_Identifier (Sloc_N,
                         Chars => New_Internal_Name ("i"));
                     New_Iden_List := New_List_Copy (L);
                     Append_To (New_Iden_List,
                       Make_Identifier (Sloc_N, Chars (Iden)));

                     Iter_Sche :=
                       Make_Iteration_Scheme (Sloc_N,
                         Loop_Parameter_Specification =>
                           Make_Loop_Parameter_Specification (Sloc_N,
                             Defining_Identifier => Iden,
                             Discrete_Subtype_Definition =>
                               Make_Range (Sloc_N,
                                 Low_Bound  =>
                                   New_Copy (Low_Bound (Choice)),
                                 High_Bound =>
                                   New_Copy (High_Bound (Choice)))));

                     if Present (Next_Ind) then
                        Assg_Stat_List := Build_Loop (Expression (Assoc),
                                                      Next_Ind,
                                                      New_Iden_List);

                     else
                        Assg_Stat_List :=
                          New_List_1 (
                            Make_Assignment_Statement (Sloc_N,
                              Name =>
                                Make_Indexed_Component (Sloc_N,
                                  Prefix =>
                                    Make_Identifier (Sloc_N, Chars (Tmp)),
                                  Expressions => New_Iden_List),
                              Expression => Expression (Assoc)));
                     end if;

                     Loop_Stat :=
                       Make_Loop_Statement (Sloc_N,
                         Identifier       => Empty,
                         Iteration_Scheme => Iter_Sche,
                         Statements       => Assg_Stat_List);

                     Append (Loop_Stat, R);

                  else

                     --  Simple choice

                     --  We just need to append the value of the choice to
                     --  the list of visible identifiers. In this case,
                     --  there is no loop created, just an assignment :
                     --  Tmp (Iden_List, Choice) := Expression (Assoc);

                     New_Iden_List := New_List_Copy (L);
                     Append (New_Copy (Choice), New_Iden_List);

                     if Present (Next_Ind) then
                        Assg_Stat_List := Build_Loop (Expression (Assoc),
                                                      Next_Ind,
                                                      New_Iden_List);

                     else
                        Assg_Stat_List :=
                          New_List_1 (
                            Make_Assignment_Statement (Sloc_N,
                              Name =>
                                Make_Indexed_Component (Sloc_N,
                                  Prefix =>
                                    Make_Identifier (Sloc_N, Chars (Tmp)),
                                  Expressions => New_Iden_List),
                              Expression => Expression (Assoc)));
                     end if;

                     Append_List (Assg_Stat_List, R);
                  end if;

                  Choice := Next (Choice);
               end loop;

               Assoc := Next (Assoc);
            end loop;
         end if;

         --  Positional associations

         --  We generate two different codes considering that the type of
         --  the index may be an enumeration type. if so, we will build
         --  a temporary whose value will be be updated by using each time
         --  the attribute 'succ of this temporary. Else, the value is
         --  directly passed in the indexed component list.

         if List_Present (Posit_List) then
            Posit := First (Posit_List);
            Assg_Stat_List := New_List;
            New_Iden_List := New_List_Copy (L);
            Nb_Comp := Uint_0;

            --  If the type of the index is an enumeration type, create a new
            --  numbered identifier, add it to the the list of identifiers and
            --  create the object declaration code Iden : Index_Type := Ind_Lo;
            --  Else, append the low bound of the index to the list of
            --  indentifiers.

            if Is_Enumeration_Type (Etype (Ind_Lo)) then
               Iden :=
                 Make_Defining_Identifier (Sloc_N, New_Internal_Name ("x"));
               Append (New_Reference_To (Iden, Sloc_N), New_Iden_List);
               Append_To (Assg_Stat_List,
                 Make_Object_Declaration (Sloc_N,
                   Defining_Identifier => Iden,
                   Object_Definition =>
                     New_Reference_To (Etype (Ind_Lo), Sloc_N),
                   Expression => New_Copy (Ind_Lo)));
            else
               Append (New_Copy (Ind_Lo), New_Iden_List);
            end if;

            --  If the next index is present, then we deal with a multi
            --  dimensional aggregate and so call recursively Build_Loop.
            --  Else, we create the following code for the assignment:

            --    Tmp (New_Iden_List) := Posit;

            if Present (Next_Ind) then
               Append_List (Build_Loop (Posit, Next_Ind, New_Iden_List),
                            Assg_Stat_List);

            else
               Append_To (Assg_Stat_List,
                 Make_Assignment_Statement (Sloc_N,
                   Name =>
                     Make_Indexed_Component (Sloc_N,
                       Prefix => Make_Identifier (Sloc_N, Chars (Tmp)),
                       Expressions => New_Iden_List),
                   Expression => New_Copy (Posit)));
            end if;

            Posit := Next (Posit);

            while Present (Posit) loop
               Nb_Comp := UI_Sum (Nb_Comp, Uint_1);

               --  In the index is an enumeration type then, create
               --  the assignment:

               --    Iden := Enum'Succ (Iden);

               --  Else, initialize a copy of the identifier list and
               --  add to this copy the code:

               --    Ind_Lo + Nb_Comp;

               if Is_Enumeration_Type (Etype (Ind_Lo)) then
                  Append_To (Assg_Stat_List,
                    Make_Assignment_Statement (Sloc_N,
                      Name       => Make_Identifier (Sloc_N, Chars (Iden)),
                      Expression =>
                        Make_Attribute_Reference (Sloc_N,
                          Prefix =>
                            Make_Identifier (Sloc_N,
                              Chars => Chars (Etype (Ind_Lo))),
                          Identifier =>
                            Make_Identifier (Sloc_N, Name_Succ),
                          Expression =>
                            Make_Identifier (Sloc_N, Chars (Iden)))));

               else
                  New_Iden_List := New_List_Copy (L);
                  Append_To (New_Iden_List,
                    Make_Op_Add (Sloc_N,
                      Left_Opnd  => New_Copy (Ind_Lo),
                      Right_Opnd => Make_Integer_Literal (Sloc_N, Nb_Comp)));
               end if;

               if Present (Next_Ind) then
                  Append_List (Build_Loop (Posit, Next_Ind, New_Iden_List),
                               Assg_Stat_List);

               else
                  Append_To (Assg_Stat_List,
                    Make_Assignment_Statement (Sloc_N,
                      Name =>
                        Make_Indexed_Component (Sloc_N,
                          Prefix => Make_Identifier (Sloc_N, Chars (Tmp)),
                          Expressions => New_Iden_List),
                      Expression => New_Copy (Posit)));
               end if;

               Posit := Next (Posit);
            end loop;

            Append_List (Assg_Stat_List, R);
         end if;

         return R;
      end Build_Loop;

      function Fill_Index (N : Node_Id; Ind : Entity_Id) return List_Id is
         L : List_Id := New_List;

         Posit_List : constant List_Id := Expressions (N);
         Assoc_List : constant List_Id := Component_Associations (N);

         Posit  : Node_Id;
         Assoc  : Node_Id;
         Choice : Node_Id;
         Agg_Lo : Node_Id;
         Agg_Hi : Node_Id;

         Next_Ind : Entity_Id := Next_Index (Ind);
         Ind_Lo   : Entity_Id;
         Ind_Hi   : Entity_Id;

         Agg_Lo_Val : Uint;
         Agg_Hi_Val : Uint;
         Nb_Comp    : Uint := Uint_0;

      begin

         --  Named associations

         if List_Present (Assoc_List) then

            --  Look for the first choice of the first association and assign
            --  the low bound and high bound of a range choice to respectively
            --  Agg_Lo ans Agg_Hi. if that's a simple choice then assign it to
            --  Agg_Lo and Agg_Hi.

            Assoc := First (Assoc_List);
            Choice := First (Choices (Assoc));

            if Nkind (Choice) = N_Range then
               Agg_Lo := Low_Bound (Choice);
               Agg_Hi := High_Bound (Choice);

            else
               Agg_Lo := Choice;
               Agg_Hi := Choice;
            end if;

            --  If the values are not static then there is only one association
            --  with only one choice. Nothing else is needed, we know the low
            --  high bound that we need for the index of the array. Else, we
            --  need to examine all the choices of all the associations to find
            --  the low and high bound. Note, that we are sure that there's no
            --  Others choice because the type of the aggregate is an
            --  unconstrained array.

            if Is_Static_Expression (Agg_Lo)
              and then Is_Static_Expression (Agg_Hi)
            then
               Agg_Lo_Val := Expr_Value (Agg_Lo);
               Agg_Hi_Val := Expr_Value (Agg_Hi);

               while Present (Assoc) loop
                  Choice := First (Choices (Assoc));

                  while Present (Choice) loop
                     if Nkind (Choice) = N_Range then
                        if UI_Lt (Expr_Value (Low_Bound (Choice)),
                                  Agg_Lo_Val)
                        then
                           Agg_Lo := Low_Bound (Choice);
                           Agg_Lo_Val := Expr_Value (Agg_Lo);
                        end if;

                        if UI_Gt (Expr_Value (High_Bound (Choice)),
                                  Agg_Hi_Val)
                        then
                           Agg_Hi := High_Bound (Choice);
                           Agg_Hi_Val := Expr_Value (Agg_Hi);
                        end if;

                     else
                        if UI_Lt (Expr_Value (Choice), Agg_Lo_Val) then
                           Agg_Lo := Choice;
                           Agg_Lo_Val := Expr_Value (Agg_Lo);
                        end if;

                        if UI_Gt (Expr_Value (Choice), Agg_Hi_Val) then
                           Agg_Hi := Choice;
                           Agg_Hi_Val := Expr_Value (Agg_Hi);
                        end if;
                     end if;

                     Choice := Next (Choice);
                  end loop;

                  Assoc := Next (Assoc);
               end loop;
            end if;

            --  Create the code Agg_Lo .. Agg_Hi and we deal with a multi
            --  dimensional aggregate, call recursively Fill_Index.

            Append_To (L,
              Make_Range (Sloc_N,
                Low_Bound  => New_Copy (Agg_Lo),
                High_Bound => New_Copy (Agg_Hi)));

            if Present (Next_Ind) then
               Append_List
                 (Fill_Index (Expression (First (Assoc_List)), Next_Ind), L);
            end if;

         --  Positional associations

         elsif List_Present (Posit_List) then

            --  Get the bounds of the index coresponding to the aggregate
            --  Compute the number of positional arguments.

            --  Note that we look directly to the second positional association
            --  because we are sure that there's at least one and we want this
            --  first one to correspond to a number of positional associations
            --  of zero.

            Get_Index_Bounds (Ind, Ind_Lo, Ind_Hi);
            Posit := Next (First (Posit_List));

            while Present (Posit) loop
               Nb_Comp := UI_Sum (Nb_Comp, Uint_1);
               Posit := Next (Posit);
            end loop;

               if Is_Enumeration_Type (Etype (Ind_Lo)) then

                  --  Create the following code for enumeration type:

                  --    Ind_Lo .. Enum'Val (Enum'Pos (Ind_Lo) + Nb_Comp)

                  Append_To (L,
                    Make_Range (Sloc_N,
                       Low_Bound  => New_Copy (Ind_Lo),
                       High_Bound =>
                         Make_Attribute_Reference (Sloc_N,
                           Prefix =>
                             Make_Identifier (Sloc_N,
                               Chars => Chars (Etype (Ind_Lo))),
                           Identifier =>
                             Make_Identifier (Sloc_N, Name_Val),
                           Expression =>
                             Make_Op_Add (Sloc_N,
                               Left_Opnd  =>
                                 Make_Attribute_Reference (Sloc_N,
                                   Prefix =>
                                     Make_Identifier (Sloc_N,
                                       Chars => Chars (Etype (Ind_Lo))),
                                     Identifier =>
                                       Make_Identifier (Sloc_N, Name_Pos),
                                     Expression => New_Copy (Ind_Lo)),
                               Right_Opnd =>
                                 Make_Integer_Literal (Sloc_N,
                                   Intval => Nb_Comp)))));

               else

                  --  Else create the following code:

                  --    Ind_Lo .. Ind_Lo + Nb_Comp

                  Append_To (L,
                    Make_Range (Sloc_N,
                      Low_Bound =>  New_Copy (Ind_Lo),
                      High_Bound =>
                        Make_Op_Add (Sloc_N,
                          Left_Opnd  => New_Copy (Ind_Lo),
                          Right_Opnd =>
                            Make_Integer_Literal (Sloc_N, Nb_Comp))));
               end if;

            if Present (Next_Ind) then
               Append_List
                 (Fill_Index (First (Posit_List), Next_Ind), L);
            end if;
         end if;

         return L;
      end Fill_Index;

   --  Processing for Resolve_Array_Aggregate

   begin
      if Nkind (Parent (N)) = N_Return_Statement
        or else Nkind (Parent (N)) = N_Parameter_Association

      --  The node N_Parameter_Association appears only if
      --  there is a formal parameter, so we need also to test
      --  if the parent is a N_Procedure_Call_Statement or a
      --  N_Function_Call node when there is not formal parameter.

        or else Nkind (Parent (N)) = N_Function_Call
        or else Nkind (Parent (N)) = N_Procedure_Call_Statement

      --  RM 4.3.7 (check reference???)

        or else Nkind (Parent (N)) = N_Qualified_Expression

      --  RM 4.3.8 (check reference???)

        or else Nkind (Parent (N)) = N_Component_Association
        or else Nkind (Parent (N)) = N_Aggregate
      then
         Full_Others := True;
      end if;

      Index := First_Index (Typ);
      Resolve_Multi_Array_Aggregate (N, Typ, Index);

      if Operating_Mode = Generate_Code and then not Error_Found then
         if Is_Constrained (Typ)
           and then Static_Indices
           and then Static_Choices
           and then Others_In_Threshold
         then
            New_Aggr := Rewrite_Array_Aggregate (N, Typ, First_Index (Typ));

            if Raise_C then
               Create_Raise_Expression (New_Aggr,
                                        Standard_Entity (S_Constraint_Error));
            end if;

            Rewrite_Substitute_Tree (N, New_Aggr);

         else
            --  If the array type of the aggregate is unconstrained then make
            --  an object declaration with a subtype indication whose index
            --  ranges will be filled by Fill_Index. The code created is:

            --    Array_Type (...);

            --  Else, create an object declaration whose code is:

            --    Tmp : Array_Type;

            Tmp := 
              Make_Defining_Identifier (Sloc_N, New_Internal_Name ("tmp"));

            if not Is_Constrained (Typ) then
               Append_To (Actions_List,
                 (Make_Object_Declaration (Sloc_N,
                   Defining_Identifier => Tmp,
                   Object_Definition   =>
                     Make_Subtype_Indication (Sloc_N,
                       Subtype_Mark => New_Occurrence_Of (Typ, Sloc_N),
                       Constraint =>
                         Make_Index_Or_Discriminant_Constraint (Sloc_N,
                           Constraints =>
                             Fill_Index (N, First_Index (Typ)))))));

            else
               Append_To (Actions_List,
                 Make_Object_Declaration (Sloc_N,
                  Defining_Identifier => Tmp,
                  Object_Definition   => New_Occurrence_Of (Typ, Sloc_N)));
            end if;

            if Raise_C then
               Append_To (Actions_List,
                 Make_Raise_Statement (Sloc_N,
                  Make_Identifier (Sloc_N,
                    Chars => Chars (Standard_Entity (S_Constraint_Error)))));

            else
               Append_List
                 (Build_Loop (N, First_Index (Typ), New_List),
                  Actions_List);
            end if;

            Expr_Actions :=
              Make_Expression_Actions (Sloc_N,
                Actions    => Actions_List,
                Expression => Make_Identifier (Sloc_N, Chars (Tmp)));

            --  Analyze the different nodes of an Expression_Actions node.

            Action := First (Actions (Expr_Actions));

            while Present (Action) loop
               Analyze (Action);
               Action := Next (Action);
            end loop;

            Analyze (Expression (Expr_Actions));
            Set_Etype (Expr_Actions, Etype (Tmp));
            Rewrite_Substitute_Tree (N, Expr_Actions);
         end if;
      end if;

   end Resolve_Array_Aggregate;

   ------------------
   -- Resolve_Call --
   ------------------

   procedure Resolve_Call (N : Node_Id; Typ : Entity_Id) is
      Subp : constant Node_Id := Name (N);
      Nam  : Entity_Id;
      I    : Interp_Index;
      It   : Interp;

      function Is_Predefined_Op (Nam : Entity_Id) return Boolean;
      --  Utility to check whether the name in the call is a predefined
      --  operator,  in  which case the call is made into an operator node.

      function Is_Predefined_Op (Nam : Entity_Id) return Boolean is
      begin
         return Is_Intrinsic (Nam)
           and then (Scope (Nam) = Standard_Standard
                        or else Chars (Scope (Nam)) = Name_System);
      end Is_Predefined_Op;

   begin
      --  The context imposes a unique interpretation with type Typ on
      --  a procedure or function call. Find the entity of the subprogram
      --  that yields the expected type, and propagate the corresponding
      --  formal constraints on the actuals. The caller has established
      --  that an interpretation exists, and emitted an error if not unique.

      --  The context type is also the type of the call node.

      Set_Etype (N, Typ);

      if Ekind (Etype (Subp)) = E_Subprogram_Type then

         --  Call to an access to subprogram, dereference made explicit in
         --  Analyze_Call.

         Nam := Etype (Subp);

      elsif not (Is_Type (Entity (Subp))) then

         --  Name correctly established in Resolve

         Nam := Entity (Subp);
         Set_Entity_With_Style_Check (Subp, Nam);

      else
         if Is_Overloaded (Subp) then
            Get_First_Interp (Subp,  I, It);

            while Present (It.Typ) loop
               if Covers (Typ, It.Typ) then
                  Nam := It.Nam;
                  Set_Entity_With_Style_Check (Subp, Nam);
                  exit;
               end if;

               Get_Next_Interp (I, It);
            end loop;                        

         else
            Compiler_Abort;
         end if;
      end if;

      --  In the case where the call is to an overloaded subprogram, Analyze
      --  calls Normalize_Actuals once per overloaded subprogram. Therefore in
      --  such a case Normalize_Actuals needs to be called once more to order
      --  the actuals correctly. Otherwise the call will have the ordering
      --  given by the last overloaded subprogram whether this is the correct
      --  one being called or not. 

      if Is_Overloaded (Subp) then
         if not Normalize_Actuals (N, Nam, False) then
            Compiler_Abort;
         end if;
      end if;

      --  In any case, call is fully resolved now. Reset Overload flag, to
      --  prevent subsequent overload resolution if node is analyzed again

      Set_Is_Overloaded (Subp, False);
      Set_Is_Overloaded (N, False);

      --  If subprogram name is a predefined operator, it was given in 
      --  functional notation. Replace call node with operator node, so
      --  that actuals can be resolved appropriately.

      if Is_Predefined_Op (Nam) then
         Make_Call_Into_Operator (N);
         Resolve_Subexpr (N, Typ);
         return;

      elsif Present (Alias (Nam)) 
        and then Is_Predefined_Op (Alias (Nam))
      then
         Set_Entity (Subp, Alias (Nam));
         Make_Call_Into_Operator (N);
         Resolve_Subexpr (N, Typ);
         return;
      end if;

      --  Propagate interpretation to actuals. Skip over those formals
      --  for which there are no matching actuals (named notation and/or
      --  default values).

      if List_Present (Parameter_Associations (N)) then
         declare
            A : Node_Id   := First_Actual (N);
            F : Entity_Id := First_Formal (Nam);

         begin
            while Present (A) loop
               if Nkind (Parent (A)) /= N_Parameter_Association
                 or else Chars (Selector_Name (Parent (A))) = Chars (F)
               then
                  Resolve_Subexpr (A, Etype (F));

                  if Ekind (F) /= E_In_Parameter
                    and then not Is_Variable (A)
                  then
                     Error_Msg_NE ("actual for& must be a variable", A, F);
                  end if;

                  if Nkind (A) in N_Subexpr then
                     --  Constant-fold each argument
                     Static_Evaluation (A);
                  end if;
                  if Ekind (F) /= E_Out_Parameter then
                     Apply_Range_Check (A, Etype (F));
                  end if;

                  A := Next_Actual (A);
               else
                  --  there is a default value for this formal, which will be
                  --  inserted during expansion. Current actual corresponds
                  --  to some subsequent formal.

                  null;
               end if;

               F := Next_Formal (F);
            end loop;
         end;

         --  Overloaded literals are rewritten as function calls, for
         --  purpose of resolution.  After resolution, we can replace
         --  the call with the literal itself.

         if Is_Dispatching_Operation (Nam) then
            Check_Dispatching_Call (N);
         end if;

      elsif Ekind (Nam) = E_Enumeration_Literal then
         Copy_Node (Subp, N);
      end if;

   end Resolve_Call;

   -------------------------------
   -- Resolve_Character_Literal --
   -------------------------------

   procedure Resolve_Character_Literal (N : Node_Id; Typ : Entity_Id) is
      B_Typ : constant Entity_Id := Base_Type (Typ);
      C     : Entity_Id;

   begin
      --  Verify that the character does belong to the type of the context

      Set_Etype (N, B_Typ);

      --  Standard character types do not have an explicit list of literals

      if B_Typ = Standard_Character then
         return;
      end if;

      C := Current_Entity (N);

      while Present (C) loop
         if Etype (C) = B_Typ then
            Set_Entity_With_Style_Check (N, C);
            return;
         end if;

         C := Homonym (C);
      end loop;

      --  On exit, the literal does not belong to any character type, and
      --  either Typ is some predefined character type (for which the
      --  literals are not explicitly stored), or else the character is not
      --  a literal of Typ, and Constraint_Error must be raised???).

   end Resolve_Character_Literal;

   ---------------------------
   -- Resolve_Comparison_Op --
   ---------------------------

   --  Context requires a boolean type, and plays no role in resolution.
   --  Processing identical to that for equality operators.

   procedure Resolve_Comparison_Op (N : Node_Id; Typ : Entity_Id) is
      L : constant Node_Id := Left_Opnd (N);
      R : constant Node_Id := Right_Opnd (N);
      T : constant Entity_Id := Find_Unique_Type (L, R);

   begin
      if T /= Any_Type then
         Resolve_Subexpr (L, T);
         Resolve_Subexpr (R, T);
      end if;
   end Resolve_Comparison_Op;

   ------------------------
   -- Resolve_Conversion --
   ------------------------

   procedure Resolve_Conversion (N : Node_Id; Typ : Entity_Id) is
      Target_Type : Entity_Id := Etype (N);
      Operand     : Node_Id := Expression (N);
      Opnd_Type   : Entity_Id := Etype (Operand);

   begin

      if not Valid_Conversion (N) then
         return;
      end if;

      --  If conversion has universal type, then we resolve using the
      --  widest available runtime type of the corresponding class.

      if Opnd_Type = Universal_Integer then
         Resolve_Subexpr (Operand, Standard_Longest_Runtime_Integer);

      elsif Opnd_Type = Universal_Real then
         Resolve_Subexpr (Expression (N), Standard_Longest_Runtime_Real);

      --  Otherwise resolve operand using its own type. 

      else
         Resolve_Subexpr (Operand, Opnd_Type);
      end if;

      --  If no suppression of range checking is specified, enable flags to do
      --  range checking for type conversion. An overflow check is necessary
      --  in the case where the source target is larger than the target type
      --  of the conversion. Currently this check only occurrs for signed
      --  integer or enumeration types since it is premature to do this for
      --  fixed point, floating point and modular types.

      if Is_Discrete_Type (Target_Type)
        and then Ekind (Target_Type) not in Modular_Kind
        and then not Range_Checks_Suppressed (Target_Type) 
      then
         if Esize (Etype (Operand)) > Esize (Target_Type) then
            Set_Do_Overflow_Check (N, True);
         end if;
         Apply_Range_Check (Operand, Target_Type);
      end if;
   end Resolve_Conversion;

   -------------------------
   -- Resolve_Equality_Op --
   -------------------------

   --  Both arguments must have the same type, and the boolean context
   --  does not participate in the resolution. The first pass verifies
   --  that the interpretation is not ambiguous, and the type of the left
   --  argument is correctly set, or is Any_Type in case of ambiguity.

   --  Equality may be dispatching (???).

   procedure Resolve_Equality_Op (N : Node_Id; Typ : Entity_Id) is
      L : constant Node_Id := Left_Opnd (N);
      R : constant Node_Id := Right_Opnd (N);
      T : constant Entity_Id := Find_Unique_Type (L, R);

   begin
      if T /= Any_Type then
         Resolve_Subexpr (L, T);
         Resolve_Subexpr (R, T);
      end if;
   end Resolve_Equality_Op;

   ----------------------------------
   -- Resolve_Explicit_Dereference --
   ----------------------------------

   procedure Resolve_Explicit_Dereference (N : Node_Id; Typ : Entity_Id) is
      P : Node_Id := Prefix (N);
      I    : Interp_Index;
      It   : Interp;

   begin
      if Is_Overloaded (P) then

         --  Use the context type to select the prefix that has the
         --  correct designated type.

            Get_First_Interp (P, I, It);
            while Present (It.Typ) loop
               exit when Covers (Typ, Designated_Type (It.Typ)); 

               Get_Next_Interp (I, It);
            end loop;                        

            Resolve_Subexpr (P, It.Typ);
      else
         Resolve_Subexpr (P, Etype (P));
      end if;
      if Is_Access_Type (Etype (P)) then
         Apply_Access_Check (N, Etype (P));
      end if;
   end Resolve_Explicit_Dereference;

   --------------------------------
   -- Resolve_Expression_Actions --
   --------------------------------

   procedure Resolve_Expression_Actions (N : Node_Id; Typ : Entity_Id) is
   begin
      Set_Etype (N, Typ);
      Resolve_Subexpr (Expression (N), Typ);
   end Resolve_Expression_Actions;

   ---------------------------------
   -- Resolve_Extension_Aggregate --
   ---------------------------------

   --  At the moment, cannot be called, because extension aggregates are
   --  not implemented, and we have already posted an error message.

   procedure Resolve_Extension_Aggregate (N : Node_Id; Typ : Entity_Id) is
   begin
      null;
   end Resolve_Extension_Aggregate;

   ------------------------
   -- Resolve_Identifier --
   ------------------------

   procedure Resolve_Identifier (N : Node_Id; Typ : Entity_Id) is
   begin
      --  If it's a named number replace it with its value

      if Is_Named_Number (Entity (N)) then
         Rewrite_Named_Number (N, Typ);

      elsif Is_Type (Entity (N)) then
         Error_Msg_N ("Invalid use of subtype mark in expression", N);
      end if;
   end Resolve_Identifier;

   -------------------------------
   -- Resolve_Indexed_Component --
   -------------------------------

   procedure Resolve_Indexed_Component (N : Node_Id; Typ : Entity_Id) is
      Name  : Node_Id := Prefix  (N);
      Expr  : Node_Id := First (Expressions (N));
      Array_Type : Entity_Id;
      Index      : Node_Id;

   begin
      if Is_Overloaded (Name) then

         --  Use the context type to select the prefix that yields the
         --  correct component type.

         Unimplemented (N, "Overloaded prefixes ");

      else
         Array_Type := Etype (Name);
         Resolve_Subexpr (Name, Etype (Name));

         if Is_Access_Type (Array_Type) then
            Apply_Access_Check (N, Array_Type);
            Array_Type := Designated_Type (Array_Type);
         end if;

         Index := (First_Index (Array_Type));

         while Present (Index) loop
            Resolve_Subexpr (Expr, Etype (Index));
            Static_Evaluation (Expr);
            Apply_Range_Check (Expr, Etype (Index));
            Index := Next_Index (Index);
            Expr  := Next (Expr);
         end loop;
      end if;
   end Resolve_Indexed_Component;

   -----------------------------
   -- Resolve_Integer_Literal --
   -----------------------------

   procedure Resolve_Integer_Literal (N : Node_Id; Typ : Entity_Id) is
   begin
      --  If the type is universal integer, then change it to the longest
      --  runtime integer type. Either the expression will be folded at
      --  compile time, in which case it doesn't matter how we type it,
      --  since folding is done using universal arithmetic anyway), or
      --  the expression doesn't get folded, in which case it's right
      --  to use the largest runtime integer type (and we will get a
      --  constraint error if the integer literal is too large in this case)

      if Typ = Universal_Integer then
         Set_Etype (N, Standard_Longest_Runtime_Integer);

      --  For all other cases, the type of the literal has been determined
      --  from context an is non-universal, so we just set the proper type.

      else
         Set_Etype (N, Typ);
      end if;

   end Resolve_Integer_Literal;

   ------------------------
   -- Resolve_Logical_Op --
   ------------------------

   procedure Resolve_Logical_Op (N : Node_Id; Typ : Entity_Id) is
      B_Typ : Entity_Id;

   begin
      --  Predefined operations on  scalar types yield the base type. On
      --  the other hand, logical operations on arrays yield the type of
      --  the arguments (and the context).

      if Is_Array_Type (Typ) then
         B_Typ := Typ;
      else
         B_Typ := Base_Type (Typ);
      end if;

      Resolve_Subexpr (Left_Opnd (N), B_Typ);
      Resolve_Subexpr (Right_Opnd (N), B_Typ);
      Set_Etype (N, B_Typ);
   end Resolve_Logical_Op;

   ---------------------------
   -- Resolve_Membership_Op --
   ---------------------------

   --  The context can only be a boolean type, and does not determine
   --  the arguments. Take the non-universal type of either argument,
   --  if any, to complete the resolution of both. If the second argument
   --  is a subtype name, there is nothing to resolve.

   procedure Resolve_Membership_Op (N : Node_Id; Typ : Entity_Id) is
      L : constant Node_Id := Left_Opnd (N);
      R : constant Node_Id := Right_Opnd (N);
      T : constant Entity_Id := Intersect_Types (L, R);

   begin
      Resolve_Subexpr (L, T);

      if Is_Name (R) then
         null;
      else
         Resolve_Subexpr (R, T);
      end if;
   end Resolve_Membership_Op;

   ------------------
   -- Resolve_Null --
   ------------------

   procedure Resolve_Null (N : Node_Id; Typ : Entity_Id) is
   begin

      --   The literal NULL takes its type from the context.
      --   (null = null) is now ambiguous in all cases ???.

      Set_Etype (N, Typ);
   end Resolve_Null;

   --------------------
   -- Resolve_Op_Not --
   --------------------

   procedure Resolve_Op_Not (N : Node_Id; Typ : Entity_Id) is
      B_Typ : Entity_Id;

   begin
      --  Predefined operations on  scalar types yield the base type. On
      --  the other hand, logical operations on arrays yield the type of
      --  the arguments (and the context).

      if Is_Array_Type (Typ) then
         B_Typ := Typ;
      else
         B_Typ := Base_Type (Typ);
      end if;

      Resolve_Subexpr (Right_Opnd (N), B_Typ);
      Set_Etype (N, B_Typ);
   end Resolve_Op_Not;

   --------------------------------------
   -- Resolve_Parenthesized_Expression --
   --------------------------------------

   procedure Resolve_Parenthesized_Expression (N : Node_Id; Typ : Entity_Id) is
   begin
      Set_Etype (N, Typ);
      Resolve_Subexpr (Expression (N), Typ);
   end Resolve_Parenthesized_Expression;

   -----------------------------
   -- Resolve_Operator_Symbol --
   -----------------------------

   procedure Resolve_Operator_Symbol (N : Node_Id; Typ : Entity_Id) is
   begin
      null;
   end Resolve_Operator_Symbol;

   -----------------------
   -- Resolve_Op_Concat --
   -----------------------

   procedure Resolve_Op_Concat (N : Node_Id; Typ : Entity_Id) is

      --  Internal procedure to resolve one argument of concatenation operator.
      --  The argument is either of the array type or of the component type.

      procedure Resolve_Concatenation_Arg (Arg : Node_Id) is
      begin
         if Has_Compatible_Type (Arg, Component_Type (Typ)) then
            Resolve_Subexpr (Arg, Component_Type (Typ));
            --  ??? make component into one-component array.
         else
            Resolve_Subexpr (Arg, Typ);
         end if;
      end Resolve_Concatenation_Arg;

   begin
      Set_Etype (N, Base_Type (Typ));
      Resolve_Concatenation_Arg (Left_Opnd (N));
      Resolve_Concatenation_Arg (Right_Opnd (N));
   end Resolve_Op_Concat;

   ----------------------
   -- Resolve_Op_Expon --
   ----------------------

   --  Used for resolving all arithmetic operators except exponentiation

   procedure Resolve_Op_Expon (N : Node_Id; Typ : Entity_Id) is
      B_Typ : constant Entity_Id := Base_Type (Typ);
      --  We do the resolution using the base type, because intermediate values
      --  in expressions always are of the base type, not a subtype of it.

   begin
      Resolve_Subexpr (Left_Opnd (N), B_Typ);
      Resolve_Subexpr (Right_Opnd (N), Standard_Integer);
      Set_Etype (N, B_Typ);
   end Resolve_Op_Expon;

   ----------------------------------
   -- Resolve_Qualified_Expression --
   ----------------------------------

   procedure Resolve_Qualified_Expression (N : Node_Id; Typ : Entity_Id) is
   begin
      Resolve_Subexpr (Expression (N), Entity (Subtype_Mark (N)));
   end Resolve_Qualified_Expression;

   -------------------
   -- Resolve_Range --
   -------------------

   procedure Resolve_Range (N : Node_Id; Typ : Entity_Id) is
      L : constant Node_Id := Low_Bound (N);
      R : constant Node_Id := High_Bound (N);

   begin
      Set_Etype (N, Typ);
      Resolve_Subexpr (L, Typ);
      Resolve_Subexpr (R, Typ);
   end Resolve_Range;

   ------------------------------
   -- Resolve_Record_Aggregate --
   ------------------------------

   --  Complete resolution of components in a record aggregate. If the
   --  record type has discriminants, we must first complete the
   --  resolution of the discrimant components, in order to determine the
   --  variant parts to which the rest of the aggregate must conform.
   --  We modify the tree by building an aggregate which contain
   --  only named arguments : we use the procedure Rewrite_Substitute_Tree.
   --  the discriminants and the components appear in the aggregate
   --  by following their order of declaration in the record.

   procedure Resolve_Record_Aggregate (N : Node_Id; Typ : Entity_Id) is

      Posit_List           : List_Id := Expressions (N);
      Named_List           : List_Id := Component_Associations (N);
      New_Assoc_List : List_Id := New_List;
      Others_List          : List_Id := New_List;

      New_Aggr   : Node_Id := New_Node (N_Aggregate, Sloc (N));
      Posit      : Node_Id;
      Named      : Node_Id;
      Choice     : Node_Id;

      All_Comp      : Elist_Id := New_Elmt_List;
      Comp_Seen     : Elist_Id := New_Elmt_List;
      Choices_Elist : Elist_Id := New_Elmt_List;

      Discr         : Entity_Id;
      Comp          : Entity_Id;
      Comp_Id       : Entity_Id;
      Var_Part      : Entity_Id;
      Others_Choice : Entity_Id;

      Comp_Elmt     : Elmt_Id;
      Selector      : Elmt_Id;
      Last_Pos_Comp : Elmt_Id := No_Elmt;

      Nb_Posit  : Nat;
      --  Number of positional arguments in aggregate.

      Nb_Discr     : Nat;
      --  Number of discriminants in record

      Nb_All_Comp  : Nat := 0;
      --  Number of elements in All_Comp element list

      Nb_Comp_Seen : Nat := 0;
      --  Number of elements in Comp_Seen element list

      Comp_Pos     : Nat := 1;
      I1           : Nat;

      Error_Found   : Boolean := False;
      Others_Passed : Boolean := False;
      Others_Alone  : Boolean := True;

      function Count_Pos (N : List_Id) return Nat;
      --  Count the number of positional arguments in aggregates

      function Count_Discriminant (Rec_Id : Entity_Id) return Nat;
      --  Count the number of discriminants in the record

      procedure Scan_Variant (N : Entity_Id);
      --  Scan the eventually nested variant parts of the record to
      --  determine all the components of the record.

      procedure Add_Association (C : Node_Id; E : Node_Id);
      --  Build a Component Association node and add it to the
      --  New_Assoc_List.

      function Count_Pos (N : List_Id) return Nat is
         Buffer : Nat := 0;
         Posit : Node_Id;

      begin
         if List_Present (N) then
            Posit := First (N);

            while Present (Posit) loop
               Buffer := Buffer + 1;
               Posit := Next (Posit);
            end loop;
         end if;

         return Buffer;
      end Count_Pos;

      function Count_Discriminant (Rec_Id : Entity_Id) return Nat is
         Buffer : Nat := 0;
         Discr  : Entity_Id;

      begin
         if Has_Discriminants (Rec_Id) then
            Discr := First_Discriminant (Rec_Id);
            while Present (Discr) loop
               Buffer := Buffer + 1;
               Discr := Next_Discriminant (Discr);
            end loop;
         end if;

         return Buffer;
      end Count_Discriminant;

      procedure Scan_Variant (N : Entity_Id) is
         Variant      : Entity_Id := First (Variants (N));
         Discr_Name   : Entity_Id := Name (N);
         Discr_Choice : Entity_Id;
         Comp_List    : Entity_Id;
         Comp_Decl    : Entity_Id;
         Comp_Iden    : Entity_Id;
         Var_Nested   : Entity_Id;

         Value_Discr : Node_Id;
         New_Assoc   : Node_Id;

         Comp_Decl_List : List_Id;

         High  : Uint;
         Low   : Uint;
         Value : Uint;

      begin
         --  First, look to see if the variant part name exists
         --  among the discriminant names.

         New_Assoc := First (New_Assoc_List);
         while Present (New_Assoc)
           and then Chars (Discr_Name) /=
             Chars (First (Choices (New_Assoc)))
         loop
            New_Assoc := Next (New_Assoc);
         end loop;

         --  If the name we are looking is not in the list of
         --  discriminants we stop the processing.

         if not Present (New_Assoc) then
            Error_Found := True;
         else
            --  Retrieve value corresponding to the discriminant scanned

            Value_Discr := Expression (New_Assoc);

            --  Look if the value of the discriminant is static
            --  if not raise error message else compute value

            if not Is_Static_Expression (Value_Discr) then
               Error_Msg_N
                 ("value for discriminant must be static", Value_Discr);
               Error_Found := True;
            else
               Value := Expr_Value (Value_Discr);

               --  Search for the discriminant value in each variants

               BCL : while Present (Variant) loop
                  Discr_Choice := First (Discrete_Choices (Variant));
                  while Present (Discr_Choice) loop
                     exit BCL when Nkind (Discr_Choice) = N_Others_Choice;

                     if Nkind (Discr_Choice) = N_Range then
                        Low := Expr_Value (Low_Bound (Discr_Choice));
                        High := Expr_Value (High_Bound (Discr_Choice));
                        exit BCL when
                          UI_Le (Low, Value) and then UI_Le (Value, High);
                     else
                        exit BCL when UI_Eq (Expr_Value (Discr_Choice), Value);
                     end if;

                     Discr_Choice := Next (Discr_Choice);
                  end loop;

                  Variant := Next (Variant);
               end loop BCL;

               --  If we have found the corresponding choice, look for the
               --  declared components which will be added to the component
               --  list. Otherwise generate an appropriate error message.

               if Present (Discr_Choice) then
                  Comp_List := Component_List (Variant);

                  --  If the component declaration is not null then select
                  --  all components declared and add them to the element
                  --  list of all the components in the record.

                  if not Null_Present (Comp_List) then
                     Comp_Decl_List := Component_Declarations (Comp_List);

                     if List_Present (Comp_Decl_List) then
                        Comp_Decl := First (Comp_Decl_List);

                        while Present (Comp_Decl) loop
                           Comp_Iden := Defining_Identifier (Comp_Decl);
                           Append_Elmt (Comp_Iden, All_Comp);
                           Nb_All_Comp := Nb_All_Comp + 1;
                           Comp_Decl := Next (Comp_Decl);
                        end loop;
                     end if;

                     --  Look for nested variant part

                     Var_Nested := Variant_Part (Comp_List);

                     if Present (Var_Nested) then
                        Scan_Variant (Var_Nested);
                     end if;
                  end if;

               else
                  Error_Msg_N
                    ("discriminant value is out of range", Discr_Name);
                  Error_Found := True;
               end if;
            end if;
         end if;
      end Scan_Variant;

      procedure Add_Association (C : Node_Id; E : Node_Id) is
         New_Assoc : Node_Id
            := New_Node (N_Component_Association, Sloc (E));
         New_Choices_List : List_Id := New_List;

      begin
         Append (New_Occurrence_Of (C, Sloc (E)), New_Choices_List);
         Set_Choices (New_Assoc, New_Choices_List);
         Set_Expression (New_Assoc, New_Copy (E));
         Append (New_Assoc, New_Assoc_List);
      end Add_Association;

   --  Start of processing for Resolve_Record_Aggregate

   begin
      --  Traverse the aggregate to call Analyze on all expresssions before
      --  beginning Semantic Analysis.

      if Is_Abstract (Typ) then
         Error_Msg_N ("type of aggregate cannot be abstract",  N);
      end if;

      if List_Present (Posit_List) then
         Posit := First (Posit_List);
         while Present (Posit) loop
            Analyze (Posit);
            Posit := Next (Posit);
         end loop;
      end if;

      if List_Present (Named_List) then
         Named := First (Named_List);
         while Present (Named) loop
            Analyze (Expression (Named));
            Named := Next (Named);
         end loop;
      end if;

      Nb_Discr := Count_Discriminant (Typ);
      Nb_Posit := Count_Pos (Posit_List);

      --  Set I1 to min (Nb_Discr, Nb_Posit)

      if Nb_Discr < Nb_Posit then
         I1 := Nb_Discr;
      else
         I1 := Nb_Posit;
      end if;

      --  Build the New_Assoc List containing the (hopefully) static
      --  value corresponding to each discriminant of the record. First we
      --  deal with the positional arguments.

      if I1 >= 1 then
         Discr := First_Discriminant (Typ);
         Posit := First (Posit_List);
         Add_Association (Discr, Posit);

         for I in 2 .. I1 loop
            Discr := Next_Discriminant (Discr);
            Posit := Next (Posit);
            Add_Association (Discr, Posit);
         end loop;
      end if;

      --  If necessary, find remaining discriminants among the named components

      for I in I1 + 1 .. Nb_Discr loop
         if I = 1 then
            Discr := First_Discriminant (Typ);
         else
            Discr := Next_Discriminant (Discr);
         end if;

         --  If the corresponding component is found, call Add_Association
         --  else report error.

         if List_Present (Named_List) then
            Named := First (Named_List);

            BCL : while Present (Named) loop
               Choice := First (Choices (Named));
               while Present (Choice) loop

                  --  Exit the whole search process when the discriminant
                  --  name is found.

                  exit BCL when Nkind (Choice) = N_Others_Choice
                    or else Chars (Discr) = Chars (Choice);
                  Choice := Next (Choice);
               end loop;

               Named := Next (Named);
            end loop BCL;

         else
            Choice := Empty;
         end if;

         if Present (Choice) then
            Add_Association (Discr, Expression (Named));
         else
            Error_Msg_NE ("no value supplied for discriminant", N, Discr);
            Error_Found := True;
         end  if;

         exit when Error_Found;
      end loop;

      if not Error_Found then

         --  Now perform type resolution of the discriminant values, and
         --  verify that they are static

         if List_Present (New_Assoc_List) then
            Named := First (New_Assoc_List);
            while Present (Named) loop
               Resolve_Complete_Context
                 (Expression (Named), Etype (First (Choices (Named))));
               Named := Next (Named);
            end loop;
         end if;

         --  Build a list with the components corresponding to the subtype
         --  selected by the discriminants. First select the components of
         --  the invariant part of the record.

         Comp := First_Component (Typ);

         --  special treatment for the first component of root tagged types 
         --  (field _tag)

         if (Is_Tagged_Type (Typ) and then (Etype (Typ) = Typ)) then

            --  add the association for the _tag field

            declare
               Loc       : Source_Ptr := Sloc (N);
               Conv_Node : Node_Id;
            begin

               Conv_Node :=
               Make_Unchecked_Type_Conversion (Loc,
                   Subtype_Mark => New_Occurrence_Of (RTE (RE_Tag), Loc),
                   Expression => 
                     New_Occurrence_Of (Access_Disp_Table (Typ), Loc));

               Set_Etype (Conv_Node, RTE (RE_Tag));
               Add_Association (First_Component (Typ), Conv_Node);
            end;

            --  don't put the field _tag into the list of components

            Comp := Next_Component (Comp);
         end if;

         while Present (Comp)
           and then Nkind (Parent (Parent (Parent (Comp)))) /= N_Variant
         loop
            Append_Elmt (Comp, All_Comp);
            Nb_All_Comp := Nb_All_Comp + 1;
            Comp := Next_Component (Comp);
         end loop;

         --  Now scan the variant part of the record declaration and
         --  collect the types corresponding to the given discriminants.

         Var_Part := Variant_Part (Component_List (Type_Definition
                                      (Parent (Base_Type (Typ)))));
         if Present (Var_Part) then
            Scan_Variant (Var_Part);
         end if;
      end if;

      --  Scan the positional arguments of the aggregate

      if not Error_Found and then List_Present (Posit_List) then
         if I1 > 0 then
            Posit := Next (Posit);
         else
            Posit := First (Posit_List);
         end if;

         Comp_Elmt := First_Elmt (All_Comp);

         while Present (Posit) loop
            if Comp_Pos > Nb_All_Comp then
               Error_Msg_N ("too many components for record aggregate",
                            N);
               Error_Found := True;
               exit;
            end if;

            Comp_Id := Id_Of (Comp_Elmt);

            Resolve_Complete_Context (Posit, Etype (Comp_Id));
            Append_Elmt (Comp_Id, Comp_Seen);
            Nb_Comp_Seen := Nb_Comp_Seen + 1;
            Add_Association (Comp_Id, Posit);
            Last_Pos_Comp := Comp_Elmt;
            Comp_Elmt := Next_Elmt (Comp_Elmt);
            Comp_Pos := Comp_Pos + 1;
            Posit := Next (Posit);
         end loop;

      end if;

      --  Now scan for the named arguments of the aggregate

      if not Error_Found and then List_Present (Named_List) then
         Named := First (Named_List);

         BLO : while Present (Named) loop
            Others_Alone := True;
            Choices_Elist := New_Elmt_List;
            Choice := First (Choices (Named));

            while Present (Choice) loop
               case Nkind (Choice) is
                  when N_Range =>
                     Error_Msg_N
                       ("range choice not allowed in record aggregate",
                        Choice);
                     Error_Found := True;
                     exit BLO;

                  when N_Others_Choice =>
                     if Others_Passed then
                        Error_Msg_N ("OTHERS must appear only once", Choice);
                        Error_Found := True;
                        exit BLO;

                     elsif not Others_Alone then
                        Error_Msg_N ("OTHERS must appear alone", Choice);
                        Error_Found := True;
                        exit BLO;

                     elsif Nb_Comp_Seen >= Nb_All_Comp then
                        Error_Msg_N
                          ("OTHERS must represent at least one component",
                                                                     Choice);
                        Error_Found := True;
                        exit BLO;

                     else
                        --  Scan every component of the record and
                        --  look if it has been seen before.
                        --  if not check the type of aggregate value

                        Comp_Elmt := First_Elmt (All_Comp);

                        while Comp_Elmt /= No_Elmt loop
                           Comp_Id := Id_Of (Comp_Elmt);
                           Selector := First_Elmt (Comp_Seen);

                           while Selector /= No_Elmt
                             and then Chars (Id_Of (Selector)) /=
                                      Chars (Comp_Id)
                           loop
                              Selector := Next_Elmt (Selector);
                           end loop;

                           if Selector = No_Elmt then
                              Resolve_Complete_Context
                                (Expression (Named), Etype (Comp_Id));
                              Append_Elmt (Comp_Id, Choices_Elist);
                              Append_Elmt (Comp_Id, Comp_Seen);
                              Append (New_Occurrence_Of (Comp_Id,
                                      Sloc (Choice)), Others_List);
                           end if;

                           Comp_Elmt := Next_Elmt (Comp_Elmt);
                        end loop;
                     end if;

                     Nb_Comp_Seen := Nb_All_Comp;
                     Others_Passed := True;
                     Others_Alone := False;

                  when others =>
                     if not Others_Alone and then Others_Passed then
                        Error_Msg_N ("OTHERS must appear alone", Choice);
                        Error_Found := True;
                        exit BLO;

                     elsif Others_Passed then
                        Error_Msg_N
                          ("OTHERS must be the last aggregate component",
                           Choice);
                        Error_Found := True;
                        exit BLO;
                     end if;

                     --  Simple choice. Verify that the named component
                     --  is one of the selected the component names

                     Comp_Elmt := First_Elmt (All_Comp);

                     while Comp_Elmt /= No_Elmt
                       and then Chars (Id_Of (Comp_Elmt)) /=
                                Chars (Choice)
                     loop
                        Comp_Elmt := Next_Elmt (Comp_Elmt);
                     end loop;

                     if Comp_Elmt = No_Elmt then
                        Discr := Empty;

                        if Has_Discriminants (Typ) then
                           Discr := First_Discriminant (Typ);

                           while Present (Discr)
                             and then Chars (Discr) /= Chars (Choice)
                           loop
                              Discr := Next_Discriminant (Discr);
                           end loop;
                        end if;

                        if not Present (Discr) then
                           Error_Msg_N ("undefined component name", Choice);
                           Error_Found := True;
                           exit BLO;
                        else
                           Append_Elmt (Discr, Choices_Elist);
                        end if;

                     else

                        --  Check that named aggregate hasn't been seen before

                        Selector := First_Elmt (Comp_Seen);

                        while Selector /= No_Elmt
                          and then Chars (Id_Of (Selector)) /=
                                   Chars (Choice)
                        loop
                           Selector := Next_Elmt (Selector);
                        end loop;

                        if Selector /= No_Elmt then
                           Error_Msg_N
                             ("duplicate value for component& in aggregate",
                                                                      Choice);
                           Error_Found := True;
                           exit BLO;
                        end if;

                        Comp_Id := Id_Of (Comp_Elmt);
                        Resolve_Complete_Context
                          (Expression (Named), Etype (Comp_Id));
                        Append_Elmt (Comp_Id, Choices_Elist);
                        Append_Elmt (Comp_Id, Comp_Seen);
                        Comp_Pos := Comp_Pos + 1;
                        Others_Alone := False;
                     end if;
               end case;

               Choice := Next (Choice);
            end loop;

            --  Verify that all choices have the same type

            Comp_Elmt := First_Elmt (Choices_Elist);
            Selector := Next_Elmt (Comp_Elmt);

            while Selector /= No_Elmt
              and then Base_Type (Etype (Id_Of (Selector))) =
                       Base_Type (Etype (Id_Of (Comp_Elmt)))
            loop
               Selector := Next_Elmt (Selector);
            end loop;

            if Selector /= No_Elmt then
               Error_Msg_N
               ("components on a choice list must have same type", N);
               Error_Found := True;
               exit BLO;
            end if;

            Named := Next (Named);
         end loop BLO;
      end if;

      --  Scan all declared components and check that everyone has
      --  been seen

      if not Error_Found then
         Comp_Elmt := First_Elmt (All_Comp);

         while Comp_Elmt /= No_Elmt loop
            Comp_Id := Id_Of (Comp_Elmt);
            Selector := First_Elmt (Comp_Seen);
            while Selector /= No_Elmt loop
               exit when Comp_Id = Id_Of (Selector);
               Selector := Next_Elmt (Selector);
            end loop;

            if Selector = No_Elmt then
               if Nb_Comp_Seen = Nb_All_Comp - 1 then
                  Error_Msg_NE
                    ("no value supplied for component&", N, Comp_Id);
                  Error_Found := True;
               end if;
            end if;

            Comp_Elmt := Next_Elmt (Comp_Elmt);
         end loop;
      end if;

      --  We need to complete the New_Assoc_list with the
      --  remaining components: those which are named in the aggregate

      if Operating_Mode = Generate_Code and then not Error_Found then
         if Last_Pos_Comp /= No_Elmt then
            Comp_Elmt := Next_Elmt (Last_Pos_Comp);
         else
            Comp_Elmt := First_Elmt (All_Comp);
         end if;

         while Comp_Elmt /= No_Elmt loop
            Comp_Id := Id_Of (Comp_Elmt);
            Named := First (Named_List);

            BLP : while Present (Named) loop
               Choice := First (Choices (Named));

               while Present (Choice) loop
                  if Nkind (Choice) = N_Others_Choice then
                     Others_Choice := First (Others_List);

                     while Present (Others_Choice) loop
                        if Chars (Others_Choice) = Chars (Comp_Id) then
                           Add_Association (Comp_Id, Expression (Named));
                           exit BLP;
                        end if;
                        Others_Choice := Next (Others_Choice);
                     end loop;

                  elsif Chars (Choice) = Chars (Comp_Id) then
                     Add_Association (Comp_Id, Expression (Named));
                     exit BLP;

                  end if;

                  Choice := Next (Choice);
               end loop;

               Named := Next (Named);
            end loop BLP;

            Comp_Elmt := Next_Elmt (Comp_Elmt);
         end loop;

         Set_Expressions (New_Aggr, No_List);
         Set_Component_Associations (New_Aggr, New_Assoc_List);
         Rewrite_Substitute_Tree (N, New_Aggr);
      end if;
   end Resolve_Record_Aggregate;

   --------------------------
   -- Resolve_Real_Literal --
   --------------------------

   procedure Resolve_Real_Literal (N : Node_Id; Typ : Entity_Id) is
   begin
      --  If the type is universal real, then change it to the longest
      --  runtime real type. Either the expression will be folded at
      --  compile time, in which case it doesn't matter how we type it,
      --  since folding is done using universal arithmetic anyway), or
      --  the expression doesn't get folded, in which case it's right
      --  to use the largest runtime real type (and we will get a
      --  constraint error if the real literal is too large in this case)

      if Typ = Universal_Real then
         Set_Etype (N, Standard_Longest_Runtime_Real);

      --  For all other cases, the type of the literal has been determined
      --  from context an is non-universal, so we just set the proper type.

      else
         Set_Etype (N, Typ);
      end if;

   end Resolve_Real_Literal;

   -----------------------
   -- Resolve_Reference --
   -----------------------

   procedure Resolve_Reference (N : Node_Id; Typ : Entity_Id) is
   begin
      null; -- TBD ???
   end Resolve_Reference;

   --------------------------------
   -- Resolve_Selected_Component --
   --------------------------------

   procedure Resolve_Selected_Component (N : Node_Id; Typ : Entity_Id) is
      P  : Node_Id := Prefix  (N);
      T  : Entity_Id := Etype (P);

   begin
      --  The prefix may be overloaded, or may involve expressions that
      --  must be further resolved.

      if Is_Overloaded (P) then

         --  Use the context type to select the prefix that has a selector
         --  of the correct name and type.

         Unimplemented (N, "Overloaded prefixes");

      else
         Resolve_Subexpr (P, T);
      end if;
      if Is_Access_Type (Etype (P)) then
         Apply_Access_Check (N, Etype (P));
      end if;
   end Resolve_Selected_Component;

   -------------------
   -- Resolve_Slice --
   -------------------

   procedure Resolve_Slice (N : Node_Id; Typ : Entity_Id) is
      Name       : constant Node_Id := Prefix (N);
      Array_Type : Entity_Id;
      Index      : Node_Id;

   begin
      if Is_Overloaded (Name) then

         --  Use the context type to select the prefix that yields the
         --  correct component type.

         Unimplemented (N, "Overloaded prefixes");

      else
         Array_Type := Etype (Name);
         Resolve_Subexpr (Name, Array_Type);

         if Is_Access_Type (Array_Type) then
            Apply_Access_Check (N, Array_Type);
            Array_Type := Designated_Type (Array_Type);
         end if;

         Index := First_Index (Array_Type);
         Resolve_Subexpr (Discrete_Range (N), Etype (Index));
      end if;
   end Resolve_Slice;

   ---------------------------
   -- Resolve_Short_Circuit --
   ---------------------------

   procedure Resolve_Short_Circuit (N : Node_Id; Typ : Entity_Id) is
   begin
      Resolve_Subexpr (Left_Opnd (N), Standard_Boolean);
      Resolve_Subexpr (Right_Opnd (N), Standard_Boolean);
   end Resolve_Short_Circuit;

   ----------------------------
   -- Resolve_String_Literal --
   ----------------------------

   procedure Resolve_String_Literal (N : Node_Id; Typ : Entity_Id) is
      Subtype_Id : Entity_Id;

   begin
      --  Must verify that all characters belong to the element type of the
      --  array type of the context. Create a special subtype for the
      --  N_String_Literal node which becomes its Etype.

      Subtype_Id := New_Implicit_Type (Sloc (N));
      Set_Ekind (Subtype_Id, E_String_Literal_Subtype);
      Set_Component_Type (Subtype_Id, Component_Type (Typ));
      Set_String_Literal_Length
        (Subtype_Id, UI_From_Int (String_Length (Strval (N))));
      Set_Etype (Subtype_Id, Base_Type (Typ));
      Set_Etype (N, Subtype_Id);
   end Resolve_String_Literal;

   ----------------------
   -- Resolve_Unary_Op --
   ----------------------

   procedure Resolve_Unary_Op (N : Node_Id; Typ : Entity_Id) is
   begin
      Set_Etype (N, Base_Type (Typ));
      Resolve_Subexpr (Right_Opnd (N), Typ);
   end Resolve_Unary_Op;

   -----------------------------
   -- Make_Call_Into_Operator --
   -----------------------------

   procedure Make_Call_Into_Operator (N : Node_Id) is
      Op_Name   : constant Name_Id := Chars (Entity (Name (N)));
      Act1      : constant Node_Id := First_Actual (N);
      Act2      : constant Node_Id := Next_Actual (Act1);
      Is_Binary : constant Boolean := Present (Act2);
      Kind      : Node_Kind;
      Op_Node   : Node_Id;

   begin

      if Is_Binary then
         if Op_Name = Name_Op_And          then Kind := N_Op_And;
         elsif Op_Name =  Name_Op_Or       then Kind := N_Op_Or;
         elsif Op_Name =  Name_Op_Xor      then Kind := N_Op_Xor;
         elsif Op_Name =  Name_Op_Eq       then Kind := N_Op_Eq;
         elsif Op_Name =  Name_Op_Ne       then Kind := N_Op_Ne;
         elsif Op_Name =  Name_Op_Lt       then Kind := N_Op_Lt;
         elsif Op_Name =  Name_Op_Le       then Kind := N_Op_Le;
         elsif Op_Name =  Name_Op_Gt       then Kind := N_Op_Gt;
         elsif Op_Name =  Name_Op_Ge       then Kind := N_Op_Ge;
         elsif Op_Name =  Name_Op_Add      then Kind := N_Op_Add;
         elsif Op_Name =  Name_Op_Subtract then Kind := N_Op_Subtract;
         elsif Op_Name =  Name_Op_Concat   then Kind := N_Op_Concat;
         elsif Op_Name =  Name_Op_Multiply then Kind := N_Op_Multiply;
         elsif Op_Name =  Name_Op_Divide   then Kind := N_Op_Divide;
         elsif Op_Name =  Name_Op_Mod      then Kind := N_Op_Mod;
         elsif Op_Name =  Name_Op_Rem      then Kind := N_Op_Rem;
         elsif Op_Name =  Name_Op_Expon    then Kind := N_Op_Expon;

         else Compiler_Abort;
         end if;

      else
         if Op_Name    =  Name_Op_Add      then Kind := N_Op_Plus;
         elsif Op_Name =  Name_Op_Subtract then Kind := N_Op_Minus;
         elsif Op_Name =  Name_Op_Abs      then Kind := N_Op_Abs;
         elsif Op_Name =  Name_Op_Not      then Kind := N_Op_Not;
         else Compiler_Abort;
         end if;
      end if;

      Op_Node := New_Node (Kind, Sloc (N));

      if Is_Binary then
         Set_Left_Opnd  (Op_Node, New_Copy (Act1));
         Set_Right_Opnd (Op_Node, New_Copy (Act2));
         Set_Chars (Op_Node, Op_Name);

      else
         Set_Right_Opnd (Op_Node, New_Copy (Act1));
         Set_Chars (Op_Node, Op_Name);
      end if;

      Set_Etype (Op_Node,  Etype (N));
      Rewrite_Substitute_Tree (N,  Op_Node);
   end Make_Call_Into_Operator;

   ----------------------
   -- Valid_Conversion --
   ----------------------

   function Valid_Conversion (N : Node_Id) return Boolean is
      Target_Type : Entity_Id := Etype (N);
      Operand     : Node_Id := Expression (N);
      Opnd_Type   : Entity_Id := Etype (Operand);

      function Conversion_Check (Valid : Boolean; Msg : Str) return Boolean is
      begin
         if not Valid then
            Error_Msg_N (Msg, Operand);
         end if;
         return Valid;
      end Conversion_Check;

   begin
      if Is_Overloaded (Operand) then
         Error_Msg_N ("ambiguous operand in  conversion", Operand);
         return False;

      elsif Unchecked_Conversion (N) then
         return True;

      elsif Chars (Current_Scope) = Name_Unchecked_Conversion then
         return True;

      elsif Is_Numeric_Type (Target_Type)  then
         return Conversion_Check (Is_Numeric_Type (Opnd_Type), 
                          "illegal operand for numeric conversion");

      elsif Is_Array_Type (Target_Type) then
         --  additional tests on  index and component types ???
         return Conversion_Check 
                (Is_Array_Type (Opnd_Type) 
                and then Number_Dimensions (Target_Type) 
                       = Number_Dimensions (Opnd_Type),
                "illegal operand for array conversion");

      elsif Ekind (Target_Type) = E_General_Access_Type then
         --  additional tests on designated types, tagged types ???
         return Conversion_Check (Is_Access_Type (Opnd_Type),
                "illegal operand for access type conversion");

      elsif Ekind (Target_Type) = E_Access_Subprogram_Type then 
         --  check that designated types are type conformant ???
         return Conversion_Check 
                (Ekind (Opnd_Type) = E_Access_Subprogram_Type, 
                "illegal operand for access subprogram conversion");
      else

         return Conversion_Check 
                (Root_Type (Target_Type) = Root_Type (Opnd_Type), 
                "illegal operand for conversion");
      end if;
   end Valid_Conversion;

   ------------------------------
   -- Rewrite_Operator_As_Call --
   ------------------------------

   procedure Rewrite_Operator_As_Call (N : Node_Id; Nam : Entity_Id) is
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

end Sem_Res;
