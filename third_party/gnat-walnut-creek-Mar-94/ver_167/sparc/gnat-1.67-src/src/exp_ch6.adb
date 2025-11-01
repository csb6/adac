------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 6                               --
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

with Atree;    use Atree;
with Comperr;  use Comperr;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Ch9;  use Exp_Ch9;
with Exp_Util; use Exp_Util;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Disp; use Sem_Disp;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Ch6 is

   ------------------------
   --  Local Subprograms --
   ------------------------

   procedure Expand_Call (N : Node_Id);
   --  This procedure contains common processing for Expand_N_Function_Call
   --  and Expand_N_Procedure_Statement.

   function Is_Unchecked_Conversion (Subp : Entity_Id) return Boolean;
   --  Utility to determine whether a function call is a call to an instance
   --  of unchecked conversion, which is transformed into a special node.

   -----------------
   -- Expand_Call --
   -----------------

   --  This procedure handles expansion of function calls and procedure call
   --  statements (i.e. it serves as the body for Expand_N_Function_Call and
   --  Expand_N_Procedure_Call_Statement. Processing for calls includes:

   --    Supply default expressions for missing arguments
   --    Replace "call" to enumeration literal function by literal itself
   --    Rewrite call to predefined operator as operator
   --    Expand dispatching call (see Expand_Dispatch_Call)

   --  Note that the handling of traceback calls is done by the caller, since
   --  it is different for function calls and procedure call statemens. This
   --  processing is done before Expand_Call is called.

   procedure Expand_Call (N : Node_Id) is
      Loc           : constant Source_Ptr := Sloc (N);
      Subp          : Entity_Id;
      Parent_Subp   : Entity_Id;
      Parent_Formal : Entity_Id;

      Actual : Node_Id;
      Formal : Entity_Id;
      Prev   : Node_Id := Empty;

      --  Internal procedure to insert argument corresponding to Formal.
      --  The value is inserted immediately after Prev, or if Prev is Empty,
      --  (case of empty argument list), then into a new list. In both cases
      --  Prev is set to the inserted default for the next call.

      procedure Insert_Default is
         Default : Node_Id;
         Insert, F_Name : Node_Id;

      begin
         Insert := New_Node (N_Parameter_Association, Loc);
         F_Name := New_Node (N_Identifier, Loc);
         Default := New_Copy (Default_Value (Formal));
         Set_Chars (F_Name, Chars (Formal));
         Set_Actual_Parameter (Insert, Default);
         Set_Selector_Name (Insert, F_Name);

         --  Case of insertion is first named actual

         if No (Prev) or else
            Nkind (Parent (Prev)) /= N_Parameter_Association
         then
            Set_Next_Named_Actual (Insert, First_Named_Actual (N));
            Set_First_Named_Actual (N, Default);

            if No (Prev) then
               if not List_Present (Parameter_Associations (N)) then
                  Set_Parameter_Associations (N, New_List);
                  Append (Insert, Parameter_Associations (N));
               end if;
            else
               Insert_After (Prev, Insert);
            end if;

         --  Case of insertion is not first named actual

         else
            Set_Next_Named_Actual (Insert, Next_Named_Actual (Parent (Prev)));
            Set_Next_Named_Actual (Parent (Prev), Default);
            Append (Insert, Parameter_Associations (N));
         end if;

         Prev := Default;
      end Insert_Default;

   --  Start of processing for Expand_Call

   begin
      --  First step, insert default parameter values

      if Nkind (Name (N)) = N_Explicit_Dereference then

         --  Call to an access to subprogram. The type of the name node is
         --  a subprogram type, from which we retrieve the signature.

         Subp := Etype (Name (N));
         Parent_Subp := Empty;

      else
         Subp := Entity (Name (N));
         Parent_Subp := Alias (Subp);
      end if;

      Formal := First_Formal (Subp);
      Actual := First_Actual (N);

      while Present (Formal) loop
         if Present (Actual) then

            --  Check for named and positional parameters in proper place

            if Nkind (Parent (Actual)) /= N_Parameter_Association
              or else Chars (Selector_Name (Parent (Actual))) = Chars (Formal)
            then
               Prev   := Actual;
               Actual := Next_Actual (Actual);
            else
               Insert_Default;
            end if;

         --  Trailing actuals are all defaults

         else
            Insert_Default;
         end if;

         Formal := Next_Formal (Formal);
      end loop;

      if Present (Parent_Subp) then

         while Present (Alias (Parent_Subp)) loop 
            Parent_Subp := Alias (Parent_Subp);
         end loop;

         --  Expand an explicit conversion for parameter of the inherited type

         Formal := First_Formal (Subp);
         Parent_Formal := First_Formal (Parent_Subp);
         Actual := First_Actual (N);
         while Present (Formal) loop
            if (Etype (Formal) /= Etype (Parent_Formal)) then
               Rewrite_Substitute_Tree (Actual,
                 Make_Unchecked_Type_Conversion (Sloc (Actual),
                   Subtype_Mark => 
                     New_Occurrence_Of (Etype (Parent_Formal), Sloc (Actual)),
                   Expression   => New_Copy (Actual)));
               Set_Etype (Actual, Etype (Parent_Formal));
            end if;

            Formal := Next_Formal (Formal);
            Parent_Formal := Next_Formal (Parent_Formal);
            Actual := Next_Actual (Actual);
         end loop;

         Set_Entity (Name (N), Parent_Subp);
         Subp := Parent_Subp;
      end if;

      --  Some more special cases for cases other than explicit dereference

      if Nkind (Name (N)) /= N_Explicit_Dereference then

         --  Calls to an enumeration literal are replaced by the literal

         if Ekind (Subp) = E_Enumeration_Literal then
            Copy_Node (Name (N), N);

         --  Calls to unchecked conversion are replaced by a conversion node

         elsif Is_Intrinsic (Subp)
           and then Is_Unchecked_Conversion (Subp)
         then
            declare
               Op_Node : constant Node_Id := 
                           New_Node (N_Type_Conversion, Sloc (N));

            begin
               Set_Subtype_Mark (Op_Node,
                     New_Occurrence_Of (Etype (Entity (Name (N))), Sloc (N)));
               Set_Expression (Op_Node,  New_Copy (First_Actual (N)));
               Set_Unchecked_Conversion (Op_Node, True);

               Set_Etype (Op_Node,  Etype (N));
               Rewrite_Substitute_Tree (N,  Op_Node);
            end;
         end if;
      end if;

      --  Deals with Dispatch_Call if we still have a call

      if (Nkind (N) = N_Function_Call
           or else Nkind (N) =  N_Procedure_Call_Statement)
        and then Present (Controlling_Argument (N)) then

         Expand_Dispatch_Call (N);

      elsif Is_Abstract (Subp) then
         Error_Msg_N ("call to abstract subprogram must be dispatching", N);
      end if;

   end Expand_Call;

   --------------------------
   -- Expand_Dispatch_Call --
   --------------------------

   --  If Suppress_Tag_Checks are not on, some tag equality tests are
   --  performed before the call. For example, for the call:

   --    F (Tagged_Arg1, Arg2, Tagged_Arg3)

   --  where Tagged_Arg1 is the controlling argument, and Tagged_Arg3 is an
   --  argument that is required to have the same tag, we first generate:

   --    if Tagged_Type (Tagged_Arg1)._Tag /=
   --       Tagged_Type (Tagged_Arg3)._Tag
   --    then
   --       raise Constraint_Error;
   --    end if;

   --  where Tagged_Type is the root type. Then the actual call is:
   --      Acc_Dt (Tagged_Type (Ctrl_Arg)._Tag.all).F___n.all (parameters)

   procedure Expand_Dispatch_Call (Call_Node : Node_Id) is
      Ctrl_Arg    : constant Node_Id   := Controlling_Argument (Call_Node);
      Tagged_Type : constant Entity_Id := Etype (Etype (Ctrl_Arg));
      Subp        : constant Entity_Id := Entity (Name (Call_Node));
      Param_List  : constant List_Id   := Parameter_Associations (Call_Node);
      Prim_Ops    : constant Elist_Id  := Primitive_Operations (Tagged_Type);
      Prim_Op     : Elmt_Id;
      Actual      : Node_Id;
      Pos_In_Dt   : Natural;
      Sloc_N      : constant Source_Ptr := Sloc (Call_Node);
      Tag_Ctrl    : Entity_Id;
      Tag_Actl    : Entity_Id;
      Prim_Ctrl   : Entity_Id;
      New_Actions : List_Id := New_List;

      New_Call_Name : Node_Id;
      New_Call      : Node_Id;

   begin
      --  Expand_Dispatch is called directly from the semantics, so we need
      --  a check to see whether expansion is active before proceeding

      if not Expander_Active then
         return;
      end if;

      --  Compute the position in the dispatch table

      Prim_Op := First_Elmt (Prim_Ops);
      Pos_In_Dt := 1;

      while Id_Of (Prim_Op) /= Subp loop
         Pos_In_Dt := Pos_In_Dt + 1;
         Prim_Op := Next_Elmt (Prim_Op);

         pragma Assert (Prim_Op /= No_Elmt, Compiler_Abort (Call_Node));
      end loop;

      --  Generate code for testing equality of tags among tagged actuals

      if List_Present (Param_List) then

         --  Get the tag component of the Etype (Ctrl_Arg) (= Tagged_Type)

         Tag_Ctrl := Tag_Component (Tagged_Type);
         pragma Assert (Present (Tag_Ctrl), Compiler_Abort (Call_Node));
         Actual := First (Param_List);

         while Present (Actual) loop
            if not Tag_Checks_Suppressed (Etype (Actual))
              and then Is_Tagged_Type (Etype (Actual))
              and then Find_Controlling_Arg (Actual) /= Ctrl_Arg

            --  "=" is the only dispatching operation allowed to get 
            --  operands with incompatible tags (it just returns false)

              and then Chars (Subp) /= Name_Op_Eq
            then

               --  Get the tag component of the Etype (Actual)

               Tag_Actl := Tag_Component (Etype (Actual));
               pragma Assert (Present (Tag_Actl), Compiler_Abort (Call_Node));

               --  Generate code for tag equality check

               Append_To (New_Actions,
                 Make_If_Statement (Sloc_N,
                   Condition =>
                     Make_Op_Ne (Sloc_N,
                       Left_Opnd =>
                         Make_Selected_Component (Sloc_N,
                           Prefix =>
                             Make_Type_Conversion (Sloc_N,
                               Subtype_Mark =>
                                 New_Occurrence_Of (Tagged_Type, Sloc_N),
                               Expression => New_Copy (Ctrl_Arg)),
                           Selector_Name =>
                             New_Reference_To (Tag_Ctrl, Sloc_N)),

                       Right_Opnd =>
                         Make_Selected_Component (Sloc_N,
                           Prefix =>
                             Make_Type_Conversion (Sloc_N,
                               Subtype_Mark =>
                                 New_Occurrence_Of (Tagged_Type, Sloc_N),
                               Expression => New_Copy (Actual)),
                           Selector_Name =>
                             New_Reference_To (Tag_Actl, Sloc_N))),

                   Then_Statements =>
                     New_List_1 (New_Constraint_Error (Sloc_N))));
            end if;

            Actual := Next (Actual);
         end loop;
      end if;

      --  Generate the call itself by calling the right function in the
      --  dispatch table:

      --    Acc_Dt (Tagged_Type (Ctrl_Arg)._Tag.all).Prim_{Pos_IN_Dt + 2}

      New_Call_Name :=
        Make_Selected_Component (Sloc_N,
          Prefix => Make_DT_Access (Sloc_N, Ctrl_Arg, Tagged_Type),
          Selector_Name =>
            Make_DT_Component (Sloc_N, Tagged_Type, Pos_In_Dt + 2));

      if Is_Empty_List (New_Actions) then

         --  Replace subprogram call by the dispatching call

         if Nkind (Call_Node) = N_Function_Call then
            New_Call :=
              Make_Function_Call (Sloc_N,
                Name => New_Call_Name,
                Parameter_Associations => New_List_Copy (Param_List));

         else
            New_Call :=
              Make_Procedure_Call_Statement (Sloc_N,
                Name => New_Call_Name,
                Parameter_Associations => New_List_Copy (Param_List));

         end if;

      else
         --  If function call then generate expression_actions node whose
         --  actions are the if statements generated and expression is
         --  the dispatching call.

         if Nkind (Call_Node) = N_Function_Call then
            New_Call :=
              Make_Expression_Actions (Sloc_N,
                Actions => New_Actions,
                Expression =>
                  Make_Function_Call (Sloc_N,
                    Name => New_Call_Name,
                    Parameter_Associations => New_List_Copy (Param_List)));

         --  Else procedure call then analyze the if statements created,
         --  insert them before the subprogram call and create the
         --  dispatching call with a procedure call.

         else
            Analyze_Declarations (New_Actions);
            Insert_List_Before (Call_Node, New_Actions);

            New_Call :=
              Make_Procedure_Call_Statement (Sloc_N,
                Name => New_Call_Name,
                Parameter_Associations => New_List_Copy (Param_List));
         end if;
      end if;

      --  Analyze the dispatching call and rewrite the subprogram call node.

      Analyze (New_Call);

      --  The controlling argument is reset to avoid problems with
      --  Rewrite_Substitute_Tree.

      Set_Controlling_Argument (Call_Node, Empty);
      Rewrite_Substitute_Tree (Call_Node, New_Call);
      Set_Controlling_Argument (Original_Node (Call_Node), Ctrl_Arg);
   end Expand_Dispatch_Call;

   ----------------------------
   -- Expand_N_Function_Call --
   ----------------------------

   procedure Expand_N_Function_Call (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      Expand_Call (N);
   end Expand_N_Function_Call;

   ---------------------------------------
   -- Expand_N_Procedure_Call_Statement --
   ---------------------------------------

   procedure Expand_N_Procedure_Call_Statement (N : Node_Id) is
      Call : Node_Id;

   begin
      Traceback_Store (N);
      Expand_Call (N);

   end Expand_N_Procedure_Call_Statement;

   ------------------------------
   -- Expand_N_Subprogram_Body --
   ------------------------------

   --  Add return statement if last statement in body is not a return
   --  statement (this makes things easier on Gigi which does not want
   --  to have to handle a missing return).

   --  Add call to Activate_Tasks if body is a task activator

   --  Add call to Establish_Task_Master if body is a master. Note that this
   --  must be done before the processing that adds a return statement.

   --  Finally, if traceback mode is active, insert a call to the traceback
   --  routine at the start of the statement sequence. This is not done in
   --  the case of library procedures, since the version information is not
   --  available at the right time in these cases.

   procedure Expand_N_Subprogram_Body (N : Node_Id) is
      S : constant List_Id := Statements (Handled_Statement_Sequence (N));
      Rtn      : Node_Id;
      Body_Ent : Entity_Id;
      Spec_Ent : Entity_Id;
      Nam      : Node_Id;

   begin
      --  Get entities for subprogram body and spec

      Nam := Defining_Unit_Name (Specification (N));

      if Nkind (Nam) = N_Defining_Program_Unit_Name then
         Body_Ent := Name (Nam);
      else
         Body_Ent := Nam;
      end if;

      if Present (Corresponding_Spec (N)) then
         Spec_Ent := Corresponding_Spec (N);
      else
         Spec_Ent := Body_Ent;
      end if;

      --  Activate tasks if this is a task activator

      Build_Task_Activation_Call (N);

      --  Establish master if this is a task master

      if Is_Task_Master (N) then
         Establish_Task_Master (N);
      end if;

      --  Return processing happens for non-generic procedures which do not
      --  have a return as the last statement in the statement sequence.. Note
      --  that we don't bother to reanalyze the new body with the added return
      --  statement, since it would involve a lot of unnecessary work that
      --  would achieve precisely nothing.

      if Ekind (Spec_Ent) = E_Procedure
        and then
          (Is_Empty_List (S)
            or else Nkind (Last (S)) /= N_Return_Statement)
      then
         Rtn := Make_Return_Statement (Sloc (Last (S)));
         Append (Rtn, S);
      end if;

   end Expand_N_Subprogram_Body;

   -----------------------------
   -- Is_Unchecked_Conversion --
   -----------------------------

   function Is_Unchecked_Conversion (Subp : Entity_Id) return Boolean is
   begin
      return Nkind (Parent (Subp)) = N_Function_Specification
        and then Present (Generic_Parent (Parent (Subp)))
        and then Chars (Generic_Parent (Parent (Subp))) =
                                       Name_Unchecked_Conversion;
   end Is_Unchecked_Conversion;

end Exp_Ch6;
