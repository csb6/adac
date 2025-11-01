------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ U T I L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.24 $                             --
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
with Debug;     use Debug;
with Einfo;     use Einfo;
with Errcount;  use Errcount;
with Lib;       use Lib;
with Namet;     use Namet;
with Nmake;     use Nmake;
with Opt;       use Opt;
with Rtsfind;   use Rtsfind;
with Sem;       use Sem;
with Sem_Ch13;  use Sem_Ch13;
with Sem_Res;   use Sem_Res;
with Sem_Util;  use Sem_Util;
with Sinfo;     use Sinfo;
with Sinput;    use Sinput;
with Snames;    use Snames;
with Stand;     use Stand;
with Table;
with Tbuild;    use Tbuild;
with Ttypes;    use Ttypes;
with Uintp;     use Uintp;

package body Exp_Util is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Expand_Composite_Equality 
     (Loc : Source_Ptr; Typ : Entity_Id; Lhs, Rhs : Node_Id) return Node_Id;
   --  Local recursive function used to expand equality for nested 
   --  composite types. Used by Expand_Record_Equality, Expand_Array_Equality.

   ----------------
   -- Local Data --
   ----------------

   --  The following table is used to save values of the Expander_Active
   --  flag when they are saved by Expander_Mode_Save_And_Set. We use an
   --  extendible table (which is a bit of overkill) because it is easier
   --  than figuring out a maximum value or bothering with range checks!

   package Expander_Flags is new Table (
      Component_Type => Boolean,
      Index_Type     => Int,
      Low_Bound      => 0,
      Initial        => 32,
      Increment      => 200,
      Table_Name     => "Expander_Flags");

   ----------------                                                          
   -- Build_Call --                                                          
   ----------------                                                          

   function Build_Call (Loc : Source_Ptr; E : Entity_Id) return Node_Id is   
      Call : Node_Id;                                                        

   begin                                                                     
      Call :=                                                                
        Make_Procedure_Call_Statement (Loc,                                  
          Name => New_Reference_To (E, Loc));                                

      Analyze (Call);                                                        
      Resolve_Subexpr (Call, Standard_Void_Type);                            
      return Call;                                                           
   end Build_Call;                                                           

   --------------------------------
   -- Expander_Mode_Save_And_Set --
   --------------------------------

   procedure Expander_Mode_Save_And_Set (Status : Boolean) is
   begin
      Expander_Flags.Increment_Last;
      Expander_Flags.Table (Expander_Flags.Last) := Expander_Active;
      Expander_Active := Status;
   end Expander_Mode_Save_And_Set;

   ---------------------------
   -- Expander_Mode_Restore --
   ---------------------------

   procedure Expander_Mode_Restore is
   begin
      Expander_Active := Expander_Flags.Table (Expander_Flags.Last);
      Expander_Flags.Decrement_Last;

      if Errors_Detected /= 0 then
         Expander_Active := False;
      end if;
   end Expander_Mode_Restore;

   ----------------------------
   -- Expand_Class_Allocator --
   ----------------------------

   --  The Node N is assumed to be a N_Allocator with a N_Qualified_Expression
   --  in its expression field

   procedure Expand_Class_Allocator
     (N : Node_Id; Acc_Type : Entity_Id; Class_Subtype : Entity_Id)
   is
      Exp        : constant Node_Id := Expression (Expression (N));
      Loc        : constant Source_Ptr := Sloc (N);
      List_Decl  : List_Id;
      Equiv_Type : Entity_Id;

   begin
      --  Expand_Class_Allocator is called directly from the semantics, so we
      --  must check to see whether expansion is active before proceeding

      if not Expander_Active then
         return;
      end if;

      List_Decl := Expand_Class_Subtype (Loc, Class_Subtype, Exp);
      Equiv_Type := Equivalent_Type (Class_Subtype);

      Rewrite_Substitute_Tree (N,
        Make_Expression_Actions (Loc,
          Actions => List_Decl,
          Expression =>
            Make_Unchecked_Type_Conversion (Loc,
              Subtype_Mark => New_Reference_To (Acc_Type, Loc),
              Expression =>
                Make_Allocator (Loc,
                  Expression =>
                    Make_Qualified_Expression (Loc,
                      Subtype_Mark => New_Reference_To (Equiv_Type, Loc),
                      Expression =>
                        Make_Unchecked_Type_Conversion (Loc,
                          Subtype_Mark => New_Reference_To (Equiv_Type, Loc),
                          Expression => New_Copy (Exp)))))));

      Analyze (N);

   end Expand_Class_Allocator;

   --------------------------
   -- Expand_Class_Subtype --
   --------------------------

   --  Create a record type used as an equivalent of any member of the class
   --  which takes its size from exp.

   --  Generate the following code:

   --   type Equiv_T is record
   --     _parent :  T (List of discriminant constaints taken from Exp);
   --     Ext__50 : String (1 .. (Exp'size - Typ'size) / Character'size);
   --   end Equiv_T;

   function Expand_Class_Subtype
     (Loc : Source_Ptr; Typ : Entity_Id; Exp : Node_Id) return List_Id
   is
      Root_Type   : constant Entity_Id := Etype (Typ);
      D           : Entity_Id;
      Equiv_Type  : Entity_Id;
      Range_Type  : Entity_Id;
      Str_Type    : Entity_Id;
      List_Def    : List_Id := Empty_List;
      Constr_Root : Entity_Id;
      List_Constr : List_Id;

   begin
      --  ??? Exp should be evaluate once but it causes mysterious bug in the
      --  code generation, sso it is commented out till further investigation 
      --  Set_Evaluate_Once (Exp, True);

      --  subtype cstr__n is T (List of discriminant constaints taken from Exp)

      if not Has_Discriminants (Root_Type) then
         Constr_Root := Root_Type;
      else
         List_Constr := New_List;
         D := First_Discriminant (Root_Type);

         while (Present (D)) loop

            Append_To (List_Constr,
              Make_Selected_Component (Loc,
                Prefix => New_Copy (Exp),
                Selector_Name => New_Reference_To (D, Loc)));

            D := Next_Discriminant (Root_Type);
         end loop;

         Constr_Root :=
           Make_Defining_Identifier (Loc, New_Internal_Name ("cstr"));

         Append_To (List_Def,
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier => Constr_Root,
             Type_Definition =>
               Make_Derived_Type_Definition (Loc,
                Subtype_Indication =>
                  Make_Subtype_Indication (Loc,
                    Subtype_Mark => New_Reference_To (Root_Type, Loc),
                    Constraint =>
                      Make_Index_Or_Discriminant_Constraint (Loc,
                        Constraints => List_Constr)))));
      end if;

      --  subtype rg__xx is Positive range
      --                           (Exp'size - typ'size) / Character'Size

      Range_Type := Make_Defining_Identifier (Loc, New_Internal_Name ("rg"));
      Append_To (List_Def,
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Range_Type,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (Standard_Positive, Loc),
              Constraint => Make_Range_Constraint (Loc,
                Range_Expression =>
                  Make_Range (Loc,
                    Low_Bound => Make_Integer_Literal (Loc, Uint_1),
                    High_Bound =>
                      Make_Op_Divide (Loc,
                        Left_Opnd =>
                          Make_Op_Subtract (Loc,
                            Left_Opnd =>
                              Make_Attribute_Reference (Loc,
                                Prefix => New_Copy (Exp),
                                Identifier =>
                                  Make_Identifier (Loc, Name_Size)),
                            Right_Opnd =>
                              Make_Attribute_Reference (Loc,
                                Prefix => New_Reference_To (Constr_Root, Loc),
                                Identifier =>
                                  Make_Identifier (Loc, Name_Size)),
                            Parens => True),
                        Right_Opnd => Make_Integer_Literal (Loc,
                          Intval =>
                            UI_From_Int (Standard_Character_Size))))))));

      --  subtype str__nn is String (rg__x);

      Str_Type := Make_Defining_Identifier (Loc, New_Internal_Name ("str"));
      Append_To (List_Def,
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Str_Type,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (Standard_String, Loc),
              Constraint =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints =>
                    New_List_1 (New_Reference_To (Range_Type, Loc))))));

      --  type Equiv_T is record
      --    _parent : ityp__n;
      --    E : Str_Type;
      --  end Equiv_T;

      Equiv_Type := Make_Defining_Identifier (Loc, New_Internal_Name ("ityp"));
      Append_To (List_Def,
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Equiv_Type,
          Type_Definition =>
            Make_Record_Definition (Loc,
              Component_List => Make_Component_List (Loc,
                Component_Declarations => New_List_2 (
                  Make_Component_Declaration (Loc,
                    Defining_Identifier => 
                      Make_Defining_Identifier (Loc, Name_uParent),
                    Subtype_Indication => New_Reference_To (Constr_Root, Loc)),
                  Make_Component_Declaration (Loc,
                    Defining_Identifier => 
                      Make_Defining_Identifier (Loc,
                        Chars => New_Internal_Name ("ext")),
                    Subtype_Indication => New_Reference_To (Str_Type, Loc))),
                Variant_Part => Empty))));

      D := First (List_Def);

      --  Insert remaining implicit types before analyzing

      if Is_Non_Empty_List (Implicit_Type_List) then
         Insert_List_Before (D, Implicit_Type_List);
         Implicit_Type_List := New_List;
      end if;

      while (Present (D)) loop

         Analyze (D);
         D := Next (D);
      end loop;

      Set_Equivalent_Type (Typ, Equiv_Type);

      --  Insert the implicit node corresponding to the Class subtype
      --  after its equivalent type

      Append_To (List_Def, Make_Implicit_Type (Sloc (Typ), Typ));

      Append_List_To (List_Def, Freeze_Entity (Equiv_Type));
      return List_Def;
   end Expand_Class_Subtype;

   ---------------------------
   -- Expand_Array_Equality --
   ---------------------------

   function Expand_Array_Equality 
     (Loc : Source_Ptr; Typ : Entity_Id; Lhs, Rhs : Node_Id) return Node_Id is

      Decls       : List_Id := New_List;
      Index_List1 : List_Id := New_List;
      Index_List2 : List_Id := New_List;
      Index       : Entity_Id := First_Index (Typ);
      Index_Type  : Entity_Id;
      Formals     : List_Id;
      Result      : Node_Id;
      Stats       : Node_Id;
      Func_Name   : Entity_Id;
      Func_Body   : Node_Id;

      A : constant Entity_Id := Make_Defining_Identifier (Loc, Name_A);
      B : constant Entity_Id := Make_Defining_Identifier (Loc, Name_B);

      function Component_Equality (Typ : Entity_Id) return Node_Id;
      --  Create one statement to compare corresponding components, designated
      --  by a full set of indices.

      function Loop_One_Dimension (N : Int) return Node_Id;
      --  Loop over the n'th dimension of the arrays. The single statement
      --  in the body of the loop is a loop over the next dimension, or  
      --  the comparison of corresponding components.

      ------------------------
      -- Component_Equality --
      ------------------------

      function Component_Equality (Typ : Entity_Id) return Node_Id is
         Test : Node_Id;
         L, R : Node_Id;

      begin

         --  if a(i1...) /= b(j1...) then return false; end if;

         L := Make_Indexed_Component (Loc,
                Prefix => Make_Identifier (Loc, Chars (A)),
                Expressions => Index_List1);

         R := Make_Indexed_Component (Loc,
                Prefix => Make_Identifier (Loc, Chars (B)),
                Expressions => Index_List2);

         if Is_Record_Type (Component_Type (Typ))
              or else Is_Array_Type (Component_Type (Typ))
         then
            Test := 
              Expand_Composite_Equality (Loc, Component_Type (Typ), L, R);
         else
            Test := Make_Op_Eq (Loc, Left_Opnd => L, Right_Opnd => R);
         end if;

         return Make_If_Statement (Loc,
           Condition => Make_Op_Not (Loc, Right_Opnd => Test),  
           Then_Statements => New_List_1 (
             Make_Return_Statement (Loc,
               Expression => New_Occurrence_Of (Standard_False, Loc))));

      end Component_Equality;

      ------------------------
      -- Loop_One_Dimension --
      ------------------------

      function Loop_One_Dimension (N : Int) return Node_Id is
         I : constant Entity_Id := Make_Defining_Identifier (Loc, 
                                                  New_Internal_Name ("i"));
         J : constant Entity_Id := Make_Defining_Identifier (Loc, 
                                                  New_Internal_Name ("j"));
         Stats : Node_Id;
      begin
         if N > Number_Dimensions (Typ) then
            return Component_Equality (Typ);

         else
            --  Generate the following:

            --  j: index_type := b'first (n);
            --  ...
            --  if a'length (n) /= b'length (n) then
            --    return false;
            --  else
            --     for i in a'range (n) loop
            --        --  loop over remaining dimensions.
            --        j := index_type'succ (j);
            --     end loop;
            --  end if;

            --  retrieve index type for current dimension.
            Index_Type := Base_Type (Etype (Index));
            Append (New_Reference_To (I, Loc), Index_List1);
            Append (New_Reference_To (J, Loc), Index_List2);

            --  Declare index for j as a local variable to the function.
            --  Index i is a loop variable.

            Append (Make_Object_Declaration (Loc,
              Defining_Identifier => J,
              Object_Definition   => New_Reference_To (Index_Type, Loc),
              Expression =>
                Make_Attribute_Reference (Loc,
                  Prefix => Make_Identifier (Loc, Chars (B)),
                  Identifier => Make_Identifier (Loc, Name_First), 
                  Expression =>
                      Make_Integer_Literal (Loc, UI_From_Int (N)))), Decls);

            Stats := Make_If_Statement (Loc,
                Condition =>
                  Make_Op_Ne (Loc,
                    Left_Opnd =>
                      Make_Attribute_Reference (Loc,
                        Prefix => Make_Identifier (Loc, Chars (A)),
                        Identifier => Make_Identifier (Loc, Name_Length),
                        Expression =>
                          Make_Integer_Literal (Loc, UI_From_Int (N))),
                    Right_Opnd =>
                      Make_Attribute_Reference (Loc,
                        Prefix => Make_Identifier (Loc, Chars (B)),
                        Identifier => Make_Identifier (Loc, Name_Length),
                        Expression =>
                          Make_Integer_Literal (Loc, UI_From_Int (N)))),

                Then_Statements => New_List_1 (
                  Make_Return_Statement (Loc,
                    Expression => New_Occurrence_Of (Standard_False, Loc))),

                Else_Statements => New_List_1 (
                  Make_Loop_Statement (Loc,
                    Identifier => Empty,
                    Iteration_Scheme =>
                      Make_Iteration_Scheme (Loc,
                        Loop_Parameter_Specification =>
                          Make_Loop_Parameter_Specification (Loc,
                            Defining_Identifier => I,
                            Discrete_Subtype_Definition =>
                              Make_Attribute_Reference (Loc,
                                Prefix => New_Reference_To (A, Loc),
                                Identifier => 
                                  Make_Identifier (Loc, Name_Range),
                                Expression =>
                                  Make_Integer_Literal 
                                               (Loc, UI_From_Int (N))))),
                    Statements => New_List_2 (
                      Loop_One_Dimension (N + 1),
                      Make_Assignment_Statement (Loc,
                        Name => New_Reference_To (J, Loc),
                        Expression =>
                          Make_Attribute_Reference (Loc,
                            Prefix => New_Reference_To (Index_Type, Loc),
                            Identifier => Make_Identifier (Loc, Name_Succ),
                            Expression => New_Reference_To (J, Loc)))))));

            Index := Next_Index (Index);
            return Stats;
         end if;   
      end Loop_One_Dimension;

   ------------------------------------------
   -- Processing for Expand_Array_Equality --
   ------------------------------------------

   begin  

      Formals := New_List_2 (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => A,
          Parameter_Type => New_Reference_To (Typ, Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier => B,
          Parameter_Type => New_Reference_To (Typ, Loc)));

      Func_Name := Make_Defining_Identifier (Loc,  New_Internal_Name ("eq"));

      Stats := Loop_One_Dimension (1);

      Func_Body :=
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name => Func_Name,
              Parameter_Specifications => Formals,
              Subtype_Mark => New_Reference_To (Standard_Boolean,  Loc)),
          Declarations =>  Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List_1 (Stats)));

         Set_Has_Completion (Func_Name, True);

         Result := Make_Expression_Actions (Loc,
            Actions => New_List_1 (Func_Body),
            Expression => Make_Function_Call (Loc,
            Name => New_Reference_To (Func_Name, Loc),
            Parameter_Associations => New_List_2 (Lhs, Rhs)));

         return Result;
   end Expand_Array_Equality;

   ----------------------------
   -- Expand_Record_Equality --
   ----------------------------

   --  For non-variant records Equality is expanded into:

   --    Lhs.Cmp1 = Rhs.Cmp1                             
   --      and then Lhs.Cmp2 = Rhs.Cmp2                  
   --      and then ...                                  
   --      and then Lhs.Cmpn = Rhs.Cmpn                  
   --                                                    
   --  The expression is folded by the back-end for adjacent fields. This
   --  function can be called for tagged records but only in the case of
   --  implemetation of predefined equality (see Predefined_Primitives_Bodies)

   function Expand_Record_Equality 
     (Loc : Source_Ptr; Typ : Entity_Id; Lhs, Rhs : Node_Id) return Node_Id is

      function Build (Comp : Entity_Id) return Node_Id;
      --  Build recursively the sequence of components equalities using the 
      --  equality (predefined or primitive) for each component type. 

      function Build (Comp : Entity_Id) return Node_Id is
         Eq_Node : Node_Id;
         L, R    : Node_Id;
         Next_Comp : Entity_Id := Next_Component (Comp);

      begin
         --  The inherited Components are skipped (they are part of the parent)

         while Present (Next_Comp) 
                 and then Next_Comp /= Original_Record_Component (Next_Comp)
         loop 
            Next_Comp := Next_Component (Next_Comp);
         end loop;

         L := Make_Selected_Component (Loc,
                Prefix => Lhs,
                Selector_Name => New_Reference_To (Comp, Loc));

         R := Make_Selected_Component (Loc,
                Prefix => Rhs,
                Selector_Name => New_Reference_To (Comp, Loc));

         if Is_Record_Type (Etype (Comp)) 
              or else Is_Array_Type (Etype (Comp))
         then

            --  recursive call to deal with recursive composite types 
            Eq_Node := Expand_Composite_Equality (Loc, Etype (Comp), L, R);
         else
            Eq_Node := Make_Op_Eq (Loc, Left_Opnd => L, Right_Opnd => R); 
         end if;

         if No (Next_Comp) then
            return Eq_Node;
         else 
            return Make_Op_And_Then (Loc, 
                     Left_Opnd  => Eq_Node, 
                     Right_Opnd => Build (Next_Comp));
         end if;
      end Build;

   --  Start of processing for Expand_Record_Equality

   begin
      --  Generates the following code: (assuming that component C2 is 
      --  also a record) 

      --  <if no component>
      --  True
      --  <else>
      --   Lhs.C1 = Rhs.C1
      --     and then Lhs.C2.C1=Rhs.C2.C1 and then ... Lhs.C2.Cn=Rhs.C2.Cn
      --     and then ...
      --     and then Lhs.Cmpn = Rhs.Cmpn

      if No (First_Component (Typ)) then 
         return New_Reference_To (Standard_True, Loc);
      else 
         return Build (First_Component (Typ));
      end if;
   end Expand_Record_Equality;

   -------------------------------
   -- Expand_Composite_Equality --
   -------------------------------

   --  This function is only called for comparing internal fields of composite
   --  types when these fields are themselves composites. This is a special
   --  case because it is not possible to respect normal Ada visibility rules.

   function Expand_Composite_Equality 
     (Loc : Source_Ptr; Typ : Entity_Id; Lhs, Rhs : Node_Id) return Node_Id is
      Full_Type : Entity_Id;
      Prim      : Elmt_Id;

   begin

      if Ekind (Typ) = E_Private_Type then
         Full_Type := Full_Declaration (Typ);
      else 
         Full_Type := Typ;
      end if;

      Full_Type := Base_Type (Full_Type);

      if Is_Array_Type (Full_Type) then

         if Is_Scalar_Type (Component_Type (Full_Type)) then
            return Make_Op_Eq (Loc, Left_Opnd  => Lhs, Right_Opnd => Rhs); 
         else
            return Expand_Array_Equality (Loc, Full_Type, Lhs, Rhs);
         end if;

      elsif Is_Tagged_Type (Full_Type) then

         --  Call the primitive operation "=" of this type 

         if Is_Class_Type (Full_Type) then
            Full_Type := Etype (Full_Type);
         end if;

         Prim := First_Elmt (Primitive_Operations (Full_Type));

         while Chars (Id_Of (Prim)) /= Name_Op_Eq loop
            Prim := Next_Elmt (Prim);
            pragma Assert (Prim /= No_Elmt);
         end loop;

         return
           Make_Function_Call (Loc,
             Name => New_Reference_To (Id_Of (Prim), Loc),
             Parameter_Associations => New_List_2 (Lhs, Rhs));

      elsif Is_Record_Type (Full_Type) then
         return Expand_Record_Equality (Loc, Full_Type, Lhs, Rhs);
      else

         --  This procedure should only be called on composite types

         pragma Assert (False);
         return Empty;
      end if;
   end Expand_Composite_Equality;

   ------------------------------
   -- Expand_Subtype_From_Expr --
   ------------------------------

   --  For an unconstained type T, change  "Val : T := Expr;" into

   --    Val : T (Expr'First .. Expr'Last) := Expr;

   --  or if Expr is a String litteral or an array Aggregate

   --    Val : T (IndexTyp'first .. IndexTyp'first + Length (Expr) - 1) 
   --        := Expr;

   procedure Expand_Subtype_From_Expr (N : Node_Id; T : Entity_Id) is
      Index_Typ : Entity_Id;
      New_Def   : Node_Id;
      Loc       : constant Source_Ptr := Sloc (N);

   begin
      --  Expand_Subtype_From_Expr is called directly from the semantics, so
      --  we must check to see whether expansion is active before proceeding.

      if not Expander_Active then                                            
         return;                                                             
      end if;                                                                

      if Nkind (Original_Node (Expression (N))) = N_Aggregate then
         Unimplemented (N, "unconstrained decl. with aggregate init value");
         return;

      elsif Is_Record_Type (T) then 
         Unimplemented (N, "unconstrained record decl. with init value");
         return;
      end if;

      Set_Evaluate_Once (Expression (N), True);      

      if Nkind (Expression (N)) = N_String_Literal then

         --  the case: T (IndexTyp'first .. IndexTyp'first + Length(Expr) - 1) 

         Index_Typ := Etype (First_Index (T));
         New_Def := Make_Subtype_Indication (Loc,
           Subtype_Mark => New_Copy (Object_Definition (N)),
           Constraint => 
             Make_Index_Or_Discriminant_Constraint (Loc,
               Constraints => New_List_1 (
                 Make_Range (Loc,
                   Low_Bound => 
                     Make_Attribute_Reference (Loc,
                       Prefix =>  New_Occurrence_Of (Index_Typ, Loc),
                       Identifier => Make_Identifier (Loc, Name_First)),

                   High_Bound => 
                     Make_Op_Subtract (Loc,
                        Left_Opnd => 
                          Make_Op_Add (Loc,
                            Left_Opnd => 
                              Make_Attribute_Reference (Loc,
                                Prefix => New_Occurrence_Of (Index_Typ, Loc),
                                Identifier => 
                                  Make_Identifier (Loc, Name_First)),

                            Right_Opnd => Make_Integer_Literal (Loc, 
                              String_Literal_Length (Etype (Expression (N))))),

                        Right_Opnd => Make_Integer_Literal (Loc, Uint_1))))));

      else
         --  The simple case : T (Expr'First .. Expr'Last)

         New_Def := Make_Subtype_Indication (Loc,
           Subtype_Mark => New_Copy (Object_Definition (N)),
           Constraint => 
             Make_Index_Or_Discriminant_Constraint (Loc,
               Constraints => New_List_1 (
                 Make_Range (Loc,
                   Low_Bound => 
                     Make_Attribute_Reference (Loc,
                       Prefix => New_Copy (Expression (N)),
                       Identifier => Make_Identifier (Loc, Name_First)),

                   High_Bound => 
                     Make_Attribute_Reference (Loc,
                       Prefix => New_Copy (Expression (N)),
                       Identifier => Make_Identifier (Loc, Name_Last))))));
      end if;

      --  Substitute the new tree

      Rewrite_Substitute_Tree (Object_Definition (N), New_Def);

   end Expand_Subtype_From_Expr;

   -----------------------------
   -- Expand_Tagged_Extension --
   -----------------------------

   --  Add a field _parent at the beginning of the record. This field
   --  contains a value of the Parent Type. It is used to implement 
   --  inheritance. Here is an example of expansion:

   --    type T2 (B, C : Int) is new T1 (A => B) with record
   --       _Parent : T1 (A => B);   <--- this is the expanded field
   --       D : Int;
   --    end;

   procedure Expand_Tagged_Extension (Def : Node_Id) is
      Indic        : constant Node_Id := Subtype_Indication (Def);
      Rec_Ext_Part : constant Node_Id := Record_Extension_Part (Def);
      Comp_List    : constant Node_Id := Component_List (Rec_Ext_Part);
      Comp_Decl    : Node_Id;
      Parent_N     : Node_Id;
      Sloc_N       : Source_Ptr;

   begin
      --  Expand_Tagged_Extension is called directly from the semantics, so
      --  we must check to see whether expansion is active before proceeding  

      if not Expander_Active then                                            
         return;                                                             
      end if;                                                                

      Sloc_N := Sloc (Comp_List);
      Parent_N := Make_Defining_Identifier (Sloc_N, Name_uParent);

      Comp_Decl :=
        Make_Component_Declaration (Sloc_N,
          Defining_Identifier => Parent_N,
          Subtype_Indication  => New_Copy (Indic));

      if Null_Present (Rec_Ext_Part) then
         Set_Component_List (
           Rec_Ext_Part,
           Make_Component_List (Sloc_N,
             Component_Declarations => New_List_1 (Comp_Decl),
             Variant_Part => Empty,
             Null_Present => False));
         Set_Null_Present (Rec_Ext_Part, False);

      elsif Is_Empty_List (Component_Declarations (Comp_List)) then
         Set_Component_Declarations (Comp_List, New_List_1 (Comp_Decl));

      else
         Insert_Before (First (Component_Declarations (Comp_List)), Comp_Decl);
      end if;

   end Expand_Tagged_Extension;

   ------------------------
   -- Expand_Tagged_Root --
   ------------------------

   procedure Expand_Tagged_Root (Def : Node_Id) is
      Comp_List : Node_Id;
      Comp_Decl : Node_Id;
      Tag_N     : Node_Id;
      Sloc_N    : Source_Ptr;

   begin
      --  Expand_Tagged_Root is called directly from the semantics, so we
      --  must check to see whether expansion is active before proceeding  

      if not Expander_Active then                                            
         return;                                                             
      end if;                                                                

      if Null_Present (Def) then
         Set_Component_List (Def,
           Make_Component_List (Sloc (Def),
             Component_Declarations => Empty_List,
             Variant_Part => Empty,
             Null_Present => True));
      end if;

      Comp_List := Component_List (Def);

      if Null_Present (Comp_List) then
         Sloc_N := Sloc (Comp_List);
      else
         Sloc_N := Sloc (First (Component_Declarations (Comp_List)));
      end if;

      Tag_N := Make_Defining_Identifier (Sloc_N, Name_uTag);
      Comp_Decl :=
        Make_Component_Declaration (Sloc_N,
          Defining_Identifier => Tag_N,
          Subtype_Indication  =>
            New_Reference_To (RTE (RE_Tag), Sloc_N));

      if Null_Present (Comp_List) then
         Set_Component_Declarations (Comp_List, New_List_1 (Comp_Decl));
         Set_Null_Present (Comp_List, False);

      else
         Insert_Before (First (Component_Declarations (Comp_List)), Comp_Decl);
      end if;

   end Expand_Tagged_Root;

   ------------------------
   -- Protect_Statements --
   ------------------------

   procedure Protect_Statements (N : Node_Id; E : Entity_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Stm : constant Node_Id    := Handled_Statement_Sequence (N);

   begin
      --  If the existing statement sequence has no exception handlers, then
      --  all we need to do is to add the specified cleanup call. If there
      --  are exception handlers present, then we have to wrap an extra
      --  block around to hold the cleanup call because of the current rule
      --  that a block cannot have both a cleanup and exception handlers.

      if Exception_Handlers (Stm) = No_List
        and then No (Identifier (Stm))
      then
         Set_Identifier (Stm, New_Occurrence_Of (E, Loc));

      else
         Set_Handled_Statement_Sequence (N,
           Make_Handled_Sequence_Of_Statements (Loc,
             Statements => New_List_1 (
               Make_Block_Statement (Loc,
                 Identifier => Empty,
                 Handled_Statement_Sequence => Stm)),
             Identifier => New_Occurrence_Of (E, Loc)));
      end if;

   end Protect_Statements;

   ---------------------
   -- Traceback_Store --
   ---------------------

   procedure Traceback_Store (N : Node_Id; Anal : Boolean := True) is
      Loc        : constant Source_Ptr := Sloc (N);
      Call       : Node_Id;
      Vname      : Name_Id;
      Kind       : Node_Kind;
      Prv        : Node_Id;
      Unum       : Unit_Number_Type;
      Unit_Node  : Node_Id;
      Sparm      : Node_Id;

   begin
      --  Immediate return if traceback flag is off, nothing to do

      if not Debug_Flag_B then
         return;
      end if;

      --  Immediate return if the node we are inserting before is another
      --  traceback store node (stops an infinite loop!)

      if Nkind (N) = N_Procedure_Call_Statement
        and then Nkind (Name (N)) = N_Identifier
        and then Entity (Name (N)) = RTE (RE_Store_TB)
      then
         return;
      end if;

      --  Immediate return if we are inserting immediately after another
      --  traceback store node (this is just to avoid unnecssary calls)

      Prv := Prev (N);

      if Present (Prv)
        and then Nkind (Prv) = N_Procedure_Call_Statement
        and then Nkind (Name (Prv)) = N_Identifier
        and then Entity (Name (Prv)) = RTE (RE_Store_TB)
      then
         return;
      end if;

      --  Immediate return if not main unit, or a subunit. All other units
      --  are being compiled for subsidiary use and do not need traceback
      --  calls inserted (furthermore, if they did have calls inserted, we
      --  get into trouble with System.Traceback itself!

      Unum := Get_Sloc_Unit_Number (Loc);

      if Unum /= Main_Unit
        and then Nkind (Unit (File.Table (Unum).Cunit)) /= N_Subunit
      then
         return;
      end if;

      --  Conditions for the generation of the traceback call are met. The
      --  call to be generated has one of the forms:

      --    Store_TB (linenum, Version_x'Address, _TB_Snam'Address);
      --    Store_TB (linenum, Version_x'Address, Null_Address);

      --  where Version_x is Version_B in a file that is a body, and
      --  Version_S in a file that is a spec. The null address form
      --  is used when no current subprogram name is visible.

      --  First decide whether to use to spec or body version, or to
      --  abandon generating anything (happens in generic unit case)

      Unit_Node := Unit (File.Table (Unum).Cunit);

      if Nkind (Unit_Node) = N_Subunit then
         Unit_Node := Proper_Body (Unit_Node);
      end if;


      if Nkind (Unit_Node) = N_Package_Declaration then
         Vname := Name_Version_S;

      elsif Nkind (Unit_Node) = N_Subprogram_Body
        or else Nkind (Unit_Node) = N_Package_Body
      then
         Vname := Name_Version_B;

      --  In some unit, probably a generic, where it makes no sense to
      --  generate traceback data in any case.

      else
         return;
      end if;

      --  Figure out what we need as third parameter

      if Present (Get_Name_Entity_Id (Name_uTB_Snam)) then
         Sparm :=
           Make_Attribute_Reference (Loc,
             Prefix     => Make_Identifier (Loc, Name_uTB_Snam),
             Identifier => Make_Identifier (Loc, Name_Address));
      else
         Sparm := New_Reference_To (RTE (RE_Null_Address), Loc);
      end if;

      --  Now build the call

      Call :=
        Make_Procedure_Call_Statement (Loc,
          Name => New_Reference_To (RTE (RE_Store_TB), Loc),

          Parameter_Associations => New_List_3 (
            Make_Integer_Literal (Loc,
              Intval => UI_From_Int (Int (Get_Line_Number (Loc)))),

            Make_Attribute_Reference (Loc,
              Prefix     => Make_Identifier (Loc, Vname),
              Identifier => Make_Identifier (Loc, Name_Address)),

            Sparm));

      if Anal then
         Analyze (Call);
      end if;

      Insert_Before (N, Call);
   end Traceback_Store;

end Exp_Util;
