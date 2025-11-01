------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 3                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.234 $                            --
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
with Exp_Util; use Exp_Util;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch5;  use Sem_Ch5;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch9;  use Sem_Ch9;
with Sem_Ch12; use Sem_Ch12;
with Sem_Ch13; use Sem_Ch13;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;
package body Sem_Ch3 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Access_Subprogram_Declaration
     (T_Name : Entity_Id; T_Def  : Node_Id);
   --  The subprogram specification yields the signature and return type of
   --  an implicit type. This implicit type is the designated type of the
   --  declared access type.

   procedure Build_Derived_Type 
     (N : Node_Id; Parent_Type : Entity_Id; Derived_Type : Entity_Id);
   --  The attributes of a derived type are a copy of the attributes of
   --  the parent type. In some cases, additional entities (copies of
   --  components of the parent type) must also be created.

   procedure Check_Incomplete (T : Entity_Id);
   --  Called to verify that an incomplete type is not used prematurely

   procedure Check_Initialization (T : Entity_Id; Exp : Node_Id);
   --  Validate the initialization of an object declaration. T is the
   --  required type, and Exp is the initialization expression.

   function Collect_Primitive_Operations (T : Entity_Id) return Elist_Id;
   --  Called upon type derivation and extension. We scan the declarative
   --  part in  which the type appears, and collect subprograms that have
   --  one subsidiary subtype of the type. These subprograms can only
   --  appear after the type itself.

   procedure Constrain_Access (Def_Id : Entity_Id; S : Node_Id);
   --  Apply a list of constraints to an access type

   procedure Constrain_Array (Def_Id     : in out Entity_Id;
                              SI         : Node_Id;
                              Related_Id : Entity_Id;
                              Suffix     : Str);
   --  Apply a list of index constraints to an unconstrained array type
   --  creating an implicit constrained array subtype. The last two 
   --  parameters are used to build a correct name for the implicit type.

   procedure Constrain_Enumeration (Def_Id : Node_Id; S : Node_Id);
   --  Constrain an enumeration type with a range constraint. This is
   --  identical to Constrain_Integer, but for the Ekind of the subtype.

   procedure Constrain_Index (Index      : Node_Id;
                              S          : Node_Id;
                              Related_Id : Entity_Id;
                              Suffix     : Str;
                              Postfix    : Nat);
   --  Process an index constraint in a constrained array declaration.
   --  The constraint can be a subtype name, or a range with or without
   --  an explicit subtype mark. The index is the corresponding index of the
   --  unconstrained array. The 3 last parameters are used to build a correct
   --  name for the implicit type that is created.

   procedure Constrain_Integer (Def_Id : Node_Id; S : Node_Id);
   --  Build an integer subtype

   procedure Constrain_Discriminated_Type (Def_Id : Entity_Id; S : Node_Id);
   --  Process discriminant constraints of composite type. Verify that values
   --  have been provided for all discriminants, that the original type is
   --  unconstrained, and that the types of the supplied expressions match
   --  the discriminant types.

   procedure Constant_Redeclaration (Id : Entity_Id; N : Node_Id);
   --  Processes full declaration of deferred constant. Id is the entity for
   --  the redeclaration, and N is the N_Object_Declaration node. The caller
   --  has not done an Enter_Name or Set_Ekind on this entity.

   procedure Derived_Type_Declaration (T : Entity_Id; N : Node_Id);
   --  Process derived type declaration

   function  Determine_Enum_Size (T : Entity_Id) return Uint;
   --  Determine the size in bits necessary to store enumeration literals
   --  of type "T". The sizes are rounded to 8, 16 or 32 bit quantites.
   --  This determination is made in the absence of representation clauses
   --  for the enumeration type.

   procedure Derive_Subprograms (Parent_Type, Derived_Type : Entity_Id);
   --  To complete type derivation, collect or retrieve the primitive
   --  operations of the parent type, and replace the subsidiary subtypes
   --  with the derived type, to build the specs of the inherited ops.

   procedure Discriminant_Redeclaration (T : Entity_Id; D_List : List_Id);
   --  Verify conformance of discriminant part on redeclarations of types

   procedure Enumeration_Type_Declaration (T : Entity_Id; Def : Node_Id);
   --  Insert each literal in symbol table, as an overloadable identifier
   --  Each enumeration type is mapped into a sequence of integers, and
   --  each literal is defined as a constant with integer value. If any
   --  of the literals are character literals, the type is a character
   --  type, which means that strings are legal aggregates for arrays of
   --  components of the type.

   procedure Expand_Others_Choice (Case_Table     : Case_Table_Type;
                                   Others_Choice  : Node_Id;
                                   Choice_Type    : Entity_Id);
   --  In the case of a variant part of a record type that has an OTHERS
   --  choice, this procedure expands the OTHERS into the actual choices
   --  that it represents. This new list of choice nodes is attached to
   --  the OTHERS node via the Others_Discrete_Choices field. The Case_Table
   --  contains all choices that have been given explicitly in the variant.

   function Find_Type_Name (N : Node_Id) return Entity_Id;
   --  Enter the identifier in a type definition, or find the entity already
   --  declared, in the case of the full declaration of an incomplete or
   --  private type.

   function Find_Type_Of_Object (Obj_Def : Node_Id) return Entity_Id;
   --  Get type entity for object referenced by Obj_Def

   procedure Floating_Point_Type_Declaration (T : Entity_Id; Def : Node_Id);
   --  Create a new float, and apply the constraint to obtain subtype of it

   procedure Freeze_Before (N : Node_Id; T : Entity_Id);
   --  Freeze T then Insert the generated Freeze nodes before the node N.

   procedure Inherit_Components (Parent_Type, Derived_Type : Entity_Id);
   --  Used by derived types and type extensions to copy components of Parent. 

   procedure Integer_Type_Declaration (T : Entity_Id; Def : Node_Id);
   --  Create a new integer, and apply the constraint to obtain subtype of it

   procedure Modular_Type_Declaration (T : Entity_Id; Def : Node_Id);
   --  Create new modular type. Verify that modulus is in  bounds and is
   --  a power of two (implementation restriction).

   function In_Visible_Part (Scope_Id : Entity_Id) return Boolean;
   --  Determine whether a declaration occurs within the visible part of a
   --  package specification. The package must be on the scope stack, and the
   --  corresponding private part must not.

   procedure New_Binary_Operator (Op : Name_Id; Typ : Entity_Id);
   --  Create an abbreviated declaration for an operator,  in order to
   --  materialize minimally operators on derived types.

   procedure Record_Extension (Parent_Type, Derived_Type : Entity_Id;
                               Extension : Node_Id; N : Node_Id);
   --  Documentation needed for this subprogram ???

   procedure Record_Type_Declaration (T : Entity_Id; N : Node_Id);
   --  Process record type declaration

   procedure Record_Type_Definition (Def : Node_Id; T : Entity_Id);
   --  Def is a record type definition node. This procedure analyzes the
   --  components in this record type definition. T is the entity for
   --  the enclosing type. It is provided so that its Has_Tasks flag
   --  can be set if any of the component have Has_Tasks set.

   --------------------------
   -- Analyze_Declarations --
   --------------------------

   procedure Analyze_Declarations (L : List_Id) is
      D         : Node_Id;
      Next_Node : Node_Id;
      Frozen : Boolean := False;

      Clear_Overload_Structures : constant Boolean :=
            (Nkind (List_Parent (L)) /= N_Expression_Actions);
      --  This flag indicates whether we can clear the overload data structures
      --  between expressions. Normally we can clear them between declarations
      --  but the one exception arises when the list of declarations (called
      --  actions in this context) appears in an expression actions node, in
      --  which case we can't clear the overload data structures, since they
      --  may be in use by the outer expression context (the expression that
      --  contains the expression actions node).

   begin
      D := First (L);

      while Present (D) loop

         --  Complete analysis of declaration

         Analyze (D);

         --  Insert all N_Implicit_Type nodes representing implicit defining
         --  ids generated for the declaration just analyzed (N) before the
         --  declaration. (See comments in New_Implicit_Type for details.)
         --  In the case where we are processing a record declaration, delay
         --  inserting the N_Implicit_Type nodes until the record has been
         --  fully processed and then insert them in the Implicit_Types
         --  field of the N_Record_Definition node.

         Install_Implicit_Types (D); 

         Next_Node := Next (D);

         if No (Next_Node) then

            --  At the end of a declarative part, freeze remaining entities 
            --  declared in it.  The end of the visible declarations of a 
            --  package specification is not the end of a declarative part if 
            --  private declarations are present. 

            if not Frozen 
              and then  Nkind (List_Parent (L)) /= N_Component_List
              and then  (Nkind (List_Parent (L)) /= N_Package_Specification
                           or else L /= Visible_Declarations (List_Parent (L))
                           or else not List_Present 
                             (Private_Declarations (List_Parent (L))))
            then
               Frozen := True;           -- else it will be fluid for ever...
               Append_List (Freeze_All, L);
            end if;

         elsif Nkind (Next_Node) /= N_Subprogram_Body
                 and then Nkind (Next_Node) /= N_Entry_Body
                 and then Nkind (Next_Node) /= N_Package_Body
                 and then Nkind (Next_Node) /= N_Protected_Body
                 and then Nkind (Next_Node) /= N_Task_Body
         then
            null;

         --  Case of internally generated declaration

         elsif Analyzed (Next_Node) then 
            null;

         --  A body that is not an instance nor an internally generated,
         --  freezes all entities so far.

         else
            Insert_List_After (D, Freeze_All);
         end if;

         D := Next (D);

         if Clear_Overload_Structures then
            Init_Interp;
         end if;
      end loop;

   end Analyze_Declarations;

   ----------------------------
   --  Analyze_Implicit_Type --
   ----------------------------

   --  Nothing to do, since the only descendent is the entity representing
   --  the type, and this was analyzed when it the type was constructed.

   procedure Analyze_Implicit_Type (N : Node_Id) is
   begin
      null;
   end Analyze_Implicit_Type;

   --------------------------------
   -- Analyze_Object_Declaration --
   --------------------------------

   procedure Analyze_Object_Declaration (N : Node_Id) is
      Id   : constant Entity_Id := Defining_Identifier (N);
      E    : constant Node_Id   := Expression (N);
      T    : Entity_Id;

   begin
      if Constant_Present (N) and then Present (Current_Entity (Id))
        and then Scope (Current_Entity (Id)) = Current_Scope
      then
         Constant_Redeclaration (Id, N);
      else

         --  Enter the identifiers at the start to catch premature usage
         --  in the initialization.

         Enter_Name (Id);
      end if;

      T := Find_Type_Of_Object (Object_Definition (N));

      --  If deferred constant, make sure context is appropriate

      if Constant_Present (N) and then No (E) then
         if (Ekind (Current_Scope) /= E_Package
           and then Ekind (Current_Scope) /= E_Generic_Package)
             or else In_Private_Part (Current_Scope)
         then
            Error_Msg_N
              ("invalid context for deferred constant declaration", N);
            Set_Constant_Present (N, False);

         --  In Ada 83, deferred constant must be of private type

         elsif Ada_83 then
            if Ekind (T) /= E_Private_Type
              and then Ekind (T) /= E_Limited_Private_Type
            then
               Error_Msg_N
                 ("deferred constant must be of private type in Ada 83", N);
            end if;
         end if;
      end if;

      --  Install anonymous types before analyzing the expression
      --  to avoid mixing them with anonymous types created for this
      --  expression (for instance in the aggregate case)

      Install_Implicit_Types (N);

      --  Process initialization expression if present 
      --  Is this check of Etype still needed? If so why???

      if Present (E) then
         Analyze (E);
         Check_Initialization (T, E);
         Apply_Range_Check (E, T);
      end if;

      --  Check improper use of unconstrained subtype

      if Is_Unconstrained_Type (T) then 
         if No (E)
           or else (Ada_83 and then not Constant_Present (N))
         then
            Error_Msg_N ("unconstrained subtype not allowed",
                         Object_Definition (N));
         else
            --  Constrain the type with the expression size for
            --  declarations such as  S : String := "literal";
            --  Same thing has to be done with records and arrays.

            Expand_Subtype_From_Expr (N, T);
            T := Find_Type_Of_Object (Object_Definition (N));
         end if;

      elsif Is_Class_Type (T) then
         if No (E) then
            Error_Msg_N
              ("initialization required in class-wide declaration ", N);

         else
            --  Define a Class subtype with a size constraint coming
            --  from the expression

            T := New_Class_Subtype (N, T); 
         end if;

      elsif Is_Abstract (T) then
         Error_Msg_N ("type of object cannot be abstract", N);
      end if;

      --  Now establish the proper kind and type of the object.

      if Constant_Present (N) then
         Set_Ekind (Id, E_Constant);

      else
         Set_Ekind (Id, E_Variable);
      end if;

      Set_Etype (Id, T);
      Set_Is_Aliased (Id,  Aliased_Present (N));

      --  An object declaration freezes its type. If there are freezing 
      --  actions to perform,  analyze the freeze nodes,  which are
      --  placed before the current declaration. 

      if not Constant_Present (N) or else Present (Expression (N)) then
         Freeze_Before (N, Base_Type (T));
      end if;
   end Analyze_Object_Declaration;

   -------------------
   -- Freeze_Before --
   -------------------

   procedure Freeze_Before (N : Node_Id; T : Entity_Id) is
      Freeze_Nodes : constant List_Id := Freeze_Entity (T);
      F            : Node_Id;

   begin
      F := First (Freeze_Nodes);

      if Present (F) then
         Insert_List_Before (N, Freeze_Nodes);
         while F /= N loop
            Analyze (F);
            F := Next (F);
         end loop;
      end if;
   end Freeze_Before;

   ----------------------------
   -- Constant_Redeclaration --
   ----------------------------

   procedure Constant_Redeclaration (Id : Entity_Id; N : Node_Id) is
      E    : constant Node_Id   := Expression (N);
      Prev : constant Entity_Id := Current_Entity (Id);
      T    : Entity_Id;

   begin
      T := Find_Type_Of_Object (Object_Definition (N));

      if Ekind (Prev) /= E_Constant
        or else Present (Expression (Parent (Prev)))
        or else Etype (Prev) /= T
      then
         Enter_Name (Id); -- error message will follow.

      else
         Set_Full_Declaration (Prev, Id);
         Set_Is_Public (Id, Is_Public (Prev));
         Set_Is_Internal (Id);
         Append_Entity (Id, Current_Scope);

         if Present (E) and then No (Etype (E)) then
            Analyze (E);
            Check_Initialization (T, E);
         end if;
      end if;
   end Constant_Redeclaration;

   --------------------------------
   -- Analyze_Number_Declaration --
   --------------------------------

   procedure Analyze_Number_Declaration (N : Node_Id) is
      Id : constant Entity_Id := Defining_Identifier (N);
      E  : constant Node_Id   := Expression (N);
      T  : Entity_Id;
      K  : Entity_Kind := E_Constant;

   begin
      Analyze (E);

      --  Verify that the expression is static and universal

      T := Etype (E);

      if Is_Integer_Type (T)  then
         K := E_Named_Integer;
      elsif Is_Real_Type (T) then
         K := E_Named_Real;
      else
         Error_Msg_N ("numeric type required for number declaration", N);
         T := Any_Type;
      end if;

      Resolve_Complete_Context (E, T);

      if not Is_Static_Expression (E) then
         Error_Msg_N ("static expression required for number declaration", N);
      end if;

      Enter_Name (Id);
      Set_Etype  (Id, T);
      Set_Ekind  (Id, K);
   end Analyze_Number_Declaration;

   ----------------------------
   -- Install_Implicit_Types --
   ----------------------------

   procedure Install_Implicit_Types (N : Node_Id) is 
   begin
      if Is_Empty_List (Implicit_Type_List) 
        or else Nkind (N) = N_Component_Declaration 
      then
         return;
      end if;

      Mark_Implicit_Private_Decls (Implicit_Type_List);

      --  Install Implicit types at the proper place either in the record
      --  definition itself or before the declaraction generating them

      if (Nkind (N) = N_Record_Definition) then      
         Set_Implicit_Types (N, Implicit_Type_List);
      else
         Insert_List_Before (N, Implicit_Type_List);
      end if;

      Implicit_Type_List := New_List;
   end Install_Implicit_Types;

   -------------------------
   -- Find_Type_Of_Object --
   -------------------------

   function Find_Type_Of_Object (Obj_Def : Node_Id) return Entity_Id is
      T  : Entity_Id;

   begin
      if Nkind (Obj_Def) = N_Constrained_Array_Definition then

         --  Case of an anonymous array subtype

         T := Empty;
         Array_Type_Declaration (T, Obj_Def);

      else
         T := Process_Subtype
                (Obj_Def, Defining_Identifier (Parent (Obj_Def)), "type");
      end if;

      return T;
   end Find_Type_Of_Object;

   --------------------------------
   -- Analyze_Subtype_Indication --
   --------------------------------

   procedure Analyze_Subtype_Indication (N : Node_Id) is
      T : constant Node_Id := Subtype_Mark (N);
      R : constant Node_Id := Range_Expression (Constraint (N));

   begin
      Analyze (T);
      Analyze (R);
      Set_Etype (N, Etype (R));
   end Analyze_Subtype_Indication;

   --------------------------
   -- Check_Initialization --
   --------------------------

   procedure Check_Initialization (T : Entity_Id; Exp : Node_Id) is

   begin
      if Is_Limited_Type (T) then
         Error_Msg_N
           ("cannot initialize entities of limited type", Exp);
      end if;

      Resolve_Complete_Context (Exp, T);
   end Check_Initialization;

   ------------------------------
   -- Analyze_Type_Declaration --
   ------------------------------

   procedure Analyze_Type_Declaration (N : Node_Id) is
      Def : constant Node_Id := Type_Definition (N);
      T   : Node_Id;

   begin
      T := Find_Type_Name (N);

      --  Elaborate the type definition according to kind, and generate
      --  susbsidiary (implicit) subtypes where needed.

      case Nkind (Def) is

         when N_Access_To_Subprogram_Definition =>
            Access_Subprogram_Declaration (T, Def);

         when N_Access_To_Object_Definition =>
            Access_Type_Declaration (T, Def);

         when N_Array_Type_Definition =>
            Array_Type_Declaration (T, Def);

         when N_Derived_Type_Definition =>
            Derived_Type_Declaration (T, N);

         when N_Enumeration_Type_Definition =>
            Enumeration_Type_Declaration (T, Def);

         when N_Floating_Point_Definition =>
            Floating_Point_Type_Declaration (T, Def);

         when N_Ordinary_Fixed_Point_Definition =>
            Unimplemented (N, "fixed point type declaration");

         when N_Signed_Integer_Type_Definition =>
            Integer_Type_Declaration (T, Def);

         when N_Modular_Type_Definition =>
            Modular_Type_Declaration (T, Def);

         when N_Record_Definition =>
            Record_Type_Declaration (T, N);

         when others =>
            Unimplemented (N, "declaration");

      end case;

      Set_Is_Private_Type (T, Has_Private_Component (T));
      Set_Is_Delayed (T, Is_Private_Type (T));

      --  Install implicit types now to avoid to get mixed  with anonymous
      --  types generated by the expansion of this declaration

      Install_Implicit_Types (N);
   end Analyze_Type_Declaration;

   --------------------
   -- Find_Type_Name --
   --------------------

   function Find_Type_Name (N : Node_Id) return Entity_Id is
      Id     : constant Entity_Id := Defining_Identifier (N);
      Prev   : constant Entity_Id := Current_Entity (Id);
      New_Id : Entity_Id;

   begin
      --  Find incomplete declaration, if some was given.

      if Present (Prev) and then Scope (Prev) = Current_Scope then

         --  Previous declaration exists. Error if not incomplete/private case

         if not Is_Incomplete_Type (Prev) then
            Error_Msg_Sloc_1 := Sloc (Prev);
            Error_Msg_N ("invalid redeclaration of& declared on line #", Id);
            New_Id := Id;

         --  Case of full declaration of incomplete type

         elsif Ekind (Prev) = E_Incomplete_Type then

            --  Indicate that the incomplete declaration has a matching
            --  full declaration. The defining occurrence of the incomplete
            --  declaration remains the visible one, and the procedure
            --  Get_Full_Declaration dereferences it whenever the type is used.

            Set_Full_Declaration (Prev,  Id);
            Append_Entity (Id, Current_Scope);
            Set_Is_Public (Id, Is_Public (Prev));
            Set_Is_Internal (Id);
            New_Id := Id;

         --  Case of full declaration of private type

         else
            --  Place all semantic information on the first entity, which
            --  remains visible. Save private declaration in the new entity,
            --  to be swapped at the end of the private part.

            Copy_Node (Prev,  Id);
            Set_Full_Declaration (Id, Prev);
            Set_Is_Private_Type (Prev, False);
            Set_Is_Limited_Type (Prev, False);
            Append_Entity (Id, Current_Scope);
            New_Id := Prev;

            if Nkind (Parent (Prev)) = N_Private_Extension_Declaration
              and then not (Abstract_Present (Parent (Prev)))
              and then Nkind (N) = N_Full_Type_Declaration
              and then Abstract_Present (Type_Definition (N))
            then
               Error_Msg_N 
                 ("full view of non-abstract extension cannot be abstract", N);
            end if;

         end if;

            --  Verify that full declaration conforms to incomplete one

         if List_Present (Discriminant_Specifications (N))
           and then Is_Incomplete_Type (Prev)
         then
            Discriminant_Redeclaration (Prev, Discriminant_Specifications (N));

         elsif Is_Incomplete_Type (Prev) 
           and then Has_Discriminants (Prev) 
         then
            Error_Msg_N ("missing discriminants in full type declaration", N);
         end if;

         return New_Id;

      else
         --  New type declaration

         Enter_Name (Id);
         return Id;
      end if;
   end Find_Type_Name;

   ---------------------
   -- Process_Subtype --
   ---------------------

   function Process_Subtype (S          : Node_Id;
                             Related_Id : Entity_Id := Empty;
                             Suffix     : Str := "")  return Entity_Id is
      P               : Node_Id;
      Def_Id          : Entity_Id;
      Subtype_Mark_Id : Entity_Id;

   begin
      --  Case of constraint present, so that we have an N_Subtype_Indication
      --  node (this node is created only if constraints are present).

      if Nkind (S) = N_Subtype_Indication then
         Find_Type (Subtype_Mark (S));
         P := Parent (S);
         Subtype_Mark_Id := Entity (Subtype_Mark (S));

         if Nkind (P) = N_Subtype_Declaration then
            Def_Id := Defining_Identifier (P);

         elsif Nkind (P) = N_Derived_Type_Definition then
            Def_Id := Defining_Identifier (Parent (P));

         else
            --  In the case of a constraint given on an array type, delay
            --  generating the implicit array subtype until any other
            --  implicit types, such as for the index constaint are
            --  generated. The defining identifier is passed as Empty
            --  to Constrain_Array and is created towards the end of
            --  that procedure.

            if Is_Array_Type (Subtype_Mark_Id) then
               Def_Id := Empty;
            else
               Def_Id := New_Implicit_Type (Sloc (S), Related_Id, Suffix);
            end if;
         end if;

         case Ekind (Subtype_Mark_Id) is

            --  If the type is a access type, the constraint applies to the
            --  type being accessed. Create the corresponding subtype of it,
            --  promote it to an implicit type, and return an access to it.

            when Access_Kind =>
               Constrain_Access (Def_Id, S);

            when Array_Kind =>
               Constrain_Array (Def_Id, S, Related_Id, Suffix);

            when Enumeration_Kind =>
               Constrain_Enumeration (Def_Id, S);

            when Fixed_Kind =>
               null; -- Constrain_Fixed (Def_Id, S);  ???

            when Float_Kind =>
               null; -- Constrain_Float (Def_Id, S);  ???

            when Integer_Kind =>
               Constrain_Integer (Def_Id, S);

            when Record_Kind | Incomplete_Kind =>
               Constrain_Discriminated_Type (Def_Id, S);

            when Task_Kind | Protected_Kind  =>
               Constrain_Discriminated_Type (Def_Id, S);

               --  For concurrent types, the record value type carries
               --  the same discriminants, so when we constrain such a
               --  type we constrain the value type as well.

               declare
                  T_Val : constant Entity_Id := Task_Value_Type (
                                    Entity (Subtype_Mark (S)));
                  T_Sub : Entity_Id;

               begin
                  if Present (T_Val) then
                     T_Sub := New_Implicit_Type (Sloc (S), Related_Id, "val");
                     Set_Ekind             (T_Sub, E_Record_Subtype);
                     Set_Etype             (T_Sub, T_Val);
                     Set_Esize             (T_Sub, Uint_0);
                     Set_Has_Discriminants (T_Sub, True);
                     Set_Is_Constrained    (T_Sub, True);
                     Set_First_Entity      (T_Sub, First_Entity (T_Val));
                     Set_Discriminant_Constraint
                       (T_Sub, Discriminant_Constraint (Def_Id));
                     Set_Task_Value_Type (Def_Id, T_Sub);
                  end if;
               end;

            when others =>
               Error_Msg_N ("invalid subtype mark in subtype indication", S);
         end case;

         return Def_Id;

      --  Case of no constraints present

      else
         Find_Type (S);
         Check_Incomplete (S);
         return Entity (S);
      end if;
   end Process_Subtype;

   ----------------------
   -- Check_Incomplete --
   ----------------------

   procedure Check_Incomplete (T : Entity_Id) is
   begin
      if Ekind (Entity (T)) = E_Incomplete_Type then
         Error_Msg_N ("invalid use of type before its full declaration", T);
      end if;
   end Check_Incomplete;

   -----------------------
   --  Check_Completion --
   -----------------------

   procedure Check_Completion is
      E : Entity_Id;
   begin
      E := First_Entity (Current_Scope);
      while Present (E) loop
         if Is_Internal (E) then
            null;

         --  The following situation requires special handling: a child
         --  unit that appears in the context clause of the body of its
         --  parent: 

         --    procedure Parent.Child (...);
         --
         --    with Parent.Child;
         --    package body Parent is

         --  Here Parent.Child appears as a local entity, but should not
         --  be flagged as requiring completion, because it is a
         --  compilation unit.

         elsif Ekind (E) = E_Function
           or else Ekind (E) = E_Procedure then
            if not Has_Completion (E) 
              and then not Is_Abstract (E)
              and then Nkind (Parent (Get_Declaration_Node (E)))
                 /= N_Compilation_Unit
              and then Chars (E) /= Name_uSize
              and then Chars (E) /= Name_uEquality
            then
               Error_Msg_N ("missing body for subprogram specification",
                  Parent (E));
            end if;

         elsif Ekind (E) = E_Package then
            if Unit_Requires_Body (E)
              and then not Has_Completion (E) 
              and then Nkind (Parent (Get_Declaration_Node (E)))
                 /= N_Compilation_Unit
            then
               Error_Msg_N ("missing body for package declaration",
                  Parent (E));
            end if;

         elsif Ekind (E) = E_Incomplete_Type
           and then No (Full_Declaration (E))
         then
            Error_Msg_N ("missing full declaration for type", Parent (E));

         elsif (Ekind (E) = E_Task_Type or else Ekind (E) = E_Protected_Type)
           and then not Has_Completion (E)
           and then not Is_Internal (E)
         then
            Error_Msg_N ("missing full declaration for type", Parent (E));

         elsif Ekind (E) = E_Constant
           and then Ekind (Etype (E)) = E_Task_Type
           and then not Has_Completion (Etype (E))
         then
            Error_Msg_N ("missing full declaration for task", Parent (E));

         elsif Ekind (E) = E_Protected_Object
           and then not Has_Completion (Etype (E))
         then
            Error_Msg_N
              ("missing full declaration for protected object", Parent (E));

         end if;

         E := Next_Entity (E);
      end loop;
   end Check_Completion;

   ---------------------------------
   -- Analyze_Subtype_Declaration --
   ---------------------------------

   procedure Analyze_Subtype_Declaration (N : Node_Id) is
      Id : Entity_Id;
      T  : Entity_Id;

   begin
      Id := Defining_Identifier (N);
      Enter_Name (Id);
      T := Process_Subtype (Subtype_Indication (N), Id, "parent");
      Set_Is_Private_Type (Id, Has_Private_Component (Etype (Id)));
      Set_Is_Limited_Type (Id, Is_Limited_Type (Etype (Id)));
      Set_Is_Delayed (T, Is_Private_Type (T));
      Set_Has_Tasks (Id, Has_Tasks (T));

      --  In the case where there is no constraint given in the subtype
      --  indication, Process_Subtype just returns the Subtype_Mark,
      --  so its semantic attributes must be established here.

      if Nkind (Subtype_Indication (N)) /= N_Subtype_Indication then
         Set_Etype (Id, Base_Type (T));

         case Ekind (T) is
            when Array_Kind =>
               Set_Ekind             (Id, E_Array_Subtype);
               Set_First_Index       (Id, First_Index (T));
               Set_Component_Type    (Id, Component_Type (T));
               Set_Is_Constrained    (Id, Is_Constrained (T));
               Set_Esize             (Id, Uint_0);

            when Enumeration_Kind =>
               Set_Ekind             (Id, E_Enumeration_Subtype);
               Set_Scalar_Range      (Id, Scalar_Range (T));
               Set_Parent_Subtype    (Id, T);
               Set_Esize             (Id, Esize (T));

            when Fixed_Kind =>                                            
               null;    -- Set_Ekind (Id, E_Fixed_Subtype); ???           

            when Float_Kind =>
               Set_Ekind             (Id, E_Float_Subtype);
               Set_Scalar_Range      (Id, Scalar_Range (T));
               Set_Parent_Subtype    (Id, T);
               Set_Esize             (Id, Esize (T));

            when Signed_Integer_Kind =>
               Set_Ekind             (Id, E_Integer_Subtype);
               Set_Scalar_Range      (Id, Scalar_Range (T));
               Set_Parent_Subtype    (Id, T);
               Set_Esize             (Id, Esize (T));

            when Modular_Kind =>
               Set_Ekind             (Id, E_Modular_Subtype);
               Set_Scalar_Range      (Id, Scalar_Range (T));
               Set_Parent_Subtype    (Id, T);
               Set_Esize             (Id, Esize (T));
               Set_Modulus           (Id, Modulus (T));

            when Record_Kind =>
               Set_Ekind             (Id, E_Record_Subtype);
               Set_Esize             (Id, Uint_0);
               Set_Has_Discriminants (Id, Has_Discriminants (T));
               Set_Is_Constrained    (Id, Is_Constrained (T));
               Set_First_Entity      (Id, First_Entity (T));
               Set_Discriminant_Constraint
                                     (Id, Discriminant_Constraint (T));

            when Private_Kind =>
               Unimplemented (N, "subtypes of private types");

            when Access_Kind =>
               Set_Ekind             (Id, E_Access_Subtype);
               Set_Directly_Designated_Type
                                     (Id, Designated_Type (T));
               Set_Esize             (Id, UI_From_Int (System_Address_Size));

            when others =>
               Compiler_Abort;
         end case;
      end if;

   end Analyze_Subtype_Declaration;

   -----------------------
   -- Constrain_Integer --
   -----------------------

   procedure Constrain_Integer (Def_Id : Node_Id; S : Node_Id) is
      T : constant Node_Id := Entity (Subtype_Mark (S));
      C : constant Node_Id := Constraint (S);
      R : Node_Id;

   begin
      Set_Ekind (Def_Id, E_Integer_Subtype);

      if Nkind (C) /= N_Range_Constraint then
         Error_Msg_N ("expect range constraint for integer type", C);
         Set_Etype (Def_Id, Any_Type);
         return;
      end if;

      R := Range_Expression (C);
      Analyze (R);
      Resolve_Complete_Context (R, T);
      Set_Scalar_Range (Def_Id, R);
      Set_Etype (Def_Id, Base_Type (T));
      Set_Parent_Subtype (Def_Id, T);
      Set_Esize (Def_Id, Esize (T));
   end Constrain_Integer;

   ---------------------------
   -- Constrain_Enumeration --
   ---------------------------

   procedure Constrain_Enumeration (Def_Id : Node_Id; S : Node_Id) is
      T : constant Entity_Id := Entity (Subtype_Mark (S));
      C : constant Node_Id := Constraint (S);
      R : Node_Id;

   begin
      Set_Ekind (Def_Id, E_Enumeration_Subtype);

      if Nkind (C) /= N_Range_Constraint then
         Error_Msg_N ("expect range constraint for enumeration type", C);
         Set_Etype (Def_Id, Any_Type);
         return;
      end if;

      R := Range_Expression (C);
      Analyze (R);
      Resolve_Complete_Context (R, T);
      Set_Scalar_Range (Def_Id, R);
      Set_First_Literal (Def_Id, Entity (Low_Bound (R)));
      Set_Etype (Def_Id, Base_Type (T));
      Set_Esize (Def_Id, Esize (T));
   end Constrain_Enumeration;

   -------------------------------------
   -- Floating_Point_Type_Declaration --
   -------------------------------------

   --  ??? This procedure is incomplete. It does not yet process bounds
   --  if given, and does not set the bounds in the resulting float type.

   procedure Floating_Point_Type_Declaration (T : Entity_Id; Def : Node_Id) is
      Digs          : constant Node_Id := Digits_Expression (Def);
      Digs_Val      : Uint;
      Implicit_Base : constant Entity_Id :=
                        New_Implicit_Type (Sloc (T), T, "base");
      Base_Type     : Entity_Id;

      --  Find if given digits value allows derivation from specified type

      function Can_Derive_From (E : Entity_Id) return Boolean is
      begin
         return UI_Le
           (Digs_Val,
            Expr_Value (Digits_Expression (Type_Definition (Parent (E)))));
      end Can_Derive_From;

   --  Start of processing for floattype declaration

   begin
      if Present (Real_Range_Specification (Def)) then
         Unimplemented
           (Real_Range_Specification (Def), "real range specification");
      end if;

      Analyze (Digs);
      Resolve_Complete_Context (Digs, Any_Integer);

      if not (Is_Integer_Type (Etype (Digs))) then
         Error_Msg_N ("digits expression must be of integer type", Def);
      elsif not Is_Static_Expression (Digs) then
         Error_Msg_N ("digits expression must be static", Def);
      end if;

      Digs_Val := Expr_Value (Digs);

      if Can_Derive_From (Standard_Short_Float) then
         Base_Type := Standard_Short_Float;
      elsif Can_Derive_From (Standard_Float) then
         Base_Type := Standard_Float;
      elsif Can_Derive_From (Standard_Long_Float) then
         Base_Type := Standard_Long_Float;
      elsif Can_Derive_From (Standard_Long_Long_Float) then
         Base_Type := Standard_Long_Long_Float;
      else
         Base_Type := Standard_Long_Long_Float;
         Error_Msg_N ("digits expression out of range", Def);
      end if;

      Set_Scalar_Range (Implicit_Base, Scalar_Range (Base_Type));
      Set_Ekind (Implicit_Base, E_Float_Type);
      Set_Etype (Implicit_Base, Implicit_Base);
      Set_Esize (Implicit_Base, Esize (Base_Type));

      Set_Ekind (T, E_Float_Subtype);
      Set_Etype (T, Implicit_Base);
      Set_Esize (T, Esize (Implicit_Base));
      Set_Scalar_Range (T, Scalar_Range (Base_Type));
      Set_Parent_Subtype (T, Implicit_Base);
   end Floating_Point_Type_Declaration;

   ---------------------------
   -- Is_Unconstrained_Type --
   ---------------------------

   function Is_Unconstrained_Type (E : Entity_Id) return Boolean is
      K : constant Entity_Kind := Ekind (E);

   begin
      if Is_Constrained (E) then return False;

      elsif K in Array_Kind then
         return not Is_Constrained (E);

      elsif K = E_Record_Type 
        or else (Is_Incomplete_Type (E) and then No (Full_Declaration (E))) 
      then
         if Has_Discriminants (E) then
            if No (Discriminant_Default_Value (First_Discriminant (E))) then
               return True;
            end if;
         end if;
      end if;

      return False;
   end Is_Unconstrained_Type;

   ------------------------------
   -- Integer_Type_Declaration --
   ------------------------------

   procedure Integer_Type_Declaration (T : Entity_Id; Def : Node_Id) is
      Lo            : constant Node_Id := Low_Bound (Def);
      Hi            : constant Node_Id := High_Bound (Def);
      Implicit_Base : constant Entity_Id :=
                        New_Implicit_Type (Sloc (T), T, "base");
      Base_Type     : Entity_Id;
      Lo_Val        : Uint;            
      Hi_Val        : Uint;            

      --  Determine whether given bounds allow derivation from specified type

      function Can_Derive_From (E : Entity_Id) return Boolean is
      begin
         return UI_Ge (Lo_Val, Expr_Value (Type_Low_Bound (E)))
           and then UI_Le (Hi_Val,  Expr_Value (Type_High_Bound (E)));
      end Can_Derive_From;

      --  If the bounds are literals, their type is the chosen base type,
      --  and the not the largest integer type that would otherwise be
      --  chosen for a literal in a non-specific context. If the bounds
      --  are constants of some other integer type, convert them explicitly
      --  to the new type.

      procedure Set_Type_Bound (Bound : Node_Id) is
         NewB : Node_Id;
      begin
         if Nkind (Bound) = N_Integer_Literal then
            Set_Etype (Bound, Implicit_Base);
         else
            NewB := New_Node (N_Type_Conversion, Sloc (Bound));
            Set_Expression (NewB,  New_Copy (Bound));
            Set_Subtype_Mark (NewB, Implicit_Base);
            Set_Etype (NewB, Implicit_Base);
            Set_Unchecked_Conversion (NewB, True);
            Rewrite_Substitute_Tree (Bound, NewB);
            Analyze (NewB);
         end if;
      end Set_Type_Bound;

   --  Start of processing for Integer_Type_Declaration

   begin
      Analyze (Lo);
      Analyze (Hi);
      Resolve_Complete_Context (Lo, Any_Integer);
      Resolve_Complete_Context (Hi, Any_Integer);

      --  If a range constraint is used as an integer type definition, each
      --  bound of the range must be defined by a static expression of some
      --  integer type, but the two bounds need not have the same integer type.
      --  (Negative bounds are allowed.) [LRM 3.5.4]

      if not (Is_Integer_Type (Etype (Lo))
        and then Is_Integer_Type (Etype (Hi)))
      then
         Error_Msg_N
           ("integer type definition bounds must be of integer type", Def);

      elsif not
        (Is_Static_Expression (Lo) and then Is_Static_Expression (Hi))
      then
         Error_Msg_N
           ("integer type definition bounds must be static", Def);
      end if;

      Lo_Val := Expr_Value (Lo);
      Hi_Val := Expr_Value (Hi);

      if Can_Derive_From (Standard_Short_Short_Integer) then
         Base_Type := Standard_Short_Short_Integer;
      elsif Can_Derive_From (Standard_Short_Integer) then
         Base_Type := Standard_Short_Integer;
      elsif Can_Derive_From (Standard_Integer) then
         Base_Type := Standard_Integer;
      elsif Can_Derive_From (Standard_Long_Integer) then
         Base_Type := Standard_Long_Integer;
      elsif Can_Derive_From (Standard_Long_Long_Integer) then
         Base_Type := Standard_Long_Long_Integer;
      else
         Base_Type := Standard_Long_Long_Integer;
         Error_Msg_N ("integer type definition bounds out of range", Def);
      end if;

      Set_Scalar_Range (Implicit_Base, Scalar_Range (Base_Type));
      Set_Ekind (Implicit_Base, E_Integer_Type);
      Set_Etype (Implicit_Base, Base_Type);
      Set_Esize (Implicit_Base, Esize (Base_Type));

      Set_Ekind (T, E_Integer_Subtype);
      Set_Etype (T, Implicit_Base);
      Set_Esize (T, Esize (Implicit_Base));
      Set_Scalar_Range (T, Def);
      Set_Parent_Subtype (T, Implicit_Base);

      Set_Type_Bound (Lo);
      Set_Type_Bound (Hi);
   end Integer_Type_Declaration;

   ------------------------------
   -- Modular_Type_Declaration --
   ------------------------------

   procedure Modular_Type_Declaration (T : Entity_Id; Def : Node_Id) is
      Mod_Expr : constant Node_Id := Expression (Def);
      M_Val    : Uint;
      M        : Uint;
      Bits     : Nat;

   begin
      Set_Etype (T, T);
      Set_Ekind (T, E_Modular_Type);
      Analyze (Mod_Expr);
      Resolve_Complete_Context (Mod_Expr, Any_Integer);

      if not Is_Static_Expression (Mod_Expr) then
         Error_Msg_N
           ("expression in modular type definition must be static", Mod_Expr);
         M_Val := UI_Power (System_Max_Binary_Modulus_Power);
      else
         M_Val := Expr_Value (Mod_Expr);
      end if;

      if UI_Le (M_Val, Uint_1) then
         Error_Msg_N ("modulus value must be greater than 1", Mod_Expr);
         M_Val := UI_Power (System_Max_Binary_Modulus_Power);
      end if;

      Set_Modulus (T, M_Val);

      --   Create bounds for the modular type based on the modulus given in
      --   the type declaration and then analyze and resolve those bounds.

      Set_Scalar_Range (T,
        Make_Range (Sloc (Mod_Expr),
          Low_Bound  =>
            Make_Integer_Literal (Sloc (Mod_Expr),
              Intval => Uint_0),
          High_Bound =>
            Make_Integer_Literal (Sloc (Mod_Expr),
              Intval => UI_Difference (M_Val, Uint_1))));

      Analyze (Low_Bound (Scalar_Range (T)));
      Analyze (High_Bound (Scalar_Range (T)));
      Resolve_Complete_Context (Low_Bound (Scalar_Range (T)), T);
      Resolve_Complete_Context (High_Bound (Scalar_Range (T)), T);

      --  Loop through powers of 2 to find number of bits required

      for Bits in Int range 1 .. System_Max_Binary_Modulus_Power loop

         --  Binary case

         if UI_Eq (M_Val, UI_Power (Bits)) then
            Set_Esize (T, UI_From_Int (Bits));
            return;

         --  Non-binary case

         elsif UI_Lt (M_Val, UI_Power (Bits)) then

            if Bits > System_Max_Nonbinary_Modulus_Power then
               Error_Msg_Uint_1 :=
                 UI_From_Int (System_Max_Nonbinary_Modulus_Power);
               Error_Msg_N
                 ("nonbinary modulus exceeds limit (2'*'*^ - 1)", Mod_Expr);
               Set_Esize (T, UI_From_Int (System_Max_Binary_Modulus_Power));
               return;

            else
               --  In the non-binary case, we must have the actual size
               --  of the object be at least enough to hold the square
               --  of the modulus.  

               Set_Esize (T, UI_From_Int (Bits * 2));
               return;
            end if;
         end if;

      end loop;

      --  If we fall through, then the size exceed System.Max_Binary_Modulus
      --  so we just signal an error and set the maximum size.

      Error_Msg_Uint_1 := UI_From_Int (System_Max_Binary_Modulus_Power);
      Error_Msg_N ("modulus exceeds limit (2'*'*^)", Mod_Expr);
      Set_Esize (T, UI_From_Int (System_Max_Binary_Modulus_Power));

   end Modular_Type_Declaration;

   ----------------------------------
   -- Enumeration_Type_Declaration --
   ----------------------------------

   procedure Enumeration_Type_Declaration (T : Entity_Id; Def : Node_Id) is
      Ev             : Uint;
      L              : Node_Id;
      Int_Lit        : Node_Id;
      R_Node, B_Node : Node_Id;
      Table_Obj      : Entity_Id;
      Table_Type     : Entity_Id;

   begin
      --  Create identifier node representing lower bound

      B_Node := New_Node (N_Identifier, Sloc (Def));
      L := First (Literals (Def));
      Set_Chars (B_Node, Chars (L));
      Set_Entity (B_Node,  L);
      Set_Etype (B_Node, T);
      Set_Is_Static (B_Node, True);
      Set_Is_Evaluated (B_Node, True);

      R_Node := New_Node (N_Range, Sloc (Def));
      Set_Low_Bound  (R_Node, B_Node);

      Set_Ekind (T, E_Enumeration_Type);
      Set_First_Literal (T, L);
      Set_Etype (T, T);

      Ev := Uint_0;

      --  Loop through literals of enumeration type

      while Present (L) loop
         Set_Ekind (L, E_Enumeration_Literal);
         Set_Etype (L, T);
         Set_Enumeration_Pos (L, Ev);
         Set_Enumeration_Rep (L, Ev);
         New_Overloaded_Entity (L);

         if Nkind (L) = N_Defining_Character_Literal then
            Set_Ekind (T, E_Character_Type);
         end if;

         Ev := UI_Sum (Ev, Uint_1);
         L := Next (L);
      end loop;

      --  Now create a node representing upper bound

      B_Node := New_Node (N_Identifier, Sloc (Def));
      Set_Chars (B_Node, Chars (Last (Literals (Def))));
      Set_Entity (B_Node,  Last (Literals (Def)));
      Set_Etype (B_Node, T);
      Set_Is_Static (B_Node, True);
      Set_Is_Evaluated (B_Node, True);

      Set_High_Bound (R_Node, B_Node);
      Set_Scalar_Range (T, R_Node);
      Set_Esize (T, Determine_Enum_Size (T));

      --  Create two defining occurrences corresponding to a enumeration
      --  table constaining the literal names and its type. This table is
      --  used in conjunction with calls to 'Image on enumeration values.
      --  This table filled in by the back-end.

      Table_Obj :=
        Make_Defining_Identifier (Sloc (Def),
          Chars => New_External_Name (Chars (T), "table"));

      Set_Is_Internal (Table_Obj);
      Append_Entity (Table_Obj, Current_Scope);
      Set_Current_Entity (Table_Obj);

      Table_Type := New_Implicit_Type (Sloc (Def), T, "enum_table");
      Set_Ekind (Table_Type, E_Enum_Table_Type);

      Set_Etype (Table_Obj, Table_Type);
      Set_Ekind (Table_Obj, E_Variable);
      Set_Public_Status (Table_Obj);

      Set_Etype (Table_Type, Table_Type);
      Set_Public_Status (Table_Type);
      Set_Component_Type (Table_Type, Standard_A_String);
      Set_First_Index (Table_Type,
        First (New_List_1 (
          New_Occurrence_Of (Standard_Positive, Sloc (Def)))));
      Int_Lit := New_Node (N_Integer_Literal, Sloc (Def));
      Set_Intval
        (Int_Lit, Enumeration_Pos (Entity (Type_High_Bound (T))));
      Set_Etype (Int_Lit, Standard_Integer);
      Set_Is_Static (Int_Lit, True);
      Set_Table_High_Bound (Table_Type, Int_Lit);
      Set_Lit_Name_Table (T, Table_Obj);
   end Enumeration_Type_Declaration;

   -------------------------
   -- Determine_Enum_Size --
   -------------------------

   function Determine_Enum_Size  (T : Entity_Id) return Uint is
      Count : Nat := 0;
      Lit   : Entity_Id;

   begin
      Lit := First_Literal (T);
      while Present (Lit) loop
         Count := Count + 1;
         Lit := Next_Literal (Lit);
      end loop;

      if Count <= 2 ** 8 then
         return Uint_8;
      elsif Count <= 2 ** 16 then
         return Uint_16;
      else
         return Uint_32;
      end if;
   end Determine_Enum_Size;

   ----------------------------
   -- Array_Type_Declaration --
   ----------------------------

   procedure Array_Type_Declaration (T : in out Entity_Id; Def : Node_Id) is
      Element_Type  : Entity_Id;
      Component_Def : constant Node_Id := Subtype_Indication (Def);
      Implicit_Base : Entity_Id;
      Index         : Node_Id;
      Related_Id    : Entity_Id := Empty;
      Nb_Index      : Nat;

   begin
      if Nkind (Def) = N_Constrained_Array_Definition then

         Index := First (Discrete_Subtype_Definitions (Def));

         --  Find proper names for the implicit types which may be public.
         --  in case of anonymous arrays we use the name of the first object
         --  of that type as prefix.

         if No (T) then
            Related_Id :=  Defining_Identifier (Parent (Def));
         else
            Related_Id := T;
         end if;

      else
         Index := First (Subtype_Marks (Def));
      end if;

      Nb_Index := 1;

      while Present (Index) loop
         Analyze (Index);
         Make_Index (Index, Related_Id, Nb_Index);
         Index := Next_Index (Index);
         Nb_Index := Nb_Index + 1;
      end loop;

      Element_Type := Process_Subtype (Component_Def, Related_Id, "component");

      --  Constrained array case

      if Nkind (Def) = N_Constrained_Array_Definition then

         --  Establish Implicit_Base as unconstrained base type

         Implicit_Base := New_Implicit_Type (Sloc (Def), Related_Id, "base");
         Set_Ekind (Implicit_Base, E_Array_Type);
         Set_Esize (Implicit_Base, Uint_0);
         Set_Etype (Implicit_Base, Implicit_Base);
         Set_Scope (Implicit_Base, Current_Scope);

         if No (T) then
            T := New_Implicit_Type (Sloc (Def), Related_Id);
         end if;

         --  The constrained array type is a subtype of the unconstrained one

         Set_Ekind          (T, E_Array_Subtype);
         Set_Esize          (T, Uint_0);
         Set_Etype          (T, Implicit_Base);
         Set_Scope          (T, Current_Scope);
         Set_Is_Constrained (T, True);
         Set_First_Index    (T, First (Discrete_Subtype_Definitions (Def)));

         --  Complete setup of implicit base type

         Set_First_Index    (Implicit_Base, First_Index (T));
         Set_Component_Type (Implicit_Base, Element_Type);
         Set_Has_Tasks      (Implicit_Base, Has_Tasks (Element_Type));

      --  Unconstrained array case

      else
         Set_Ekind       (T, E_Array_Type);
         Set_Esize       (T, Uint_0);
         Set_Etype       (T, T);
         Set_Scope       (T, Current_Scope);
         Set_First_Index (T, First (Subtype_Marks (Def)));
      end if;

      Set_Component_Type (T, Element_Type);
      Set_Has_Tasks (T, Has_Tasks (Element_Type));

      if Aliased_Present (Def) then
         Set_Is_Aliased (T);
         Set_Is_Aliased (Etype (T));
      end if;

      if Number_Dimensions (T) = 1 then
         New_Binary_Operator (Name_Op_Concat, T);
      end if;

      --  In the case of an unconstrained array the parser has already
      --  verified that all the indices are unconstrained but we still
      --  need to make sure that the element type is constrained.

      if Is_Unconstrained_Type (Element_Type) then
         Error_Msg_N
           ("unconstrained element type in array declaration ",
            Component_Def);

      elsif Is_Abstract (Element_Type) then
         Error_Msg_N ("The type of a component cannot be abstract ", 
              Component_Def);
      end if;

      Set_Is_Limited_Type (T, Is_Limited_Type (Element_Type));
   end Array_Type_Declaration;

   ----------------
   -- Make_Index --
   ----------------

   procedure Make_Index (I : Node_Id;
                         Related_Id : Entity_Id := Empty;
                         Postfix : Nat := 1) is
      L : Entity_Id;
      --  Type of low bound of range

      H : Entity_Id;
      --  Type of high bound of range

      R      : Node_Id;
      T      : Entity_Id;
      Def_Id : Entity_Id;

   begin
      --  For a discrete range used in a constrained array definition and
      --  defined by a range, an implicit conversion to the predefined type
      --  INTEGER is assumed if each bound is either a numeric literal, a named
      --  number, or an attribute, and the type of both bounds (prior to the
      --  implicit conversion) is the type universal_integer. Otherwise, both
      --  bounds must be of the same discrete type, other than universal
      --  integer; this type must be determinable independently of the
      --  context, but using the fact that the type must be discrete and that
      --  both bounds must have the same type.

      --  Character literals also have a universal type in the absence of
      --  of additional context,  and are resolved to Standard_Character.

      if Nkind (I) = N_Range then

         --  The index is given by a range constraint. The bounds are known
         --  to be of a consistent type.

         L := Etype (Low_Bound (I));
         H := Etype (High_Bound (I));

         if L = Universal_Integer and H = Universal_Integer then
            T := Standard_Integer;
         elsif L = Universal_Integer then
            T := H;
         elsif L = Any_Character and then H = Any_Character then
            T := Standard_Character;
         else
            T := L;
         end if;

         R := I;
         Resolve_Complete_Context (R, T);

      elsif Nkind (I) = N_Subtype_Indication then

         --  The index is given by a subtype with a range constraint.

         T :=  Base_Type (Entity (Subtype_Mark (I)));
         R := Range_Expression (Constraint (I));
         Resolve_Complete_Context (R, T);

      elsif Nkind (I) = N_Attribute_Reference then

         --  The parser guarantees that the attribute is a RANGE attribute

         Analyze (I);
         T := Etype (I);
         Resolve_Complete_Context (I, T);
         R := I;

      --  If none of the above, must be a subtype (nothing to do)

      else
         if not Is_Name (I) or else not Is_Type (Entity (I)) then
            Error_Msg_N ("invalid subtype mark in discrete range ", I);
            Set_Etype (I, Any_Integer);
         end if;

         return; 
      end if;

      if not Is_Discrete_Type (T) then
         Error_Msg_N ("discrete type required for range", I);
         Set_Etype (I, Any_Type);
         return;

      elsif T = Any_Type then
         Set_Etype (I, Any_Type);
         return;
      end if;

      Def_Id := New_Implicit_Type (Sloc (I), Related_Id, "index_", Postfix);
      Set_Etype (Def_Id, Base_Type (T));

      if Is_Integer_Type (T) then
         Set_Ekind (Def_Id, E_Integer_Subtype);
      else
         Set_Ekind (Def_Id, E_Enumeration_Subtype);
      end if;

      Set_Esize (Def_Id, Esize (T));
      Set_Scalar_Range (Def_Id, R);
      Set_Parent_Subtype (Def_Id, T);
      Set_Etype (I, Def_Id);

   end Make_Index;

   ---------------------
   -- Constrain_Array --
   ---------------------

   procedure Constrain_Array (Def_Id     : in out Entity_Id;
                              SI         : Node_Id;
                              Related_Id : Entity_Id;
                              Suffix     : Str) is

      T : Entity_Id;
      C : constant Node_Id := Constraint (SI);
      Number_Of_Constraints : Nat := 0;
      Index : Node_Id;
      S     : Entity_Id;

   begin
      if Nkind (C) /= N_Index_Or_Discriminant_Constraint then
         Error_Msg_N ("incorrect constraint given for array type", C);
         return;
      end if;

      T := Entity (Subtype_Mark (SI));

      if Ekind (T) in Access_Kind then 
         T := Designated_Type (T); 
      end if;

      --  If an index constraint follows a subtype mark in a subtype indication
      --  then the type or subtype denoted by the subtype mark must not already
      --  impose an index constraint. The subtype mark must denote either an
      --  unconstrained array type or an access type whose designated type
      --  is such an array type... [LRM 3.6.1]

      if Is_Constrained (T) then
         Error_Msg_N
          ("array type is already constrained", Subtype_Mark (SI));
         return;
      end if;

      S := First (Constraints (C));

      while Present (S) loop
         Number_Of_Constraints := Number_Of_Constraints + 1;
         S := Next (S);
      end loop;

      --  In either case, the index constraint must provide a discrete range
      --  for each index of the array type and the type of each discrete range
      --  must be the same as that of the corresponding index. [LRM 3.6.1]

      if Number_Of_Constraints /= Number_Dimensions (T) then
         Error_Msg_NE
           ("incorrect no. of index constraints for type&", C, T);
         return;
      end if;

      S := First (Constraints (C));
      Index := First_Index (T);
      Analyze (Index);

      --  Apply constraints to each index type

      for I in 1 .. Number_Of_Constraints loop
         Constrain_Index (Index, S, Related_Id, Suffix, I);
         Index := Next (Index);
         S := Next (S);
      end loop;

      if No (Def_Id) then
         Def_Id := New_Implicit_Type (Sloc (SI), Related_Id, Suffix);
      end if;

      Set_Ekind          (Def_Id, E_Array_Subtype);
      Set_Esize          (Def_Id, Uint_0);
      Set_Etype          (Def_Id, Base_Type (T));
      Set_First_Index    (Def_Id, First (Constraints (C)));
      Set_Component_Type (Def_Id, Component_Type (T));
      Set_Has_Tasks      (Def_Id,  Has_Tasks (T));
      Set_Is_Constrained (Def_Id);
   end Constrain_Array;

   ---------------------
   -- Constrain_Index --
   ---------------------

   procedure Constrain_Index (Index      : Node_Id;
                              S          : Node_Id;
                              Related_Id : Entity_Id;
                              Suffix     : Str;
                              Postfix    : Nat) is
      Def_Id : Entity_Id;
      R      : Node_Id;
      T      : constant Entity_Id := Etype (Index);

   begin
      if Nkind (S) = N_Range 
        or else Nkind (S) = N_Attribute_Reference 
      then
         --  A Range attribute will transformed into N_Range by Resolve.

         Analyze (S);
         Set_Etype (S, T);
         R := S;
         Resolve_Complete_Context (R, T);

         if not (Covers (T, Etype (Low_Bound (S)))
           or else Covers (Etype (Low_Bound (S)), T)
           or else Covers (T, Etype (High_Bound (S)))
           or else Covers (Etype (High_Bound (S)), T))
         then
            Error_Msg_NE ("invalid index constraint for&", S, Index);
         end if;

      elsif Nkind (S) = N_Subtype_Indication then
         Analyze (Subtype_Mark (S));
         R := Range_Expression (Constraint (S));
         Analyze (R);

         if Base_Type (Entity (Subtype_Mark (S))) /= Base_Type (T) then
            Error_Msg_NE ("invalid index constraint for&", R, Index);
            Set_Etype (S, Any_Type);
            return;
         else
            Resolve_Complete_Context (R, T);
         end if;

      elsif Is_Name (S) then     -- Subtype_Mark
         Analyze (S);

         if Base_Type (Entity (S)) /= Base_Type (T) then
            Error_Msg_NE ("invalid index constraint for&", S, Index);
         end if;

         --  If the constraint is a subtype mark, there are no anonymous
         --  subtypes to construct.

         return;

      else
         Error_Msg_N ("invalid index constraint", S);
         return;
      end if;

      Def_Id := New_Implicit_Type (Sloc (S), Related_Id, Suffix, Postfix);
      Set_Etype (Def_Id, Base_Type (T));

      if Is_Integer_Type (T) then
         Set_Ekind (Def_Id, E_Integer_Subtype);
      else
         Set_Ekind (Def_Id, E_Enumeration_Subtype);
      end if;

      Set_Esize (Def_Id, Esize (T));
      Set_Scalar_Range (Def_Id, R);
      Set_Parent_Subtype (Def_Id, T);
      Set_Etype (S, Def_Id);
   end Constrain_Index;

   -----------------------------
   -- Record_Type_Declaration --
   -----------------------------

   procedure Record_Type_Declaration (T : Entity_Id; N : Node_Id) is
      Def : constant Node_Id := Type_Definition (N);

   begin
      --  Records constitute a scope for the component declarations within.
      --  The scope is created prior to the processing of these declarations.
      --  Discriminants are processed first, so that they are visible when
      --  processing the other components. The Ekind of the record type itself
      --  is set to E_Record_Type (subtypes appear as E_Record_Subtype).
      --  If an incomplete or private type declaration was already given for
      --  the type, then this scope already exists, and the discriminants have
      --  been declared within. We must verify that the full declaration
      --  matches the incomplete one.

      New_Scope (T); -- Enter record scope
      Set_Is_Tagged_Type (T, Tagged_Present (Def));

      --  Type is abstract if full declaration carries keyword, or if 
      --  previous partial view did.

      Set_Is_Abstract (T, Is_Abstract (T) or else Abstract_Present (Def));

      --  If an incomplete or private type declaration was already given for
      --  the type, the discriminants have been declared. Else process them now

      if not Is_Incomplete_Type (T) then
         if List_Present (Discriminant_Specifications (N)) then
            Process_The_Discriminants (N);
         end if;

      elsif Has_Discriminants (T) then

         --  Make the discriminants visible to component declarations.

         declare
            D    : Entity_Id := First_Discriminant (T);
            Prev : Entity_Id;

         begin
            while Present (D) loop
               Prev := Current_Entity (D);
               Set_Current_Entity (D);
               Set_Is_Directly_Visible (D);
               Set_Homonym (D, Prev);
               D := Next_Discriminant (D);
            end loop;
         end;
      end if;

      Set_Ekind (T, E_Record_Type);
      Set_Etype (T, T);
      Set_Esize (T, Uint_0);
      Record_Type_Definition (Def, T);

      if Tagged_Present (Def) then
         Make_Class_Type (T);
         Set_Primitive_Operations (T, New_Elmt_List);
      end if;

      End_Scope; -- Exit record scope

   end Record_Type_Declaration;

   -------------------------------
   -- Process_The_Discriminants --
   -------------------------------

   procedure Process_The_Discriminants (N : Node_Id) is
      Id                  : Node_Id;
      Discr               : Node_Id;
      Discr_Type          : Entity_Id;
      Default_Present     : Boolean := False;
      Default_Not_Present : Boolean := False;
      D_Minal             : Entity_Id;
      Elist               : Elist_Id;

   begin
      --  A composite type other than an array type can have discriminants.
      --  Discriminants of non-limited types must have a discrete type.
      --  On entry, the current scope is the composite type.

      --  The discriminants are initially entered into the scope of the type
      --  via Enter_Name with the default Ekind of E_Void to prevent premature
      --  use, as explained at the end of this procedure.

      Elist := New_Elmt_List;

      Discr := First (Discriminant_Specifications (N));
      while Present (Discr) loop
         Enter_Name (Defining_Identifier (Discr));

         if Nkind (Discriminant_Type (Discr)) = N_Access_Definition then
            Discr_Type := Access_Definition (Discriminant_Type (Discr));

            if Ada_83 then
               Error_Msg_N ("Access definition not supported in Ada83", Discr);

            elsif not Is_Limited_Type (Current_Scope) then
               Error_Msg_N ("Only limited types can have access discriminants",
               Discr);
            end if;

         else
            Analyze (Discriminant_Type (Discr));
            Discr_Type := Etype (Discriminant_Type (Discr));
            if not Is_Discrete_Type (Discr_Type) then
               Error_Msg_N ("discriminants must have a discrete type",
                          Discriminant_Type (Discr));
            end if;
         end if;

         Set_Etype (Defining_Identifier (Discr), Discr_Type);

         --  If a discriminant specification includes the assignment compound
         --  delimiter followed by an expression,the expression is the default
         --  expression of the discriminant; the default expression must be of
         --  the type of the discriminant. [LRM 3.7.1]

         if Present (Expression (Discr)) then
            Analyze (Expression (Discr));
            Resolve_Complete_Context (Expression (Discr), Discr_Type);
            Default_Present := True;
            Append_Elmt (Expression (Discr), Elist);

            --  Tag the defining identifiers for the discriminants with their
            --  corresponding default expressions from the tree.

            Set_Discriminant_Default_Value 
              (Defining_Identifier (Discr), Expression (Discr));

         else
            Default_Not_Present := True;
         end if;

         Discr := Next (Discr);
      end loop;

      --  An element list consisting of the default expressions of the
      --  discriminants is constructed in the above loop and used to set
      --  the Discriminant_Constraint attribute for the type. If an object
      --  is declared of this (record or task) type without any explicit
      --  discriminant constraint given, this element list will form the
      --  actual parameters for the corresponding initialization procedure
      --  for the type.

      Set_Discriminant_Constraint (Current_Scope, Elist);

      --  Default expressions must be provided either for all or for none
      --  of the discriminants of a discriminant part. [LRM 3.7.1]

      if Default_Present and Default_Not_Present then
         Error_Msg_N
          ("incomplete specification of defaults for discriminants", N);
      end if;

      --  The use of the name of a discriminant is not allowed in default
      --  expressions of a discriminant part if the specification of the
      --  discriminant is itself given in the discriminant part. [LRM 3.7.1]

      --  To detect this, the discriminant names are entered initially with an
      --  Ekind of E_Void (which is the default Ekind given by Enter_Name). Any
      --  attempt to use a void entity (for example in an expression that is
      --  type-checked) produces the error message: premature usage.  Now after
      --  completing the semantic analysis of the discriminant part, we can set
      --  the Ekind of all the discriminants appropriately.

      Discr := First (Discriminant_Specifications (N));

      while Present (Discr) loop
         Id := Defining_Identifier (Discr);
         Set_Ekind (Id, E_Discriminant);

         --  Initialize the Original_Record_Component to the entity itself
         --  the New_Copy call in Build_Derived_Type will automatically
         --  propagate the right value to descendants

         Set_Original_Record_Component (Id, Id);

         --  Create discriminal, that is to say the associated entity
         --  to be used in initialization procedures for the type,
         --  in which a discriminal is a formal parameter whose actual
         --  is the value of the corresponding discriminant constraint. 
         --  Discriminals are not used during semantic analysis, and are
         --  not fully defined entities until expansion. Thus they are not
         --  given a scope until intialization procedures are built. 

         D_Minal := Make_Defining_Identifier (Sloc (N), 
                                     New_Internal_Name ("discriminal")); 
         Set_Ekind (D_Minal,  E_In_Parameter);
         Set_Etype (D_Minal, Etype (Id));
         Set_Discriminal (Id, D_Minal); 

         Discr := Next (Discr);
      end loop;

      Set_Has_Discriminants (Current_Scope);
   end Process_The_Discriminants;

   --------------------------------
   -- Discriminant_Redeclaration --
   --------------------------------

   procedure Discriminant_Redeclaration (T : Entity_Id; D_List : List_Id) is
   begin
      null; -- For now ???
   end Discriminant_Redeclaration;

   ----------------------------
   -- Record_Type_Definition --
   ----------------------------

   procedure Record_Type_Definition (Def : Node_Id; T : Entity_Id) is
      Anon_List : List_Id;
      Component : Entity_Id;

   begin
      if Tagged_Present (Def) then
         Expand_Tagged_Root (Def);
      end if;

      --  If the component list of a record type is defined by the reserved
      --  word null and there is no discriminant part, then the record type has
      --  no components and all records of the type are null records. [LRM 3.7]

      if No (Component_List (Def))
        or else Null_Present (Component_List (Def)) 
      then
         return;
      end if;

      Analyze_Declarations (Component_Declarations (Component_List (Def)));

      if Present (Variant_Part (Component_List (Def))) then
         Analyze (Variant_Part (Component_List (Def)));
      end if;

      Component := First_Entity (Current_Scope);

      --  After completing the semantic analysis of the record definition,
      --  then record components are accessible.

      while Present (Component) loop
         if Ekind (Component) = E_Void then
            Set_Ekind (Component, E_Component);
         end if;

         if Has_Tasks (Etype (Component)) then
            Set_Has_Tasks (T, True);
         end if;

         Component := Next_Entity (Component);
      end loop;

      --  If any implicit subtypes were generated for component declarations
      --  of this record, set the Implicit_Type field to be the list of the
      --  N_Implicit_Type nodes (contained in Implicit_Type_List) that were
      --  generated. See New_Implicit_Type for more details.

      Install_Implicit_Types (Def);

   end Record_Type_Definition;

   -----------------------------------
   -- Analyze_Component_Declaration --
   -----------------------------------

   procedure Analyze_Component_Declaration (N : Node_Id) is
      Id : constant Entity_Id := Defining_Identifier (N);
      T  : Entity_Id;

   begin
      Enter_Name (Defining_Identifier (N));
      T := Find_Type_Of_Object (Subtype_Indication (N));

      --  If a component declaration includes the assignment compound delimiter
      --  followed by an expression, the expression is the default expression
      --  of the record component; the default expression must be of the type
      --  of the component. Default expressions are not allowed for components
      --  that are of a limited type. [LRM 3.7 (5)]

      if Present (Expression (N)) then
         Analyze (Expression (N));
         Check_Initialization (T, Expression (N));
      end if;

      if Is_Unconstrained_Type (T) then
         Error_Msg_N
           ("unconstrained subtype in component declaration",
            Subtype_Indication (N));

      elsif Is_Abstract (T) then
         Error_Msg_N ("The type of a component cannot be abstract", N);
      end if;

      if Is_Limited_Type (T) then
         Set_Is_Limited_Type (Current_Scope);
      end if;

      Set_Ekind (Id, E_Component);
      Set_Etype (Id, T);
      Set_Is_Aliased (Id, Aliased_Present (N));

      --  Initialyze the Original_Record_Component to the entity itself
      --  the New_Copy call in Build_Derived_Type will automatically
      --  propagate the right value to descendants

      Set_Original_Record_Component (Id, Id);

   end Analyze_Component_Declaration;

   --------------------------
   -- Analyze_Variant_Part --
   --------------------------

   procedure Analyze_Variant_Part (N : Node_Id) is

      Case_Table     : Case_Table_Type (1 .. Number_Of_Case_Choices (N));
      Choice         : Node_Id;
      Choice_Count   : Nat := 0;
      Discr_Name     : Node_Id;
      Discr_Type     : Entity_Id;
      E              : Entity_Id;
      Hi             : Node_Id;
      Invalid_Case   : Boolean := False;
      Kind           : Node_Kind;
      Lo             : Node_Id;
      Others_Present : Boolean := False;
      Variant        : Node_Id;

      procedure Check_Choice (Lo, Hi : Node_Id; Position : Node_Id);
      --  Check_Choice checks whether the given bounds of a choice are
      --  static. If not a message is issued, otherwise the bounds are
      --  entered into the case table.

      procedure Check_Choice (Lo, Hi : Node_Id; Position : Node_Id) is
      begin
         --  The simple expressions and discrete ranges given as choices
         --  in a variant part must be static. [LRM 3.7.3]

         if not (Is_Static_Expression (Lo) and then
                 Is_Static_Expression (Hi))
         then
            Error_Msg_N ("choice given in variant part not static", Position);
            Invalid_Case := True;
         else
            Choice_Count := Choice_Count + 1;
            Case_Table (Choice_Count).Choice_Lo := Lo;
            Case_Table (Choice_Count).Choice_Hi := Hi;
            Case_Table (Choice_Count).Choice_Node := Position;
         end if;
      end Check_Choice;

   --  Start of processing for Analyze_Variant_Part

   begin
      Discr_Name := Name (N);
      Analyze (Discr_Name);

      if Ekind (Entity (Discr_Name)) /= E_Discriminant then
         Error_Msg_N ("invalid discriminant name in variant part", Discr_Name);
      end if;

      Discr_Type := Etype (Entity (Discr_Name));

      --  The type of the discriminant of a variant part must not be a
      --  generic formal type. [LRM 3.7.3]

      if Is_Generic_Type (Discr_Type) then
         Error_Msg_N
           ("discriminant of variant part cannot be generic", Discr_Name);
         return;
      end if;

      --  Now check each of the case choices against Exp_Base_Type.

      Variant := First (Variants (N));

      while Present (Variant) loop
         Choice := First (Discrete_Choices (Variant));

         while Present (Choice) loop
            Analyze (Choice);
            Kind := Nkind (Choice);

            if Kind = N_Range then
               Resolve_Complete_Context (Choice, Discr_Type);
               Check_Choice (Low_Bound (Choice), High_Bound (Choice), Choice);

            elsif (Kind = N_Identifier or else Kind = N_Selected_Component)
              and then Is_Type (Entity (Choice))
            then
               E  := Entity (Choice);
               Lo := Type_Low_Bound (E);
               Hi := Type_High_Bound (E);
               Check_Choice (Lo, Hi, Choice);

            elsif Kind = N_Subtype_Indication then
               Compiler_Abort;        -- for now ???

            --  The choice others is only allowed for the last variant and as
            --  its only choice; it stands for all values (possibly none) not
            --  given in the choices of previous variants. [LRM 3.7.3]

            elsif Kind = N_Others_Choice then
               if not (Choice = First (Discrete_Choices (Variant))
                 and then Choice = Last (Discrete_Choices (Variant))
                 and then Variant = Last (Variants (N)))
               then
                  Error_Msg_N
                    ("the choice OTHERS must appear alone and last", Choice);
                  return;
               end if;

               Others_Present := True;

            else -- an expression
               Resolve_Complete_Context (Choice, Discr_Type);
               Check_Choice (Choice, Choice, Choice);
            end if;

            Choice := Next (Choice);
         end loop;

         if not Null_Present (Component_List (Variant)) then
            Analyze_Declarations
              (Component_Declarations (Component_List (Variant)));

            if Present (Variant_Part (Component_List (Variant))) then
               Analyze (Variant_Part (Component_List (Variant)));
            end if;
         end if;

         Variant := Next (Variant);
      end loop;

      if not Invalid_Case and then Case_Table'Length > 0 then
         Check_Case_Choices (Case_Table, N, Discr_Type, Others_Present);
         if Others_Present then

            --  Fill in Others_Discrete_Choices field of the OTHERS choice

            Choice := Last (Discrete_Choices (Last (Variants (N))));
            Expand_Others_Choice (Case_Table, Choice, Discr_Type);
         end if;

      end if;

   end Analyze_Variant_Part;

   --------------------------
   -- Expand_Others_Choice --
   --------------------------

   procedure Expand_Others_Choice (Case_Table     : Case_Table_Type;
                                   Others_Choice  : Node_Id;
                                   Choice_Type    : Entity_Id) is
      Choice      : Node_Id;
      Choice_List : List_Id := New_List;
      Exp_Lo      : Node_Id;
      Exp_Hi      : Node_Id;
      Hi          : Uint;
      Lo          : Uint;
      Loc         : Source_Ptr := Sloc (Others_Choice);
      Previous_Hi : Uint;

      function Lit_Of (Value : Uint) return Node_Id;
      --  Returns the Node_Id for the enumeration literal corresponding to the
      --  position given by Value within the enumeration type Choice_Type.

      function Build_Choice (Value1, Value2 : Uint) return Node_Id;
      --  Builds a node representing the missing choices given by the
      --  Value1 and Value2. A N_Range node is built if there is more than
      --  one literal value missing. Otherwise a single N_Integer_Literal,
      --  N_Identifier or N_Character_Literal is built depending on what
      --  Choice_Type is.

      ------------
      -- Lit_Of --
      ------------

      function Lit_Of (Value : Uint) return Node_Id is
         Lit : Entity_Id;

      begin
         --  In the case where the literal is of type Character, there needs
         --  to be some special handling since there is no explicit chain
         --  of literals to search. Instead, a N_Character_Literal node
         --  is created with the appropriate Char_Code and Chars fields.

         if Base_Type (Choice_Type) = Standard_Character then
            Name_Buffer (1) := ''';
            Name_Buffer (2) := Char'Val (UI_To_Int (Value));
            Name_Buffer (3) := ''';
            Name_Len := 3;
            Lit := New_Node (N_Character_Literal, Loc);
            Set_Chars (Lit, Name_Find);
            Set_Char_Literal_Value (Lit, Char_Code (UI_To_Int (Value)));
            Set_Etype (Lit, Choice_Type);
            Set_Is_Static (Lit, True);
            return Lit;

         --  Otherwise, iterate through the literals list of Choice_Type
         --  "Value" number of times until the desired literal is reached
         --  and then return an occurrence of it.

         else
            Lit := First_Literal (Choice_Type);
            for I in 1 .. UI_To_Int (Value) loop
               Lit := Next_Literal (Lit);
            end loop;

            return New_Occurrence_Of (Lit, Loc);
         end if;
      end Lit_Of;

      ------------------
      -- Build_Choice --
      ------------------

      function Build_Choice (Value1, Value2 : Uint) return Node_Id is
         Lit_Node : Node_Id;
         Lo, Hi   : Node_Id;
         Lo_Val   : Uint;
         Hi_Val   : Uint;

      begin
         --  If there is only one choice value missing between Value1 and
         --  Value2, build an integer or enumeration literal to represent it.

         if UI_Eq (UI_Difference (Value2, Value1), Uint_1) then

            if Is_Integer_Type (Choice_Type) then
               Lit_Node := Make_Integer_Literal (Loc, Value1);
               Set_Etype (Lit_Node, Choice_Type);
            else
               Lit_Node := Lit_Of (Value1);
            end if;

         --  Otherwise is more that one choice value that is missing between
         --  Value1 and Value2, therefore build a N_Range node of either
         --  integer or enumeration literals.

         else
            Hi_Val := UI_Difference (Value2, Uint_1);
            if Is_Integer_Type (Choice_Type) then
               Lo := Make_Integer_Literal (Loc, Value1);
               Set_Etype (Lo, Choice_Type);
               Hi := Make_Integer_Literal (Loc, Hi_Val);
               Set_Etype (Hi, Choice_Type);
               Lit_Node :=
                 Make_Range (Loc,
                   Low_Bound  => Lo,
                   High_Bound => Hi);

            else
               Lit_Node :=
                 Make_Range (Loc,
                   Low_Bound  => Lit_Of (Value1),
                   High_Bound => Lit_Of (Hi_Val));
            end if;
         end if;
         return Lit_Node;
      end Build_Choice;

   --  Start of processing for Expand_Others_Choice

   begin
      --  Establish the bound values for the variant depending upon whether
      --  the type of the discriminant name is static or not.

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

      --  Build the node for any missing choices that are smaller than any
      --  explicit choices given in the variant.

      if UI_Lt (Expr_Value (Exp_Lo), Lo) then
         Append (Build_Choice (Expr_Value (Exp_Lo), Lo), Choice_List);
      end if;

      --  Build the nodes representing any missing choices that lie between
      --  the explicit ones given in the variant.

      for I in Case_Table'First + 1 .. Case_Table'Last loop
         Lo := Expr_Value (Case_Table (I).Choice_Lo);
         Hi := Expr_Value (Case_Table (I).Choice_Hi);
         Choice := Case_Table (I).Choice_Node;

         if UI_Ne (Lo, UI_Sum (Previous_Hi, Uint_1)) then
            Append (Build_Choice (UI_Sum (Previous_Hi, Uint_1), Lo),
                    Choice_List);
         end if;

         Previous_Hi := Hi;
      end loop;

      --  Build the node for any missing choices that are greater than any
      --  explicit choices given in the variant.

      if UI_Gt (Expr_Value (Exp_Hi), Hi) then
         Append (Build_Choice (UI_Sum (Hi, Uint_1), Expr_Value (Exp_Hi)),
                 Choice_List);
      end if;

      Set_Others_Discrete_Choices (Others_Choice, Choice_List);
   end Expand_Others_Choice;

   ----------------------------------
   -- Constrain_Discriminated_Type --
   ----------------------------------

   procedure Constrain_Discriminated_Type (Def_Id : Entity_Id; S : Node_Id) is
      T     : Node_Id;
      C     : Node_Id;
      N     : Node_Id;
      Elist : Elist_Id;
      Id    : Entity_Id;
      Discr : Entity_Id;
      Number_Discriminants : Nat := 0;

   begin
      C := Constraint (S);

      if Nkind (C) /= N_Index_Or_Discriminant_Constraint then
         Error_Msg_N ("incorrect constraint given for record type", C);
         return;
      end if;

      --  A discriminant constraint is only allowed in a subtype indication,
      --  after a subtype mark. This subtype mark must denote either a type
      --  with discriminants, or an access type whose designated type is a
      --  type with discriminants. A discriminant constraint specifies the
      --  values of these discriminants. [LRM 3.7.2 (5)]

      T := Entity (Subtype_Mark (S));

      if Ekind (T) in Access_Kind then
         T := Designated_Type (T);
      end if;

      if Is_Constrained (T) then
         Error_Msg_N
           ("record type is already constrained", Subtype_Mark (S));
         return;

      elsif not Has_Discriminants (T) then
         Error_Msg_N
           ("invalid constraint: record type has no discriminant", C);
         Set_Etype (Def_Id, Any_Type);
         return;
      end if;

      --  Count the number of discriminants of the record type so that
      --  an array of the proper size can be allocated to hold the
      --  expressions given by the discriminant constraint.

      Discr := First_Discriminant (T);

      while Present (Discr) loop
         Number_Discriminants := Number_Discriminants + 1;
         Discr := Next_Discriminant (Discr);
      end loop;

      declare
         Discriminant_Expr : array (1 .. Number_Discriminants) of Node_Id;
         E         : Entity_Id;
         Position  : Nat := 1;
         Id2       : Entity_Id;
         Not_Found : Boolean;

         function Pos_Of_Discr (T : Entity_Id; Discr : Entity_Id) return Nat;
         --  Return the Position number (starting at 1) of a discriminant
         --  (Discr) within the discriminant list of the record type (T).

         function Pos_Of_Discr (T : Entity_Id; Discr : Entity_Id) return Nat is
            I : Nat := 1;
            D : Entity_Id;

         begin
            D := First_Discriminant (T);

            while Present (D) loop
               if D = Discr then
                  return I;
               end if;

               D := Next_Discriminant (D);
               I := I + 1;
            end loop;

            --  Note: Since this function is called on discriminants that are
            --  known to belong to the record type, falling through the loop
            --  with no match signals an internal compiler error.

            Compiler_Abort;
         end Pos_Of_Discr;

      begin
         for I in Discriminant_Expr'range loop
            Discriminant_Expr (I) := Empty;
         end loop;

         Discr := First_Discriminant (T);
         N := First (Constraints (C));

         --  The following loop will process the positional associations only
         --  and will exit when a named association is seen. The named
         --  associations will then be processed by the subsequent loop.

         while Present (N) loop
            exit when Nkind (N) = N_Discriminant_Association; -- Named Assoc

            --  For a positional association, the (single) discriminant is
            --  implicitly specified by position, in textual order. [LRM 3.7.2]

            if No (Discr) then
               Error_Msg_N ("too many constraints given for record", C);
               return;

            else
               Analyze (N);
               Discriminant_Expr (Position) := N;
               Resolve_Complete_Context (N, Base_Type (Etype (Discr)));
               Position := Position + 1;
               Discr := Next_Discriminant (Discr);
            end if;

            N := Next (N);
         end loop;

         --  There should only be named associations left on the discriminant
         --  constraint. Any positional assoication are in error.

         while Present (N) loop

            if Nkind (N) = N_Discriminant_Association then
               E := Empty;
               Analyze (Expression (N));

               --  Search the entity list of the record looking at only the
               --  discriminants (which always appear first) to see if the
               --  simple name given in the constraint matches any of them.

               Id := First (Selector_Names (N));

               while Present (Id) loop
                  Not_Found := True;
                  Id2 := First_Entity (T);

                  while Present (Id2)
                    and then Ekind (Id2) = E_Discriminant
                  loop
                     if Chars (Id2) = Chars (Id) then
                        Not_Found := False;
                        exit;
                     end if;

                     Id2 := Next_Entity (Id2);
                  end loop;

                  if Not_Found then
                     Error_Msg_N ("& does not match any discriminant", N);
                     return;
                  end if;

                  Position := Pos_Of_Discr (T, Id2);

                  if No (Discriminant_Expr (Position)) then
                     Discriminant_Expr (Position) := Expression (N);
                     Resolve_Complete_Context
                       (Expression (N), Base_Type (Etype (Id2)));
                  else
                     Error_Msg_N
                       ("duplicate constraint for discriminant&", Id);
                  end if;

                  --  A discriminant association with more than one
                  --  discriminant name is only allowed if the named
                  --  discriminants are all of the same type ... [LRM 3.7.2]

                  if E = Empty then
                     E := Etype (Id2);

                  elsif Etype (Id2) /= E then
                     Error_Msg_N ("all discriminants in an association " &
                                  "must have the same type", N);
                  end if;

                  Id := Next (Id);
               end loop;

            else -- Positional Association

               --  Named associations can be given in any order, but if both
               --  positional and named associations are used in the same
               --  discriminant constraint, then positional associations must
               --  occur first, at their normal position. Hence once a named
               --  association is used, the rest of the discriminant constraint
               --  must use only named associations.

               Error_Msg_N ("positional association follows named one", N);
               return;
            end if;

            N := Next (N);
         end loop;

         --  Furthermore, for each discriminant association (whether named or
         --  positional), the expression and the associated discriminants must
         --  have the same type. A discriminant constraint must provide exactly
         --  one value for each discriminant of the type. [LRM 3.7.2]

         --  missing code here???

         for I in 1 .. Number_Discriminants loop
            if No (Discriminant_Expr (I)) then
               Error_Msg_N ("too few constraints given for record", C);
               return;
            end if;
         end loop;

         --  Build an element list consisting of the expressions given in the
         --  discriminant constraint. The list is constructed after resolving
         --  any named discriminant associations and therefore the expressions
         --  appear in the textual order of the discriminants.

         Elist := New_Elmt_List;

         for I in 1 .. Number_Discriminants loop
            Append_Elmt (Discriminant_Expr (I), Elist);
         end loop;

      end; -- end of declare block

      if Ekind (T) = E_Record_Type then
         Set_Ekind (Def_Id, E_Record_Subtype);

      elsif Ekind (T) = E_Task_Type then
         Set_Ekind (Def_Id, E_Task_Subtype);

      elsif Ekind (T) = E_Protected_Type then
         Set_Ekind (Def_Id, E_Protected_Subtype);

      else
         --  incomplete or private type.
         Set_Ekind (Def_Id, Ekind (T));
      end if;

      Set_Etype                   (Def_Id, T);
      Set_Is_Tagged_Type          (Def_Id, Is_Tagged_Type (T));
      Set_Has_Discriminants       (Def_Id);
      Set_Is_Constrained          (Def_Id);
      Set_First_Entity            (Def_Id, First_Entity (T));
      Set_Discriminant_Constraint (Def_Id, Elist);
      Set_Has_Tasks               (Def_Id, Has_Tasks (T));

      --  Subtypes introduced by component declarations do not need to be
      --  marked as delayed, and do not get freeze nodes, because the semantics
      --  verifies that the parents of the subtypes are frozen before the
      --  enclosing record is frozen.

      if not Is_Type (Scope (Def_Id)) then
         Set_Is_Private_Type (Def_Id, Is_Private_Type (T));
         Set_Is_Delayed (Def_Id,  Is_Delayed (T));
      end if;

      --  ??? This function is awfully long, could use some breaking up ???

   end Constrain_Discriminated_Type;

   ------------------------
   -- Check_Discriminant --
   ------------------------

   --  Within a record type definition the only allowed uses of the name
   --  of a discriminant of the record type are: in the default expressions
   --  for record components; in a variant part as the discriminant name;
   --  and in a component subtype definition, either as a bound in an index
   --  constraint, or to specify a discriminant value in a discriminant
   --  constraint. A discriminant name used in these component subtype
   --  definitions must appear by itself, not as part of a larger expression.
   --  [LRM 3.7.1]

   --  Verify that when a discriminant appears in an index constraint or a
   --  discriminant constraint of a record component, it appears by itself
   --  and not as part of a larger expression.

   function Check_Discriminant (Exp_Node : Node_Id) return Boolean is
   begin
      return True;
   end Check_Discriminant;

   ------------------------------
   -- Derived_Type_Declaration --
   ------------------------------

   procedure Derived_Type_Declaration (T : Entity_Id; N : Node_Id) is
      Def       : constant Node_Id := Type_Definition (N);
      Indic     : constant Node_Id := Subtype_Indication (Def);
      Extension : constant Node_Id := Record_Extension_Part (Def);
      Parent_Type    : Entity_Id;
      Parent_Subtype : Entity_Id;

   begin
      --  If subtype indication, constraint to be applied later to derived type

      if Nkind (Indic) = N_Subtype_Indication then
         Find_Type (Subtype_Mark (Indic));
         Parent_Type := Entity (Subtype_Mark (Indic));

      --  Else we have subtype mark without a constraint

      else
         Find_Type (Indic);
         Parent_Type := Entity (Indic);
      end if;

      --  A derived type defined in a package specification cannot be used for
      --  further derivation until the end of its visible part. This means that
      --  derivation in the private part of the package is allowed.

      if (Is_Derived_Type (Parent_Type)
            and then Ada_83
            and then In_Visible_Part (Scope (Parent_Type)))
        or else (Is_Incomplete_Type (Parent_Type)
            and then No (Full_Declaration (Parent_Type)))
      then
         Error_Msg_N
           ("premature derivation of derived or private type", Indic);
      end if;

      if Present (Extension) then
         if not Is_Tagged_Type (Parent_Type) then
            Error_Msg_N
              ("parent of type extension must be a tagged type ", Indic);

         else
            --  oo-extension
            Freeze_Before (N, Parent_Type);
            Expand_Tagged_Extension (Def);
            Record_Extension (Parent_Type, T, Extension, N);
         end if;

      elsif Is_Tagged_Type (Parent_Type) then
         Error_Msg_N
           ("a type derived from a tagged type must have an extension", Indic);
      end if;

      Build_Derived_Type (N, Parent_Type, T);

      Derive_Subprograms (Parent_Type, T);

      if Present (Extension) and then Is_Tagged_Type (Parent_Type) then
         Make_Class_Type (T);
      end if;
   end Derived_Type_Declaration;

   ------------------------
   -- Build_Derived_Type --
   ------------------------

   procedure Build_Derived_Type
     (N : Node_Id; Parent_Type : Entity_Id; Derived_Type : Entity_Id)
   is
      Indic : constant Node_Id := Subtype_Indication (Type_Definition (N));
      Implicit_Base  : Entity_Id;
      T              : Entity_Id;

   begin
      --  Copy common attributes

      Set_Ekind (Derived_Type, Ekind (Base_Type (Parent_Type)));
      Set_Esize (Derived_Type, Esize (Parent_Type));
      Set_Etype (Derived_Type, Base_Type (Parent_Type));
      Set_Scope (Derived_Type, Current_Scope);

      case Ekind (Parent_Type) is

         when Numeric_Kind =>

            --  Process the subtype indication including a validation check on
            --  the constraint if any.

            T := Process_Subtype (Indic);

            --  Introduce an implicit base type for the derived type even if
            --  there is no constraint attached to it, since this seems closer
            --  to the Ada semantics.

            Implicit_Base :=
              New_Implicit_Type (Sloc (N), Derived_Type, "base");
            Set_Ekind (Implicit_Base, Ekind (Base_Type (Parent_Type)));
            Set_Etype (Implicit_Base, Parent_Type);
            Set_Esize (Implicit_Base, Esize (Parent_Type));
            Set_Scalar_Range (Implicit_Base, Scalar_Range (Parent_Type));

            --  Make "Derived_Type", which is the defining identifier of the
            --  derived type declaration, be a subtype of the introduced
            --  implicit base type. In the case where there is no constraint
            --  given the Ekind will have to be set here since it is not set
            --  by Process_Subtype.

            Set_Etype (Derived_Type, Implicit_Base);
            Set_Parent_Subtype (Derived_Type, Implicit_Base);

            if Nkind (Indic) /= N_Subtype_Indication then
               Set_Ekind (Derived_Type, Subtype_Kind (Ekind (Parent_Type)));
               Set_Scalar_Range (Derived_Type, Scalar_Range (Parent_Type));
            end if;

            if Ekind (Parent_Type) in Modular_Kind then
               Set_Modulus (Derived_Type, Modulus (Parent_Type));
            end if;

         when Array_Kind =>
            Set_First_Index    (Derived_Type, First_Index    (Parent_Type));
            Set_Component_Type (Derived_Type, Component_Type (Parent_Type));
            Set_Is_Constrained (Derived_Type, Is_Constrained (Parent_Type));

         --  A derived record type has the same fields and types as the parent.
         --  The declaration may have a discriminant part, in which case they
         --  must conform to the discriminants of the parent type. We assume
         --  the discriminant names, types and default values must be the same.

         --  For untagged types, we generate a copy of the parent record defi-
         --  nition, using the generic instantiation mechanism as a general
         --  purpose copy/instantiate facility, and we replace the derived type
         --  definition with a record type definition. For tagged types we
         --  only need to duplicate the entities and we treat the parent type
         --  as a single special component.

         when Record_Kind =>

            --  Tagged type case

            if Is_Tagged_Type (Parent_Type) then
               Inherit_Components (Parent_Type, Derived_Type);
               Set_Is_Tagged_Type (Derived_Type);
               Set_Primitive_Operations (Derived_Type, New_Elmt_List);

            --  Non-tagged type, replace derived type definition

            else
               Rewrite_Substitute_Tree (Type_Definition (N),
                 Instantiate_Record (Parent_Type,  Derived_Type));

               Set_Is_Constrained (Derived_Type, Is_Constrained (Parent_Type));
               Set_Has_Discriminants
                 (Derived_Type, Has_Discriminants (Parent_Type));
            end if;

         --  A derived enumeration type has the literals of its parent, but
         --  these are marked as derived, to enforce the overloading rules.
         --  The first literal of the parent is the entity that follows the
         --  parent in its scope. We cannot use the attribute First_Literal
         --  because the parent itself may be derived and have no explicit tree

         when Enumeration_Kind =>
            declare
               Literal : Entity_Id := Next_Entity (Parent_Type);
               New_Lit : Entity_Id;
            begin
               Unimplemented (N, "derived enumeration types");
               while Present (Literal) loop
                  New_Lit := New_Copy (Literal);
                  Set_Etype (New_Lit, Derived_Type);
                  Set_Alias (New_Lit, Literal);
                  New_Overloaded_Entity (New_Lit);
                  Literal := Next_Entity (Literal);
               end loop;

               Set_Scalar_Range (Derived_Type, Scalar_Range (Parent_Type));
            end;

         when Access_Kind =>
            Set_Directly_Designated_Type
              (Derived_Type, Designated_Type (Parent_Type));

         when Incomplete_Kind =>
            Set_Full_Declaration (Derived_Type, 
                                        Full_Declaration (Parent_Type));
            Set_Is_Limited_Type (Derived_Type, Is_Limited_Type (Parent_Type));

         when others =>
            Compiler_Abort;
      end case;

   end Build_Derived_Type;

   ------------------------
   -- Inherit_Components --
   ------------------------

   procedure Inherit_Components (Parent_Type, Derived_Type : Entity_Id) is
      Comp     : Entity_Id := First_Entity (Parent_Type);
      New_Comp : Entity_Id;

   begin
      while Present (Comp) loop

         --  The entities in the parent_Type include components and
         --  implicit types. If those types do not depend on disc-
         --  riminants, they can be elaborated once and shared by
         --  both parent and derived type. If the implicit types
         --  depend on the discriminants, a more complex instantia-
         --  tion mechanism must be used (TBSL).

         if (Ekind (Comp) = E_Discriminant or else Ekind (Comp) = E_Component)
           and then Chars (Comp) /= Name_uParent
         then
            New_Comp := New_Copy (Comp);
            Append_Entity (New_Comp, Derived_Type);
         end if;

         Comp := Next_Entity (Comp);
      end loop;
   end Inherit_Components;

   ---------------------
   -- Is_Derived_Type --
   ---------------------

   function Is_Derived_Type (Type_Id : Entity_Id) return Boolean is
   begin
      return Base_Type (Type_Id) /= Root_Type (Type_Id)
        and not Is_Generic_Type (Type_Id);
   end Is_Derived_Type;

   ---------------------
   -- In_Visible_Part --
   ---------------------

   function In_Visible_Part (Scope_Id : Entity_Id) return Boolean is
   begin
      return
         (Ekind (Scope_Id) = E_Package or Ekind (Scope_Id) = E_Generic_Package)
           and then In_Open_Scopes (Scope_Id)
           and then not Is_Package_Body (Scope_Id)
           and then not In_Private_Part (Scope_Id);
   end In_Visible_Part;

   ----------------------------------
   -- Collect_Primitive_Operations --
   ----------------------------------

   function Collect_Primitive_Operations (T : Entity_Id) return Elist_Id is
      Op_List : Elist_Id  := New_Elmt_List;
      Id      : Entity_Id := Next_Entity (T);
      Formal  : Entity_Id;
      Is_Prim : Boolean;

   begin
      --  For tagged types, the primitive operations are collected as they
      --  are declared, and held in an explicit list which is simply returned.

      if Is_Tagged_Type (T) then
         return Primitive_Operations (T);
      end if;

      while Present (Id) loop
         if Is_Overloadable (Id) then
            Is_Prim := False;

            if Base_Type (Etype (Id)) = Base_Type (T) then
               Is_Prim := True;
            else
               Formal := First_Formal (Id);
               while Present (Formal) loop
                  if Base_Type (Etype (Formal)) = Base_Type (T) then
                     Is_Prim := True;
                     exit;
                  end if;

                  Formal := Next_Formal (Formal);
               end loop;
            end if;

            if Is_Prim then
               Append_Elmt (Id, Op_List);
            end if;
         end if;

         Id := Next_Entity (Id);

      end loop;

      return Op_List;
   end Collect_Primitive_Operations;

   ------------------------
   -- Derive_Subprograms --
   ------------------------

   procedure Derive_Subprograms (Parent_Type, Derived_Type  : Entity_Id) is
      Op_List    : Elist_Id  := Collect_Primitive_Operations (Parent_Type);
      Elmt       : Elmt_Id;
      Subp       : Entity_Id;
      New_Subp   : Entity_Id;
      Formal     : Entity_Id;
      New_Formal : Entity_Id;

      procedure Replace_Type (Id, New_Id : Entity_Id) is
      begin
         if Base_Type (Etype (Id)) = Base_Type (Parent_Type) then
            Set_Etype (New_Id, Derived_Type);
         else
            Set_Etype (New_Id, Etype (Id));
         end if;
      end Replace_Type;

   --  Start of processing for Derive_Subprograms

   begin
      Elmt := First_Elmt (Op_List);

      while Elmt  /= No_Elmt loop
         Subp := Id_Of (Elmt);
         New_Subp :=
           Extend_Node (New_Node (N_Defining_Identifier, Sloc (Derived_Type)));
         Set_Ekind (New_Subp, Ekind (Subp));
         Set_Chars (New_Subp, Chars (Subp));
         Replace_Type (Subp, New_Subp);

         Formal := First_Formal (Subp);
         while Present (Formal) loop
            New_Formal := New_Copy (Formal);
            Append_Entity (New_Formal, New_Subp);
            Replace_Type (Formal, New_Formal);
            Formal := Next_Formal (Formal);
         end loop;

         Set_Alias (New_Subp, Subp);
         New_Overloaded_Entity (New_Subp);

         --  Indicate that a derived subprogram does not require a body.

         Set_Has_Completion (New_Subp);

         --  A derived function with a controlling result is abstract.

         if Is_Abstract (Subp)
           or else Etype (New_Subp) = Derived_Type 
         then
            Set_Is_Abstract (New_Subp);
         end if;

         Elmt := Next_Elmt (Elmt);
      end loop;
   end Derive_Subprograms;

   ----------------------
   -- Record_Extension --
   ----------------------

   procedure Record_Extension (Parent_Type, Derived_Type : Entity_Id;
                               Extension : Node_Id; N : Node_Id) is
   begin
      --  If parent is a private type, then derived type must be constructed
      --  as extension of the full declaration, which must be retrieved. Is
      --  this comment right ??? (appears to cover all incomplete types???)

      if Is_Incomplete_Type (Parent_Type) then
         null;

      --  Otherwise enter scope of derived type to compile new components

      else
         New_Scope (Derived_Type);
         if List_Present (Discriminant_Specifications (N)) then
            Process_The_Discriminants (N);
         else
            Set_Has_Discriminants (Derived_Type, False);
         end if;

         Set_Ekind (Derived_Type, Ekind (Parent_Type));

         if Has_Discriminants (Derived_Type) then
            Set_Is_Constrained (Derived_Type,
              Present (Discriminant_Default_Value (
                First_Discriminant (Derived_Type))));
         else
            Set_Is_Constrained (Derived_Type, True);
         end if;

         Record_Type_Definition (Extension, Derived_Type);
         End_Scope;
      end if;
   end Record_Extension;

   -------------------------------------------
   -- Analyze_Private_Extension_Declaration --
   -------------------------------------------

   procedure Analyze_Private_Extension_Declaration (N : Node_Id) is
      T     : constant Entity_Id := Defining_Identifier (N);
      Indic : constant Node_Id   := Subtype_Indication (N);
      Parent_Type : Entity_Id;

   begin
      Enter_Name (T);

      Set_Is_Tagged_Type (T);
      Set_Ekind (T, E_Private_Type);
      Set_Is_Abstract (T, Abstract_Present (N));
      Make_Class_Type (T);
      New_Scope (T);

      --  The discriminants must be processed first, and made visible,
      --  because they may be used to constain the discriminants of the
      --  parent.

      if List_Present (Discriminant_Specifications (N)) then
         Process_The_Discriminants (N);
      end if;

      Find_Type (Indic);
      Parent_Type := Entity (Indic);

      if not Is_Tagged_Type (Parent_Type) then
         Error_Msg_N 
           ("parent of type extension must be a tagged type ", Indic);
         return;
      end if;

      Inherit_Components (Parent_Type, T);
      End_Scope;

      Derive_Subprograms (Parent_Type, T);
   end Analyze_Private_Extension_Declaration;

   ---------------------
   -- Make_Class_Type --
   ---------------------

   procedure Make_Class_Type  (T : Entity_Id) is
      Class_Type : Entity_Id;
      Prev_Class : Entity_Id; 
      Class_Name : Name_Id;

   begin
      Prev_Class := Next_Entity (T);

      while Present (Prev_Class) 
        and then (not Is_Class_Type (Prev_Class) 
                               or else Etype (Prev_Class) /= T) 
      loop
         Prev_Class := Next_Entity (Prev_Class);
      end loop;

      --  If previous private declaration exists (which generated class
      --  type) then we are all set

      if Present (Prev_Class) then
         return;

      --  Otherwise construct the class type

      else
         --  We construct the implicit type node by hand instead of using
         --  New_Implicit_Type because the new type must be insterted after
         --  and not before the actual type definition 

         Class_Type := 
           New_External_Entity (E_Void, Scope (T), Sloc (T), T, 
                                "class", 0, "ityp");
         Set_Public_Status (Class_Type);
         Insert_After (Parent (T), Make_Implicit_Type (Sloc (T), Class_Type));

         Class_Name := Chars (Class_Type);
         Copy_Node (T, Class_Type);
         Set_Chars (Class_Type, Class_Name);
         Set_Primitive_Operations (Class_Type,  New_Elmt_List);
         Set_Ekind (Class_Type, E_Class_Type);

         --  Entity is currently last one in scope.

         Set_Next_Entity (Class_Type, Empty);
      end if;

      Set_Etype (Class_Type, T);
   end Make_Class_Type;

   ----------------------------------
   -- Analyze_Incomplete_Type_Decl --
   ----------------------------------

   procedure Analyze_Incomplete_Type_Decl (N : Node_Id) is
      T : Node_Id;

   begin
      --  Process an incomplete declaration. The identifier must not have been
      --  declared already in the scope. However, an incomplete declaration may
      --  appear in the private part of a package, for a private type that has
      --  already been declared.

      --  In this case, the discriminants (if any) must match.

      T := Find_Type_Name (N);
      Set_Ekind (T, E_Incomplete_Type);
      Set_Etype (T, T);
      New_Scope (T);

      if List_Present (Discriminant_Specifications (N)) then
         Process_The_Discriminants (N);
      end if;

      End_Scope;
   end Analyze_Incomplete_Type_Decl;

   ----------------------------
   -- Access_Type_Declaration --
   ----------------------------

   procedure Access_Type_Declaration (T : Entity_Id; Def : Node_Id) is
      S : constant Node_Id := Subtype_Indication (Def);

   begin
      --  Check for permissible use of incomplete type

      if Nkind (S) /= N_Subtype_Indication then
         Analyze (S);

         if Ekind (Entity (S)) = E_Incomplete_Type then
            Set_Directly_Designated_Type (T, Entity (S));
         else
            Set_Directly_Designated_Type (T, Process_Subtype (S, T, "parent"));
         end if;

      else
         Set_Directly_Designated_Type (T, Process_Subtype (S, T, "parent"));
      end if;

      if All_Present (Def) or Constant_Present (Def) then
         Set_Ekind (T, E_General_Access_Type);
      else
         Set_Ekind (T, E_Access_Type);
      end if;

      Set_Etype (T, T);
      Set_Esize (T, UI_From_Int (System_Address_Size));

      --  Note that Has_Tasks is always false, since the access type itself
      --  is not a task type. See Einfo for more description on this point.

      Set_Has_Tasks (T, False);

   end Access_Type_Declaration;

   -----------------------------------
   -- Access_Subprogram_Declaration --
   -----------------------------------

   procedure Access_Subprogram_Declaration
     (T_Name : Entity_Id; T_Def : Node_Id)
   is
      Formals    : constant List_Id   := Parameter_Specifications (T_Def);
      Desig_Type : constant Entity_Id := New_Implicit_Type (Sloc (T_Def));

   begin
      Set_Ekind (Desig_Type, E_Subprogram_Type);

      if Nkind (T_Def) = N_Access_Function_Definition then
         Find_Name (Subtype_Mark (T_Def));
         Set_Etype (Desig_Type, Entity (Subtype_Mark (T_Def)));
      else
         Set_Etype (Desig_Type, Standard_Void_Type);
      end if;

      if List_Present (Formals) then
         New_Scope (Desig_Type);
         Process_Formals (Desig_Type, Formals);
         End_Scope;
      end if;

      Set_Ekind (T_Name, E_Access_Subprogram_Type);
      Set_Etype (T_Name, T_Name);
      Set_Directly_Designated_Type (T_Name, Desig_Type);
   end Access_Subprogram_Declaration;

   ----------------------
   -- Constrain_Access --
   ----------------------

   procedure Constrain_Access (Def_Id : Entity_Id; S : Node_Id) is
      T             : constant Entity_Id := Entity (Subtype_Mark (S));
      Desig_Type    : constant Entity_Id := Designated_Type (T);
      Desig_Subtype : Entity_Id := New_Implicit_Type (Sloc (S));

   begin
      Set_Ekind (Def_Id, E_Access_Subtype);
      Set_Directly_Designated_Type (Def_Id, Desig_Subtype);

      if Ekind (Desig_Type) = E_Array_Type
        or else Ekind (Desig_Type) = E_String_Type
      then
         Constrain_Array (Desig_Subtype, S, Def_Id, "parent");

      elsif Ekind (Desig_Type) = E_Record_Type
        or else Ekind (Desig_Type) = E_Task_Type
        or else Ekind (Desig_Type) = E_Protected_Type
      then
         Constrain_Discriminated_Type (Desig_Subtype, S);

      else
         Error_Msg_N ("invalid constraint on access type", S);
         Set_Etype (Def_Id, Any_Type);
         return;
      end if;

      Set_Etype (Def_Id, T);
   end Constrain_Access;

   -----------------------
   -- Access_Definition --
   -----------------------

   function Access_Definition (N : Node_Id) return Entity_Id is
      Anon_Type : constant Entity_Id := New_Implicit_Type (Sloc (N), 
                                          Scope_Id => Scope (Current_Scope));
   begin
      if (Ekind (Current_Scope) = E_Entry
        or else Ekind (Current_Scope) = E_Entry_Family)
        and then Is_Task_Type (Etype (Scope (Current_Scope)))
      then
         Error_Msg_N ("Task entries cannot have access parameters", N);
      end if;

      Find_Type (Subtype_Mark (N));
      Set_Ekind (Anon_Type, E_Anonymous_Access_Type);
      Set_Etype (Anon_Type, Anon_Type);
      Set_Directly_Designated_Type (Anon_Type, Entity (Subtype_Mark (N)));
      return Anon_Type;
   end Access_Definition;

   -------------------------
   -- New_Binary_Operator --
   -------------------------

   procedure New_Binary_Operator (Op : Name_Id; Typ : Entity_Id) is
      Op_Node : Entity_Id;

      function Make_Op_Formal (Typ : Entity_Id) return Entity_Id;
      --  Create abbreviated declaration for the formal of a predefined 
      --  Operator (too abbreviated I suspect, the defining identifier
      --  lacks both a source location and a Chars field. Who provides
      --  them, no one???

      function Make_Op_Formal (Typ : Entity_Id) return Entity_Id is
         Formal : Entity_Id;

      begin
         Formal :=
           Extend_Node (New_Node (N_Defining_Identifier, No_Location));
         Set_Ekind (Formal, E_In_Parameter);
         Set_Etype (Formal, Typ);
         return Formal;
      end Make_Op_Formal;

   --  Start of processing for Make_Op_Formal

   begin
      Op_Node := Extend_Node
                (New_Node (N_Defining_Identifier, Standard_Location));
      Set_Ekind   (Op_Node, E_Operator);
      Set_Etype   (Op_Node, Typ);
      Set_Chars   (Op_Node, Op);
      Set_Homonym (Op_Node, Get_Name_Entity_Id (Op));
      Set_Name_Entity_Id (Op, Op_Node);
      Set_Is_Directly_Visible (Op_Node);
      Append_Entity (Op_Node, Current_Scope);

      Append_Entity (Make_Op_Formal (Typ), Op_Node);
      Append_Entity (Make_Op_Formal (Typ), Op_Node);

   end New_Binary_Operator;

   -----------------------
   -- New_Class_Subtype --
   -----------------------

   function New_Class_Subtype
     (N : Node_Id; Ctyp : Entity_Id) return Entity_Id
   is 
      Class_Subtype      : Entity_Id;
      Class_Subtype_Name : Name_Id;

   begin
      pragma Assert (Is_Class_Type (Ctyp), Compiler_Abort (Ctyp));

      --  we delay the implicit type node  construction till expansion
      --  because the new type must be insterted after
      --  and not before the equivalent type definition (See 
      --  Expand_Class_Subtype) 

      Class_Subtype := New_External_Entity (E_Void, Current_Scope, Sloc (Ctyp),
                         Ctyp, "subclass", 0, "ityp");
      Set_Public_Status (Class_Subtype);
      Class_Subtype_Name := Chars (Class_Subtype);
      Copy_Node (Ctyp, Class_Subtype);
      Set_Chars (Class_Subtype, Class_Subtype_Name);
      Set_Ekind (Class_Subtype, E_Class_Subtype);
      Set_Next_Entity (Class_Subtype, Empty);

      return Class_Subtype;
   end New_Class_Subtype;

end Sem_Ch3;
