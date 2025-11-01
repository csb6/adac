------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 2                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.82 $                             --
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

with Alloc;    use Alloc;
with Atree;    use Atree;
with Comperr;  use Comperr;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Util; use Exp_Util;
with Lib;      use Lib;
with Opt;      use Opt;
with Output;   use Output;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch10; use Sem_Ch10;
with Sem_Ch13; use Sem_Ch13;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Table;
with Tbuild;   use Tbuild;

package body Sem_Ch12 is

   use Atree.Unchecked_Access;
   --  This package performs untyped traversals of the tree, therefore it
   --  needs direct access to the fields of a node.

   ----------------------------------------
   -- Data Structures For Instance Table --
   ----------------------------------------

   --  A hash table is used to keep track of the mapping between generic
   --  tree nodes and their corresponding instantiated tree nodes.

   type Hash_Entry_Id is new Nat;
   --  Id of hash table entry is its subscript in the hash table

   No_Entry : constant Hash_Entry_Id := 0;
   --  Value used to mark no chain, or end of chain

   type Hash_Entry is record
      Next     : Hash_Entry_Id;
      Old_Node : Node_Id;
      New_Node : Node_Id;
   end record;

   Num_Hash_Headers : constant := 512;
   --  Number of headers in Hash_Headers array

   Hash_Headers : array (Nat range 0 .. Num_Hash_Headers - 1) of Hash_Entry_Id;
   --  Table of hash headers, each entry points to chain of entries in the
   --  hash table whose hash value matches the subscript in the header table.
   --  The hash code is simply the Node_Id value mod Num_Hash_Headers.

   package Hash_Instance is new Table (
      Component_Type => Hash_Entry,
      Index_Type     => Hash_Entry_Id,
      Low_Bound      => 1,
      Initial        => Alloc_Hash_Sub_Initial,
      Increment      => Alloc_Hash_Sub_Increment,
      Table_Name     => "Hash_Gen");

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Analyze_Formal_Array_Type   (T : in out Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Decimal_Fixed_Point (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Derived_Type        (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Discrete_Type       (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Floating_Type       (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Signed_Integer_Type (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Modular_Type        (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Ordinary_Fixed_Type (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Formal_Private_Type        (T : Entity_Id; Def : Node_Id);
   procedure Analyze_Generic_Formal_Part        (N : Node_Id);
   procedure Analyze_Generic_Access_Type        (T : Entity_Id; Def : Node_Id);

   procedure Analyze_Associations (Formals, Actuals : List_Id);
   procedure Analyze_Subprogram_Instantiation (N : Node_Id; K : Entity_Kind);

   procedure Build_Instance_Compilation_Unit_Nodes
      (N : Node_Id; Act_Body, Act_Decl, Gen_Decl, Gen_Body : Node_Id);
   --  This procedure is used in the case where the generic instance of a
   --  subprogram body or package body is a library unit. In this case, the
   --  original library unit node for the generic instantiation must be
   --  replaced by the resulting generic body, and a link made to a new
   --  compilation unit node for the generic declaration. The argument N is
   --  the original generic instantiation. Act_Body and Act_Decl are the body
   --  and declaration of the instance (either package body and declaration
   --  nodes or subprogram body and declaration nodes depending on the case).
   --  Gen_Decl and Gen_Body are the declaration and body of the generic
   --  (either generic package body and generic package declaration, or generic
   --  subprogram body and generic subprogram declaration, depending on the
   --  case.) On return, the node N has been rewritten with the actual body.

   procedure Instantiate_Entities (Gen_Scope, Act_Scope : Entity_Id);
   --  Create instances of each entity declared in the generic unit.

   procedure Set_Instance_Of (Old_Node, New_Node : Node_Id);
   --  Set New_Node to be the instance of Old_Node.

   function Get_Instance_Of (Old_Node : Node_Id) return Node_Id;
   --  Find the instantiation of a node. If the node is a generic entity,
   --  its instance is created by the previous procedure. It it is a non-
   --  generic entity, it is its own instance. If the node is not an
   --  entity, its instance is created by Instantiate_Node.

   procedure Initialize_Instances;
   --  Initialize hash table that maps generic nodes to their instances.

   procedure Complete_Entities (Gen_Scope : Entity_Id);
   --  After instantiation of the generic tree, make second pass over
   --  instantiated entities, to set correctly those attributes that are
   --  themselves nodes, and that are kind-specific.

   procedure Instantiate_Object            (Formal, Actual : Node_Id);
   procedure Instantiate_Type              (Formal, Actual : Node_Id);
   procedure Instantiate_Formal_Subprogram (Formal, Actual : Node_Id);
   procedure Instantiate_Formal_Package    (Formal, Actual : Node_Id);

   function Instantiate_Tree (N : Node_Id) return Node_Id;
   --  Third step of generic instantiation, top-level routine.

   function Instantiate_Node (N : Node_Id; Parent_Id : Node_Id) return Node_Id;
   --  Copy node N, and recursively traverse and copy its descendants.

   function Instantiate_List (L : List_Id; Parent_Id : Node_Id) return List_Id;
   --  Copy a list of trees.

   function Instantiate_Elist (E : Elist_Id) return Elist_Id;
   --  Copy an element list.

   procedure Load_Parent_Of_Generic (N : Entity_Id; Spec : Node_Id);
   --  If the generic appears in a separate non-generic library unit,
   --  load the corresponding body to retrieve the body of the generic.

   -------------------------------------------
   --  Analyze_Generic_Package_Declaration  --
   -------------------------------------------

   procedure Analyze_Generic_Package_Declaration (N : Node_Id) is
      Id : constant Entity_Id := Defining_Unit_Simple_Name (Specification (N));

   begin
      --  After processing the generic formals, analysis proceeds
      --  as for a non-generic package.

      Enter_Name (Id);
      Set_Ekind (Id, E_Generic_Package);
      Set_Etype (Id, Standard_Void_Type);
      New_Scope (Id);
      Analyze_Generic_Formal_Part (N);
      Analyze (Specification (N));
      End_Package_Scope (Id);
   end Analyze_Generic_Package_Declaration;

   ---------------------------------------------
   --  Analyze_Generic_Subprogram_Declaration --
   ---------------------------------------------

   procedure Analyze_Generic_Subprogram_Declaration (N : Node_Id) is
      Spec    : constant Node_Id   := Specification (N);
      Id      : constant Entity_Id := Defining_Unit_Simple_Name (Spec);
      Formals : constant List_Id   := Parameter_Specifications (Spec);

   begin
      Enter_Name (Id);

      if Nkind (Spec) = N_Function_Specification then
         Set_Ekind (Id, E_Generic_Function);
      else
         Set_Ekind (Id, E_Generic_Procedure);
         Set_Etype (Id, Standard_Void_Type);
      end if;

      New_Scope (Id);
      Analyze_Generic_Formal_Part (N);

      if List_Present (Formals) then
         Process_Formals (Id, Formals);
      end if;

      if Nkind (Spec) = N_Function_Specification then
         Find_Type (Subtype_Mark (Spec));
         Set_Etype (Id, Entity (Subtype_Mark (Spec)));
      end if;

      End_Scope;
   end Analyze_Generic_Subprogram_Declaration;

   ----------------------------------
   --  Analyze_Generic_Formal_Part --
   ----------------------------------

   procedure Analyze_Generic_Formal_Part (N : Node_Id) is
      Gen_Parm_Decl : Node_Id;

   begin
      --  The generic formals are processed in the scope of the generic
      --  unit, where they are directly visible. The scope is installed
      --  by the caller.

      Gen_Parm_Decl := First (Generic_Formal_Declarations (N));

      while Present (Gen_Parm_Decl) loop
         Analyze (Gen_Parm_Decl);
         Gen_Parm_Decl := Next (Gen_Parm_Decl);
      end loop;
   end Analyze_Generic_Formal_Part;

   -----------------------------------
   -- Analyze_Package_Instantiation --
   -----------------------------------

   procedure Analyze_Package_Instantiation (N : Node_Id) is
      Act_Body    : Node_Id;
      Act_Body_Id : Entity_Id;
      Act_Decl    : Node_Id   := New_Node (N_Package_Declaration, Sloc (N));
      Act_Decl_Id : Entity_Id := New_Copy (Defining_Unit_Simple_Name (N));

      Actuals     : List_Id   := Generic_Associations (N);
      Formals     : List_Id;
      Gen_Id      : Node_Id   := Name (N);
      Gen_Decl    : Node_Id;
      Gen_Body    : Node_Id;
      Gen_Body_Id : Entity_Id;
      Gen_Unit    : Entity_Id;
      Spec        : Node_Id;

   begin
      Initialize_Instances;
      Enter_Name (Act_Decl_Id);
      Set_Ekind (Act_Decl_Id, E_Package);
      Set_Has_Completion (Act_Decl_Id);
      Set_Etype (Act_Decl_Id, Standard_Void_Type);

      Find_Name (Gen_Id);
      Gen_Unit := Entity (Gen_Id);

      --  Verify that it is the name of a generic package

      if Etype (Gen_Unit) = Any_Type then
         return;
      end if;

      if Ekind (Gen_Unit) /= E_Generic_Package then
         Error_Msg_N
            ("expect name of generic package in instantiation", Gen_Id);

      else
         Spec := Parent (Gen_Unit);
         Gen_Decl := Parent (Spec);
         Formals := Generic_Formal_Declarations (Gen_Decl);
         Analyze_Associations (Formals, Actuals);
         Instantiate_Entities (Gen_Unit, Act_Decl_Id);

         --  Instantiate package specification, and insert in  current
         --  declarative part , BEFORE the instantiation node, to avoid
         --  redoing the semantic analysis of the instance.

         Set_Specification (Act_Decl, Instantiate_Tree (Spec));
         Set_Generic_Parent (Specification (Act_Decl), Gen_Unit);
         --  If present, instantiate package body, and insert after spec.

         Gen_Body_Id := Corresponding_Body (Parent (Spec));

         if No (Gen_Body_Id)
           and then Operating_Mode = Generate_Code
           and then Unit_Requires_Body (Gen_Unit)
         then
            Load_Parent_Of_Generic (N, Spec);
            Gen_Body_Id := Corresponding_Body (Parent (Spec));
         end if;

         if Present (Gen_Body_Id) then
            Act_Body_Id := New_Copy (Gen_Body_Id);
            Set_Chars (Act_Body_Id, Chars (Act_Decl_Id));
            Set_Is_Internal (Act_Body_Id);
            Set_Ekind (Act_Body_Id, E_Package);

            --  First, instantiate entities local to the body.

            Instantiate_Entities (Gen_Body_Id, Act_Body_Id);

            --  Finally, instantiate tree for package body.

            Gen_Body := Parent (Gen_Body_Id);
            Act_Body := Instantiate_Tree (Gen_Body);
            Set_Corresponding_Body (Act_Decl, Act_Body_Id);
            Set_Corresponding_Spec (Act_Body, Act_Decl_Id);

            --  If the instantiation is a library unit, then build
            --  the resulting compilation unit nodes for the instance

            if Nkind (Parent (N)) = N_Compilation_Unit then
               Build_Instance_Compilation_Unit_Nodes
                 (N, Act_Body, Act_Decl, Gen_Decl, Gen_Body);

               --  Make sure created entities are set as public

               Set_Is_Public (Act_Body_Id, True);
               Set_Is_Public (Act_Decl_Id, True);

            --  If the instantiation is not a library unit, then append
            --  the entities to the list of implicitly generated entities

            else
               Append (Act_Decl, Implicit_Type_List);
               Append (Act_Body, Implicit_Type_List);
            end if;

            Complete_Entities (Gen_Unit);
            Complete_Entities (Gen_Body_Id);

         --  Case of package does not need a body

         else
            --  If the instantiation of the declaration is a library unit,
            --  then rewrite the original package instantiation as a package
            --  declaration in the compilation unit node.

            if Nkind (Parent (N)) = N_Compilation_Unit then
               Rewrite_Substitute_Tree (N, Act_Decl);

            --  If the instantiation is not a library unit, then append the
            --  entities to the list of implicitly generated entities.

            else
               Append (Act_Decl, Implicit_Type_List);
            end if;

            Complete_Entities (Gen_Unit);
         end if;
      end if;
   end Analyze_Package_Instantiation;

   -------------------------------------
   -- Analyze_Procedure_Instantiation --
   -------------------------------------

   procedure Analyze_Procedure_Instantiation (N : Node_Id) is
   begin
      Analyze_Subprogram_Instantiation (N, E_Procedure);
   end Analyze_Procedure_Instantiation;

   ------------------------------------
   -- Analyze_Function_Instantiation --
   ------------------------------------

   procedure Analyze_Function_Instantiation (N : Node_Id) is
   begin
      Analyze_Subprogram_Instantiation (N, E_Function);
   end Analyze_Function_Instantiation;

   ------------------------------------
   -- Analyze_Subprogram_Instantiation --
   ------------------------------------

   procedure Analyze_Subprogram_Instantiation (N : Node_Id; K : Entity_Kind) is
      Act_Decl     : Node_Id := New_Node (N_Subprogram_Declaration, Sloc (N));
      Act_Decl_Id  : Entity_Id := New_Copy (Defining_Unit_Simple_Name (N));
      Act_Body     : Node_Id;
      Act_Body_Id  : Entity_Id;

      Actuals      : List_Id := Generic_Associations (N);
      Formals      : List_Id;
      Gen_Id       : Node_Id := Name (N);
      Gen_Unit     : Entity_Id;
      Gen_Body     : Node_Id;
      Gen_Decl     : Node_Id;
      Gen_Body_Id  : Entity_Id;
      Spec         : Node_Id;

   begin
      Initialize_Instances;
      Set_Ekind (Act_Decl_Id, K);
      Set_Has_Completion (Act_Decl_Id);
      Set_Etype (Act_Decl_Id, Any_Type);

      Find_Name (Gen_Id);
      Gen_Unit := Entity (Gen_Id);

      if Etype (Gen_Unit) = Any_Type then return; end if;

      --  Verify that it is a generic subprogram of the right kind.

      if K = E_Procedure and then Ekind (Gen_Unit) /= E_Generic_Procedure then
         Error_Msg_N
            ("expect name of generic procedure in instantiation", Gen_Id);

      elsif K = E_Function and then Ekind (Gen_Unit) /= E_Generic_Function then
         Error_Msg_N
            ("expect name of generic function in instantiation", Gen_Id);

      else
         Spec := Parent (Gen_Unit);
         Gen_Decl := Parent (Spec);
         Formals := Generic_Formal_Declarations (Gen_Decl);
         Analyze_Associations (Formals, Actuals);

         --  Instantiate subprogram declaration,

         Instantiate_Entities (Gen_Unit, Act_Decl_Id);
         Set_Specification (Act_Decl,  Instantiate_Tree (Spec));
         Set_Generic_Parent (Specification (Act_Decl), Gen_Unit);
         Set_Etype (Act_Decl_Id, Get_Instance_Of (Etype (Gen_Unit)));

         --  Now subprogram is callable

         New_Overloaded_Entity (Act_Decl_Id);

         if Chars (Gen_Unit) = Name_Unchecked_Conversion
           and then Is_Intrinsic (Gen_Unit)
         then
            Validate_Unchecked_Conversion (N, Act_Decl_Id);
            Set_Is_Intrinsic (Act_Decl_Id);
            return;
         end if;

         --  Instantiate subprogram body, and insert before instantiation node

         Gen_Body_Id := Corresponding_Body (Parent (Spec));

         --  If we are generating code, the body of the generic must be
         --  present. It may be in the body of another non-generic library
         --  unit, which must be loaded.

         if No (Gen_Body_Id) and then Operating_Mode = Generate_Code then
            Load_Parent_Of_Generic (N, Spec);
            Gen_Body_Id := Corresponding_Body (Parent (Spec));
         end if;

         if Present (Gen_Body_Id) then
            Act_Body_Id := New_Copy (Gen_Body_Id);
            Set_Chars (Act_Body_Id, Chars (Act_Decl_Id));
            Set_Is_Internal (Act_Body_Id);
            Set_Ekind (Act_Body_Id, K);
            Set_Etype (Act_Body_Id, Get_Instance_Of (Etype (Gen_Unit)));
            Set_Scope (Act_Body_Id, Current_Scope);

            Instantiate_Entities (Gen_Body_Id, Act_Body_Id);
            Gen_Body := Parent (Parent (Gen_Body_Id));
            Act_Body := Instantiate_Tree (Gen_Body);
            Set_Defining_Unit_Name (Specification (Act_Body), Act_Body_Id);
            Set_Corresponding_Body (Act_Decl, Act_Body_Id);
            Set_Corresponding_Spec (Act_Body, Act_Decl_Id);

            --  If the instantiation is a library unit, then build the
            --  resulting compilation unit nodes for the instance

            if Nkind (Parent (N)) = N_Compilation_Unit then
               Build_Instance_Compilation_Unit_Nodes
                 (N, Act_Body, Act_Decl, Gen_Decl, Gen_Body);

               --  Make sure created entities are set as public

               Set_Is_Public (Act_Body_Id, True);
               Set_Is_Public (Act_Decl_Id, True);

            --  If the instantiation is not a library unit, then append
            --  the entities to the list of implicitly generated entities

            else
               Append (Act_Decl, Implicit_Type_List);
               Append (Act_Body, Implicit_Type_List);
            end if;

            Append_Entity (Act_Body_Id, Current_Scope);
            Complete_Entities (Gen_Body_Id);

         --  If the body was not required, nothing to do. If it was not
         --  found, a fatal error was already emitted.

         else
            null;
         end if;

         Complete_Entities (Gen_Unit);
      end if;
   end Analyze_Subprogram_Instantiation;

   ------------------------
   -- Instantiate_Record --
   ------------------------

   function Instantiate_Record (Parent_Type, Derived_Type : Entity_Id)
                                                       return Node_Id is
      Type_Def : Node_Id;
   begin
      Initialize_Instances;
      Instantiate_Entities (Parent_Type, Derived_Type);
      Type_Def := Instantiate_Tree (Type_Definition (Parent (Parent_Type)));
      Complete_Entities (Parent_Type);
      return Type_Def;
   end Instantiate_Record;

   ----------------------------
   -- Load_Parent_Of_Generic --
   ----------------------------

   procedure Load_Parent_Of_Generic (N : Entity_Id; Spec : Node_Id) is
   begin
      if Get_Sloc_Unit_Number (Sloc (N)) /=
         Get_Sloc_Unit_Number (Sloc (Spec))
      then
         --  Find body of parent of spec, and analyze it

         Load_Needed_Body
           (File.Table (Get_Sloc_Unit_Number (Sloc (Spec))).Cunit);
      end if;
   end Load_Parent_Of_Generic;

   --------------------------
   -- Analyze_Associations --
   --------------------------

   procedure Analyze_Associations (Formals, Actuals : List_Id) is
      Actual : Node_Id;
      Formal : Node_Id;
      Match  : Node_Id;
      Named  : Node_Id;
      First_Named : Node_Id := Empty;
      Num_Matched : Integer := 0;
      Num_Actuals : Integer := 0;

      function Matching_Actual (F : Entity_Id) return Node_Id is
         Found : Node_Id;
      begin
         if No (Actual) then 
            --  End of list of purely positional parameters.
            Found := Empty;

         elsif No (Selector_Name (Actual)) then
            --  Positional parameter corresponds to current formal.
            Found := Explicit_Generic_Actual_Parameter (Actual);
            Num_Matched := Num_Matched + 1;
            Actual := Next (Actual);
         else
            --  Scan list of named actuals to find the one with the
            --  desired name. all remaining actuals have explicit names.
            Found := Empty;
            while Present (Actual) loop
               if Chars (Selector_Name (Actual)) = Chars (F) then
                  Found := Explicit_Generic_Actual_Parameter (Actual);
                  Num_Matched := Num_Matched + 1;
                  exit;
               end if;
               Actual := Next (Actual);
            end loop;
            --  Reset for subsequent searches.
            Actual := First_Named;
         end if;

         return Found;
      end Matching_Actual;

   --  Start processing for Analyze_Associations

   begin
      --  If named associations are present, save the first named association
      --   (it may of course be Empty) to facilitate subsequent name search.

      if List_Present (Actuals) then
         First_Named := First (Actuals);

         while Present (First_Named)
           and then No (Selector_Name (First_Named))
         loop
            Num_Actuals := Num_Actuals + 1;
            First_Named := Next (First_Named);
         end loop;
      end if;

      Named := First_Named;
      while Present (Named) loop
         if No (Selector_Name (Named)) then
            Error_Msg_N ("invalid positional actual after named one", Named);
            --  No point in  countinuing with associations.
            return;
         end if;

         Num_Actuals := Num_Actuals + 1;
         Named := Next (Named);
      end loop;

      if List_Present (Formals) then
         Formal := First (Formals);

         if List_Present (Actuals) then
            Actual := First (Actuals);

            while Present (Actual) loop
               Analyze (Explicit_Generic_Actual_Parameter (Actual));
               Actual := Next (Actual);
            end loop;

            Actual := First (Actuals);

         else -- all formals should have default values
            Actual := Empty;
         end if;

         while Present (Formal) loop

            case Nkind (Formal) is
               when N_Formal_Object_Declaration =>
                  Instantiate_Object 
                    (Defining_Identifier (Formal),
                     Matching_Actual (Defining_Identifier (Formal)));

               when N_Formal_Type_Declaration =>
                  Match := Matching_Actual (Defining_Identifier (Formal));
                  if No (Match) then
                     Error_Msg_N 
                      ("missing actual for generic formal ", 
                                                 List_Parent (Actuals));
                     Set_Instance_Of (Defining_Identifier (Formal), Any_Type);
                  else 
                     Instantiate_Type (Formal, Match);
                  end if;

               when N_Formal_Subprogram_Declaration =>
                  Instantiate_Formal_Subprogram (Formal,
                            Matching_Actual (Defining_Unit_Name (
                                                 Specification (Formal))));

               when N_Formal_Package_Declaration =>
                  Instantiate_Formal_Package (Formal, 
                            Matching_Actual (Defining_Identifier (Formal)));

               when others => null;

            end case;

            Formal := Next (Formal);
         end loop;
         if Num_Actuals > Num_Matched then
            Error_Msg_N
              ("unmatched actuals in instantiation", List_Parent (Actuals));
         end if;

      elsif List_Present (Actuals) then
         Error_Msg_N
           ("too many actuals in generic instantiation",
            List_Parent (Actuals));
      end if;
   end Analyze_Associations;

   ---------------------------------
   -- Analyze_Generic_Association --
   ---------------------------------

   procedure Analyze_Generic_Association (N : Node_Id) is
   begin
      --  ??? why is this an abort, TBSL? (should be unimplemented?)
      Compiler_Abort;
   end Analyze_Generic_Association;

   ---------------------------------------
   -- Analyze_Formal_Object_Declaration --
   ---------------------------------------

   procedure Analyze_Formal_Object_Declaration (N : Node_Id) is
      Id : Node_Id;
      E  : constant Node_Id := Expression (N);
      K  : Entity_Kind;
      T  : Node_Id;

   begin
      --  Determine the mode of the formal object

      if Out_Present (N) then
         if In_Present (N) then
            K := E_Generic_In_Out_Parameter;
         else
            Error_Msg_N ("formal generic objects cannot have mode OUT", N);
            K := E_Generic_In_Out_Parameter;
         end if;

      else
         K := E_Generic_In_Parameter;
      end if;

      Find_Type (Subtype_Mark (N));
      T := Entity (Subtype_Mark (N));

      case K is
         when E_Generic_In_Parameter =>
            if Is_Limited_Type (T) then
               Error_Msg_N
                ("generic formal of mode IN must not be of limited type", N);
            end if;

            if Present (E) then
               Analyze (E);
               Resolve_Complete_Context (E, T);
               --  if is_deferred_constant(opt_init) then
               --  errmsg("Deferred constant cannot be default expression",
               --  " for a generic parameter","7.4.3");
               --  end if;
            end if;

         when E_Generic_In_Out_Parameter =>

            --  Constraints will be inherited from actual. This is described
            --  by regarding the subtype of the in out parameter as an extra
            --  generic parameter, obtained from the actual at instantiation.
            --  subtype_mark := make_generic_subtype (subtype_mark);

            if Present (E) then
               Error_Msg_N
                ("initialization not allowed for `IN OUT` formals", N);
            end if;

         when others => Compiler_Abort;
      end case;

      Id := Defining_Identifier (N);
      Enter_Name (Id);
      Set_Ekind (Id, K);
      Set_Etype (Id, T);
   end Analyze_Formal_Object_Declaration;

   -------------------------------------
   -- Analyze_Formal_Type_Declaration --
   -------------------------------------

   procedure Analyze_Formal_Type_Declaration (N : Node_Id) is
      Def : constant Node_Id   := Formal_Type_Definition (N);
      T   : Entity_Id := Defining_Identifier (N);

   begin
      --  Enter the new name, and branch to specific routine.

      Enter_Name (T);
      Set_Is_Generic_Type (T);

      case Nkind (Def) is
         when N_Formal_Private_Type_Definition
                        => Analyze_Formal_Private_Type (T, Def);

         when N_Formal_Derived_Type_Definition
                        => Analyze_Formal_Derived_Type (T, Def);

         when N_Formal_Discrete_Type_Definition
                        => Analyze_Formal_Discrete_Type (T, Def);

         when N_Formal_Signed_Integer_Type_Definition
                        => Analyze_Formal_Signed_Integer_Type (T, Def);

         when N_Formal_Modular_Type_Definition
                        => Analyze_Formal_Modular_Type (T, Def);

         when N_Formal_Floating_Point_Definition
                        => Analyze_Formal_Floating_Type (T, Def);

         when N_Formal_Ordinary_Fixed_Point_Definition
                        => Analyze_Formal_Ordinary_Fixed_Type (T, Def);

         when N_Formal_Decimal_Fixed_Point_Definition
                        => Analyze_Formal_Decimal_Fixed_Point (T, Def);

         when N_Array_Type_Definition
                        => Analyze_Formal_Array_Type (T, Def);

         when N_Access_To_Object_Definition
                        => Analyze_Generic_Access_Type (T, Def);

         when others => Compiler_Abort;
      end case;
   end Analyze_Formal_Type_Declaration;

   ---------------------------------
   -- Analyze_Formal_Private_Type --
   ---------------------------------

   procedure Analyze_Formal_Private_Type (T : Entity_Id; Def : Node_Id) is
   begin
      if Limited_Present (Def) then
         Set_Ekind (T, E_Limited_Private_Type);
      else
         Set_Ekind (T, E_Private_Type);
      end if;

      Set_Etype (T, T);

      --  Process_Discriminants (name, opt_discr);
      --  Popscope ();

      if Tagged_Present (Def) then
         Set_Is_Tagged_Type (T);

         --  Initialize dispatch_table

         Make_Class_Type (T);
      end if;
   end Analyze_Formal_Private_Type;

   ---------------------------------
   -- Analyze_Formal_Derived_Type --
   ---------------------------------

   procedure Analyze_Formal_Derived_Type (T : Entity_Id; Def : Node_Id) is
   begin
      Unimplemented (Def, "formal derived types");
   end Analyze_Formal_Derived_Type;

   ----------------------------------
   -- Analyze_Formal_Discrete_Type --
   ----------------------------------

   procedure Analyze_Formal_Discrete_Type (T : Entity_Id; Def : Node_Id) is
      --  The operations defined for a discrete types are those of an
      --  enumeration type. The size is set to an arbitrary value, for use
      --  in analyzing the generic unit.
   begin
      Set_Ekind (T, E_Enumeration_Type);
      Set_Etype (T, T);
      Set_Esize (T, Esize (Standard_Integer));
   end Analyze_Formal_Discrete_Type;

   ----------------------------------------
   -- Analyze_Formal_Signed_Integer_Type --
   ----------------------------------------

   procedure Analyze_Formal_Signed_Integer_Type
     (T : Entity_Id; Def : Node_Id)
   is
      Base : constant Entity_Id :=
        New_Internal_Entity
          (E_Integer_Type, Current_Scope, Sloc (Def), "gen");

   begin
      Set_Ekind (T, E_Integer_Subtype);
      Set_Etype (T, Base);
      Set_Etype (Base, Base);
      Set_Is_Generic_Type (Base);
      Set_Esize (T, Esize (Standard_Integer));
      Set_Esize (Base, Esize (Standard_Integer));
      Set_Scalar_Range (T, Scalar_Range (Standard_Integer));
      Set_Scalar_Range (Base, Scalar_Range (Standard_Integer));
   end Analyze_Formal_Signed_Integer_Type;

   ---------------------------------
   -- Analyze_Formal_Modular_Type --
   ---------------------------------

   procedure Analyze_Formal_Modular_Type (T : Entity_Id; Def : Node_Id) is
   begin
      Unimplemented (Def, "formal modular types");
   end Analyze_Formal_Modular_Type;

   ----------------------------------
   -- Analyze_Formal_Floating_Type --
   ---------------------------------

   procedure Analyze_Formal_Floating_Type (T : Entity_Id; Def : Node_Id) is
      Base : constant Entity_Id :=
        New_Internal_Entity
          (E_Float_Type, Current_Scope, Sloc (Def), "gen");

   begin
      Set_Ekind (T, E_Float_Subtype);
      Set_Etype (T, Base);
      Set_Etype (Base, Base);
      Set_Is_Generic_Type (Base);
      Set_Esize (T, Esize (Standard_Float));
      Set_Esize (Base, Esize (Standard_Float));
      Set_Scalar_Range (T, Scalar_Range (Standard_Float));
      Set_Scalar_Range (Base, Scalar_Range (Standard_Float));
   end Analyze_Formal_Floating_Type;

   ----------------------------------------
   -- Analyze_Formal_Ordinary_Fixed_Type --
   ----------------------------------------

   procedure Analyze_Formal_Ordinary_Fixed_Type
     (T : Entity_Id; Def : Node_Id) is
   begin
      Unimplemented (Def, "formal ordinary fixed types");
   end Analyze_Formal_Ordinary_Fixed_Type;

   ----------------------------------------
   -- Analyze_Formal_Decimal_Fixed_Point --
   ----------------------------------------

   procedure Analyze_Formal_Decimal_Fixed_Point
     (T : Entity_Id; Def : Node_Id) is

   begin
      Unimplemented (Def, "formal decimal fixed types");
   end Analyze_Formal_Decimal_Fixed_Point;

   -------------------------------
   -- Analyze_Formal_Array_Type --
   -------------------------------

   procedure Analyze_Formal_Array_Type (T : in out Entity_Id; Def : Node_Id) is
      I : Node_Id;

   begin
      --  Treated like a non-generic array declaration, with
      --  additional semantic checks.

      Array_Type_Declaration (T, Def);

      if Is_Incomplete_Type (Component_Type (T)) 
        and then not Is_Generic_Type (Component_Type (T))
      then
         Error_Msg_N ("premature usage of incomplete type", Def);

      elsif Is_Internal (Component_Type (T)) then
         Error_Msg_N
           ("only a subtype mark is allowed in a formal", Def);
      end if;

      I := First_Index (T);

      while Present (I) loop
         if Is_Internal (I) then
            Error_Msg_N
              ("only a subtype mark is allowed in a formal", Def);
         end if;
         I := Next_Index (I);
      end loop;
   end Analyze_Formal_Array_Type;

   ---------------------------------
   -- Analyze_Generic_Access_Type --
   ---------------------------------

   procedure Analyze_Generic_Access_Type (T : Entity_Id; Def : Node_Id) is
   begin
      Access_Type_Declaration (T, Def);

      if Is_Incomplete_Type (Designated_Type (T))
        and then not Is_Generic_Type (Designated_Type (T))
      then
         Error_Msg_N ("premature usage of incomplete type", Def);

      elsif Is_Internal (Designated_Type (T)) then
         Error_Msg_N
           ("only a subtype mark is allowed in a formal", Def);
      end if;
   end Analyze_Generic_Access_Type;

   -------------------------------
   -- Analyze_Formal_Subprogram --
   -------------------------------

   procedure Analyze_Formal_Subprogram (N : Node_Id) is
      Spec : constant Node_Id := Specification (N);

   begin
      --  Default name is resolved at the point of instantiation

      if Box_Present (N) then
         null;

      --  Else default is bound at the point of generic declaration

      elsif Present (Default_Name (N)) then
         Find_Name (Default_Name (N));
      end if;

      Analyze_Subprogram_Declaration (N);
      Set_Has_Completion (Defining_Unit_Name (Spec));
   end Analyze_Formal_Subprogram;

   ----------------------------
   -- Analyze_Formal_Package --
   ----------------------------

   procedure Analyze_Formal_Package (N : Node_Id) is
      Actuals   : List_Id   := Generic_Associations (N);
      Decls     : Node_Id;
      Formal_Id : Entity_Id := Defining_Identifier (N);
      Formals   : List_Id;
      Gen_Id    : Node_Id   := Name (N);
      Gen_Unit  : Entity_Id;
      Spec      : Node_Id;

   begin
      Enter_Name (Formal_Id);
      Set_Ekind (Formal_Id, E_Package);
      Set_Etype (Formal_Id,  Standard_Void_Type);
      Find_Name (Gen_Id);
      Gen_Unit := Entity (Gen_Id);

      if Ekind (Gen_Unit) /= E_Generic_Package then
         Error_Msg_N ("Expect generic package name", Gen_Id);
         return;
      end if;

      --  The formal package is treated like a regular instance, but only
      --  the specification needs to be instantiated, to make entities visible.
      --  If there are no generic associations, the generic parameters appear
      --  as local entities and are instantiated like them. Otherwise the
      --  associations are analyzed and the parameters matched to the actuals.

      Set_Generic_Parent (N, Gen_Unit);
      Spec := Parent (Gen_Unit);
      Formals := Generic_Formal_Declarations (Parent (Spec));

      if not Box_Present (N) then
         Analyze_Associations (Formals, Actuals);
      end if;

      Instantiate_Entities (Gen_Unit, Formal_Id);

      --  The tree for the declarations has to be instantiated as well, to
      --  allow the completion of the instantiation of the entities. The tree
      --  itself is not kept as part of the current generic unit.

      Decls := Instantiate_Tree (Spec);
      Complete_Entities (Gen_Unit);
   end Analyze_Formal_Package;

   ---------------------------------------------
   --  Build_Instance_Compilation_Unit_Nodes  --
   ---------------------------------------------

   procedure Build_Instance_Compilation_Unit_Nodes
      (N : Node_Id; Act_Body, Act_Decl, Gen_Decl, Gen_Body : Node_Id)
   is
      Decl_Cunit : Node_Id;
      Body_Cunit : Node_Id;
      Citem      : Node_Id;

   begin

      --  A new compilation unit node is built for the instance declaration

      Decl_Cunit := New_Node (N_Compilation_Unit, Sloc (N));
      Set_Context_Items (Decl_Cunit, Empty_List);
      Set_Unit          (Decl_Cunit, Act_Decl);
      Set_Parent_Spec (Act_Decl, Parent_Spec (N));
      Set_Body_Required (Decl_Cunit, True);

      --  We use the original instantiation compilation unit as the resulting
      --  compilation unit of the instance, since this is the main unit.

      Rewrite_Substitute_Tree (N, Act_Body);
      Body_Cunit := Parent (N);
      --  The two compilation unit nodes are linked by the Library_Unit field

      Set_Library_Unit  (Decl_Cunit, Body_Cunit);
      Set_Library_Unit  (Body_Cunit, Decl_Cunit);

      --  The context clause items on the instantiation, which are now
      --  attached to the body compilation unit (since the body overwrote
      --  the orginal instantiation node), semantically belong on the spec,
      --  so copy them there. It's harmless to leave them on the body as well.
      --  In fact one could argue that they belong in both places.

      Citem := First (Context_Items (Body_Cunit));
      while Present (Citem) loop
         Append (New_Copy (Citem), Context_Items (Decl_Cunit));
         Citem := Next (Citem);
      end loop;

   end Build_Instance_Compilation_Unit_Nodes;

   ------------------------
   -- Instantiate_Object --
   ------------------------

   procedure Instantiate_Object (Formal, Actual : Node_Id) is
      Act_Type   : Entity_Id := Get_Instance_Of (Etype (Formal));
      Act_Object : Entity_Id;
      Decl_Node  : Node_Id;

   begin
      if Get_Instance_Of (Formal) /= Formal then
         Error_Msg_N ( "Duplicate instantiation of generic parameter", Actual);
      end if;

      if Ekind (Formal) = E_Generic_In_Parameter then

         --  The instantiation of a generic formal in-parameter is a
         --  constant declaration.We create an internal constant whose
         --  initial value is the generic actual, and we establish this
         --  internal constant as the instance of the generic formal.

         Decl_Node := New_Node (N_Object_Declaration, Sloc (Formal));
         Set_Constant_Present (Decl_Node, True);

         Analyze (Actual);
         Resolve_Complete_Context (Actual, Act_Type);
         Set_Expression (Decl_Node, Instantiate_Tree (Actual));
         Set_Object_Definition (Decl_Node,
                          New_Occurrence_Of (Act_Type, Sloc (Formal)));

         --  If the actual type is unconstrained, the actual derives its
         --  bound from the initial value. The process here mimics what is
         --  done for object declarations, and should eventually be
         --  common code in the expander.

         if (Act_Type = Standard_String)
           or else
                (Is_String_Type (Act_Type) and not Is_Constrained (Act_Type))
         then
            Expand_Subtype_From_Expr (Decl_Node, Act_Type);
            Act_Type := Process_Subtype (Object_Definition (Decl_Node));
         end if;

         Act_Object := New_Internal_Entity
           (E_Constant, Current_Scope, Sloc (Formal), "actual_in");

      else
         --  An IN OUT generic actual must be a name. The instantiation
         --  is equivalent to a renaming declaration.

         Act_Object := New_Internal_Entity 
           (E_Variable, Current_Scope, Sloc (Formal), "actual_inout");

         Decl_Node := New_Node (N_Object_Renaming_Declaration, Sloc (Formal));

         Find_Name (Actual);
         Set_Name (Decl_Node, Actual);
         Set_Subtype_Mark (Decl_Node,
                               New_Occurrence_Of (Act_Type, Sloc (Formal)));
         Set_Renamed_Object (Act_Object, Instantiate_Tree (Actual));
      end if;

      Set_Instance_Of (Formal, Act_Object);
      Set_Etype (Act_Object, Act_Type);
      Set_Public_Status (Act_Object);
      Set_Defining_Identifier (Decl_Node, Act_Object);

      --  The declaration is inserted before the instantiations, because
      --  it may be used in subsequent subprogram signatures, etc.

      Append (Decl_Node, Implicit_Type_List);
   end Instantiate_Object;

   ----------------------
   -- Instantiate_Type --
   ----------------------

   procedure Instantiate_Type (Formal, Actual : Node_Id) is
      Gen_T : Entity_Id := Defining_Identifier (Formal);
      Act_T : Entity_Id;

   begin
      if Get_Instance_Of (Gen_T) /= Gen_T then
         Error_Msg_N ( "Duplicate instantiation of generic type", Actual);

      else
         Act_T := Entity (Actual);
         Set_Instance_Of (Gen_T, Act_T);

         if not Is_Abstract (Gen_T)
           and then Is_Abstract (Act_T)
         then
            Error_Msg_N 
              ("actual of non-abstract formal cannot be abstract", Actual);
         end if;

         if Is_Scalar_Type (Gen_T) then
            Set_Instance_Of (Etype (Gen_T), Etype (Act_T));
            if Is_Integer_Type (Gen_T) 
              and then not Is_Integer_Type (Act_T)
            then
               Error_Msg_N 
                  ("expect integer type in  instantiation", Parent (Actual));
            elsif Is_Float_Type (Gen_T) 
              and then not Is_Float_Type (Act_T)
            then
               Error_Msg_N 
                  ("expect float type in  instantiation", Parent (Actual));
            elsif Is_Fixed_Type (Gen_T) 
              and then not Is_Fixed_Type (Act_T)
            then
               Error_Msg_N 
                  ("expect fixed type in  instantiation", Parent (Actual));
            --  Decimal Types ???
            end if;
         end if;
      end if;
   end Instantiate_Type;

   -----------------------------------
   -- Instantiate_Formal_Subprogram --
   -----------------------------------

   procedure Instantiate_Formal_Subprogram (Formal, Actual : Node_Id) is
      Formal_Sub : constant Entity_Id :=
        Defining_Unit_Name (Specification (Formal));

   begin
      if Present (Actual) then
         Find_Name (Actual);
         Set_Instance_Of (Formal_Sub, Entity (Actual));

         if Is_Abstract (Entity (Actual)) then
            Error_Msg_N
              ("generic actual subprogram cannot be abstract", Actual);
         end if;

      elsif Present (Default_Name (Formal)) then
         Set_Instance_Of (Formal_Sub, Entity (Default_Name (Formal)));

      else
         Unimplemented (Parent (Actual), "default formal subprograms");
      end if;

   end Instantiate_Formal_Subprogram;

   --------------------------------
   -- Instantiate_Formal_Package --
   --------------------------------

   procedure Instantiate_Formal_Package (Formal, Actual : Node_Id) is
      Act_Entity : Entity_Id;
      Gen_Entity : Entity_Id;
   begin
      if No (Actual) then
         Error_Msg_N ("missing actual for generic formal ", Parent (Actual));
         Set_Instance_Of (Defining_Identifier (Formal), Any_Id);

      elsif Ekind (Actual) /= E_Package
        or else Generic_Parent (Actual) /= Generic_Parent (Formal)
      then
         Error_Msg_N ("expect package instance to instantiate formal",
           Parent (Actual));

      else
         Set_Instance_Of (Defining_Identifier (Formal), Actual);
         Gen_Entity := First_Entity (Formal);
         Act_Entity := First_Entity (Actual);
         --  The entities in the generic formal are instantiated with the
         --  corresponding entities in the generic actual.
         while Present (Gen_Entity) loop
            Set_Instance_Of (Gen_Entity, Act_Entity);
            Gen_Entity := Next_Entity (Gen_Entity);
            Act_Entity := Next_Entity (Act_Entity);
         end loop;
      end if;
   end Instantiate_Formal_Package;

   ---------------------------
   --  Instantiate_Entities --
   ---------------------------

   --  The second step of generic instantiation is the construction of
   --  entities in the instance,  in one-to-one correspondence with
   --  entities in the generic unit. We traverse the entity chains in
   --  order, and build defining occurrences for the actual entities. Each
   --  generic entity points to its instance. The Etype can be instantiated
   --  at the same time, because they are not forward references in the
   --  chain. Other attributes are updated subsequently. All scopes within
   --  the unit are instantiated recursively.

   --  On entry, the generic formals have already been matched to the actuals

   --  If the new entity is a package, then its local declarations
   --  are visible on exit, and can become use visible. The local entities
   --  are entered in the visibility table  accordingly. At the end, there
   --  is no need to unlink them, it is sufficient to exit the scope.

   procedure Instantiate_Entities (Gen_Scope, Act_Scope : Entity_Id) is
      Gen_Id : Entity_Id := First_Entity (Gen_Scope);
      New_Id : Entity_Id;

   begin
      Set_Instance_Of (Gen_Scope, Act_Scope);

      --  The new scope contains no entities

      Set_First_Entity (Act_Scope, Empty);
      Set_Last_Entity  (Act_Scope, Empty);
      New_Scope (Act_Scope);

      while Present (Gen_Id) loop
         if Get_Instance_Of (Gen_Id) = Gen_Id then
            New_Id := New_Copy (Gen_Id);
            Set_Instance_Of (Gen_Id, New_Id);
            Set_Etype       (New_Id, Get_Instance_Of (Etype (Gen_Id)));
            Set_Scope       (New_Id, Act_Scope);
            Set_Public_Status (New_Id);
            Set_Is_Directly_Visible (New_Id, False);
            Set_Next_Entity (New_Id, Empty);
            Set_Homonym (New_Id, Empty);
            Append_Entity (New_Id, Act_Scope);

            if Present (First_Entity (Gen_Id))
              and then not Is_Array_Type (Gen_Id)
            then
               Instantiate_Entities (Gen_Id, New_Id);
            end if;

         --  Already instantiated. Entity lives elsewhere

         else
            null;
         end if;

         Gen_Id := Next_Entity (Gen_Id);
      end loop;

      if Ekind (Act_Scope) = E_Package then
         Set_First_Private_Entity
           (Act_Scope, Get_Instance_Of (First_Private_Entity (Gen_Scope)));

         --  Make new local entities visible

         New_Id := First_Entity (Act_Scope);

         while Present (New_Id)
           and then New_Id /= First_Private_Entity (Act_Scope)
         loop
            if not Is_Internal (New_Id) then
               if No (Current_Entity (New_Id)) then
                  Set_Current_Entity (New_Id);
               else
                  --  Chain at end of homonym chain.

                  declare
                     Prev : Entity_Id := Current_Entity (New_Id);

                  begin
                     while Present (Homonym (Prev)) loop
                        Prev := Homonym (Prev);
                     end loop;
                     Set_Homonym (Prev, New_Id);
                  end;
               end if;
            end if;

            New_Id := Next_Entity (New_Id);
         end loop;
      end if;

      Pop_Scope;
   end Instantiate_Entities;

   ---------------------
   -- Complete_Entities --
   ---------------------

   procedure Complete_Entities (Gen_Scope : Entity_Id) is
      E   : Entity_Id;
      Act : Entity_Id;

   begin

      E := First_Entity (Gen_Scope);
      while Present (E) loop
         Act := Get_Instance_Of (E);

         if Act = E then null;
         elsif Is_Generic_Type (E) then null;

         else
         --  Some semantic attributes involve forward references, and
         --  are specific to the entity kind. They are established
         --  now, after entity and tree instantiation. Eventually, these
         --  should be set automatically by a fuller traversal of the tree.

            case Ekind (E) is
               when E_In_Parameter =>
                  Set_Default_Value (Act,
                     Get_Instance_Of (Default_Value (E)));

               when E_Discriminant =>
                  Set_Discriminant_Default_Value (Act,
                     Get_Instance_Of (Discriminant_Default_Value (E)));

               when Scalar_Kind  =>
                  Set_Scalar_Range (Act,
                     Get_Instance_Of (Scalar_Range (E)));

                  if No (Scalar_Range (Act)) then

                     --  Range of E is not instantiated: E is an implicit
                     --  subtype in an allocator.

                     Set_Scalar_Range (Act,
                        Instantiate_Tree (Scalar_Range (E)));
                  end if;

               when Array_Kind =>
                  Set_Component_Type (Act,
                     Get_Instance_Of (Component_Type (E)));
                  Set_First_Index (Act,
                     Get_Instance_Of (First_Index (E)));

                  if Ekind (E) =  E_Slice_Subtype then
                     Set_Slice_Range (Act,
                        Get_Instance_Of (Slice_Range (E)));
                     if No (Slice_Range (Act)) then
                        Set_Slice_Range (Act,
                           Instantiate_Tree (Slice_Range (E)));
                     end if;
                  end if;

               when Access_Kind =>
                  Set_Directly_Designated_Type (Act,
                     Get_Instance_Of (Directly_Designated_Type (E)));

               when E_Incomplete_Type =>
                  Set_Full_Declaration (Act,
                     Get_Instance_Of (Full_Declaration (E)));

               when others => null;
            end case;
         end if;

         if Present (First_Entity (E))
           and then not Is_Array_Type (E)
         then
            --  Entity is a scope. Complete its internal entities

            Complete_Entities (E);
         end if;

         E := Next_Entity (E);
      end loop;
   end Complete_Entities;

   ----------------------
   -- Instantiate_Tree --
   ----------------------

   function Instantiate_Tree (N : Node_Id) return Node_Id is
      Result : Node_Id;

   begin
      Result := Instantiate_Node (N, Empty);

      --  Place result in current declarative part.

      return Result;
   end Instantiate_Tree;

   ----------------------
   -- Instantiate_Node --
   ----------------------

   function Instantiate_Node (N : Node_Id; Parent_Id : Node_Id)
                                                       return Node_Id is

      function Instantiate_Descendant (D : Int) return Int;
      --  This procedure tests the given value of one of the Fields referenced
      --  by the current node to determine whether to copy it recursively.
      --  The field may hold a node id, a list id, or an elist id, or a plain
      --  value (Uint, Char) in which case it need not be copied.

      New_N : Node_Id;

      function Instantiate_Descendant (D : Int) return Int is

      begin
         if D in Node_Range then
            if D = Int (Empty) then
               return Int (D);

            elsif Nkind (Node_Id (D)) = N_Defining_Identifier then

               --  Point to the corresponding instance.
               --  The instantiation of entities does not set their
               --  parent pointer. This is done now that the rest of the
               --  tree is constructed.

               declare
                  Id1 : constant Entity_Id := Node_Id (D);
                  Id2 : constant Entity_Id := Get_Instance_Of (Id1);

               begin
                  if Present (Id2) then
                     if No (Parent (Id2)) and then Parent (Id1) = N then
                        Set_Parent (Id2, New_N);
                     end if;
                     return Int (Id2);
                  else
                     return Int (Id1);
                  end if;
               end;

            else
               return Int (Instantiate_Node (Node_Id (D), New_N));
            end if;

         elsif D in List_Range then
            if D = Int (No_List)
              or else Is_Empty_List (List_Id (D))
            then
               return Int (D);
            else
               return Int (Instantiate_List (List_Id (D), New_N));
            end if;

         elsif D in Elist_Range then

            if D = Int (No_Elist)
              or else Is_Empty_Elmt_List (Elist_Id (D))
            then
               return Int (D);
            else
               return Int (Instantiate_Elist (Elist_Id (D)));
            end if;

         else
            --  Field is not Id of copyable structure: return as is

            return D;
         end if;
      end Instantiate_Descendant;

   --  Start processing for Instantiate_Node

   begin
      if N = Empty then
         return N;
      end if;

      --  If node is already instantiated, return its instance.
      New_N := Get_Instance_Of (N);

      if Present (New_N) then
         return New_N;
      end if;

      if Debug_Flag_E then
         Write_Str (" instantiate node ");
         Write_Int (Int (N));
         Write_String (Node_Kind'Image (Nkind (N)));
         Write_Eol;
      end if;
      New_N := New_Copy (N);
      Set_Instance_Of (N, New_N);
      if not Is_List_Member (N) then
         Set_Parent (New_N, Parent_Id);
      end if;

      Set_Field1 (New_N, Instantiate_Descendant (Field1 (N)));
      Set_Field2 (New_N, Instantiate_Descendant (Field2 (N)));
      Set_Field3 (New_N, Instantiate_Descendant (Field3 (N)));
      Set_Field4 (New_N, Instantiate_Descendant (Field4 (N)));
      Set_Field5 (New_N, Instantiate_Descendant (Field5 (N)));

      if Has_Extension (N) then
         --  Currently true only for Defining_Identifier nodes,
         --  but this may change.

         Set_Field6  (New_N, Instantiate_Descendant (Field6  (N)));
         Set_Field7  (New_N, Instantiate_Descendant (Field7  (N)));
         Set_Field8  (New_N, Instantiate_Descendant (Field8  (N)));
         Set_Field9  (New_N, Instantiate_Descendant (Field9  (N)));
         Set_Field10 (New_N, Instantiate_Descendant (Field10 (N)));
         Set_Field11 (New_N, Instantiate_Descendant (Field11 (N)));
         Set_Field12 (New_N, Instantiate_Descendant (Field12 (N)));
      end if;

      if Is_Name (N) then
         declare
            Nam : Entity_Id := Entity (N);

         begin
            --  An implicit subtype may be created for an instance of a
            --  generic IN parameter. The type of any reference to the
            --  generic formal must carry this type, rather than the instance
            --  of the type of the formal, which may be unconstrained.

            if Present (Nam) and then Get_Instance_Of (Nam) /= Nam
               and then Ekind (Nam) = E_Generic_In_Parameter then
               Set_Etype (New_N, Etype (Get_Instance_Of (Nam)));
            end if;
         end;
      end if;

      return New_N;
   end Instantiate_Node;

   ----------------------
   -- Instantiate_List --
   ----------------------

   function Instantiate_List (L : List_Id; Parent_Id : Node_Id) return List_Id
   is
      N      : Node_Id;
      New_L  : List_Id;

   begin
      N := First (L);

      if N = Empty then
         return L;

      else
         New_L := New_List;
         Set_Parent (New_L, Parent_Id);
         N := First (L);

         while Present (N) loop
            Append (Instantiate_Node (N, Empty), New_L);
            N := Next (N);
         end loop;

         return New_L;
      end if;

   end Instantiate_List;

   -----------------
   -- Copy_Elist --
   -----------------

   function Instantiate_Elist (E : Elist_Id) return Elist_Id is
      M : Elmt_Id;
      L : Elist_Id := New_Elmt_List;

   begin
      M := First_Elmt (E);

      while M /= No_Elmt loop
         Append_Elmt (Instantiate_Node (Id_Of (M), Empty), L);
         M := Next_Elmt (M);
      end loop;

      return L;
   end Instantiate_Elist;

   --------------------------
   -- Initialize_Instances --
   --------------------------

   procedure Initialize_Instances is
   begin
      for I in Hash_Headers'range loop
         Hash_Headers (I) := No_Entry;
      end loop;

      Hash_Instance.Init;
   end Initialize_Instances;

   ---------------------
   -- Set_Instance_Of --
   ---------------------

   procedure Set_Instance_Of (Old_Node, New_Node : Node_Id) is
      Hash_Code : Nat := Int (Old_Node) mod Num_Hash_Headers;
      Index : Hash_Entry_Id := Hash_Headers (Hash_Code);
   begin
      Hash_Instance.Increment_Last;
      Hash_Instance.Table (Hash_Instance.Last).Next     := No_Entry;
      Hash_Instance.Table (Hash_Instance.Last).New_Node := New_Node;
      Hash_Instance.Table (Hash_Instance.Last).Old_Node := Old_Node;

      if Index = No_Entry then
         Hash_Headers (Hash_Code) := Hash_Instance.Last;
      else
         while Hash_Instance.Table (Index).Next /= No_Entry loop
               Index := Hash_Instance.Table (Index).Next;
         end loop;
         Hash_Instance.Table (Index).Next := Hash_Instance.Last;
      end if;

      if Debug_Flag_E then
         Write_String ("instantiate entity "); Write_Int (Int (Old_Node));
         Write_String ("   into  "); Write_Int (Int (New_Node));
         Write_Eol;
      end if;
   end Set_Instance_Of;

   ---------------------
   -- Get_Instance_Of --
   ---------------------

   function Get_Instance_Of (Old_Node : Node_Id) return Node_Id is
      Hash_Code : Nat := Int (Old_Node) mod Num_Hash_Headers;
      Index : Hash_Entry_Id := Hash_Headers (Hash_Code);

   begin
      if Old_Node = Empty then
         return Empty;
      else
         loop
            if Index = No_Entry then
               if Nkind (Old_Node) = N_Defining_Identifier then
                  return Old_Node;
               else
                  return Empty;
               end if;
            end if;

            if Old_Node = Hash_Instance.Table (Index).Old_Node then
               return Hash_Instance.Table (Index).New_Node;
            else
               Index := Hash_Instance.Table (Index).Next;
            end if;
         end loop;
      end if;

   end Get_Instance_Of;

end Sem_Ch12;
