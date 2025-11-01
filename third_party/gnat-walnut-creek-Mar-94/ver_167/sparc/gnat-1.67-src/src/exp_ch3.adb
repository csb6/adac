------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 3                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.66 $                             --
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
with Namet;    use Namet;
with Nmake;    use Nmake;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Snames;   use Snames;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Ch3 is

   -------------------------
   --  Locals Subprograms --
   -------------------------


   function Predefined_Primitive_Specs (Tag_Typ : Entity_Id) return List_Id;
   --  Create a list with the specs of the predefined primitive operations.
   --  This list contains _size and _equality which are the dispatching 
   --  versions of 'Size and "=". These specs must be inserted just after
   --  the tagged type definition. The rest of the code relies on the fact 
   --  that _Size is first in the dispatch table and _equality is 2nd.
   --  Here is the generated code :
   --
   --    function _Size (X : Tag_Typ) return Long_Long_Integer;
   --    function "=" (X : Tag_Typ; Y : Tag_Typ) return Boolean;

   function Predefined_Primitive_Bodies (Tag_Typ : Entity_Id) return List_Id;
   --  Create the bodies of the predefined primitives which are to be inserted 
   --  just after the expansion of the Dispatch Table (as soon as the type 
   --  is frozen). Here is the generated code :
   --
   --    function _Size (X : Tag_Typ) return Long_Long_Integer is
   --    begin
   --      return X'Size;
   --    end _size;
   --
   --    function "=" (X : Tag_Typ; Y : Tag_Typ) return Boolean is
   --    begin
   --      return <Expand_Components_Equality (X, Y)>;
   --    end "=";


   function Build_Discriminant_Formals
     (Rec_Id : Entity_Id; Use_Dl : Boolean) return List_Id; 
   --  This function uses the discriminants of a type to build a list
   --  of formal parameters, used in the following function. If the flag
   --  -Use_Dl- is set, the list is built using the already defined
   --  discriminals of the type. Otherwise new identifiers are created,
   --  with the source name of the discriminants.

   function Init_Formals (Typ : Entity_Id) return List_Id;
   --  This function builds the list of formals for an initialization routine.
   --  The first formal is always _Init with the given type. For task value
   --  record types and types containing tasks, two additional formals are
   --  added:
   --
   --    _Master : Master_Id 
   --    _Chain  : in out Activation_Chain
   --
   --  The caller must append additional entries for discriminants if required.

   ----------------------------
   --  Build_Array_Init_Proc --
   ----------------------------

   function Build_Array_Init_Proc (A_Type : Entity_Id) return Node_Id is
      Comp_Type  : constant Entity_Id  := Component_Type (A_Type);
      Loc        : constant Source_Ptr := Sloc (A_Type);
      Index_List : List_Id;
      Proc_Id    : Entity_Id;
      Proc_Body  : Node_Id;

      function Init_Component return Node_Id;
      --  Create one statement to initialize one array component, designated
      --  by a full set of indices.

      function Init_One_Dimension (N : Int) return Node_Id;
      --  Create loop to initialize one dimension of the array. The single
      --  statement in the body of the loop initializes the inner dimensions if
      --  any,or else a single component.

      --------------------
      -- Init_Component --
      --------------------

      function Init_Component return Node_Id is
         Comp : Node_Id;

      begin
         Comp :=
           Make_Indexed_Component (Loc,
             Prefix => Make_Identifier (Loc, Name_uInit),
             Expressions => Index_List);

         if Is_Access_Type (Comp_Type) then
            return
              Make_Assignment_Statement (Loc,
                Name => Comp,
                Expression => Make_Null (Loc));
         else
            return Build_Initialization_Call (Loc, Comp, Comp_Type);
         end if;
      end Init_Component;

      ------------------------
      -- Init_One_Dimension --
      ------------------------

      function Init_One_Dimension (N : Int) return Node_Id is
         Index : Entity_Id;

      begin
         if N > Number_Dimensions (A_Type) then
            return Init_Component;

         else
            Index :=
              Make_Defining_Identifier (Loc, New_Internal_Name ("index"));

            Append (New_Reference_To (Index, Loc), Index_List);

            return
              Make_Loop_Statement (Loc,
                Identifier => Empty,
                Iteration_Scheme =>
                  Make_Iteration_Scheme (Loc,
                    Loop_Parameter_Specification =>
                      Make_Loop_Parameter_Specification (Loc,
                        Defining_Identifier => Index,
                        Discrete_Subtype_Definition =>
                          Make_Attribute_Reference (Loc,
                            Prefix => Make_Identifier (Loc, Name_uInit),
                            Identifier => Make_Identifier (Loc, Name_Range),
                            Expression =>
                              Make_Integer_Literal (Loc, UI_From_Int (N))))),
                Statements => New_List_1 (Init_One_Dimension (N + 1)));
         end if;
      end Init_One_Dimension;

   ------------------------------------------
   -- Processing for Build_Array_Init_Proc --
   ------------------------------------------

   begin  
      Index_List := New_List;

      if Present (Init_Proc (Comp_Type))
        or else Is_Access_Type (Comp_Type)
        or else Has_Tasks (Comp_Type)
      then
         Proc_Id :=
           Make_Defining_Identifier (Loc,
             Chars => New_External_Name (Chars (A_Type), "init"));

         Proc_Body :=
           Make_Subprogram_Body (Loc,
             Specification =>
               Make_Procedure_Specification (Loc,
                 Defining_Unit_Name => Proc_Id,
                 Parameter_Specifications => Init_Formals (A_Type)),
             Declarations => New_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List_1 (Init_One_Dimension (1))));

         Analyze (Proc_Body);
         Set_Init_Proc (A_Type, Proc_Id);

         Set_Is_Public      (Proc_Id, Is_Public (A_Type));
         Set_Is_Inlined     (Proc_Id, True);
         Set_Is_Internal    (Proc_Id, True);
         Set_Has_Completion (Proc_Id, True);
         return Proc_Body;

      --  Return Error if no initialization procedure generated (not really an
      --  error, just a convenient trick so that the attempt to insert the
      --  result into the declaration list is simply ignored).

      else
         return Error;
      end if;

   end Build_Array_Init_Proc;

   ---------------------------------
   --  Build_Discriminant_Formals --
   ---------------------------------

   function Build_Discriminant_Formals (Rec_Id : Entity_Id;
                                       Use_Dl : Boolean) return List_Id is
      D               : Entity_Id;
      Formal          : Entity_Id;
      Loc             : constant Source_Ptr := Sloc (Rec_Id);
      Param_Spec_Node : Node_Id;
      Parameter_List  : List_Id := New_List;

   begin
      if Has_Discriminants (Rec_Id) then
         D := First_Discriminant (Rec_Id);

         while Present (D) loop
            if Use_Dl then
               Formal := Discriminal (D);
            else
               Formal := Make_Defining_Identifier (Loc,  Chars (D));
            end if;

            Param_Spec_Node :=
              Make_Parameter_Specification (Loc,
                  Defining_Identifier => Formal,
                Parameter_Type =>
                  New_Reference_To (Etype (D), Loc));
            Append (Param_Spec_Node, Parameter_List);
            D := Next_Discriminant (D);
         end loop;
      end if;

      return Parameter_List;
   end Build_Discriminant_Formals;

   --------------------------------
   -- Build_Discr_Checking_Funcs --
   --------------------------------

   procedure Build_Discr_Checking_Funcs (N : Node_Id) is
      Rec_Id            : Entity_Id;
      Loc               : Source_Ptr;
      Enclosing_Func_Id : Entity_Id;
      Insertion_Node    : Node_Id := N;
      Sequence          : Nat := 1;
      Type_Def          : Node_Id;
      V                 : Node_Id;

      -----------------------
      -- Local Subprograms --
      -----------------------

      function Build_Case_Statement (Case_Id : Entity_Id;
                                     Variant : Node_Id) return Node_Id;
      --  TBSL need documentation for this spec

      function  Build_Function (Case_Id : Entity_Id;
                                Variant : Node_Id) return Entity_Id;
      --  Build the discriminant checking function for a given variant

      procedure Build_Functions (Variant_Part_Node : Node_Id);
      --  Builds the discriminant checking function for each variant of the
      --  given variant part of the record type.

      procedure Build_Functions (Variant_Part_Node : Node_Id) is
         Component_List_Node : Node_Id;
         Decl                : Entity_Id;
         Discr_Name          : Entity_Id;
         Func_Id             : Entity_Id;
         Variant             : Node_Id;
         Saved_Enclosing_Func_Id : Entity_Id;

      begin
         --  Build the discriminant checking function for each variant, label
         --  all components of that variant with the function's name.

         Discr_Name := Entity (Name (Variant_Part_Node));
         Variant := First (Variants (Variant_Part_Node));

         while Present (Variant) loop
            Func_Id := Build_Function (Discr_Name, Variant);
            Component_List_Node := Component_List (Variant);

            if not Null_Present (Component_List_Node) then
               Decl := First (Component_Declarations (Component_List_Node));

               while Present (Decl) loop
                  Set_Discriminant_Checking_Func
                    (Defining_Identifier (Decl), Func_Id);
                  Decl := Next (Decl);
               end loop;

               if Present (Variant_Part (Component_List_Node)) then
                  Saved_Enclosing_Func_Id := Enclosing_Func_Id;
                  Enclosing_Func_Id := Func_Id;
                  Build_Functions (Variant_Part (Component_List_Node));
                  Enclosing_Func_Id := Saved_Enclosing_Func_Id;
               end if;
            end if;

            Variant := Next (Variant);
         end loop;
      end Build_Functions;

      function Build_Function (Case_Id : Entity_Id;
                               Variant : Node_Id) return Entity_Id is
         Body_Node           : Node_Id;
         Buf                 : Str (1 .. 6);
         Func_Id             : Entity_Id;
         Parameter_List      : List_Id;
         Spec_Node           : Node_Id;
         V                   : Nat := Sequence;

      begin
         Body_Node := New_Node (N_Subprogram_Body, Loc);
         Buf (1) := 'd';

         --  Add decimal representation of V to characters in name buffer,
         --  updating Name_Len to reflect the stored digits.

         for I in reverse Pos range 2 .. 6 loop
            Buf (I) := Char'Val (Char'Pos ('0') + V rem 10);
            V := V / 10;
         end loop;

         Sequence := Sequence + 1;

         Func_Id :=
           Make_Defining_Identifier (Loc,
             Chars => New_External_Name (Chars (Rec_Id), Buf));

         Spec_Node := New_Node (N_Function_Specification, Loc);
         Set_Defining_Unit_Name (Spec_Node, Func_Id);

         Parameter_List := Build_Discriminant_Formals (Rec_Id, False);

         Set_Parameter_Specifications (Spec_Node, Parameter_List);
         Set_Subtype_Mark (Spec_Node,
                           New_Reference_To (Standard_Boolean,  Loc));
         Set_Specification (Body_Node, Spec_Node);
         Set_Declarations (Body_Node, New_List);

         Set_Handled_Statement_Sequence (Body_Node,
           Make_Handled_Sequence_Of_Statements (Loc,
             Statements => New_List_1 (
               Build_Case_Statement (Case_Id, Variant))));

         Analyze (Body_Node);
         Set_Is_Inlined (Func_Id);
         Set_Is_Pure (Func_Id);

         --  Make the discriminant checking function have the same public
         --  status as that of the record type which it checkes.

         Set_Is_Public (Func_Id, Is_Public (Rec_Id));
         Set_Is_Internal (Func_Id);
         Insert_After (Insertion_Node, Body_Node);
         Insertion_Node := Body_Node;
         return Func_Id;
      end Build_Function;

      function Build_Case_Statement (Case_Id : Entity_Id;
                                     Variant : Node_Id) return Node_Id
      is
         Actuals_List   : List_Id;
         Alt_List       : List_Id := New_List;
         Case_Node      : Node_Id;
         Case_Alt_Node  : Node_Id;
         Choice         : Node_Id;
         Choice_List    : List_Id;
         D              : Entity_Id;
         Return_Node    : Node_Id;

      begin
         --  Build a case statement containing only two alternatives. The
         --  first alternative corresponds exactly to the discrete choices
         --  given on the variant with contains the components that we are
         --  generating the checks for. If the discriminant is one of these
         --  return False. The other alternative consists of the choice
         --  "Others" and will return True indicating the discriminant did
         --  not match.

         Case_Node := New_Node (N_Case_Statement, Loc);
         --  Replace the discriminant which controls the variant, with the
         --  name of the formal of the checking function.
         Set_Expression (Case_Node, 
              Make_Identifier (Loc, Chars (Case_Id)));

         Choice := First (Discrete_Choices (Variant));

         if Nkind (Choice) = N_Others_Choice then
            Choice_List := New_List_Copy (Others_Discrete_Choices (Choice));
         else
            Choice_List := New_List_Copy (Discrete_Choices (Variant));
         end if;

         Case_Alt_Node := New_Node (N_Case_Statement_Alternative, Loc);
         Set_Discrete_Choices (Case_Alt_Node, Choice_List);

         --  In case this is a nested variant, we need to return the result
         --  of the discriminant checking function for the immediately
         --  enclosing variant.

         if Present (Enclosing_Func_Id) then
            Actuals_List := New_List;

            D := First_Discriminant (Rec_Id);
            while Present (D) loop
               Append (Make_Identifier (Loc, Chars (D)), Actuals_List);
               D := Next_Discriminant (D);
            end loop;

            Return_Node :=
              Make_Return_Statement (Loc,
                Expression =>
                  Make_Function_Call (Loc,
                    Name =>
                      New_Reference_To (Enclosing_Func_Id,  Loc),
                    Parameter_Associations =>
                      Actuals_List));

         else
            Return_Node :=
              Make_Return_Statement (Loc,
                Expression =>
                  New_Reference_To (Standard_False, Loc));
         end if;

         Set_Statements (Case_Alt_Node, New_List_1 (Return_Node));
         Append (Case_Alt_Node, Alt_List);

         Case_Alt_Node := New_Node (N_Case_Statement_Alternative, Loc);
         Choice_List := New_List_1 (New_Node (N_Others_Choice, Loc));
         Set_Discrete_Choices (Case_Alt_Node, Choice_List);

         Return_Node :=
           Make_Return_Statement (Loc,
             Expression =>
               New_Reference_To (Standard_True, Loc));

         Set_Statements (Case_Alt_Node, New_List_1 (Return_Node));
         Append (Case_Alt_Node, Alt_List);

         Set_Alternatives (Case_Node, Alt_List);
         return Case_Node;
      end Build_Case_Statement;

   --  Start of processing for Build_Discr_Checking_Funcs

   begin
      Type_Def := Type_Definition (N);

      if Nkind (Type_Def) = N_Record_Definition then
         if No (Component_List (Type_Def)) then   -- null record.
            return;
         else
            V := Variant_Part (Component_List (Type_Def));
         end if;

      elsif Nkind (Type_Def) = N_Derived_Type_Definition then
         if No (Component_List (Record_Extension_Part (Type_Def))) then
            return;
         else
            V := Variant_Part
                   (Component_List (Record_Extension_Part (Type_Def)));
         end if;

      else
         Compiler_Abort;
      end if;

      if Present (V) then
         Loc := Sloc (N);
         Enclosing_Func_Id := Empty;
         Rec_Id := Defining_Identifier (N);
         Build_Functions (V);
      end if;
   end Build_Discr_Checking_Funcs;

   ----------------------------
   -- Build_Record_Init_Proc --
   ----------------------------

   function Build_Record_Init_Proc
     (N : Node_Id; Pe : Entity_Id) return Node_Id
   is
      Loc            : constant Source_Ptr := Sloc (N);
      Proc_Id        : Entity_Id;
      Rec_Type       : Entity_Id;

      --------------------------------------------------
      -- Local Subprograms for Build_Record_Init_Proc --
      --------------------------------------------------

      function Build_Init_Actuals (Id : Entity_Id) return List_Id;
      --  Build the list of actual parameters for the call to the
      --  initialization procedure for a component of a record which is
      --  itself a record type or subtype.

      function Build_Assignment (Id : Entity_Id; N : Node_Id) return Node_Id;
      --  Build a assignment statement node which assigns to record component
      --  its default expression if defined.


      procedure Build_Discriminant_Assignments (Statement_List : List_Id);
      --  If the record has discriminants, adds assignment statements to
      --  statement list to initialize the discriminant values from the
      --  arguments of the initialization procedure.

      function Build_Init_Call (Id : Entity_Id) return Node_Id;
      --  Build the tree representing a call to a record initialization
      --  procedure along with the appropriate actual parameters.

      function Build_Init_Statements (Comp_List : Node_Id) return List_Id;
      --  Build a list representing a sequence of statements which initialize
      --  components of the given component list. This may involve building
      --  case statements for the variant parts.

      function Build_Init_Procedure return Node_Id;
      --  Build the tree corresponding to the procedure specification and body
      --  of the initialization procedure (by calling all the preceding
      --  auxillary routines).

      function Requires_Init_Proc (Rec_Id : Entity_Id) return Boolean;
      --  Determines whether a record initialization procedure needs to be
      --  generated for the given record type.

      ----------------------
      -- Build_Assignment --
      ----------------------

      function Build_Assignment (Id : Entity_Id; N : Node_Id) return Node_Id is
         Assign_Node   : Node_Id;
         Selector_Node : Node_Id;

      begin
         Selector_Node := New_Node (N_Selected_Component, Loc);
         Set_Prefix (Selector_Node, Make_Identifier (Loc, Name_uInit));
         Set_Selector_Name (Selector_Node, New_Occurrence_Of (Id, Loc));
         Assign_Node := New_Node (N_Assignment_Statement, Loc);
         Set_Name (Assign_Node, Selector_Node);
         Set_Expression (Assign_Node, N);
         return Assign_Node;
      end Build_Assignment;



      ------------------------
      -- Build_Init_Actuals --
      ------------------------

      --  References to a discriminant inside the record type declaration
      --  can appear either in the subtype_indication to constrain a
      --  record or an array, or as part of a larger expression given for
      --  the initial value of a component. In both of these cases N appears
      --  in the record initialization procedure and needs to be replaced by
      --  the formal parameter of the initialization procedure which
      --  corresponds to that discriminant.

      --  In the example below, references to D1 and D2 in proc_1 are
      --  replaced by references to In_3 and In_4 respectively.

      --  A similar replacement is done for calls to any record
      --  initialization procedure for any components that are themselves
      --  of a record type.

      --  type R (D1, D2 : Integer) is record
      --     X : Integer := F * D1;
      --     Y : Integer := F * D2;
      --  end record;

      --  procedure proc_1 (Out_2 : out R; D1 : Integer; D2 : Integer) is
      --  begin
      --     Out_2.D1 := D1;
      --     Out_2.D2 := D2;
      --     Out_2.X := F * D1;
      --     Out_2.Y := F * D2;
      --  end;

      function Build_Init_Actuals (Id : Entity_Id) return List_Id is
         Elmt           : Elmt_Id;
         Exp_Node       : Node_Id;
         Parameter_List : List_Id;
         Selector_Node  : Node_Id;
         Typ            : constant Entity_Id := Etype (Id);

      begin
         --  First actual is _Init, the component to be initialized

         Parameter_List := New_List;
         Selector_Node := New_Node (N_Selected_Component, Loc);
         Set_Prefix (Selector_Node, Make_Identifier (Loc, Name_uInit));
         Set_Selector_Name (Selector_Node, New_Occurrence_Of (Id, Loc));
         Append (Convert_Task (Selector_Node, Typ), Parameter_List);

         --  For the task case, pass _Master as the value of the _Master
         --  parameter, and _Chain as the value of the _Chain parameter.
         --  In other words we are just passing down the parameters that
         --  were passed to the record initialization procedure itself.

         if Has_Tasks (Typ) then
            Append_To (Parameter_List, Make_Identifier (Loc, Name_uMaster));
            Append_To (Parameter_List, Make_Identifier (Loc, Name_uChain));
         end if;

         --  Add discriminant values if discriminants are present

         if Is_Record_Type (Typ)
           and then Has_Discriminants (Typ)
         then
            Elmt := First_Elmt (Discriminant_Constraint (Etype (Id)));

            while Elmt /= No_Elmt loop
               Exp_Node := Id_Of (Elmt);

               --  Replace any possible references to the discriminant in the
               --  call to the record initialization procedure with references
               --  to the appropriate formal parameter.

               if Nkind (Exp_Node) = N_Identifier
                  and then Ekind (Entity (Exp_Node)) = E_Discriminant
               then
                  Append_To (Parameter_List, New_Reference_To (
                              Discriminal (Entity (Exp_Node)), Loc));
               else
                  Append_To (Parameter_List, New_Copy (Exp_Node));
               end if;

               Elmt := Next_Elmt (Elmt);
            end loop;
         end if;

         return Parameter_List;
      end Build_Init_Actuals;

      ---------------------
      -- Build_Init_Call --
      ---------------------

      function Build_Init_Call (Id : Entity_Id) return Node_Id is
         Call_Node : Node_Id;

      begin
         Call_Node :=
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of (Base_Init_Proc (Etype (Id)), Loc),
             Parameter_Associations => Build_Init_Actuals (Id));

         return Call_Node;
      end Build_Init_Call;

      ---------------------------
      -- Build_Init_Statements --
      ---------------------------

      function Build_Init_Statements (Comp_List : Node_Id) return List_Id is
         Alt_List       : List_Id;
         Case_Node      : Node_Id;
         Case_Alt_Node  : Node_Id;
         Choice         : Node_Id;
         Choice_List    : List_Id;
         Decl           : Node_Id;
         Exp_Node       : Node_Id;
         Id             : Node_Id;
         Statement_List : List_Id;
         Variant        : Node_Id;

      begin
         if Null_Present (Comp_List) then
            return New_List_1 (Make_Null_Statement (Loc));
         end if;

         Statement_List := New_List;
         Decl := First (Component_Declarations (Comp_List));

         while Present (Decl) loop
            Id := Defining_Identifier (Decl);

            if Present (Expression (Decl)) then
               Exp_Node := New_Copy (Expression (Decl));
               Append (Build_Assignment (Id, Exp_Node), Statement_List);

            elsif Is_Access_Type (Etype (Id)) then
               Append_To (Statement_List,
                 Build_Assignment (Id, Make_Null (Loc)));

            elsif Is_Record_Type (Etype (Id))
              and then Present (Base_Init_Proc (Etype (Id)))
            then
               Append (Build_Init_Call (Id), Statement_List);

            elsif Is_Array_Type (Etype (Id))
              and then Present (Base_Init_Proc (Etype (Id)))
            then
               Append (Build_Init_Call (Id), Statement_List);

            elsif Has_Tasks (Etype (Id)) then
               Append (Build_Init_Call (Id), Statement_List);

            --  Case of no initialization procedure present for the a
            --  record type having no default expressions or discriminants.

            else
               null;
            end if;

            Decl := Next (Decl);
         end loop;

         --  Process the variant part

         if Present (Variant_Part (Comp_List)) then
            Case_Node := New_Node (N_Case_Statement, Loc);
            Alt_List := New_List;

            Variant := First (Variants (Variant_Part (Comp_List)));

            while Present (Variant) loop
               Case_Alt_Node := New_Node (N_Case_Statement_Alternative, Loc);
               Choice_List := New_List;

               Choice := First (Discrete_Choices (Variant));

               while Present (Choice) loop
                  Append (New_Copy (Choice), Choice_List);
                  Choice := Next (Choice);
               end loop;

               Set_Discrete_Choices (Case_Alt_Node, Choice_List);
               Append (Case_Alt_Node, Alt_List);
               Set_Statements
                 (Case_Alt_Node,
                  Build_Init_Statements (Component_List (Variant)));
               Variant := Next (Variant);
            end loop;

            Set_Expression (
              Case_Node,
                 New_Reference_To (Discriminal (
                           Entity (Name (Variant_Part (Comp_List)))), Loc));

            --  The expression of the case statement which is a reference to
            --  one of the discriminants is replaced by the appropriate formal
            --  parameter of the initialization procedure.

            Set_Alternatives (Case_Node, Alt_List);
            Append (Case_Node, Statement_List);
         end if;

         --  For a task record type, add the task create call

         if Is_Task_Record_Type (Rec_Type) then
            Append (Make_Task_Create_Call (Rec_Type), Statement_List);
         end if;

         --  If no initializations when generated for component declarations
         --  corresponding to this Statement_List, append a null statement
         --  to the Statement_List to make it a valid Ada tree.

         if Is_Empty_List (Statement_List) then
            Append (New_Node (N_Null_Statement, Loc), Statement_List);
         end if;

         return Statement_List;
      end Build_Init_Statements;

      --------------------------
      -- Build_Init_Procedure --
      --------------------------

      function Build_Init_Procedure return Node_Id is
         Body_Node             : Node_Id;
         Handled_Stmt_Node     : Node_Id;
         Parameters            : List_Id;
         Proc_Spec_Node        : Node_Id;
         Statement_List        : List_Id;
         Record_Extension_Node : Node_Id;

      begin
         Statement_List := New_List;
         Body_Node := New_Node (N_Subprogram_Body, Loc);

         Proc_Id :=
           Make_Defining_Identifier (Loc,
             Chars => New_External_Name (Chars (Rec_Type), "Init"));

         Proc_Spec_Node := New_Node (N_Procedure_Specification, Loc);
         Set_Defining_Unit_Name (Proc_Spec_Node, Proc_Id);

         Set_Init_Proc (Rec_Type, Proc_Id);
         Build_Discriminant_Assignments (Statement_List);

         Parameters := Init_Formals (Rec_Type);
         Append_List_To (Parameters, 
               Build_Discriminant_Formals (Rec_Type, True));
         Set_Parameter_Specifications (Proc_Spec_Node, Parameters);

         Set_Specification (Body_Node, Proc_Spec_Node);
         Set_Declarations (Body_Node, New_List);

         if Nkind (Type_Definition (N)) = N_Record_Definition then
            if not Null_Present (Type_Definition (N)) then
               Append_List_To (Statement_List,
                 Build_Init_Statements (
                   Component_List (Type_Definition (N))));
            end if;

         else
            --  N is a Derived_Type_Definition with a possible non-empty
            --  extension. The initialization of a type extension consists
            --  in the initialization of the components in the extension.

            Record_Extension_Node :=
              Record_Extension_Part (Type_Definition (N));

            if not Null_Present (Record_Extension_Node) then
               Append_List_To (Statement_List,
                 Build_Init_Statements
                   (Component_List (Record_Extension_Node)));
            end if;
         end if;

         --  Add here the assignment to instantiate the Tag

         --  This instantiation is done at the end because the instantiation
         --  of the __Parent field calls the Record_Init_Proc for the parent
         --  Parent which instaciate the Tag with a wrong value.
         --  The assignement corresponds to the code:

         --     Out__XXX._Tag := System.Tag (_Dispatch_Table_Ptr_XXX);

         --  where _Dispatch_Table_Ptr__XXX is an access to the correct
         --  dispatch table (see Expand_Dispatch_Table for details).

         if Is_Tagged_Type (Rec_Type) then

            Append_To (Statement_List,
              Make_Assignment_Statement (Loc,
                Name =>
                  Make_Selected_Component (Loc,
                    Prefix => Make_Identifier (Loc, Name_uInit),
                    Selector_Name =>
                      New_Reference_To (Tag_Component (Rec_Type), Loc)),
                Expression =>
                  Make_Unchecked_Type_Conversion (Loc,
                  Subtype_Mark => New_Reference_To (RTE (RE_Tag), Loc),
                  Expression =>
                    New_Reference_To (Access_Disp_Table (Rec_Type), Loc))));
         end if;

         Handled_Stmt_Node := New_Node (N_Handled_Sequence_Of_Statements, Loc);
         Set_Statements (Handled_Stmt_Node, Statement_List);
         Set_Exception_Handlers (Handled_Stmt_Node, No_List);
         Set_Handled_Statement_Sequence (Body_Node, Handled_Stmt_Node);
         return Body_Node;

      end Build_Init_Procedure;

      ------------------------------------
      -- Build_Discriminant_Assignments --
      ------------------------------------

      procedure Build_Discriminant_Assignments (Statement_List : List_Id) is
         Assign_Node : Node_Id;
         D : Entity_Id;

      begin
         if Has_Discriminants (Rec_Type) then
            D := First_Discriminant (Rec_Type);

            while Present (D) loop
               Assign_Node :=
                 Build_Assignment (D, 
                         New_Reference_To (Discriminal (D), Loc));
               Set_Assignment_OK (Assign_Node, True);
               Append (Assign_Node, Statement_List);
               D := Next_Discriminant (D);
            end loop;
         end if;
      end Build_Discriminant_Assignments;

      ------------------------
      -- Requires_Init_Proc --
      ------------------------

      function Requires_Init_Proc (Rec_Id : Entity_Id) return Boolean is
         Comp_Decl : Node_Id;
         Id        : Entity_Id;

      begin
         --  An initialization procedure needs to be generated only if at
         --  least one of the following applies:

         --  1. Discriminants are present, since they need to be initialized
         --     with the appropriate discriminant constraint expressions.

         --  2. The type is a tagged type, since the implicit Tag component
         --     needs to be initialized with a pointer to the dispatch table.

         --  3. The type contains tasks

         --  4. One or more components has an initial value

         --  5. One or more components is for a type which itself requires
         --     an initialization procedure.

         --  6. One or more components is an access type (which needs to be
         --     initialized to null).

         --  7. The type is the record type built for a task type (since at
         --     the very least, Create_Task must be called)

         if Has_Discriminants (Rec_Id)
           or else Is_Tagged_Type (Rec_Id)
           or else Is_Task_Record_Type (Rec_Id)
           or else Has_Tasks (Rec_Id)
         then
            return True;
         end if;

         Id := First_Component (Rec_Id);

         while Present (Id) loop
            Comp_Decl := Parent (Id);

            if Present (Expression (Comp_Decl))
              or else Present (Base_Init_Proc (Etype (Id)))
              or else Is_Access_Type (Etype (Id))
            then
               return True;
            end if;

            Id := Next_Component (Id);
         end loop;

         return False;
      end Requires_Init_Proc;

   -------------------------------------------
   -- Processing for Build_Record_Init_Proc --
   -------------------------------------------

   begin
      Rec_Type := Defining_Identifier (N);

      --  Case of declaration for a record type,  but this may be the
      --  This may be full declaration of a private type,  in which case
      --  the visible entity is a record, and the private entity has been
      --  exchanged with it in the private part of the current package.
      --  The initialization procedure is built for the record type, which
      --  is retrievable from the private entity.

      if Ekind (Rec_Type) in Incomplete_Kind then
         Rec_Type := Full_Declaration (Rec_Type);
      end if;

      --  Derived types that have no type extension can use the initialization
      --  procedure of their parent and do not need a procedure of their own.
      --  In that case set the Init_Proc attribute to reference the name of the
      --  parent's initialization procedure. Otherwise generate a new procedure

      if Nkind (Type_Definition (N)) = N_Derived_Type_Definition and
         not Is_Tagged_Type (Rec_Type)
      then
         Set_Init_Proc (Rec_Type, Base_Init_Proc (Root_Type (Rec_Type)));
         return Error;

      --  Otherwise if we need an initialization procedure, then build one,
      --  mark it as public and inlinable and as having a completion.

      elsif Requires_Init_Proc (Rec_Type) then
         declare
            Rec_Init : constant Node_Id := Build_Init_Procedure;

         begin
            --  Another freezing kludge, make sure the task body procedure
            --  spec gets analyzed before the record task type, so that the
            --  reference to the body in the latter works correctly.

            if Is_Task_Record_Type (Rec_Type) then
               Analyze (Next (N));
            end if;

            Analyze (Rec_Init);
            Set_Is_Public      (Proc_Id, Is_Public (Pe));
            Set_Is_Inlined     (Proc_Id, True);
            Set_Is_Internal    (Proc_Id, True);
            Set_Has_Completion (Proc_Id, True);

            --  Return resulting record initialization procedure body. Note
            --  that we don't call Analyze, since the body will be inserted
            --  into the declaration list past the current point, so we will
            --  encounter it and analyze it in the normal course of processing
            --  declarations sequentially.

            return Rec_Init;
         end;

      --  Here if no record initialization procedure is required. We return
      --  Error (although it's not really an error, just a trick to make it
      --  easy for the caller to ignore this case, since an attempt to stick
      --  Error into the declaration list will be ignored).

      else
         return Error;
      end if;
   end Build_Record_Init_Proc;

   -----------------
   -- Freeze_Type --
   -----------------

   --  Full type declarations are expanded at the point at which the type
   --  is frozen. The formal N is the Freeze_Node for the type, and the
   --  various initialization procedures generated by the expansion are
   --  inserted at the point of freezing, i.e. before or after N.

   procedure Freeze_Type (N : Node_Id) is
      Def_Id    : constant Entity_Id := Entity (N);
      Type_Decl : Node_Id := Parent (Def_Id);
      Type_Def  : Node_Id;
      Parent_E  : Entity_Id;

   begin

      ---------------------------------------------------
      -- Freeze Processing for Record Type Declaration --
      ---------------------------------------------------

      if Ekind (Def_Id) = E_Record_Type 
        and then Nkind (Type_Decl) /= N_Implicit_Type
      then
         if Nkind (Type_Decl) = N_Private_Type_Declaration then

            --  Scan the declarative part to find the entity that points
            --  to the current full type. The full declaration for the type
            --  is found there.

            declare
               E : Entity_Id := Next_Entity (Def_Id);

            begin
               while Present (E) loop
                  if Ekind (E) in Incomplete_Kind
                    and then Full_Declaration (E) = Def_Id then
                     Type_Decl := Parent (E);
                     exit;
                  end if;
                  E := Next_Entity (E);
               end loop;
            end;
         end if;

         Type_Def := Type_Definition (Type_Decl);

         if Is_Tagged_Type (Def_Id) then
            Insert_List_Before (N, Expand_Dispatch_Table (Def_Id));
         end if;

         --  Before building the record initialization procedure, if we
         --  are dealing with a task record value type, then we must go
         --  through the discriminants, exchanging discriminals between
         --  the task type and the task record value type. See the section
         --  "Handling of Discriminants" in the Einfo spec for details.

         if Is_Task_Record_Type (Def_Id)
           and then Has_Discriminants (Def_Id)
         then
            declare
               Ttyp : constant Entity_Id := Corresponding_Task_Type (Def_Id);

               Task_Discr : Entity_Id;
               Rec_Discr  : Entity_Id;
               Temp       : Entity_Id;

            begin
               Task_Discr := First_Discriminant (Ttyp);
               Rec_Discr  := First_Discriminant (Def_Id);

               while Present (Task_Discr) loop
                  Temp := Discriminal (Task_Discr);
                  Set_Discriminal (Task_Discr, Discriminal (Rec_Discr));
                  Set_Discriminal (Rec_Discr,  Temp);

                  Task_Discr := Next_Discriminant (Task_Discr);
                  Rec_Discr  := Next_Discriminant (Rec_Discr);
               end loop;
            end;
         end if;

         Insert_After (N, Build_Record_Init_Proc (Type_Decl, Def_Id));
         Build_Discr_Checking_Funcs (Type_Decl);

      --------------------------------------------------
      -- Freeze Processing for Array Type Declaration --
      --------------------------------------------------

      --  Build initialization procedure if one is required

      elsif Is_Array_Type (Def_Id) then
         if (No (Init_Proc (Base_Type (Def_Id)))) then
            Insert_After (N, Build_Array_Init_Proc (Base_Type (Def_Id)));
         end if;

      end if;

   end Freeze_Type;

   ------------------------------------
   -- Expand_N_Full_Type_Declaration --
   ------------------------------------

   procedure Expand_N_Full_Type_Declaration (N : Node_Id) is
      Def_Id : constant Entity_Id := Defining_Identifier (N);

   begin

      --------------------------------------
      -- Type Declaration For Tagged Type --
      --------------------------------------

      --  For tagged types, create the specification for the _Size function,
      --  the implicit primitive operation used to compute the Size attribute.

      if Is_Tagged_Type (Def_Id) then
         Insert_List_After (N, Predefined_Primitive_Specs (Def_Id));

      --------------------------------------
      -- Type Declaration for Access Type --
      --------------------------------------

      --  If the designated types is a task type or contains tasks, then
      --  we make sure that a _Master variable is declared in the current
      --  scope, and then declare a renaming for it:

      --    atype___Master : Master_Id renames _Master;

      --  where atyp is the name of the access type. This declaration is
      --  used when an allocator for the access type is expanded

      elsif Is_Access_Type (Def_Id) then
         if Has_Tasks (Designated_Type (Def_Id)) then
            Build_Master_Entity (Def_Id);

            declare
               Loc : constant Source_Ptr := Sloc (Def_Id);
               Id  : constant Entity_Id :=
                       Make_Defining_Identifier (Loc,
                         New_External_Name (Chars (Def_Id), "master"));

            begin
               Insert_After (N,
                 Make_Object_Renaming_Declaration (Loc,
                   Defining_Identifier => Id,
                   Subtype_Mark =>
                     New_Reference_To (RTE (RE_Master_Id), Loc),
                   Name => Make_Identifier (Loc, Name_uMaster)));

               Set_Master_Id (Def_Id, Id);
            end;
         end if;

      end if;

   end Expand_N_Full_Type_Declaration;

   ---------------------------------
   -- Expand_N_Object_Declaration --
   ---------------------------------

   --  First we do special processing for objects of a tagged type where this
   --  is the point at which the type is frozen. The creation of the dispatch
   --  table and the initialization procedure have to be deffered to this
   --  point, since we reference previously declared primitive subprograms.

   --  For all types, we call an initialization procedure if there is one

   procedure Expand_N_Object_Declaration (N : Node_Id) is
      Typ      : constant Entity_Id  := Etype (Defining_Identifier (N));
      Obj_Def  : constant Node_Id    := Object_Definition (N);
      Expr     : constant Node_Id    := Expression (N);
      Loc      : constant Source_Ptr := Sloc (N);
      New_Expr : Node_Id; 

   begin
      --  If tasks being declared, make sure we have an activation chain
      --  defined for the tasks (has no effect if we already have one), and
      --  also that a Master variable is established and that the appropriate
      --  enclosing construct is established as a task master.

      if Has_Tasks (Typ) then
         Build_Activation_Chain_Entity (N);
         Build_Master_Entity (Typ);
      end if;

      --  Call type initialization procedure if there is one. We build
      --  the call and put it immediately after the object declaration,
      --  so that it will be expanded in the usual manner. Note that this
      --  will result in proper handling of defaulted discriminants.

      if Present (Base_Init_Proc (Typ))
        and then No (Expr)
      then
         Insert_After (N, 
           Build_Initialization_Call (Loc,
             New_Reference_To (Defining_Identifier (N), Loc), Typ));
      end if;

      --  If the type is a class type (which of course must have an initial
      --  value specified), we first make sure that the expression has a 
      --  class-wide type by forcing an unchecked conversion if needed. That
      --  is important because the expression may come from a view conversion
      --  in which case its static type doesn't correspond to its tag. Then we
      --  generate the equivalent record whose size is the actual size
      --  of the initial value. 

      if Ekind (Typ) = E_Class_Subtype then

         if not Is_Class_Type (Etype (Expr)) then
            New_Expr := 
              Make_Unchecked_Type_Conversion (Loc,
                Subtype_Mark => New_Reference_To (Etype (Obj_Def), Loc),
                Expression => New_Copy (Expr));

            Analyze (New_Expr);
            Resolve_Subexpr (New_Expr, Typ);
         else
            New_Expr := New_Copy (Expr);
         end if;

         Insert_List_Before (N,
           Expand_Class_Subtype (Loc, Typ, New_Expr));

         --  Now convert the expression to the new equivalent type.

         Rewrite_Substitute_Tree (Expr,
           Make_Unchecked_Type_Conversion (Loc,
             Subtype_Mark => New_Reference_To (Equivalent_Type (Typ), Loc),
             Expression => New_Expr));

         Analyze (Expr);
         Resolve_Subexpr (Expr, Equivalent_Type (Typ));

      end if;

   end Expand_N_Object_Declaration;

   -------------------------------
   -- Build_Initialization_Call --
   -------------------------------

   function Build_Initialization_Call (Loc    : Source_Ptr;
                                       Id_Ref : Node_Id;
                                       Typ    : Entity_Id) return Node_Id is
      Args  : List_Id;
      Discr : Elmt_Id;

   begin
      --  First argument (_Init) is the object to be initialized, which is
      --  passed as the second parameter of the call to this procedure.

      Args := New_List_1 (Convert_Task (Id_Ref, Typ));

      --  In the tasks case, add _Master as the value of the _Master parameter
      --  and _Chain as the value of the _Chain parameter. At the outer level,
      --  these will be variables holding the corresponding values obtained
      --  from GNARL. At inner levels, they will be the parameters passed down
      --  through the outer routines.

      if Has_Tasks (Typ) then
         Append_To (Args, Make_Identifier (Loc, Name_uMaster));
         Append_To (Args, Make_Identifier (Loc, Name_uChain));
      end if;

      --  Add discriminant values if discriminants are present

      if Has_Discriminants (Typ) then
         Discr := First_Elmt (Discriminant_Constraint (Typ));

         while Discr /= No_Elmt loop
            Append (New_Copy (Id_Of (Discr)), Args);
            Discr := Next_Elmt (Discr);
         end loop;
      end if;

      --  Return the node for the call to the initialization procedure

      return Make_Procedure_Call_Statement (Loc,
         Name => New_Occurrence_Of (Base_Init_Proc (Typ), Loc),
         Parameter_Associations => Args);

   end Build_Initialization_Call;

   --------------------------------
   -- Predefined_Primitive_Specs --
   --------------------------------

   function Predefined_Primitive_Specs (Tag_Typ : Entity_Id) 
     return List_Id is

      Loc    : constant Source_Ptr := Sloc (Tag_Typ);
      Param1 : Entity_Id; 
      Param2 : Entity_Id; 
      F_Id   :  Entity_Id; 
      Decl   : Node_Id;
      Res    : List_Id := New_List;

   begin
      --  Spec of _Size

      Param1 := Make_Defining_Identifier (Loc, Name_X);
      F_Id := Make_Defining_Identifier (Loc, Name_uSize);
      Decl :=
        Make_Subprogram_Declaration (Loc,
          Specification => Make_Function_Specification (Loc,
            Defining_Unit_Name => F_Id,
            Parameter_Specifications => New_List_1 (
              Make_Parameter_Specification (Loc,
                Defining_Identifier => Param1,
                Parameter_Type => New_Reference_To (Tag_Typ, Loc))),
            Subtype_Mark =>
              New_Reference_To (Standard_Long_Long_Integer, Loc)));
      Analyze (Decl);
      Set_Is_Public (F_Id, Is_Public (Tag_Typ));
      Append_To (Res, Decl);

      --  Spec of "="

      if not Is_Limited_Type (Tag_Typ) then

         Param1 := Make_Defining_Identifier (Loc, Name_X);
         Param2 := Make_Defining_Identifier (Loc, Name_Y);
         F_Id := Make_Defining_Identifier (Loc, Name_Op_Eq);
         Decl :=
           Make_Subprogram_Declaration (Loc,
             Specification => Make_Function_Specification (Loc,
               Defining_Unit_Name => F_Id,
               Parameter_Specifications => New_List_2 (
                 Make_Parameter_Specification (Loc,
                   Defining_Identifier => Param1,
                   Parameter_Type => New_Reference_To (Tag_Typ, Loc)),
                 Make_Parameter_Specification (Loc,
                   Defining_Identifier => Param2,
                   Parameter_Type => New_Reference_To (Tag_Typ, Loc))),
               Subtype_Mark =>
                 New_Reference_To (Standard_Boolean, Loc)));
         Analyze (Decl);
         Set_Is_Public (F_Id, Is_Public (Tag_Typ));
         Set_Is_Internal (F_Id);
         Append_To (Res, Decl);
      end if;

      return Res;
   end Predefined_Primitive_Specs;

   ---------------------------------
   -- Predefined_Primitive_Bodies --
   ---------------------------------

   function Predefined_Primitive_Bodies (Tag_Typ : Entity_Id) 
     return List_Id is

      Loc    : constant Source_Ptr := Sloc (Tag_Typ);
      Param1 : Entity_Id; 
      Param2 : Entity_Id; 
      F_Id   :  Entity_Id; 
      Decl   : Node_Id;
      Res    : List_Id := New_List;
      Prim   : Elmt_Id;

   begin
      --  Body of _Size

      Param1 := Make_Defining_Identifier (Loc, Name_X);
      F_Id := Make_Defining_Identifier (Loc, Name_uSize);

      Decl :=
        Make_Subprogram_Body (Loc,
          Specification => Make_Function_Specification (Loc,
            Defining_Unit_Name => F_Id,
            Parameter_Specifications => New_List_1 (
              Make_Parameter_Specification (Loc,
                Defining_Identifier => Param1,
                Parameter_Type => New_Reference_To (Tag_Typ, Loc))),
            Subtype_Mark =>
              New_Reference_To (Standard_Long_Long_Integer, Loc)),

          Declarations => Empty_List,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, New_List_1 (
              Make_Return_Statement (Loc,
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix => Make_Identifier (Loc, Name_X),
                    Identifier => Make_Identifier (Loc, Name_Size))))));

      Analyze (Decl);
      Set_Is_Public (F_Id, Is_Public (Tag_Typ));
      Append_To (Res, Decl);

      --  Body of "=". 
      --  It has to be supplied only if the type is not limited and 
      --  there is no user-defined "=".

      if not Is_Limited_Type (Tag_Typ) then 

            Prim := First_Elmt (Primitive_Operations (Tag_Typ));
            while Chars (Id_Of (Prim)) /= Name_Op_Eq loop
               Prim := Next_Elmt (Prim);
               pragma Assert (Prim /= No_Elmt);
            end loop;

         if Is_Internal (Id_Of (Prim)) then

            Param1 := Make_Defining_Identifier (Loc, Name_X);
            Param2 := Make_Defining_Identifier (Loc, Name_Y);
            F_Id := Make_Defining_Identifier (Loc, Name_Op_Eq);

            Decl :=
              Make_Subprogram_Body (Loc,
                Specification => Make_Function_Specification (Loc,
                  Defining_Unit_Name => F_Id,
                  Parameter_Specifications => New_List_2 (
                    Make_Parameter_Specification (Loc,
                      Defining_Identifier => Param1,
                      Parameter_Type => New_Reference_To (Tag_Typ, Loc)),
                    Make_Parameter_Specification (Loc,
                      Defining_Identifier => Param2,
                      Parameter_Type => New_Reference_To (Tag_Typ, Loc))),
                  Subtype_Mark =>
                    New_Reference_To (Standard_Boolean, Loc)),
                Declarations => Empty_List,

                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc, New_List_1 (
                    Make_Return_Statement (Loc,
                      Expression =>
                        Expand_Record_Equality (Loc,
                          Typ => Tag_Typ,
                          Lhs => Make_Identifier (Loc, Name_X),
                          Rhs => Make_Identifier (Loc, Name_Y))))));

            Analyze (Decl);
            Set_Is_Public (F_Id, Is_Public (Tag_Typ));
            Append_To (Res, Decl);
         end if;
      end if;

      return Res;
   end Predefined_Primitive_Bodies;

   ---------------------------
   -- Expand_N_Variant_Part --
   ---------------------------

   --  If the last variant does not contain the Others choice, replace
   --  it with an N_Others_Choice node since Gigi always wants an Others.
   --  Note that we do not bother to call Analyze on the modified variant
   --  part, since it's only effect would be to compute the contents of
   --  the Others_Discrete_Choices node laboriously, and of course we
   --  already know the list of choices that corresponds to the others
   --  choice (it's the list we are replacing!)

   procedure Expand_N_Variant_Part (N : Node_Id) is
      Last_Var    : constant Node_Id := Last (Variants (N));
      Others_Node : Node_Id;

   begin
      if Nkind (First (Discrete_Choices (Last_Var))) /= N_Others_Choice then
         Others_Node := Make_Others_Choice (Sloc (Last_Var));
         Set_Others_Discrete_Choices
           (Others_Node, Discrete_Choices (Last_Var));
         Set_Discrete_Choices (Last_Var, New_List_1 (Others_Node));
      end if;
   end Expand_N_Variant_Part;

   ----------------------------
   --  Expand_Dispatch_Table --
   ----------------------------

   --  The following code is built to generate the dispatch table. Note that
   --  the types for access to primitives are constructed as implicit types
   --  (i.e. the full tree for the type, as implied by the documentation
   --  given here is not in fact constructed, but the required entities are
   --  constructed).

   --  Note also the pragma Suppress on the index type of tha Ancestor tag
   --  table. It is needed for the sake of class-wide objects ; as a matter
   --  of fact these values use the root Dispatch Table type, but their actual
   --  Dispatch Table can contain a Table of Ancestor bigger than the Root one
   --  and in this case we don't want to generate an index constraint error.

   --   type {Tname}___prim_ptr_1 is access {profile of 1st primitive};
   --   type {Tname}___prim_ptr_2 is access {profile of 2nd primitive};
   --   ...
   --   type {Tname}___prim_ptr_n is access {profile of nth primitive};
   --
   --   type {Tname}___TT_Type is
   --     array (0 .. {_Idepth}) of Ada.Tags.Tag;
   --   pragma Suppress (Index_Check, {Tname}___TT_Type);
   --
   --   type {Tname}___TT_Ptr_Type is access {Tname}___TT_Type;
   --   {Tname}___Tag_Table : aliased {Tname}___TT_Type;
   --
   --   type DT_Type is record
   --     _Idepth : Natural               := {inheritance depth};
   --     _Tags   : {Tname}___TT_Ptr_Type := {Tname}___Tag_Table'access;
   --     f_1     : {Tname}___prim_ptr_1  := f'access;
   --     g_2     : {Tname}___prim_ptr_2  := g'access;
   --     ...
   --     h_n     : {Tname}___prim_ptr_n  := h'access;
   --   end record;
   --
   --   type {Tname}___DT_Ptr_Type is access {Tname}___DT_Type;
   --
   --   {Tname}___Disp_Table : aliased {Tname}___DT_Type;
   --   {Tname}___DT_Ptr : {Tname}___DT_Ptr_Type :=
   --     {Tname}___Disp_Table'access;
   --
   --   {Tname}___Tag_Table (0) := Ada.Tags.Tag ({Tname}___DT_Ptr);
   --   {Tname}___Tag_Table (1) := {Tname}___DT_Ptr._Tags (0);
   --   {Tname}___Tag_Table (2) := {Tname}___DT_Ptr._Tags (1);
   --    ...
   --   {Tname}___Tag_Table (n) := {Tname}___DT_Ptr._Tags (n - 1);

   function Expand_Dispatch_Table (Tagged_Type : Entity_Id) return List_Id is

      Loc   : constant Source_Ptr := Sloc (Tagged_Type);
      Tname : constant Name_Id    := Chars (Tagged_Type);

      --  TT stands for (ancestor) Tag Table
      --  DT stands for Dispatch Table

      Name_TTT  : constant Name_Id := New_External_Name (Tname, "tt_type");
      Name_TTI  : constant Name_Id := New_External_Name (Tname, "tt_type_ind");
      Name_TT   : constant Name_Id := New_External_Name (Tname, "tag_table");
      Name_TTPT : constant Name_Id := New_External_Name (Tname, "tt_ptr_type");
      Name_DTT  : constant Name_Id := New_External_Name (Tname, "dt_type");
      Name_DT   : constant Name_Id := New_External_Name (Tname, "disp_table");
      Name_DTPT : constant Name_Id := New_External_Name (Tname, "dt_ptr_type");
      Name_DTP  : constant Name_Id := New_External_Name (Tname, "dt_ptr");

      TT_Type        : constant Node_Id :=
                         Make_Defining_Identifier (Loc, Name_TTT);
      TT_Type_Index  : constant Node_Id :=
                         Make_Defining_Identifier (Loc, Name_TTI);
      Tag_Table      : constant Node_Id :=
                         Make_Defining_Identifier (Loc, Name_TT);
      TT_Ptr_Type    : constant Node_Id :=
                         Make_Defining_Identifier (Loc, Name_TTPT);
      Tags           : constant Node_Id :=
                         Make_Defining_Identifier (Loc, Name_uTags);
      DT_Type        : constant Node_Id :=
                         Make_Defining_Identifier (Loc, Name_DTT);
      Dispatch_Table : constant Node_Id :=
                         Make_Defining_Identifier (Loc, Name_DT);
      DT_Ptr_Type    : constant Node_Id :=
                         Make_Defining_Identifier (Loc, Name_DTPT);
      DT_Ptr         : constant Node_Id :=
                         Make_Defining_Identifier (Loc, Name_DTP);
      Inh_Dep        : constant Node_Id :=
                         Make_Defining_Identifier (Loc, Name_uIdepth);
      Access_Prim : Node_Id;
      Desig_Type  : Node_Id;
      Prim_Field  : Node_Id;
      D           : Node_Id;

      Nb_Primitives     : Nat;
      Inheritance_Depth : Nat;
      Primitives_List   : Elist_Id := Primitive_Operations (Tagged_Type);
      I_Elmt            : Elmt_Id;
      Parent_Type       : Entity_Id;
      New_Prim_List     : List_Id := New_List;
      New_Tree_List     : List_Id := New_List;
      New_Comp_List     : List_Id := New_List;

      Prim       : Entity_Id;
      Old_Formal : Entity_Id;
      New_Formal : Entity_Id;
      Field_Name : Name_Id;
      Def_Exp    : Node_Id;

   begin
      --  Count the primitives and generate the corresponding access type

      Nb_Primitives := 0;
      I_Elmt := First_Elmt (Primitives_List);

      while (I_Elmt /= No_Elmt) loop

         Prim := Id_Of (I_Elmt);

            Nb_Primitives := Nb_Primitives + 1;

         --  The following code doesn't create the tree corresponding
         --  to the statement:

         --    type {Tname}___prim_ptr_XX is access {profile of prim1};

         --  because of visibility problems.

         --  Thus, only the entities necessary are created to cause subsequent
         --  GNAT processing to act as if such type declaration as been done.

         --  This code is NOT analyzed

         --  Create a defining identifier for each access

         Access_Prim :=
           Make_Defining_Identifier (Loc,
            Chars =>
              New_External_Name (Tname, "prim_ptr", Nb_Primitives));

         --  Make the identifier visible

         Enter_Name (Access_Prim);

         Desig_Type := New_Implicit_Type (Loc,
                         Tagged_Type, "prim_ptr_", Nb_Primitives);
         Set_Ekind (Desig_Type, E_Subprogram_Type);

         if Ekind (Prim) = E_Function then
            Set_Etype (Desig_Type, Etype (Prim));

         elsif Ekind (Prim) = E_Procedure then
            Set_Etype (Desig_Type, Standard_Void_Type);
         else
            Compiler_Abort;
         end if;

         --  Create a new list of parameters which is a copy of the old formal
         --  list including the creation of a new set of matching entities.

         Old_Formal := First_Formal (Prim);

         if Present (Old_Formal) then
            New_Formal := New_Copy (Old_Formal);
            Set_First_Entity (Desig_Type, New_Formal);
            Old_Formal := Next_Formal (Old_Formal);

            while Present (Old_Formal) loop
               Set_Next_Entity (New_Formal, New_Copy (Old_Formal));
               New_Formal := Next_Entity (New_Formal);
               Old_Formal := Next_Formal (Old_Formal);
            end loop;
         end if;

         Set_Ekind (Access_Prim, E_Access_Subprogram_Type);
         Set_Etype (Access_Prim, Access_Prim);
         Set_Directly_Designated_Type (Access_Prim, Desig_Type);

         --  Create the code for the dispatch table:

         --    f__{N} : {Tname}___prim_ptr_N := f'access;

         --  where N is Nb_Primitives and 'f' is the name of the primitive if
         --  it is not an operator. This code is stored in a list that will be
         --  inserted at the end of the components list of the dispatch table.
         --  If f is abstract, f'access is replaced by null.

         if Is_Operator_Symbol_Name (Chars (Prim)) then
            Field_Name := New_External_Name (Name_F, "", Nb_Primitives);
         else
            Field_Name := New_External_Name (Chars (Prim), "", Nb_Primitives);
         end if;

         if Is_Abstract (Prim) then
            Def_Exp := Make_Null (Loc);
         else
            Def_Exp :=
              Make_Attribute_Reference (Loc,
                Prefix => New_Occurrence_Of (Prim, Loc),
                Identifier => Make_Identifier (Loc, Name_Access),
                Expression => Empty);
         end if;

         Prim_Field :=
           Make_Component_Declaration (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Field_Name),
             Aliased_Present => False,
             Subtype_Indication => New_Reference_To (Access_Prim, Loc),
             Expression => Def_Exp);

         if Chars (Prim) = Name_uSize then
            Prepend_To (New_Prim_List, Prim_Field);
         else
            Append_To (New_Prim_List,  Prim_Field);
         end if;

         I_Elmt := Next_Elmt (I_Elmt);
      end loop;

      --  Count ancestors to compute the inheritance depth

      Inheritance_Depth := 0;
      Parent_Type := Tagged_Type;

      while Parent_Type /= Etype (Parent_Type) loop
         Inheritance_Depth := Inheritance_Depth + 1;
         Parent_Type := Etype (Parent_Type);
      end loop;

      --  Generate the array of tags of ancestors:

      --    subtype {Tname}___TT_Type_Ind is
      --      Integer range 0 .. Inheritance_Depth;

      Append_To (New_Tree_List,
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => TT_Type_Index,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (Standard_Integer, Loc),
              Constraint =>
                Make_Range_Constraint (Loc,
                  Range_Expression =>
                    Make_Range (Loc,
                      Low_Bound => Make_Integer_Literal (Loc, Uint_0),
                      High_Bound =>
                        Make_Integer_Literal (Loc,
                          UI_From_Int (Int (Inheritance_Depth))))))));

      --   pragma Suppress (Index_Check, {Tname}___TT_Type_Ind);

      Append_To (New_Tree_List,
        Make_Pragma (Loc,
          Identifier => Make_Identifier (Loc, Name_Suppress),
          Pragma_Argument_Associations => New_List_2 (
             Make_Pragma_Argument_Association (Loc,
               Expression => Make_Identifier (Loc, Name_Index_Check)),
             Make_Pragma_Argument_Association (Loc,
               Expression => New_Reference_To (TT_Type_Index, Loc)))));

      --    type {Tname}___TT_Type is
      --      array ({Tname}___TT_Type_Ind) of Ada.Tags.Tag;

      Append_To (New_Tree_List,
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => TT_Type,
          Discriminant_Specifications => No_List,
          Type_Definition =>
            Make_Constrained_Array_Definition (Loc,
              Discrete_Subtype_Definitions =>
                New_List_1 (New_Reference_To (TT_Type_Index, Loc)),
              Aliased_Present => False,
              Subtype_Indication =>
                New_Reference_To (RTE (RE_Tag), Loc))));

      --  type {Tname}___TT_Ptr_Type is access {Tname}___TT_Type;

      Append_To (New_Tree_List,
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => TT_Ptr_Type,
          Discriminant_Specifications => No_List,
          Type_Definition =>
            Make_Access_To_Object_Definition (Loc,
              All_Present => False,
              Subtype_Indication => New_Reference_To (TT_Type, Loc),
              Constant_Present => False)));

      --  {Tname}___Tag_Table : aliased {Tname}___TT_Type;

      Append_To (New_Tree_List,
        Make_Object_Declaration (Loc,
          Defining_Identifier  => Tag_Table,
          Aliased_Present      => True,
          Constant_Present     => False,
          Object_Definition    => New_Reference_To (TT_Type, Loc),
          Expression           => Empty));

      --  Generate the code for the dispatch table itself. First step is
      --  to create the list of components and the dispatch table type.

      --  First component of the record for dispatch_table:
      --    _Idepth : Natural := {Inheritance_Depth};

      Append_To (New_Comp_List,
        Make_Component_Declaration (Loc,
          Defining_Identifier => Inh_Dep,
          Aliased_Present => False,
          Subtype_Indication => New_Reference_To (Standard_Natural, Loc),
          Expression =>
            Make_Integer_Literal (Loc,
              UI_From_Int (Int (Inheritance_Depth)))));

      --  Second component of the record od the dispatch table:
      --    _Tags : {Tname}___TT_Ptr_Type := {Tname}___Tag_Table'access;

      Append_To (New_Comp_List,
        Make_Component_Declaration (Loc,
          Defining_Identifier => Tags,
          Aliased_Present => False,
          Subtype_Indication => New_Reference_To (TT_Ptr_Type, Loc),
          Expression =>
            Make_Attribute_Reference (Loc,
              Prefix => New_Reference_To (Tag_Table, Loc),
              Identifier => Make_Identifier (Loc, Name_Access),
              Expression => Empty)));

      --  Add the accesses to any primitives operations created before

      if not Is_Empty_List (New_Prim_List) then
         Append_List (New_Prim_List, New_Comp_List);
      end if;

      --  Now the list of components is complete so we create the dispatch
      --  table type:

      --    type DT_Type is record
      --      uIdepth : Natural      := {Inheritance_Depth};
      --      _Tags   : {Tname}___TT_Ptr_Type := {Tname}___Tag_Table'access;
      --      f_1     : {Tname}___prim_ptr_1  := f'access;
      --      g_2     : {Tname}___prim_ptr_2  := g'access;
      --      ...
      --      h_n     : {Tname}___prim_ptr_n  := h'access;
      --    end record;

      Append_To (New_Tree_List,
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => DT_Type,
          Discriminant_Specifications => No_List,
          Type_Definition =>
            Make_Record_Definition (Loc,
              Tagged_Present => False,
              Limited_Present => False,
              Component_List =>
                Make_Component_List (Loc,
                  Component_Declarations => New_Comp_List,
                  Variant_Part => Empty,
                  Null_Present => False))));

      --  type {Tname}___DT_Ptr_Type is access {Tname}___DT_Type;

      Append_To (New_Tree_List,
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => DT_Ptr_Type,
          Discriminant_Specifications => No_List,
          Type_Definition =>
            Make_Access_To_Object_Definition (Loc,
              All_Present => False,
              Subtype_Indication =>
                New_Reference_To (DT_Type, Loc),
              Constant_Present => False)));

      --  {Tname}___Disp_Table : aliased {Tname}___DT_Type;

      Append_To (New_Tree_List,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Dispatch_Table,
          Aliased_Present => True,
          Constant_Present => False,
          Object_Definition => New_Reference_To (DT_Type, Loc),
          Expression => Empty));

      --  {Tname}___DT_Ptr : {Tname}___DT_Ptr_Type :=
      --    {Tname}___Disp_Table'access;

      Append_To (New_Tree_List,
        Make_Object_Declaration (Loc,
          Defining_Identifier => DT_Ptr,
          Aliased_Present => False,
          Constant_Present => False,
          Object_Definition => New_Reference_To (DT_Ptr_Type, Loc),
          Expression =>
            Make_Attribute_Reference (Loc,
              Prefix => New_Reference_To (Dispatch_Table, Loc),
              Identifier => Make_Identifier (Loc, Name_Access),
              Expression => Empty)));

      --  Set Access_Disp_Table field to be the dispatch table pointer

      Set_Access_Disp_Table (Tagged_Type, DT_Ptr);

      --  Initialize the Tag_Table:
      --    {Tname}___Tag_Table (0) := System.Tag ({Tname}___DT_Ptr);

      Append_To (New_Tree_List,
        Make_Assignment_Statement (Loc,
          Name =>
            Make_Indexed_Component (Loc,
              Prefix => New_Reference_To (Tag_Table, Loc),
              Expressions => New_List_1 (
                Make_Integer_Literal (Loc, Uint_0))),
          Expression =>
            Make_Unchecked_Type_Conversion (Loc,
              Subtype_Mark =>
                New_Reference_To (RTE (RE_Tag), Loc),
              Expression => New_Reference_To (DT_Ptr, Loc))));

      Parent_Type := Etype (Tagged_Type);

      for I in 1 .. Inheritance_Depth loop

         --  {Tname}___Tag_Table (I) := {Tname}___DT_Ptr._Tags (I - 1);

         Append_To (New_Tree_List,
           Make_Assignment_Statement (Loc,
             Name =>
               Make_Indexed_Component (Loc,
                 Prefix => New_Reference_To (Tag_Table, Loc),
                 Expressions => New_List_1 (
                   Make_Integer_Literal (Loc, UI_From_Int (Int (I))))),

             Expression =>
               Make_Indexed_Component (Loc,
                 Prefix =>
                   Make_Selected_Component (Loc,
                     Prefix =>
                       New_Reference_To (Access_Disp_Table (Parent_Type), Loc),
                     Selector_Name =>
                       Make_DT_Component (Loc, Parent_Type, 2)),
                 Expressions => New_List_1 (
                   Make_Integer_Literal (Loc, UI_From_Int (Int (I) - 1))))));
      end loop;

      D := First (New_Tree_List);

      while Present (D) loop 
         Analyze (D);
         Install_Implicit_Types (D);
         D := Next (D);
      end loop;

      Append_List_To (New_Tree_List, 
        Predefined_Primitive_Bodies (Tagged_Type));

      return New_Tree_List;
   end Expand_Dispatch_Table;

   ------------------
   -- Init_Formals --
   ------------------

   function Init_Formals (Typ : Entity_Id) return List_Id is
      Formals : List_Id;
      Loc : constant Source_Ptr := Sloc (Typ);

   begin
      --  First parameter is always _Init : in out typ. Note that we need
      --  this to be in/out because in the case of the task record value,
      --  there are default record fields (_Priority and _Size) that may be
      --  referenced in the generated initialization routine.

      Formals := New_List_1 (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => 
            Make_Defining_Identifier (Loc, Name_uInit),
          In_Present  => True,
          Out_Present => True,
          Parameter_Type => New_Reference_To (Typ, Loc)));

      --  For task record value, or type that contains tasks, add two more
      --  formals, _Master : Master_Id and _Chain : in out Activation_Chain
      --  We also add these parameters for the task record type case.

      if Has_Tasks (Typ) 
        or else (Is_Record_Type (Typ) and then Is_Task_Record_Type (Typ))
      then
         Append_To (Formals,
           Make_Parameter_Specification (Loc,
             Defining_Identifier => 
               Make_Defining_Identifier (Loc, Name_uMaster),
             Parameter_Type => New_Reference_To (RTE (RE_Master_Id), Loc)));

         Append_To (Formals,
           Make_Parameter_Specification (Loc,
             Defining_Identifier => 
               Make_Defining_Identifier (Loc, Name_uChain),
             In_Present => True,
             Out_Present => True,
             Parameter_Type =>
               New_Reference_To (RTE (RE_Activation_Chain), Loc)));
      end if;

      return Formals;
   end Init_Formals;

end Exp_Ch3;
