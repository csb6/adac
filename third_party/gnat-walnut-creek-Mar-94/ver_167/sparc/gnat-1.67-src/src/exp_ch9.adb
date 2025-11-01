------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 9                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.37 $                             --
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
with Exp_Ch3;  use Exp_Ch3;
with Exp_Util; use Exp_Util;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Res;  use Sem_Res;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Ch9 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_Discriminal_Declarations
     (Decls : List_Id; Typ : Node_Id; Name : Name_Id; Loc : Source_Ptr);
   --  This routine is used to add discriminal declarations to task bodies
   --  (and protected bodies ???). The discriminants are available by normal
   --  selection from the concurrent object (whose name is passed as the third
   --  parameter). Discriminant references inside the task body have already
   --  been replaced by references to the corresponding discriminals. The
   --  declarations constructed by this procedure hook the references up with
   --  the objects:
   --
   --    discriminal_name : discr_type renames name.discriminant_name;
   --
   --  Obviously we could have expanded the discriminant references in the
   --  first place to be the appropriate selection, but this turns out to
   --  be hard to do because it would introduce difference in handling of
   --  discriminant references depending on their location.

   function Build_Task_Proc_Specification (T : Entity_Id) return Node_Id;
   --  This routine constructs a specification for the procedure that we will
   --  build for the task body. The specification has the form
   --
   --    procedure name___Body (_Task : access name___Val);
   --
   --  where name is the character name taken from the task type entity that
   --  is passed as the argument to the procedure, and name___Val is the task
   --  value type that is associated with the task type.

   function Find_Task_Pragma (T : Node_Id; P : Name_Id) return Node_Id;
   --  Searches the task definition T for the first occurrence of the pragma
   --  whose name is given by P. The caller has ensured that the pragma is
   --  present in the task definition.

   function Task_Ref (N : Node_Id) return Node_Id;
   --  Given the name of a task, or the name of an access to a task, this
   --  function returns an expression which references the associated Task_Id.

   ----------------------------------
   -- Add_Discriminal_Declarations --
   ----------------------------------

   procedure Add_Discriminal_Declarations
     (Decls : List_Id; Typ : Node_Id; Name : Name_Id; Loc : Source_Ptr) 
   is
      D : Entity_Id;

   begin
      if Has_Discriminants (Typ) then
         D := First_Discriminant (Typ);

         while Present (D) loop
            Prepend_To (Decls,
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Discriminal (D),
                Subtype_Mark => New_Reference_To (Etype (D), Loc),
                Name =>
                  Make_Selected_Component (Loc,
                    Prefix        => Make_Identifier (Loc, Name),
                    Selector_Name => Make_Identifier (Loc, Chars (D)))));

            D := Next_Discriminant (D);
         end loop;
      end if;
   end Add_Discriminal_Declarations;

   -----------------------------------
   -- Build_Activation_Chain_Entity --
   -----------------------------------

   procedure Build_Activation_Chain_Entity (N : Node_Id) is
      P : Node_Id;

   begin
      --  Loop to find enclosing construct containing activation chain variable

      --  Note??? the test for generic package declaration should be removed
      --  when generics are done right (because then we will never see a
      --  generic node in the expander).

      P := Parent (N);
      while Nkind (P) /= N_Subprogram_Body
        and then Nkind (P) /= N_Generic_Package_Declaration -- ???
        and then Nkind (P) /= N_Package_Declaration
        and then Nkind (P) /= N_Package_Body
        and then Nkind (P) /= N_Block_Statement
      loop
         P := Parent (P);
      end loop;

      --  If we are in a package body, the activation chain variable is
      --  allocated in the corresponding spec

      if Nkind (P) = N_Package_Body then
         P := Parent (Corresponding_Spec (P));
         pragma Assert (Nkind (P) = N_Package_Declaration,
                        Compiler_Abort (P));
      end if;

      --  If activation chain entity not already declared, declare it

      if No (Activation_Chain_Entity (P)) then
         Set_Activation_Chain_Entity
           (P, Make_Defining_Identifier (Sloc (P), Name_uChain));

         Prepend (
            Make_Object_Declaration (Sloc (P),
              Defining_Identifier => Activation_Chain_Entity (P),
              Object_Definition   =>
                New_Reference_To (RTE (RE_Activation_Chain), Sloc (P))),
            Declarations (P));

         Analyze (First (Declarations (P)));
      end if;

   end Build_Activation_Chain_Entity;

   --------------------------
   -- Build_Call_With_Task --
   --------------------------

   function Build_Call_With_Task
     (N : Node_Id; E : Entity_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      return
        Make_Function_Call (Loc,
          Name => New_Reference_To (E, Loc),
          Parameter_Associations => New_List_1 (Task_Ref (N)));
   end Build_Call_With_Task;

   -------------------------
   -- Build_Master_Entity --
   -------------------------

   procedure Build_Master_Entity (E : Entity_Id) is
      Loc  : constant Source_Ptr := Sloc (E);
      P    : Node_Id;
      Decl : Node_Id;

   begin
      --  Nothing to do if we already built a master entity for this scope

      if Has_Master_Entity (Scope (E)) then
         return;
      end if;

      --  Otherwise first build the master entity
      --    _Master : constant Master_Id := Current_Master;
      --  and insert it just before the current declaration

      Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => 
            Make_Defining_Identifier (Loc, Name_uMaster),
          Constant_Present => True,
          Object_Definition => New_Reference_To (RTE (RE_Master_Id), Loc),
          Expression => New_Reference_To (RTE (RE_Current_Master), Loc));

      Analyze (Decl);
      P := Parent (E);
      Insert_Before (P, Decl);
      Set_Has_Master_Entity (Scope (E));

      --  Now mark the containing scope as a task master

      loop
         P := Parent (P);

         --  If we fall off the top, we are at the outer level, and the
         --  environment task is our effective master, so nothing to mark.

         if Nkind (P) = N_Task_Body
           or else Nkind (P) = N_Block_Statement
           or else Nkind (P) = N_Subprogram_Body
         then
            Set_Is_Task_Master (P, True);
            return;
         end if;
      end loop;
   end Build_Master_Entity;

   --------------------------------
   -- Build_Task_Activation_Call --
   --------------------------------

   procedure Build_Task_Activation_Call (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Chain : Entity_Id;
      Call  : Node_Id;
      P     : Node_Id;

   begin
      --  Get the activation chain entity. Except in the case of a package
      --  body, this is in the node that was passed. For a package body,we
      --  have to find the corresponding package declaration node.

      --  Note: can remove generic case when we do generics properly???

      if Nkind (N) = N_Package_Body then
         P := Corresponding_Spec (N);

         loop
            P := Parent (P);
            exit when Nkind (P) = N_Package_Declaration
              or else Nkind (P) = N_Generic_Package_Declaration; -- ???
         end loop;

         Chain := Activation_Chain_Entity (P);

      else
         Chain := Activation_Chain_Entity (N);
      end if;

      if Present (Chain) then
         Call :=
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_Activate_Tasks), Loc),
             Parameter_Associations =>
               New_List_1 (New_Reference_To (Chain, Loc)));

         Analyze (Call);
         Resolve_Subexpr (Call, Standard_Void_Type);

         Prepend (Call, Statements (Handled_Statement_Sequence (N)));
      end if;

   end Build_Task_Activation_Call;

   -------------------------------
   -- Build_Task_Allocate_Block --
   -------------------------------

   procedure Build_Task_Allocate_Block
     (Actions : List_Id; N : Node_Id; Args : List_Id)
   is
      PtrT   : constant Entity_Id  := Etype (N);
      T      : constant Entity_Id  := Entity (Expression (N));
      Init   : constant Entity_Id  := Base_Init_Proc (T);
      Loc    : constant Source_Ptr := Sloc (N);
      Blkent : Entity_Id;
      Block  : Node_Id;

   begin
      Blkent := Make_Defining_Identifier (Loc, New_Internal_Name ("alloc"));

      Block :=
        Make_Block_Statement (Loc,
          Identifier => New_Reference_To (Blkent, Loc),
          Declarations => New_List_2 (

            --  _Chain  : Activation_Chain;

            Make_Object_Declaration (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, Name_uChain),
              Object_Definition   =>
                New_Reference_To (RTE (RE_Activation_Chain), Loc)),

            --  procedure _Expunge is
            --  begin
            --     Expunge_Unactivated_Tasks (_Chain);
            --  end;

            Make_Subprogram_Body (Loc,
              Specification =>
                Make_Procedure_Specification (Loc,
                  Defining_Unit_Name =>
                    Make_Defining_Identifier (Loc, Name_uExpunge)),

              Declarations => Empty_List,

              Handled_Statement_Sequence =>
                Make_Handled_Sequence_Of_Statements (Loc,
                  Statements => New_List_1 (
                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Reference_To (
                          RTE (RE_Expunge_Unactivated_Tasks), Loc),
                      Parameter_Associations => New_List_1 (
                        Make_Identifier (Loc, Name_uChain)) ))))),

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,

              Statements => New_List_2 (

               --  Init (Args);

                Make_Procedure_Call_Statement (Loc,
                  Name => New_Reference_To (Init, Loc),
                  Parameter_Associations => Args),

               --  Activate_Tasks (_Chain);

                Make_Procedure_Call_Statement (Loc,
                  Name => New_Reference_To (RTE (RE_Activate_Tasks), Loc),
                  Parameter_Associations => New_List_1 (
                    Make_Identifier (Loc, Name_uChain)))),

              Identifier => Make_Identifier (Loc, Name_uExpunge)),

          Has_Created_Identifier => True);

      Append_To (Actions,
        Make_Implicit_Label_Declaration (Loc,
          Defining_Identifier => Blkent,
          Label => Block));

      Append_To (Actions, Block);

   end Build_Task_Allocate_Block;

   -----------------------------------
   -- Build_Task_Proc_Specification --
   -----------------------------------

   function Build_Task_Proc_Specification (T : Entity_Id) return Node_Id is
      Loc : constant Source_Ptr := Sloc (T);
      Nam : constant Name_Id := Chars (T);

   begin
      return
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name =>
            Make_Defining_Identifier (Loc,
              Chars => New_External_Name (Nam, "body")),

          Parameter_Specifications =>
            New_List_1 (
              Make_Parameter_Specification (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc, Name_uTask),
                Parameter_Type =>
                  Make_Access_Definition (Loc,
                    Subtype_Mark =>
                      New_Reference_To (Task_Value_Type (T), Loc)))));

   end Build_Task_Proc_Specification;

   --------------------------
   -- Complete_Master_Call --
   --------------------------

   function Complete_Master_Call (N : Node_Id) return Node_Id is
   begin
      return
        Make_Procedure_Call_Statement (Sloc (N),
          Name => New_Reference_To (RTE (RE_Complete_Master), Sloc (N)));
   end Complete_Master_Call;

   ------------------
   -- Convert_Task --
   ------------------

   function Convert_Task (N : Node_Id; Typ : Entity_Id) return Node_Id is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      if not Is_Task_Type (Typ) then
         return N;

      else
         return
           Make_Unchecked_Type_Conversion (Loc,
             Subtype_Mark =>
               New_Reference_To (Task_Value_Type (Typ), Loc),
               Expression => New_Copy (N));
      end if;
   end Convert_Task;

   ---------------------------
   -- Establish_Task_Master --
   ---------------------------

   procedure Establish_Task_Master (N : Node_Id) is
   begin
      Prepend (
        Build_Call (Sloc (N), RTE (RE_Enter_Master)),
        Declarations (N));

      Protect_Statements (N, RTE (RE_Complete_Master));

   end Establish_Task_Master;

   --------------------------------------
   -- Expand_N_Single_Task_Declaration --
   --------------------------------------

   --  First, we expand the single task declaration as a task type
   --  declaration (the field all correspond in the two node types,
   --  this is the reason for the dummy Discriminant_Specifications
   --  field in the N_Single_Task_Declaration node). Second, we treat
   --  the declaration as an object declaration for the task object
   --  and in particular we generate the appropriate initialization.

   procedure Expand_N_Single_Task_Declaration (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Id  : constant Entity_Id  := Defining_Identifier (N);

   begin
      Expand_N_Task_Type_Declaration (N);
      Insert_After (N,
        Build_Initialization_Call (Loc,
          New_Reference_To (Id, Loc), Etype (Id)));

   end Expand_N_Single_Task_Declaration;

   ----------------------
   -- Expand_Task_Body --
   ----------------------

   --  Given a task body

   --    task body tname is
   --       declarations
   --    begin
   --       statements
   --    end x;

   --  This expansion routine converts it into a procedure and sets the
   --  elaboration flag for the procedure to true, to represent the fact
   --  that the task body is now elaborated:

   --    procedure tname___body (_Task : access tname___val) is
   --       discrimal : dtype renames _Task.discriminant;

   --    begin
   --       System.Task_Stages.Complete_Activation;
   --       statements
   --    at end
   --       System.Task_Stages.Complete_Task;
   --    end;

   --    tname___elab := True;

   --  In addition, if the task body is an activator, then a call to
   --  activate tasks is added at the start of the statements, before
   --  the call to Complete_Activation, and if in addition the task is
   --  a master then it must be established as a master.

   --  There is one discriminal declaration line generated for each
   --  discriminant that is present to provide an easy reference point
   --  for discriminant references inside the body (see Exp_Ch2.Expand_Name).

   --  Note on relationship to GNARLI definition. In the GNARLI definition, 
   --  task body procedures have a profile (Arg : System.Address). That is
   --  needed because GNARLI has to use the same access-to-subprogram type 
   --  for all task types. We depend here on knowing that in GNAT, passing 
   --  an address argument by value is identical to passing a a record value
   --  by access (in either case a single pointer is passed), so even though
   --  this procedure has the wrong profile. In fact it's all OK, since the
   --  callings sequence is identical.

   procedure Expand_N_Task_Body (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Ttyp : constant Entity_Id  := Corresponding_Spec (N);
      Tval : constant Entity_Id  := Task_Value_Type (Ttyp);

   begin
      Add_Discriminal_Declarations (Declarations (N), Ttyp, Name_uTask, Loc);

      if Is_Task_Master (N) then
         Establish_Task_Master (N);
      end if;

      Build_Task_Activation_Call (N);

      Protect_Statements (N, RTE (RE_Complete_Task));

      Prepend (
        Build_Call (Loc, RTE (RE_Complete_Activation)),
        Statements (Handled_Statement_Sequence (N)));

      Rewrite_Substitute_Tree (N,
        Make_Subprogram_Body (Loc,
          Specification => Build_Task_Proc_Specification (Ttyp),
          Declarations  => Declarations (N),
          Handled_Statement_Sequence => Handled_Statement_Sequence (N)));

      Analyze (N);

      Insert_After (N,
        Make_Assignment_Statement (Loc,
          Name => 
            Make_Identifier (Loc, New_External_Name (Chars (Ttyp), "elab")),
          Expression => New_Reference_To (Standard_True, Loc)));

   end Expand_N_Task_Body;

   ------------------------------------
   -- Expand_N_Task_Type_Declaration --
   ------------------------------------

   --  Note: this routine is also used by Expand_N_Single_Task_Declaration.
   --  The fields in the two nodes are similar precisely to allow this
   --  approach. We take care to use the task type, rather than the node
   --  itself, so that we get the proper type (the anonymously created type)
   --  in the single task case.

   --  We have several things to do. First we must create a Boolean flag used
   --  to mark if the body is elaborated yet. This variable gets set to True
   --  when the body of the task is elaborated (we can't rely on the normal
   --  ABE mechanism for the task body, since we need to pass an access to
   --  this elaboration boolean to the runtime routines).

   --    task___Elab : aliased Boolean := False;

   --  Next a variable is declared to hold the task stack size (either
   --  the default, which is the initial value given here, or a value that
   --  is set by a pragma Storage_Size appearing later on.

   --    task___Size : Size_Type := Unspecified_Size;

   --  Next we create a corresponding record type declaration used to represent
   --  values of this task. The general form of this type declaration is

   --    type task___Val (discriminants) is record
   --      _Task_Id     : Task_Id;
   --      entry_family : array (bounds) of Void;
   --      _Priority    : Integer   := priority_expression;
   --      _Size        : Size_Type := size_expression;
   --    end record;

   --  The discriminants are present only if the corresponding task type has
   --  discriminants, and they exactly mirror the task type discriminants.

   --  The Id field is always present. It contains the Task_Id value, as
   --  set by the call to Create_Task. Note that although the task is
   --  limited, the task value record type is not limited, so there is no
   --  problem in passing this field as an out parameter to Create_Task.

   --  One entry_family component is present for each entry family in the
   --  task definition. The bounds correspond to the bounds of the entry
   --  family (which may depend on discriminants). The element type is
   --  void, since we only need the bounds information for determining
   --  the entry index. Note that the use of an anonymous array would
   --  normally be illegal in this context, but this is a parser check,
   --  and the semantics is quite prepared to handle such a case.

   --  The Size field is present only if a Task_Stack_Size pragma appears in
   --  the task definition. The expression captures the argument that was
   --  present in the pragma, and is used to override the task stack size
   --  otherwise associated with the task type.

   --  The Priority field is present only if a Priority or Interrupt_Priority
   --  pragma appears in the task definition. The expression captures the
   --  argument that was present in the pragma, and is used to provide
   --  the Size parameter to the call to Create_Task.

   --  When a task is declared, an instance of the task value record is
   --  created. The elaboration of this declaration creates the correct
   --  bounds for the entry families, and also evaluates the size and
   --  priority expressions if needed. The initialization routine for
   --  the task type itself then calls Create_Task with appropriate
   --  parameters to initialize the value of the Task_Id field.

   --  Note: the address of this record is passed as the "Discriminants"
   --  parameter for Create_Task. Since Create_Task merely passes this onto
   --  the body procedure, it does not matter that it does not quite match
   --  the GNARLI model of what is being passed (the record contains more
   --  than just the discriminants, but the discriminants can be found from
   --  the record value).

   --  The Entity_Id for this created record type is placed in the
   --  Task_Value_Type field of the associated task type entity.

   --  Finally we create a procedure specification for the task body procedure:

   --    procedure task___Body (_Task : access task___Val);

   --  Note that this must come after the record type declaration, since
   --  the spec refers to this type. It turns out that the initialization
   --  procedure for the value type references the task body spec, but that's
   --  fine, since it won't be generated till the freeze point for the type,
   --  which is certainly after the task body spec declaration.

   procedure Expand_N_Task_Type_Declaration (N : Node_Id) is
      Loc       : constant Source_Ptr := Sloc (N);
      Tasktyp   : constant Entity_Id  := Etype (Defining_Identifier (N));
      Tasknm    : constant Name_Id    := Chars (Tasktyp);
      Taskdef   : constant Node_Id    := Task_Definition (N);
      Proc_Spec : Node_Id;
      Rec_Ent   : Entity_Id;
      Rec_Decl  : Node_Id;
      Cdecls    : List_Id;
      Dlist     : List_Id;
      Disc      : Node_Id;
      Efam      : Entity_Id;
      Elab_Decl : Node_Id;
      Size_Decl : Node_Id;
      Body_Decl : Node_Id;

   begin
      --  First create the elaboration variable

      Elab_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => 
            Make_Defining_Identifier (Loc,
              Chars => New_External_Name (Tasknm, "elab")),
          Aliased_Present      => True,
          Object_Definition    => New_Reference_To (Standard_Boolean, Loc),
          Expression           => New_Reference_To (Standard_False, Loc));
      Insert_After (N, Elab_Decl);

      --  Next create the declaration of the size variable

      Size_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => 
            Make_Defining_Identifier (Loc,
              Chars => New_External_Name (Tasknm, "size")),
          Object_Definition => New_Reference_To (RTE (RE_Size_Type), Loc),
          Expression => New_Reference_To (RTE (RE_Unspecified_Size), Loc));
      Insert_After (Elab_Decl, Size_Decl);

      --  Next create the record declaration for the task value type.
      --  This is done last, since the corresponding record initialization
      --  procedure will reference the previously created entities.

      Rec_Ent :=
        Make_Defining_Identifier (Loc, New_External_Name (Tasknm, "val"));
      Set_Task_Value_Type (Tasktyp, Rec_Ent);
      Set_Ekind (Rec_Ent, E_Record_Type);
      Set_Is_Task_Record_Type (Rec_Ent, True);
      Set_Corresponding_Task_Type (Rec_Ent, Tasktyp);

      --  Initialize component list, we will fill it in later

      Cdecls := New_List;

      --  Make a copy of the discriminant specifications

      if List_Present (Discriminant_Specifications (N)) then
         Dlist := New_List;
         Disc := First (Discriminant_Specifications (N));

         while Present (Disc) loop
            Append_To (Dlist,
              Make_Discriminant_Specification (Loc,
                Defining_Identifier =>
                  New_Copy (Defining_Identifier (Disc)),
                Discriminant_Type =>
                  New_Copy (Discriminant_Type (Disc)),
                Expression =>
                  New_Copy (Expression (Disc))));
            Disc := Next (Disc);
         end loop;

      else
         Dlist := No_List;
      end if;

      --  Now we can construct the record type declaration

      Rec_Decl :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Rec_Ent,
          Discriminant_Specifications => Dlist,
          Type_Definition =>
            Make_Record_Definition (Loc,
              Component_List =>
                Make_Component_List (Loc,
                  Component_Declarations => Cdecls)));

      --  Fill in the component declarations. First the _Task_Id field:

      Append_To (Cdecls,
        Make_Component_Declaration (Loc,
          Defining_Identifier => 
            Make_Defining_Identifier (Loc, Name_uTask_Id),
          Subtype_Indication => New_Reference_To (RTE (RE_Task_ID), Loc)));

      --  Add components for entry families

      Efam := First_Entity (Tasktyp);

      while Present (Efam) loop
         if Ekind (Efam) = E_Entry_Family then
            Append_To (Cdecls,
              Make_Component_Declaration (Loc,
                Defining_Identifier => 
                  Make_Defining_Identifier (Loc, Chars (Efam)),
                Subtype_Indication =>
                  Make_Constrained_Array_Definition (Loc,
                    Discrete_Subtype_Definitions => (New_List_1 (
                      New_Copy (Discrete_Subtype_Definition (Parent (Efam))))),
                    Subtype_Indication =>
                      New_Reference_To (Standard_Character, Loc))));
         end if;

         Efam := Next_Entity (Efam);
      end loop;

      --  Add the priority component if a priority pragma is present

      if Present (Taskdef) and then Has_Priority_Pragma (Taskdef) then
         Append_To (Cdecls,
           Make_Component_Declaration (Loc,
             Defining_Identifier => 
               Make_Defining_Identifier (Loc, Name_uPriority),
             Subtype_Indication => New_Reference_To (Standard_Integer, Loc),
             Expression => New_Copy (
               Expression (First (
                 Pragma_Argument_Associations (
                   Find_Task_Pragma (Taskdef, Name_Priority)))))));
      end if;

      --  Add the task_size component if a priority pragma is present

      if Present (Taskdef)
        and then Has_Task_Stack_Size_Pragma (Taskdef)
      then
         Append_To (Cdecls,
           Make_Component_Declaration (Loc,
             Defining_Identifier => 
               Make_Defining_Identifier (Loc, Name_uSize),
             Subtype_Indication => New_Reference_To (RTE (RE_Size_Type), Loc),
             Expression => New_Copy (
               Expression (First (
                 Pragma_Argument_Associations (
                   Find_Task_Pragma (Taskdef, Name_Task_Stack_Size)))))));
      end if;

      Insert_After (Size_Decl, Rec_Decl);

      --  Analyze the record declaration immediately after construction, 
      --  because the initialization procedure is needed for single task 
      --  declarations before the next entity is analyzed.

      Analyze (Rec_Decl);

      --  Finally create the declaration of the task body procedure

      Proc_Spec := Build_Task_Proc_Specification (Tasktyp);
      Body_Decl :=
        Make_Subprogram_Declaration (Loc, 
          Specification => Proc_Spec);
      Insert_After (Rec_Decl, Body_Decl);

   end Expand_N_Task_Type_Declaration;

   ----------------------
   -- Find_Task_Pragma --
   ----------------------

   function Find_Task_Pragma (T : Node_Id; P : Name_Id) return Node_Id is
      N : Node_Id;

   begin
      N := First (Visible_Declarations (T));

      while Present (N) loop
         if Nkind (N) = N_Pragma and then Chars (Identifier (N)) = P then
            return N;
         else
            N := Next (N);
         end if;
      end loop;

      N := First (Private_Declarations (T));

      while Present (N) loop
         if Nkind (N) = N_Pragma and then Chars (Identifier (N)) = P then
            return N;
         else
            N := Next (N);
         end if;
      end loop;

      Compiler_Abort;

   end Find_Task_Pragma;

   ---------------------------
   -- Make_Task_Create_Call --
   ---------------------------

   function Make_Task_Create_Call (Task_Rec : Entity_Id) return Node_Id is
      Loc    : constant Source_Ptr := Sloc (Task_Rec);
      Tdef   : Node_Id;
      Tdec   : Node_Id;
      Ttyp   : Node_Id;
      Tnam   : Name_Id;
      Args   : List_Id;
      Ent    : Entity_Id;
      Eindx  : Nat;
      Ecount : Node_Id;

   begin
      Ttyp := Corresponding_Task_Type (Task_Rec);
      Tnam := Chars (Ttyp);

      --  Get task declaration. In the case of a task type declaration, this
      --  is simply the parent of the task type entity. In the single task
      --  declaration, this parent will be the implicit type, and we can find
      --  the corresponding single task declaration by searching forward in
      --  the declaration list in the tree.

      Tdec := Parent (Ttyp);

      while Nkind (Tdec) /= N_Task_Type_Declaration
        and then Nkind (Tdec) /= N_Single_Task_Declaration
      loop
         Tdec := Next (Tdec);
      end loop;

      --  Now we can find the task definition from this declaration

      Tdef := Task_Definition (Tdec);

      --  Build the parameter list for the call. Note that _Init is the name
      --  of the formal for the object to be initialized, which is the task
      --  value record itself.

      Args := New_List;

      --  Size parameter. If no Task_Stack_Size pragma is present, then
      --  the size is taken from the task___Size variable for the type,
      --  which is either Unspecified_Size, or has been reset by the use
      --  of a Storage_Size attribute definition clause. If a pragma is
      --  present, then the size is taken from the _Size field of the
      --  task value record, which was set from the pragma value.

      if Present (Tdef)  
        and then Has_Task_Stack_Size_Pragma (Tdef) 
      then
         Append_To (Args,
           Make_Selected_Component (Loc,
             Prefix => Make_Identifier (Loc, Name_uInit),
             Selector_Name => Make_Identifier (Loc, Name_uSize)));
      else
         Append_To (Args,
           Make_Identifier (Loc, New_External_Name (Tnam, "size")));
      end if;

      --  Priority parameter. Set to Unspecified_Priority unless there is a
      --  priority pragma, in which case we take the value from the pragma.

      if Present (Tdef) 
        and then Has_Priority_Pragma (Tdef)
      then
         Append_To (Args,
           Make_Selected_Component (Loc,
             Prefix => Make_Identifier (Loc, Name_uInit),
             Selector_Name => Make_Identifier (Loc, Name_uPriority)));
      else
         Append_To (Args,
           New_Reference_To (RTE (RE_Unspecified_Priority), Loc));
      end if;

      --  Number of entries. This is an expression of the form:
      --
      --    n + _Init.a'Length + _Init.a'B'Length + ...
      --
      --  where a,b... are the entry family names for the task definition

      Ent := First_Entity (Ttyp);
      Eindx := 0;

      --  Count number of non-family entries

      while Present (Ent) loop
         if Ekind (Ent) = E_Entry then
            Eindx := Eindx + 1;
         end if;

         Ent := Next_Entity (Ent);
      end loop;

      Ecount := Make_Integer_Literal (Loc, UI_From_Int (Eindx));

      --  Loop through entry families building the addition nodes

      Ent := First_Entity (Ttyp);
      while Present (Ent) loop
         if Ekind (Ent) = E_Entry_Family then
            Ecount :=
              Make_Op_Add (Loc,
                Left_Opnd  => Ecount,
                Right_Opnd =>
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      Make_Selected_Component (Loc,
                        Prefix => Make_Identifier (Loc, Name_uInit),
                        Selector_Name => Make_Identifier (Loc, Chars (Ent))),
                    Identifier => Make_Identifier (Loc, Name_Length)));
         end if;

         Ent := Next_Entity (Ent);
      end loop;

      Append_To (Args, Ecount);

      --  Master parameter. This is a reference to the _Master parameter of
      --  the initialization procedure.

      Append_To (Args, Make_Identifier (Loc, Name_uMaster));

      --  State parameter. This is a pointer to the task body procedure. We get
      --  the required value by taking the address of the task body procedure,
      --  and then converting it (with an unchecked conversion) to the type
      --  required by the task kernel. See description of Expand_Task_Body
      --  for further details.

      Append_To (Args,
        Make_Unchecked_Type_Conversion (Loc,
          Subtype_Mark => New_Reference_To (RTE (RE_Init_State), Loc),
          Expression =>
            Make_Attribute_Reference (Loc,
              Prefix => 
                Make_Identifier (Loc, New_External_Name (Tnam, "body")),
              Identifier => Make_Identifier (Loc, Name_Address))));

      --  Discriminants parameter. This is just the address of the task
      --  value record itself (which contains the discriminant values

      Append_To (Args,
        Make_Attribute_Reference (Loc,
          Prefix     => Make_Identifier (Loc, Name_uInit),
          Identifier => Make_Identifier (Loc, Name_Address)));

      --  Elaborated parameter. This is an access to the elaboration Boolean

      Append_To (Args,
        Make_Attribute_Reference (Loc,
          Prefix => 
            Make_Identifier (Loc, New_External_Name (Tnam, "elab")),
          Identifier => Make_Identifier (Loc, Name_Access)));

      --  Chain parameter. This is a reference to the _Chain parameter of
      --  the initialization procedure.

      Append_To (Args, Make_Identifier (Loc, Name_uChain));

      --  Created_Task parameter. This is the _Task_Id field of the task
      --  record value

      Append_To (Args,
        Make_Selected_Component (Loc,
          Prefix => Make_Identifier (Loc, Name_uInit),
          Selector_Name => Make_Identifier (Loc, Name_uTask_Id)));

      return
        Make_Procedure_Call_Statement (Loc,
          Name => New_Reference_To (RTE (RE_Create_Task), Loc),
          Parameter_Associations => Args);

   end Make_Task_Create_Call;

   --------------
   -- Task_Ref --
   --------------

   --  The expression returned for a task has the form:

   --    task___Value!(name)._Task_Id

   --  For the case of an access to a task, there is an extra explicit
   --  dereference:

   --    task___Value!(name.all)._Task_Id

   --  here task___Value is the type for the associated record, which
   --  contains the required _Task_Id field.

   function Task_Ref (N : Node_Id) return Node_Id is
      Loc  : constant Source_Ptr := Sloc (N);
      Ntyp : constant Entity_Id  := Etype (N);

   begin
      if Is_Access_Type (Ntyp) then
         return
           Make_Selected_Component (Loc,
             Prefix =>
               Make_Unchecked_Type_Conversion (Loc,
                 Subtype_Mark => New_Reference_To (
                     Task_Value_Type (Designated_Type (Ntyp)), Loc),
                 Expression => Make_Explicit_Dereference (Loc, N)),
             Selector_Name => Make_Identifier (Loc, Name_uTask_Id));

      else
         pragma Assert (Is_Task_Type (Ntyp), Compiler_Abort (N));

         return
           Make_Selected_Component (Loc,
             Prefix =>
               Make_Unchecked_Type_Conversion (Loc,
                 Subtype_Mark => 
                   New_Reference_To (Task_Value_Type (Ntyp), Loc),
                 Expression => N),
             Selector_Name => Make_Identifier (Loc, Name_uTask_Id));
      end if;
   end Task_Ref;

end Exp_Ch9;
