------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 6                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.99 $                             --
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

with Atree;        use Atree;
with Casing;       use Casing;
with Debug;        use Debug;
with Einfo;        use Einfo;
with Errout;       use Errout;
with Exp_Util;     use Exp_Util;
with Lib;          use Lib;
with Namet;        use Namet;
with Nmake;        use Nmake;
with Output;       use Output;
with Rtsfind;      use Rtsfind;
with Sem;          use Sem;
with Sem_Ch3;      use Sem_Ch3;
with Sem_Ch4;      use Sem_Ch4;
with Sem_Ch8;      use Sem_Ch8;
with Sem_Ch9;      use Sem_Ch9;
with Sem_Res;      use Sem_Res;
with Sem_Util;     use Sem_Util;
with Sem_Disp;     use Sem_Disp;
with Stand;        use Stand;
with Sinfo;        use Sinfo;
with Sinfo.Change; use Sinfo.Change;
with Snames;       use Snames;
with Stringt;      use Stringt;
with Tbuild;       use Tbuild;

package body Sem_Ch6 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Generic_Subprogram_Body (N : Node_Id; Gen_Id : Entity_Id);
   --  Analyze a generic subprogram body

   procedure Check_Conformance (Old_Id, New_Id : Entity_Id);
   --  Verify that the specifications given in a subprogram declaration
   --  conforms to that given in the body.

   procedure Enter_Overloaded_Entity (S : Entity_Id);
   --  This procedure makes S, a new overloaded entity, into the first
   --  visible entity with that name.

   procedure Establish_Traceback_Data (Loc : Source_Ptr; Decls : List_Id);
   --  Called from Analyze_Subprogram_Body to insert declarations for the
   --  traceback data (simple name of procedure and traceback names entry
   --  to reference this name and the name of the enclosing file). Loc is
   --  the source location for the created declarations, which are inserted
   --  at the start of Decls. On entry, Name_Buffer contains the subprogram
   --  name that is to be used.

   procedure Install_Entity (E : Entity_Id);
   --  Make single entity visible. Used for generic formals as well.

   procedure Install_Formals (Id : Entity_Id);
   --  On entry to a subprogram body, make the formals visible. Note
   --  that simply placing the subprogram on the scope stack is not
   --  sufficient: the formals must become the current entities for
   --  their names.

   procedure May_Need_Actuals (Fun : Entity_Id);
   --  Flag functions that can be called without parameters, i.e. those that
   --  have no parameters, or those for which defaults exist for all parameters

   procedure Valid_Operator_Definition (Designator : Entity_Id);
   --  Verify that an operator definition has the proper number of formals

   ---------------------------------------------
   -- Analyze_Abstract_Subprogram_Declaration --
   ---------------------------------------------

   procedure Analyze_Abstract_Subprogram_Declaration (N : Node_Id) is
      Designator : constant Entity_Id := Analyze_Spec (Specification (N));

   begin
      New_Overloaded_Entity (Designator);
      Set_Is_Abstract (Designator);
   end Analyze_Abstract_Subprogram_Declaration; 

   ----------------------------
   -- Analyze_Function_Call  --
   ----------------------------

   procedure Analyze_Function_Call (N : Node_Id) is
      P      : constant Node_Id := Name (N);
      L      : constant List_Id := Parameter_Associations (N);
      Actual : Node_Id;

   begin
      Analyze (P);

      --  If error analyzing name, then set Any_Type as result type and return

      if Etype (P) = Any_Type then
         Set_Etype (N, Any_Type);
         return;
      end if;

      --  Otherwise analyze the parameters

      if List_Present (L) then
         Actual := First (L);
         while Present (Actual) loop
            Analyze_Expression (Actual);
            Actual := Next (Actual);
         end loop;
      end if;

      Analyze_Call (N);

   end Analyze_Function_Call;

   -------------------------------------
   -- Analyze_Generic_Subprogram_Body --
   -------------------------------------

   procedure Analyze_Generic_Subprogram_Body
     (N : Node_Id; Gen_Id : Entity_Id)
   is
      Spec    : constant Node_Id     := Specification (N);
      Kind    : constant Entity_Kind := Ekind (Gen_Id);
      Nam     : Entity_Id;
      First_Generic_Formal : Entity_Id := First_Entity (Gen_Id);
      Last_Formal  : Entity_Id := Last_Entity (Gen_Id);

   begin
      --  Within the body of the generic, the subprogram is callable, and
      --  behaves like the corresponding non-generic unit.

      Nam := Defining_Unit_Simple_Name (Spec);

      if Kind = E_Generic_Procedure
        and then Nkind (Spec) /= N_Procedure_Specification
      then
         Error_Msg_N ("invalid body for generic procedure ", Nam);
         return;

      elsif Kind = E_Generic_Function
        and then Nkind (Spec) /= N_Function_Specification
      then
         Error_Msg_N ("invalid body for generic function ", Nam);
         return;
      end if;

      Set_Corresponding_Body (Parent (Parent (Gen_Id)), Nam);
      Set_Corresponding_Spec (N, Gen_Id);
      Set_Has_Completion (Nam);

      if Nkind (N) = N_Subprogram_Body_Stub then
         return;
      end if;

      --  Make generic parameters directly visible in the body. They are
      --  needed to process the formals declarations. Then make formals
      --  visible in a separate step.

      declare
         E : Entity_Id := First_Entity (Gen_Id);

      begin
         while Present (E)
           and then Ekind (E) not in  E_In_Parameter .. E_In_Out_Parameter
         loop
            Install_Entity (E);
            E := Next_Entity (E);
         end loop;

         --  Now generic formals are visible, and the specification can be
         --  analyzed, for subsequent conformance check.

         Nam := Analyze_Spec (Spec);

         if Present (E) then

            --  E is the first formal parameter, which must be the first
            --  entity in the subprogram body.

            Set_First_Entity (Gen_Id,  E);

            --  Now make formal parameters visible

            while Present (E)
              and then Ekind (E) in E_In_Parameter .. E_In_Out_Parameter
            loop
               Install_Entity (E);
               E := Next_Entity (E);
            end loop;
         end if;
      end;

      Append_Entity (Nam, Current_Scope);

      --  Visible generic entity is callable within its own body.

      Set_Ekind (Gen_Id, Ekind (Nam));
      New_Scope (Gen_Id);

      --  Check_Conformance (Gen_Id, Nam);

      Analyze_Declarations (Declarations (N));
      Check_Completion;
      Analyze (Handled_Statement_Sequence (N));
      End_Use (Declarations (N));

      --  Prior to exiting the scope, include generic formals again
      --  in the set of local entities.

      Set_First_Entity (Gen_Id, First_Generic_Formal);
      End_Scope;

      --  Chain local declarations to the id for the body, as for the
      --  non-generic case.

      if Present (Last_Formal) then
         if Present (Last_Entity (Nam)) then
            Set_Next_Entity (Last_Entity (Nam), Next_Entity (Last_Formal));
         else
            Set_First_Entity (Nam, Next_Entity (Last_Formal));
         end if;
         Set_Next_Entity (Last_Formal, Empty);

      --  Case of no generic parameters, no formals

      else
         Set_First_Entity (Nam, First_Entity (Gen_Id));
         Set_First_Entity (Gen_Id, Empty);
      end if;

      --  Outside of its body, unit is generic again.

      Set_Ekind (Gen_Id,  Kind);

   end Analyze_Generic_Subprogram_Body;

   -----------------------------
   -- Analyze_Operator_Symbol --
   -----------------------------

   --  This procedure only gets called if an operator symbol is analyzed in
   --  a context (e.g. in an expression) where an operator symbol is not
   --  semantically permissible. The scanner parses out all strings that are
   --  of the form of operator symbols as such. It is this routine that is
   --  in charge of correcting this "mistake" made by the scanner.

   procedure Analyze_Operator_Symbol (N : Node_Id) is
   begin
      Change_Operator_Symbol_To_String_Literal (N);
      Set_Etype (N, Any_String);
   end Analyze_Operator_Symbol;

   ----------------------------
   -- Analyze_Procedure_Call --
   ----------------------------

   procedure Analyze_Procedure_Call (N : Node_Id) is
      P       : constant Node_Id := Name (N);
      Actuals : constant List_Id := Parameter_Associations (N);
      Actual  : Node_Id;

   begin
      --  The syntactic construct : Prefix actual_parameters can denote a
      --  a procedure call or an entry call. The prefix may denote an
      --  access to subprogram type,  in which case an implicit dereference
      --  applies. If the prefix is an indexed component (without implicit
      --  dereference) then the construct denotes a call to a member of an
      --  entry family. If the prefix is a simple name, it may still denote
      --  a call to a parameterless member of an entry family. Resolution of
      --  these various interpretations is delicate.

      Analyze (P);

      --  If error analyzing prefix, then set Any_Type as result and return

      if Etype (P) = Any_Type then
         Set_Etype (N, Any_Type);
         return;
      end if;

      --  Otherwise analyze the parameters

      if List_Present (Actuals) then
         Actual := First (Actuals);

         while Present (Actual) loop
            Analyze_Expression (Actual);
            Actual := Next (Actual);
         end loop;
      end if;

      if Is_Name (P) then
         Analyze_Call (N);
         Resolve_Complete_Context (N, Standard_Void_Type);

      elsif Nkind (P) = N_Explicit_Dereference then
         if Ekind (Etype (P)) = E_Subprogram_Type then
            Analyze_Call (N);
            Resolve_Complete_Context (N, Standard_Void_Type);

         else
            Error_Msg_N ("expect access to procedure in call", P);
         end if;

      --  Then name can be a selected component or an indexed component
      --  that yields an access to subprogram. Such a prefix is legal if
      --  the call has parameter associations.

      elsif (Is_Access_Type (Etype (P))
             and then Ekind (Designated_Type (Etype (P))) = E_Subprogram_Type)
      then
         if List_Present (Actuals) then
            Analyze_Call (N);
            Resolve_Complete_Context (N, Standard_Void_Type);
         else
            Error_Msg_N ("missing explicit dereference in call ", N);
         end if;

      --  If not an access to subprogram, then the prefix must resolve to
      --  the name of an entry, entry family, or protected operation.

      elsif Nkind (P) = N_Selected_Component then
         Analyze_Entry_Call (N);

      else
         Error_Msg_N ("Invalid procedure or entry call", N);
      end if;
   end Analyze_Procedure_Call;

   ------------------
   -- Analyze_Spec --
   ------------------

   function Analyze_Spec (N : Node_Id) return Entity_Id is
      Designator : constant Entity_Id := Defining_Unit_Simple_Name (N);
      Formals    : constant List_Id := Parameter_Specifications (N);

   begin
      if Nkind (N) = N_Function_Specification then
         Set_Ekind (Designator, E_Function);
         Find_Type (Subtype_Mark (N));
         Set_Etype (Designator, Entity (Subtype_Mark (N)));

      else
         Set_Ekind (Designator, E_Procedure);
         Set_Etype (Designator, Standard_Void_Type);
      end if;

      if List_Present (Formals) then
         Set_Scope (Designator, Current_Scope);
         New_Scope (Designator);
         Process_Formals (Designator, Formals);
         End_Scope;
      end if;

      if Nkind (N) = N_Function_Specification then
         if Nkind (Designator) = N_Defining_Operator_Symbol then
            Valid_Operator_Definition (Designator);
         end if;

         May_Need_Actuals (Designator);

         if Is_Abstract (Etype (Designator)) 
           and then Nkind (Parent (N)) /= N_Abstract_Subprogram_Declaration
         then Error_Msg_N 
            ("function that returns abstract type must be abstract", N); 
         end if;
      end if;

      return Designator;
   end Analyze_Spec;

   -----------------------------
   -- Analyze_Subprogram_Body --
   -----------------------------

   --  This procedure is called for regular subprogram bodies, generic bodies,
   --  and for subprogram stubs of both kinds. In the case of stubs, only the
   --  specification matters, and is used to create a proper declaration for
   --  the subprogram, or to perform conformance checks.

   procedure Analyze_Subprogram_Body (N : Node_Id) is
      Spec        : constant Node_Id    := Specification (N);
      Nam         : constant Entity_Id  := Defining_Unit_Simple_Name (Spec);
      Gen_Id      : constant Entity_Id  := Current_Entity (Nam);
      Decls       : constant List_Id    := Declarations (N);
      Loc         : Source_Ptr;
      Subp        : Entity_Id;
      Prev        : Entity_Id;
      Last_Formal : Entity_Id;
      Vsn_Name    : Name_Id;

   begin
      Trace_Scope (N, Nam, " Analyze subprogram");
      Set_Ekind (Nam, E_Subprogram_Body);

      --  Generic subprograms are handled separately. They always have a
      --  generic specification.

      if Present (Gen_Id)
        and then (Ekind (Gen_Id) = E_Generic_Procedure
                    or else Ekind (Gen_Id) = E_Generic_Function)
      then
         Analyze_Generic_Subprogram_Body (N, Gen_Id);
         return;

      --  Non-generic case, find the subprogram declaration, if one was
      --  seen, or enter new overloaded entity in the current scope.

      else
         Subp := Analyze_Spec (Spec);
         Prev := Find_Corresponding_Spec (N);
      end if;

      if Debug_Flag_B then
         if Is_Empty_List (Decls) then
            Loc := Sloc (N);
         else
            Loc := Sloc (First (Decls));
         end if;

         Get_Name_String (Chars (Nam));
         Establish_Traceback_Data (Loc, Decls);
      end if;

      --  Place subprogram on scope stack, and make formals visible. If there
      --  is a spec, the visible entity remains that of the spec. The defining
      --  entity for the body is entered in the chain of entities in that case,
      --  to insure that it is instantiated if it appears in  a generic unit.

      if Present (Prev) then
         Check_Conformance (Prev, Subp);

         if Nkind (N) /= N_Subprogram_Body_Stub then
            Set_Corresponding_Spec (N, Prev);
         end if;

         Set_Corresponding_Body (Get_Declaration_Node (Prev), Subp);
         Install_Formals (Prev);
         Append_Entity (Subp, Current_Scope);
         Last_Formal := Last_Entity (Prev);
         New_Scope (Prev);

      else
         New_Overloaded_Entity (Subp);
         Install_Formals (Subp);
         New_Scope (Subp);
      end if;

      Set_Has_Completion (Subp);

      if Nkind (N) = N_Subprogram_Body_Stub then
         End_Scope; -- nothing else to process

      else
         Analyze_Declarations (Declarations (N));
         Check_Completion;

         --  Add traceback call to start of statements in statement 
         --  sequence if the traceback flag is set. This is done at
         --  this point (rather than in N_Expand_Subprogram_Body) so
         --  that Version_B is properly visible for a library subprogram.

         if Debug_Flag_B then
            Traceback_Store
              (First (Statements (Handled_Statement_Sequence (N))), False);
         end if;

         Analyze (Handled_Statement_Sequence (N));
         End_Use (Declarations (N));
         End_Scope;

         if Present (Prev) then

            --  Chain the declared entities on the id for the body.
            --  The id for the spec only holds the formals.

            if Present (Last_Formal) then
               Set_Next_Entity
                  (Last_Entity (Subp), Next_Entity (Last_Formal));
               Set_Next_Entity (Last_Formal, Empty);

            else
               Set_First_Entity (Subp, First_Entity (Prev));
               Set_First_Entity (Prev, Empty);
            end if;
         end if;
      end if;
   end Analyze_Subprogram_Body;

   ------------------------------------
   -- Analyze_Subprogram_Declaration --
   ------------------------------------

   procedure Analyze_Subprogram_Declaration (N : Node_Id) is
      Designator : constant Entity_Id := Analyze_Spec (Specification (N));
      F          : Entity_Id;

   begin
      Trace_Scope (N, Defining_Unit_Simple_Name (Specification (N)),
                                         " Analyze subprogram spec. ");

      New_Overloaded_Entity (Designator);

      --  If a type in the profile depends on a private type without a full
      --  declaration, indicate that the subprogram is delayed.

      if Has_Private_Component (Etype (Designator)) then
         Set_Is_Delayed (Designator);
      else
         F := First_Formal (Designator);
         while Present (F) loop
            if Has_Private_Component (Etype (F)) then
               Set_Is_Delayed (Designator);
               exit;
            end if;
            F := Next_Formal (F);
         end loop;
      end if;

   end Analyze_Subprogram_Declaration;

   -----------------------
   -- Check_Conformance --
   -----------------------

   procedure Check_Conformance (Old_Id, New_Id : Entity_Id) is
      Old_Spec : constant List_Id :=
        Parameter_Specifications 
               (Specification (Get_Declaration_Node (Old_Id)));

      New_Spec : constant List_Id :=
        Parameter_Specifications 
               (Specification (Get_Declaration_Node (New_Id)));

      Old_Param_Spec : Node_Id;
      New_Param_Spec : Node_Id;
      Old_Formal     : Entity_Id;
      New_Formal     : Entity_Id;

      procedure Conformance_Error is
      begin
         Error_Msg_Sloc_1 := Sloc (Old_Id);
         Error_Msg_N ("no conformance with line #", New_Id);
      end Conformance_Error;

   --  Start of processing for Check_Conformance

   begin
      if not List_Present (Old_Spec) then
         if not List_Present (New_Spec) then
            return;
         else
            Conformance_Error;
         end if;

      else
         Old_Param_Spec := First (Old_Spec);
         New_Param_Spec := First (New_Spec);

         while Present (Old_Param_Spec)
           and then Present (New_Param_Spec)
         loop

            Old_Formal := Defining_Identifier (Old_Param_Spec);
            New_Formal := Defining_Identifier (New_Param_Spec);

            if Chars (Old_Formal) = Chars (New_Formal)
              and then Ekind (Old_Formal) = Ekind (New_Formal)

              and then (Etype (Old_Formal) = Etype (New_Formal)

                or else
                    Full_Declaration (Etype (Old_Formal))
                                                  = Etype (New_Formal)

                or else (Ekind (Etype (Old_Formal)) = E_Anonymous_Access_Type
                and then Ekind (Etype (New_Formal)) = E_Anonymous_Access_Type
                and then Directly_Designated_Type (Etype (Old_Formal)) =
                Directly_Designated_Type (Etype (New_Formal))))
            then
               null;
            else
               Conformance_Error;
               return;
            end if;

            --  Here is where we check the More_Ids and Prev_Ids flags to
            --  make sure they match. This catches a misconformance like

            --    A,B : Integer
            --    A : Integer; B : Integer

            --  which are represented identically in the tree except for the
            --  setting of these flags.

            if More_Ids (Old_Param_Spec) /= More_Ids (New_Param_Spec)
              or else Prev_Ids (Old_Param_Spec) /= Prev_Ids (New_Param_Spec)
            then
               Conformance_Error;
            end if;

            Old_Param_Spec := Next (Old_Param_Spec);
            New_Param_Spec := Next (New_Param_Spec);
         end loop;
      end if;
   end Check_Conformance;

   -----------------------------
   -- Enter_Overloaded_Entity --
   -----------------------------

   procedure Enter_Overloaded_Entity (S : Entity_Id) is
      E : Entity_Id;

   begin
      E := Current_Entity (S);
      Set_Is_Directly_Visible (S);
      Set_Current_Entity (S);
      Set_Homonym (S, E);

      if Present (E) and then Scope (E) = Current_Scope then
         Set_Has_Homonym (E);
         Set_Has_Homonym (S);
      end if;

      Append_Entity (S, Current_Scope);
      Check_Dispatching_Operation (S);
      Set_Public_Status (S);

      if Debug_Flag_E then
         Write_Str ("New overloaded entity chain: ");
         Write_Name (Chars (S));
         E := S;

         while Present (E) loop
            Write_Str (" "); Write_Int (Int (E));
            E := Homonym (E);
         end loop;

         Write_Eol;
      end if;
   end Enter_Overloaded_Entity;

   ------------------------------
   -- Establish_Traceback_Data --
   ------------------------------

   --  If the debug B flag is set then establish the traceback data

   --    _TB_Snam : aliased constant String := "subprogram name" & NUL;

   --  This declaration is placed at the start of the subprogram declarations.

   procedure Establish_Traceback_Data
     (Loc : Source_Ptr; Decls : List_Id)
   is
      Unit_Num  : constant Unit_Number_Type := Get_Sloc_Unit_Number (Loc);
      Unit_Node : Node_Id;
      Str       : String_Id;
      Vsn_Name  : Name_Id;

   begin
      --  Nothing to do if not in main unit (no point in generating calls
      --  for other than the main unit, since we won't generate code anyway)

      if Unit_Num /= Main_Unit then
         return;
      end if;

      --  Build string constant for _TB_Snam declaration

      Set_Casing (File.Table (Unit_Num).Identifier_Casing, Mixed_Case);
      Start_String;

      for I in 1 .. Name_Len loop
         Store_String_Char (Get_Char_Code (Name_Buffer (I)));
      end loop;

      Store_String_Char (Get_Char_Code (NUL));
      Str := End_String;

      --  Insert declaration of _TB_Snam

      Prepend_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_uTB_Snam),
          Aliased_Present     => True,
          Constant_Present    => True,
          Object_Definition   => New_Reference_To (Standard_String, Loc),
          Expression          => Make_String_Literal (Loc, Str)));

   end Establish_Traceback_Data;

   -----------------------------
   -- Find_Corresponding_Spec --
   -----------------------------

   function Find_Corresponding_Spec (N : Node_Id) return Entity_Id is
      Spec       : constant Node_Id   := Specification (N);
      Designator : constant Entity_Id := Defining_Unit_Simple_Name (Spec);
      E          : Entity_Id;

   begin
      E := Current_Entity (Designator);

      while Present (E) loop

         if Scope (E) = Current_Scope
           and then Type_Conformant (E, Designator)
         then
            if not Has_Completion (E) then

               if Nkind (N) /= N_Subprogram_Body_Stub then
                  Set_Corresponding_Spec (N, E);
               end if;

               Set_Has_Completion (E);
               return E;

            --  If body already exists, this is an error unless the
            --  previous declaration is the implicit declaration of
            --  a derived subprogram.

            elsif No (Alias (E)) then
               Error_Msg_N ("duplicate subprogram body", N);
            end if;
         end if;

         E := Homonym (E);
      end loop;

      --  On exit, we know that no previous declaration of subprogram exists

      return Empty;
   end Find_Corresponding_Spec;

   --------------------
   -- Install_Entity --
   --------------------

   procedure Install_Entity (E : Entity_Id) is
      Prev : constant Entity_Id := Current_Entity (E);

   begin
      Set_Is_Directly_Visible (E);
      Set_Current_Entity (E);
      Set_Homonym (E, Prev);
   end Install_Entity;

   ---------------------
   -- Install_Formals --
   ---------------------

   procedure Install_Formals (Id : Entity_Id) is
      F : Entity_Id;

   begin
      F := First_Formal (Id);

      while Present (F) loop
         Install_Entity (F);
         F := Next_Formal (F);
      end loop;
   end Install_Formals;

   ----------------------
   -- May_Need_Actuals --
   ----------------------

   procedure May_Need_Actuals (Fun : Entity_Id) is
      F : Entity_Id;
      B : Boolean;

   begin
      F := First_Formal (Fun);
      B := True;

      while Present (F) loop
         if No (Default_Value (F)) then
            B := False;
            exit;
         end if;

         F := Next_Formal (F);
      end loop;

      Set_Needs_No_Actuals (Fun, B);
   end May_Need_Actuals;

   ---------------------
   -- Mode_Conformant --
   ---------------------

   function Mode_Conformant (S1, S2 : Entity_Id) return Boolean is
      P1 : Entity_Id;
      P2 : Entity_Id;

      function Same_Type (T1 : Entity_Id; T2 : Entity_Id) return Boolean;
      --  Check whether T1 and T2 are the same type (for conformance purposes)

      function Same_Type (T1 : Entity_Id; T2 : Entity_Id) return Boolean is
      begin
         --  For a general access type, the designated subtypes must
         --  match statically.

         return Base_Type (T1) = Base_Type (T2)
           or else (Ekind (T1) = E_Anonymous_Access_Type
             and then Ekind (T2) = E_Anonymous_Access_Type
             and then Directly_Designated_Type (T1) =
                      Directly_Designated_Type (T2));
      end Same_Type;

   --  Start of processing for Mode_Conformant

   begin
      P1 := First_Formal (S1);
      P2 := First_Formal (S2);

      if not Same_Type (Etype (S1), Etype (S2)) then
         return False;

      else
         while Present (P1) and then Present (P2) loop
            if not Same_Type (Etype (P1), Etype (P2))
              or else Ekind (P1) /= Ekind (P2)
            then
               return False;
            end if;

            P1 := Next_Formal (P1);
            P2 := Next_Formal (P2);
         end loop;

         --  Both must be empty on exit from list to return True

         return P1 = P2;
      end if;
   end Mode_Conformant;

   ---------------------------
   -- New_Overloaded_Entity --
   ---------------------------

   procedure New_Overloaded_Entity (S : Entity_Id) is
      E        : Entity_Id := Current_Entity (S);
      Prev_Vis : Entity_Id := Empty;

   begin
      if No (E) or else Scope (E) /= Current_Scope then
         Enter_Overloaded_Entity (S);

      elsif not Is_Overloadable (E) then
         Error_Msg_N ("duplicate identifier:&", S);

      else
         --  E exists and is overloadable. Determine whether S is the body
         --  of E, a new overloaded entity with a different signature, or
         --  an error altogether.

         while Present (E) and then Scope (E) = Current_Scope loop
            if Type_Conformant (E, S) then

               --  If the old and new entities have the same profile and
               --  one is not the body of the other, then this is an error,
               --  unless one of them is implicitly declared.

               if Present (Alias (S)) 
                 or else (Is_Internal (S) and then Chars (S) = Name_Op_Eq)
               then

                  --  S is a derived operation, which remains hidden by
                  --  an existing declaration.

                  return;

               elsif Present (Alias (E)) 
                  or else (Is_Internal (E) and then Chars (E) = Name_Op_Eq)
               then

                  --  E is a derived operation which is being overridden.
                  --  Remove E from further visibility. Furthermore,  if
                  --  E is a dispatching operation, it must be replaced in
                  --  the list of primitive operations of its type.

                  declare
                     Prev : Entity_Id;

                  begin
                     Prev := First_Entity (Current_Scope);

                     while Next_Entity (Prev) /= E loop
                        Prev := Next_Entity (Prev);
                     end loop;

                     --  E must be removed both from the entity_list of the
                     --  current scope, and from the visibility chain

                     if Debug_Flag_E then
                        Write_Str ("Override implicit operation ");
                        Write_Int (Int (E));
                        Write_Eol;
                     end if;

                     if Prev_Vis /= Empty then
                        Set_Homonym (Prev_Vis, Homonym (E));
                        --  Skip E in the visibility chain

                     else
                        Set_Name_Entity_Id (Chars (E), Homonym (E));
                     end if;

                     Set_Next_Entity (Prev, Next_Entity (E));
                     if No (Next_Entity (Prev)) then
                        Set_Last_Entity (Current_Scope, Prev);
                     end if;

                     Enter_Overloaded_Entity (S);

                     if Is_Dispatching_Operation (E) then
                        Override_Dispatching_Operation (E, S);
                     end if;

                     return;
                  end;

               else
                  Error_Msg_Sloc_1 := Sloc (E);
                  Error_Msg_NE
                    ("declaration of& conflicts with line #", S, S);
                  return;
               end if;

            else
               null;
            end if;

            Prev_Vis := E;
            E := Homonym (E);
         end loop;

         --  On exit, we know that S is a new entity

         Enter_Overloaded_Entity (S);
      end if;

   end New_Overloaded_Entity;

   ---------------------
   -- Process_Formals --
   ---------------------

   procedure Process_Formals (S : Entity_Id; T : List_Id) is
      Param_Spec  : Node_Id;
      Formal      : Entity_Id;
      Formal_Type : Entity_Id;
      Default     : Node_Id;

   begin
      --  In order to prevent premature use of the formals in the same formal
      --  part, the Ekind is left undefined until all default expressions are
      --  analyzed. The Ekind is established in a separate loop at the end.

      Param_Spec := First (T);

      while Present (Param_Spec) loop
         if Nkind (Parameter_Type (Param_Spec)) /= N_Access_Definition then

            --  Ordinary parameters

            Find_Type (Parameter_Type (Param_Spec));
            Formal_Type := Entity (Parameter_Type (Param_Spec));

            if Ekind (Formal_Type) = E_Incomplete_Type then
               Error_Msg_N
                  ("invalid use of incomplete type&", 
                                             Parameter_Type (Param_Spec));
            end if;

         else
            --  An access formal type

            Formal_Type := Access_Definition (Parameter_Type (Param_Spec));
         end if;

         Formal := Defining_Identifier (Param_Spec);
         Enter_Name (Formal);
         Set_Etype (Formal, Formal_Type);

         Default :=  Expression (Param_Spec);

         if Present (Default) then
            Analyze_Expression (Default);
            Resolve_Complete_Context (Default, Formal_Type);

            if Out_Present (Param_Spec) then
               Error_Msg_N
                 ("default initialization only allowed for IN parameters",
                  Param_Spec);
            end if;
         end if;

         Param_Spec := Next (Param_Spec);
      end loop;

      --  Now set the kind (mode) of each formal

      Param_Spec := First (T);

      while Present (Param_Spec) loop
         Formal := Defining_Identifier (Param_Spec);
         Set_Formal_Mode (Formal);

         if Ekind (Formal) = E_In_Parameter then
            Set_Default_Value (Formal, Expression (Param_Spec));
         end if;

         Param_Spec := Next (Param_Spec);
      end loop;

   end Process_Formals;

   ---------------------
   -- Set_Formal_Mode --
   ---------------------

   procedure Set_Formal_Mode (Formal_Id : Entity_Id) is
      Spec : constant Node_Id := Parent (Formal_Id);

   begin
      if Out_Present (Spec) then

         if Ekind (Scope (Formal_Id)) = E_Function
           or else Ekind (Scope (Formal_Id)) = E_Generic_Function
         then
            Error_Msg_N ( "functions can only have IN parameters ", Spec);
            Set_Ekind (Formal_Id, E_In_Parameter);

         elsif In_Present (Spec) then
            Set_Ekind (Formal_Id, E_In_Out_Parameter);

         else
            Set_Ekind (Formal_Id, E_Out_Parameter);
         end if;

      else
         Set_Ekind (Formal_Id, E_In_Parameter);
      end if;
   end Set_Formal_Mode;

   ---------------------
   -- Type_Conformant --
   ---------------------

   function Type_Conformant (S1, S2 : Entity_Id) return Boolean is
      P1 : Entity_Id;
      P2 : Entity_Id;

      function Same_Type (T1, T2 : Entity_Id) return Boolean;
      --  If the specification of the subprogram includes private types,
      --  the body may appear where the full declaration is visible. Both
      --  views of the type are still conformant.

      function Same_Type (T1, T2 : Entity_Id) return Boolean is
      begin
         return Base_Type (T1) = Base_Type (T2)

           or else (Ekind (T1) = E_Anonymous_Access_Type
           and then Ekind (T2) = E_Anonymous_Access_Type
           and then Same_Type (Directly_Designated_Type (T1),
             Directly_Designated_Type (T2)));

      end Same_Type;

   --  Start of processing for Type_Conformant

   begin
      if not Same_Type (Etype (S1), Etype (S2)) then
         return False;

      else
         P1 := First_Formal (S1);
         P2 := First_Formal (S2);

         while Present (P1) and then Present (P2) loop
            if not Same_Type (Etype (P1), Etype (P2)) then
               return False;
            end if;

            P1 := Next_Formal (P1);
            P2 := Next_Formal (P2);
         end loop;

         --  Both must be empty on exit from list to return True

         return (P1 = P2);
      end if;
   end Type_Conformant;

   -------------------------------
   -- Valid_Operator_Definition --
   -------------------------------

   procedure Valid_Operator_Definition (Designator : Entity_Id) is
      N    : Integer := 0;
      F    : Entity_Id;
      Id   : constant Name_Id := Chars (Designator);
      N_OK : Boolean;

   begin
      F := First_Formal (Designator);

      while Present (F) loop
         N := N + 1;

         if Present (Default_Value (F)) then
            Error_Msg_N
              ("default values not allowed for operator parameters",
               Parent (F));
         end if;

         F := Next_Formal (F);
      end loop;

      --  Verify that user-defined operators have proper number of arguments
      --  First case of operators which can only be unary

      if Id = Name_Op_Not
        or else Id = Name_Op_Abs
      then
         N_OK := (N = 1);

      --  Case of operators which can be unary or binary

      elsif Id = Name_Op_Add
        or Id = Name_Op_Subtract
      then
         N_OK := (N in 1 .. 2);

      --  All other operators can only be binary

      else
         N_OK := (N = 2);
      end if;

      if not N_OK then
         Error_Msg_N
           ("incorrect number of arguments for operator", Designator);
      end if;

      --  TBSL: no explicit definition of inequality can return Boolean ???
   end Valid_Operator_Definition;

end Sem_Ch6;
