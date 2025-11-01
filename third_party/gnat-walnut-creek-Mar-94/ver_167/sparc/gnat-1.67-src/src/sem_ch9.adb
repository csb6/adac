------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 9                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.47 $                             --
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
with Opt;      use Opt;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch4;  use Sem_Ch4;
with Sem_Ch5;  use Sem_Ch5;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;

package body Sem_Ch9 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Install_Declarations (Spec : Entity_Id);
   --  Utility to make visible in corresponding body the entities defined
   --  in task, protected type declaration, or entry declaration.

   -----------------------
   -- Analyze_Task_Type --
   -----------------------

   procedure Analyze_Task_Type (N : Node_Id) is
      Id : Node_Id := Defining_Identifier (N);

   begin
      Enter_Name (Id);
      Set_Ekind (Id, E_Task_Type);
      Set_Has_Tasks (Id, True);
      Set_Esize (Id, UI_From_Int (System_Address_Size));
      Set_Etype (Id, Id);
      Set_Is_Limited_Type (Id);
      New_Scope (Id);

      if List_Present (Discriminant_Specifications (N)) then
         if Ada_83 then
            Error_Msg_N ("task discriminants not supported in Ada83", N);
         end if;

         Process_The_Discriminants (N);
      end if;

      if Present (Task_Definition (N)) then
         Analyze_Task_Definition (Task_Definition (N));
      end if;

      End_Scope;
   end Analyze_Task_Type;

   -------------------------
   -- Analyze_Single_Task --
   -------------------------

   procedure Analyze_Single_Task (N : Node_Id) is
      Id : constant Node_Id   := Defining_Identifier (N);
      T  : constant Entity_Id := New_Implicit_Type (Sloc (N), Id, "task_type");

   begin
      --  The anonymous task type is implicitly declared before the 
      --  current declaration.

      Install_Implicit_Types (N);
      Enter_Name (Id);
      Set_Ekind (Id, E_Constant);
      Set_Ekind (T, E_Task_Type);
      Set_Etype (T, T);
      Set_Esize (T, UI_From_Int (System_Address_Size));
      Set_Is_Limited_Type (T);
      Set_Etype (Id, T);
      Set_Has_Tasks (T, True);

      if Present (Task_Definition (N)) then
         New_Scope (T);
         Analyze_Task_Definition (Task_Definition (N));
         End_Scope;
      end if;
   end Analyze_Single_Task;

   -----------------------------
   -- Analyze_Task_Definition --
   -----------------------------

   procedure Analyze_Task_Definition (N : Node_Id) is
      L : Entity_Id;
      E_Index : Uint := Uint_1;

   begin
      if List_Present (Visible_Declarations (N)) then
         Analyze_Declarations (Visible_Declarations (N));
      end if;

      if List_Present (Private_Declarations (N)) then
         L := Last_Entity (Current_Scope);
         Analyze_Declarations (Private_Declarations (N));

         if Present (L) then
            Set_First_Private_Entity (Current_Scope,  Next_Entity (L));
         else
            Set_First_Private_Entity (Current_Scope,
                                             First_Entity (Current_Scope));
         end if;
      end if;

      --  Assign consecutive Entry_Index values to single entries.

      L := First_Entity (Current_Scope);

      while Present (L) loop
         if Ekind (L) = E_Entry then
            Set_Entry_Index (L, E_Index);
            E_Index := UI_Sum (E_Index,  Uint_1); 
         end if;

         L := Next_Entity (L);
      end loop;

      --  The entry index of entry families is one greater than that of
      --  the last single entry. 

      L := First_Entity (Current_Scope);

      while Present (L) loop
         if Ekind (L) = E_Entry_Family then
            Set_Entry_Index (L, E_Index);
         end if;

         L := Next_Entity (L);
      end loop;

   end Analyze_Task_Definition;

   -----------------------
   -- Analyze_Task_Body --
   -----------------------

   procedure Analyze_Task_Body (N : Node_Id) is
      Body_Id : Entity_Id := Defining_Identifier (N);
      Spec_Id : Entity_Id := Current_Entity (Body_Id);

   begin
      Set_Ekind (Body_Id, E_Task_Body);

      if No (Spec_Id)
        or else Scope (Spec_Id) /= Current_Scope
        or else Ekind (Etype (Spec_Id)) /= E_Task_Type
      then
         Error_Msg_N ("missing specification for task body", Body_Id);
         return;
      end if;

      if Ekind (Spec_Id) = E_Constant then
         --  The declarations are attached to the type (in the case of a
         --  single task, to the implicit type of the task). I do not
         --  understand this comment in this context (RBKD -- TBSL)
         Spec_Id := Etype (Spec_Id);
      end if;

      New_Scope (Spec_Id);
      Set_Corresponding_Spec (N, Spec_Id);
      Set_Has_Completion (Spec_Id);
      Install_Declarations (Spec_Id);

      Analyze_Declarations (Declarations (N));
      Analyze (Handled_Statement_Sequence (N));
      End_Use (Declarations (N));
      End_Scope;
   end Analyze_Task_Body;

   ----------------------------
   -- Analyze_Protected_Type --
   ----------------------------

   procedure Analyze_Protected_Type (N : Node_Id) is
      Id : Node_Id := Defining_Identifier (N);

   begin
      Enter_Name (Id);
      Set_Ekind (Id, E_Protected_Type);
      Set_Etype (Id, Id);
      Set_Is_Limited_Type (Id);
      New_Scope (Id);

      if List_Present (Discriminant_Specifications (N)) then
         Process_The_Discriminants (N);
      end if;

      Analyze (Protected_Definition (N));
      End_Scope;
   end Analyze_Protected_Type;

   ------------------------------
   -- Analyze_Single_Protected --
   ------------------------------

   procedure Analyze_Single_Protected (N : Node_Id) is
      Id : constant Node_Id   := Defining_Identifier (N);
      T  : constant Entity_Id := New_Implicit_Type 
                                   (Sloc (N), Id, "protected_type");

   begin
      --  The anonymous protected type is implicitly declared before the 
      --  current declaration.
      Install_Implicit_Types (N);
      Enter_Name (Id);
      Set_Ekind (Id, E_Protected_Object);
      Set_Ekind (T, E_Protected_Type);
      Set_Etype (T, T);
      Set_Is_Limited_Type (T);
      Set_Etype (Id, T);
      New_Scope (T);
      Analyze (Protected_Definition (N));
      End_Scope;
   end Analyze_Single_Protected;

   ----------------------------------
   -- Analyze_Protected_Definition --
   ----------------------------------

   procedure Analyze_Protected_Definition (N : Node_Id) is
      L : Entity_Id;

   begin
      Analyze_Declarations (Visible_Declarations (N));

      if List_Present (Private_Declarations (N)) then
         L := Last_Entity (Current_Scope);
         Analyze_Declarations (Private_Declarations (N));
         Set_First_Private_Entity (Current_Scope,  Next_Entity (L));
      end if;
   end Analyze_Protected_Definition;

   ----------------------------
   -- Analyze_Protected_Body --
   ----------------------------

   procedure Analyze_Protected_Body (N : Node_Id) is
      Body_Id : Entity_Id := Defining_Identifier (N);
      Spec_Id : Entity_Id := Current_Entity (Body_Id);

   begin
      Set_Ekind (Body_Id, E_Protected_Body);

      if No (Spec_Id)
        or else Scope (Spec_Id) /= Current_Scope
        or else Ekind (Etype (Spec_Id)) /= E_Protected_Type
      then
         Error_Msg_N ("missing specification for protected body", Body_Id);
         return;
      end if;

      --  The declarations are always attached to the type

      if Ekind (Spec_Id) = E_Protected_Object then
         Spec_Id := Etype (Spec_Id);
      end if;

      New_Scope (Spec_Id);
      Set_Has_Completion (Spec_Id);
      Install_Declarations (Spec_Id);

      Analyze_Declarations (Declarations (N));
      End_Scope;
   end Analyze_Protected_Body;

   -------------------------------
   -- Analyze_Entry_Declaration --
   -------------------------------

   procedure Analyze_Entry_Declaration (N : Node_Id) is
      Id      : Entity_Id := Defining_Identifier (N);
      D_Sdef  : Node_Id   := Discrete_Subtype_Definition (N);
      Formals : List_Id   := Parameter_Specifications (N);

   begin
      if No (D_Sdef) then
         Set_Ekind (Id, E_Entry);
      else
         Enter_Name (Id);
         Set_Ekind (Id, E_Entry_Family);
         Analyze (D_Sdef);
         Make_Index (D_Sdef, Id);
      end if;

      Set_Etype (Id, Standard_Void_Type);

      if List_Present (Formals) then
         New_Scope (Id);
         Process_Formals (Id, Formals);
         End_Scope;
      end if;

      if Ekind (Id) = E_Entry then
         New_Overloaded_Entity (Id);
      end if;

   end Analyze_Entry_Declaration;

   ------------------------
   -- Analyze_Entry_Call --
   ------------------------

   procedure Analyze_Entry_Call (N : Node_Id) is
      Actuals    : constant List_Id := Parameter_Associations (N);
      Nam        : constant Node_Id := Name (N);
      Entry_Name : Entity_Id;

   begin
      --  The parser only builds procedure calls. After analyzing the prefix
      --  we know that this is an entry call. Rebuild the node accordingly.
      --  Resolve possible syntactic ambiguities between calls to single
      --  entries,  and calls to parameterless entry families.

      if Nkind (Nam) = N_Selected_Component then
         Entry_Name := Entity (Selector_Name (Nam));
      elsif Nkind (Nam) = N_Indexed_Component then
         Unimplemented (N, "entry family calls");
         return;
      else
         --  TBSL, the following is a junk error message!
         Unimplemented (N, "this call");
         return;
      end if;

      Set_Name (N,  New_Occurrence_Of (Entry_Name,  Sloc (N)));
      Analyze_Call (N);
      Resolve_Complete_Context (N, Standard_Void_Type);
      Change_Node (N, N_Entry_Call_Statement);
      Set_Name (N, Nam);
      Set_Parameter_Associations (N, Actuals);

      --  The prefix denotes a task or protected object that contains
      --  the entry. Both the entry and the prefix may be overloaded.
      --  If this is an external call, then after resolving the entry,
      --  use the type of its scope to complete the resolution of its
      --  prefix. (TBD).
   end Analyze_Entry_Call;

   ------------------------
   -- Analyze_Entry_Body --
   ------------------------

   procedure Analyze_Entry_Body (N : Node_Id) is
      Id         : constant Entity_Id := Defining_Identifier (N);
      Decls      : constant List_Id   := Declarations (N);
      Stats      : constant Node_Id   := Handled_Statement_Sequence (N);
      Cond       : constant Node_Id   := Condition (N);
      Entry_Name : Entity_Id;
      E          : Entity_Id;

   begin
      --  Entry_Name is initialized to Any_Id. It should get reset to the
      --  matching entry entity. An error is signalled if it is not reset

      Entry_Name := Any_Id;

      Analyze (Entry_Body_Formal_Part (N));

      if Present (Entry_Index_Specification (Entry_Body_Formal_Part (N))) then
         Set_Ekind (Id, E_Entry_Family);
      else
         Set_Ekind (Id, E_Entry);
      end if;

      Set_Etype (Id, Standard_Void_Type);

      E := First_Entity (Current_Scope);
      while Present (E) loop
         if Chars (E) = Chars (Id)
           and then (Ekind (E) = Ekind (Id))
           and then Type_Conformant (E, Id)
         then
            Entry_Name := E;
            exit;
         end if;

         E := Next_Entity (E);
      end loop;

      if Entry_Name = Any_Id then
         Error_Msg_N ("no entry declaration matches entry body",  N);
         return;
      else
         Set_Has_Completion (Entry_Name);
      end if;

      if Present (Cond) then
         Analyze (Cond);
         Resolve_Complete_Context (Cond, Any_Boolean);
      end if;

      New_Scope (Entry_Name);

      if List_Present (Decls) then
         Install_Declarations (Entry_Name);
         Analyze_Declarations (Decls);
      end if;

      if Present (Stats) then
         Analyze (Stats);
      end if;

      End_Scope;
   end Analyze_Entry_Body;

   ------------------------
   -- Analyze_Entry_Body --
   ------------------------

   procedure Analyze_Entry_Body_Formal_Part (N : Node_Id) is
      Id         : constant Entity_Id := Defining_Identifier (Parent (N));
      Index      : constant Node_Id   := Entry_Index_Specification (N);
      Formals    : constant List_Id   := Parameter_Specifications (N);

   begin
      if Present (Index) then
         Analyze (Index);
      end if;

      if List_Present (Formals) then
         New_Scope (Id);
         Process_Formals (Id, Formals);
         End_Scope;
      end if;
   end Analyze_Entry_Body_Formal_Part;

   ---------------------------------------
   -- Analyze_Entry_Index_Specification --
   ---------------------------------------

   procedure Analyze_Entry_Index_Specification (N : Node_Id) is
   begin
      Unimplemented (N, "entry index specification");
   end Analyze_Entry_Index_Specification;

   ------------------------------
   -- Analyze_Accept_Statement --
   ------------------------------

   procedure Analyze_Accept_Statement (N : Node_Id) is
      Ityp       : Entity_Id;
      Entry_Name : Entity_Id;
      E          : Entity_Id;
      Kind       : Entity_Kind;
      Task_Name  : Entity_Id;

      Nam     : constant Entity_Id := Accept_Name (N);
      Formals : constant List_Id   := Parameter_Specifications (N);
      Index   : constant Node_Id   := Entry_Index (N);
      Stats   : constant Node_Id   := Handled_Statement_Sequence (N);

   begin
      --  Entry name is initialized to Any_Id. It should get reset to the
      --  matching entry entity. An error is signalled if it is not reset.

      Entry_Name := Any_Id;

      for I in reverse 0 .. Scope_Stack.Last loop
         Task_Name := Scope_Stack.Table (I).Entity;
         exit when Ekind (Etype (Task_Name)) = E_Task_Type;
         Kind :=  Ekind (Task_Name);

         if Kind /= E_Block and then Kind /= E_Loop
           and then Kind /= E_Entry and then Kind /= E_Entry_Family
         then
            Error_Msg_N ("enclosing body of accept must be a task", N);
            return;
         end if;
      end loop;

      if Ekind (Etype (Task_Name)) /= E_Task_Type then
         Error_Msg_N ("invalid context for accept statement",  N);
         return;
      end if;

      --  In order to process the parameters, we create a defining
      --  identifier that can be used as the name of the scope. The
      --  name of the accept statement itself is not a defining identifier.

      if Present (Index) then
         Ityp := New_Internal_Entity 
           (E_Entry_Family, Current_Scope, Sloc (N), "entry");
      else
         Ityp := New_Internal_Entity
           (E_Entry, Current_Scope, Sloc (N), "entry");
      end if;

      Set_Etype (Ityp, Standard_Void_Type);

      if List_Present (Formals) then
         New_Scope (Ityp);
         Process_Formals (Ityp, Formals);
         End_Scope;
      end if;

      E := First_Entity (Etype (Task_Name));

      while Present (E) loop
         if Chars (E) = Chars (Nam)
           and then (Ekind (E) = Ekind (Ityp))
           and then Type_Conformant (E, Ityp)
         then
            Entry_Name := E;
            exit;
         end if;

         E := Next_Entity (E);
      end loop;

      if Entry_Name = Any_Id then
         Error_Msg_N ("no entry declaration matches accept statement",  N);
         return;
      else
         Set_Entity (Nam, Entry_Name);
      end if;

      for I in reverse 0 .. Scope_Stack.Last loop
         exit when Task_Name = Scope_Stack.Table (I).Entity;

         if Entry_Name = Scope_Stack.Table (I).Entity then
            Error_Msg_N ("accept statement cannot appear with an accept" &
            " statement for the same entry",  N);
         end if;
      end loop;

      if Ekind (E) = E_Entry_Family then
         if No (Index) then
            Error_Msg_N ("missing entry index in accept for entry family", N);
         else
            Analyze (Index);
         end if;

      elsif Present (Index) then
         Error_Msg_N ("invalid entry index in accept for simple entry", N);
      end if;

      if Present (Stats) then
         New_Scope (Entry_Name);
         Install_Declarations (Entry_Name);
         Analyze (Stats);
         End_Scope;
      end if;

   end Analyze_Accept_Statement;

   -------------------
   -- Analyze_Requeue --
   ---------------------

   procedure Analyze_Requeue (N : Node_Id) is
   begin
      Unimplemented (N, "Requeue");
   end Analyze_Requeue;

   -------------------------
   -- Analyze_Delay_Until --
   -------------------------
   procedure Analyze_Delay_Until (N : Node_Id) is
   begin
      Unimplemented (N, "Delay statements");
   end Analyze_Delay_Until;

   ----------------------------
   -- Analyze_Delay_Relative --
   ----------------------------

   procedure Analyze_Delay_Relative (N : Node_Id) is
   begin
      Unimplemented (N, "Delay statements");
   end Analyze_Delay_relative;

   ------------------------------
   -- Analyze_Selective_Accept --
   ------------------------------

   procedure Analyze_Selective_Accept (N : Node_Id) is
      Alts : List_Id := Selective_Accept_Alternatives (N);
      Alt  : Node_Id;
      Accept_Present : Boolean := False;
      Terminate_Present : Boolean := False;
      Delay_Present : Boolean := False;

   begin
      Alt := First (Alts);
      while Present (Alt) loop
         Analyze (Alt);

         if Nkind (Alt) = N_Delay_Alternative then
            Delay_Present := True;
         elsif Nkind (Alt) = N_Terminate_Alternative then
            if Terminate_Present then
               Error_Msg_N ("Only one terminate alternative allowed", N);
            else
               Terminate_Present := True;
            end if;
         else
            Accept_Present := True;
         end if;

         Alt := Next (Alt);
      end loop;

      if Terminate_Present and then Delay_Present then
         Error_Msg_N ("at most one of terminate or delay alternative", N);

      elsif not Accept_Present then
         Error_Msg_N 
           ("select must contain at least one accept alternative", N);
      end if;

      if List_Present (Else_Statements (N)) then
         if Terminate_Present or else Delay_Present then
            Error_Msg_N ("else part not allowed with other alternatives", N);
         end if;

         Analyze_Statements (Else_Statements (N));
      end if;
   end Analyze_Selective_Accept;

   ---------------------------------
   -- Analyze_Accept_Alternative  --
   ---------------------------------

   procedure Analyze_Accept_Alternative (N : Node_Id) is
   begin
      Analyze (Accept_Statement (N));

      if Present (Condition (N)) then
         Analyze (Condition (N));
         Resolve_Complete_Context (Condition (N), Any_Boolean);
      end if;

      if List_Present (Statements (N)) then
         Analyze_Statements (Statements (N));
      end if;
   end Analyze_Accept_Alternative;

   --------------------------------
   -- Analyze_Delay_alternative  --
   --------------------------------

   procedure Analyze_Delay_Alternative (N : Node_Id) is
   begin
      Analyze (Delay_Statement (N));

      if Present (Condition (N)) then
         Analyze (Condition (N));
         Resolve_Complete_Context (Condition (N), Any_Boolean);
      end if;

      if List_Present (Statements (N)) then
         Analyze_Statements (Statements (N));
      end if;
   end Analyze_Delay_Alternative;

   -----------------------------------
   -- Analyze_Terminate_Alternative --
   -----------------------------------

   procedure Analyze_Terminate_Alternative (N : Node_Id) is
   begin
      if Present (Condition (N)) then
         Analyze (Condition (N));
         Resolve_Complete_Context (Condition (N), Any_Boolean);
      end if;

      if List_Present (Statements (N)) then
         Analyze_Statements (Statements (N));
      end if;
   end Analyze_Terminate_Alternative;

   ------------------------------------
   -- Analyze_Conditional_Entry_Call --
   ------------------------------------

   procedure Analyze_Conditional_Entry_Call (N : Node_Id) is
   begin
      Unimplemented (N, "conditional entry call");
   end Analyze_Conditional_Entry_Call;

   ------------------------------------
   -- Analyze_Entry_Call_Alternative --
   ------------------------------------

   procedure Analyze_Entry_Call_Alternative (N : Node_Id) is
   begin
      Unimplemented (N, "entry call alternative");
   end Analyze_Entry_Call_Alternative;

   ------------------------------
   -- Analyze_Timed_Entry_Call --
   ------------------------------

   procedure Analyze_Timed_Entry_Call (N : Node_Id) is
   begin
      Unimplemented (N, "timed entry call");
   end Analyze_Timed_Entry_Call;

   ---------------------------------
   -- Analyze_Asynchronous_Select --
   ---------------------------------

   procedure Analyze_Asynchronous_Select (N : Node_Id) is
   begin
      Unimplemented (N, "asynchronous select");
   end Analyze_Asynchronous_Select;

   ------------------------------------
   -- Analyze_Triggering_Alternative --
   ------------------------------------

   procedure Analyze_Triggering_Alternative (N : Node_Id) is
   begin
      Unimplemented (N, "triggering alternative");
   end Analyze_Triggering_Alternative;

   ----------------------------
   -- Analyze_Abortable_Part --
   ----------------------------

   procedure Analyze_Abortable_Part (N : Node_Id) is
   begin
      Unimplemented (N, "abortable part");
   end Analyze_Abortable_Part;

   -----------------------------
   -- Analyze_Abort_Statement --
   -----------------------------

   procedure Analyze_Abort_Statement (N : Node_Id) is
   begin
      Unimplemented (N, "abort");
   end Analyze_Abort_Statement;

   --------------------------
   -- Install_Declarations --
   --------------------------

   procedure Install_Declarations (Spec : Entity_Id) is
      E    : Entity_Id;
      Prev : Entity_Id;

   begin
      E := First_Entity (Spec);

      while Present (E) loop
         Prev := Current_Entity (E);
         Set_Current_Entity (E);
         Set_Homonym (E, Prev);
         E := Next_Entity (E);
      end loop;
   end Install_Declarations;

begin
   null;
end Sem_Ch9;
