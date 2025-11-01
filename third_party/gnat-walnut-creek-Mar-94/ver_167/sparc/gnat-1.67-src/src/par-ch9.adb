------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R . C H 9                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.40 $                             --
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

separate (Par)
package body Ch9 is

   --  Local subprograms, used only in this chapter

   function P_Accept_Alternative                   return Node_Id;
   function P_Delay_Alternative                    return Node_Id;
   function P_Entry_Body_Formal_Part               return Node_Id;
   function P_Entry_Declaration                    return Node_Id;
   function P_Entry_Index_Specification            return Node_Id;
   function P_Protected_Definition                 return Node_Id;
   function P_Protected_Operation_Declaration_Opt  return Node_Id;
   function P_Protected_Operation_Items            return List_Id;
   function P_Task_Definition                      return Node_Id;
   function P_Task_Items                           return List_Id;

   ---------------------------
   -- 9.1  Task (also 10.2) --
   ---------------------------

   --  TASK_TYPE_DECLARATION ::=
   --    task type DEFINING_IDENTIFIER [KNOWN_DISCRIMINANT_PART]
   --      [is TASK_DEFINITION];

   --  SINGLE_TASK_DECLARATION ::=
   --    task DEFINING_IDENTIFIER [is TASK_DEFINITION];

   --  TASK_BODY ::=
   --    task body task_DEFINING_IDENTIFIER is
   --      DECLARATIVE_PART
   --    begin
   --      HANDLED_SEQUENCE_OF_STATEMENTS
   --    end [task_IDENTIFIER]

   --  TASK_BODY_STUB ::=
   --    task body DEFINING_IDENTIFIER is separate;

   --  This routine scans out a task declaration, task body, or task stub

   --  The caller has checked that the initial token is TASK and scanned
   --  past it, so that Token is set to the token after TASK

   --  Error recovery: cannot raise Error_Resync

   function P_Task return Node_Id is
      Name_Node  : Node_Id;
      Task_Node  : Node_Id;
      Task_Sloc  : Source_Ptr;

   begin
      Push_Scope_Stack;
      Scope.Table (Scope.Last).Etyp := E_Name;
      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Lreq := False;
      Task_Sloc := Prev_Token_Ptr;

      if Token = Tok_Body then
         Scan; -- past BODY
         Name_Node := P_Defining_Identifier;
         Scope.Table (Scope.Last).Labl := Name_Node;
         TF_Is;

         --  Task stub

         if Token = Tok_Separate then
            Scan; -- past SEPARATE
            Task_Node := New_Node (N_Task_Body_Stub, Task_Sloc);
            Set_Defining_Identifier (Task_Node, Name_Node);
            TF_Semicolon;
            Pop_Scope_Stack; -- remove unused entry

         --  Task body

         else
            Task_Node := New_Node (N_Task_Body, Task_Sloc);
            Set_Defining_Identifier (Task_Node, Name_Node);
            P_Decls_Begin_End (Task_Node);
         end if;

         return Task_Node;

      --  Otherwise we must have a task declaration

      else
         if Token = Tok_Type then
            Scan; -- past TYPE
            Task_Node := New_Node (N_Task_Type_Declaration, Task_Sloc);
            Name_Node := P_Defining_Identifier;
            Set_Defining_Identifier (Task_Node, Name_Node);
            Scope.Table (Scope.Last).Labl := Name_Node;
            Set_Discriminant_Specifications
              (Task_Node, P_Known_Discriminant_Part_Opt);

         else
            Task_Node := New_Node (N_Single_Task_Declaration, Task_Sloc);
            Name_Node := P_Defining_Identifier;
            Set_Defining_Identifier (Task_Node, Name_Node);
            Scope.Table (Scope.Last).Labl := Name_Node;

            if Token = Tok_Left_Paren then
               Error_Msg_SC ("discriminant part not allowed here!");
               Resync_Past_Right_Paren_Or_EOL;
            end if;

         end if;

         --  Parse optional task definition. Note that P_Task_Definition scans
         --  out the semicolon as well as the task definition itself.

         if Token = Tok_Semicolon then
            Pop_Scope_Stack; -- Remove unused entry
            Scan; -- past semicolon
         else
            TF_Is; -- must have IS if no semicolon
            Set_Task_Definition (Task_Node, P_Task_Definition);
         end if;

         return Task_Node;
      end if;
   end P_Task;

   --------------------------------
   -- 9.1  Task Type Declaration --
   --------------------------------

   --  Parsed by P_Task (9.1)

   ----------------------------------
   -- 9.1  Single Task Declaration --
   ----------------------------------

   --  Parsed by P_Task (9.1)

   --------------------
   -- 9.1  Task Body --
   --------------------

   --  Parsed by P_Task (9.1)

   --------------------------
   -- 9.1  Task Definition --
   --------------------------

   --  TASK_DEFINITION ::=
   --      {TASK_ITEM}
   --    [private
   --      {TASK_ITEM}
   --    end [task_IDENTIFIER];

   --  The caller has already made the scope stack entry

   --  Note: there is a small deviation from official syntax here in that we
   --  regard the semicolon after end as part of the Task_Definition, and in
   --  the official syntax, it's part of the enclosing declaration. The reason
   --  for this deviation is that otherwise the end processing would have to
   --  be special cased, which would be a nuisance!

   --  Error recovery:  cannot raise Error_Resync

   function P_Task_Definition return Node_Id is
      Def_Node  : Node_Id;

   begin
      Def_Node := New_Node (N_Task_Definition, Token_Ptr);
      Set_Visible_Declarations (Def_Node, P_Task_Items);

      if Token = Tok_Private then
         Scan; -- past PRIVATE
         Set_Private_Declarations (Def_Node, P_Task_Items);

         --  Deal gracefully with multiple PRIVATE parts

         while Token = Tok_Private loop
            Error_Msg_SC ("Only one private part allowed per task");
            Scan; -- past PRIVATE
            Append_List (P_Task_Items, Private_Declarations (Def_Node));
         end loop;
      end if;

      End_Statements;
      return Def_Node;
   end P_Task_Definition;

   ---------------------
   -- 9.1  Task Items --
   ---------------------

   --  TASK_ITEM ::= ENTRY_DECLARATION | REPRESENTATION_CLAUSE

   --  This subprogram scans a (possibly empty) list of task items and pragmas

   --  Error recovery:  cannot raise Error_Resync

   --  Note: a pragma can also be returned in this position

   function P_Task_Items return List_Id is
      Items      : List_Id;
      Item_Node  : Node_Id;
      Decl_Sloc  : Source_Ptr;

   begin
      --  Get rid of active SIS entry from outer scope. This means we will
      --  miss some nested cases, but it doesn't seem worth the effort. See
      --  discussion in Par for further details

      SIS_Entry_Active := False;

      --  Loop to scan out task items

      Items := New_List;

      Decl_Loop : loop
         Decl_Sloc := Token_Ptr;

         if Token = Tok_Pragma then
            Append (P_Pragma, Items);

         elsif Token = Tok_Entry then
            Append (P_Entry_Declaration, Items);

         elsif Token = Tok_For then
            --  Representation clause in task declaration. The only rep
            --  clause which is legal in a protected is an address clause,
            --  so that is what we try to scan out.

            Item_Node := P_Representation_Clause;

            if Nkind (Item_Node) = N_At_Clause then
               Append (Item_Node, Items);
            else
               Error_Msg
                 ("the only representation clause " &
                  "allowed here is an address clause!", Decl_Sloc);
            end if;

         elsif Token in Token_Class_Declk then
            Error_Msg_SC ("Illegal declaration in task definition!");
            Resync_Past_Semicolon;

         else
            exit Decl_Loop;
         end if;
      end loop Decl_Loop;

      return Items;
   end P_Task_Items;

   --------------------------------
   -- 9.4  Protected (also 10.2) --
   --------------------------------

   --  PROTECTED_TYPE_DECLARATION ::=
   --    protected type DEFINING_IDENTIFIER [KNOWN_DISCRIMINANT_PART]
   --      is PROTECTED_DEFINITION;

   --  SINGLE_PROTECTED_DECLARATION ::=
   --    protected DEFINING_IDENTIFIER is PROTECTED_DEFINITION;

   --  PROTECTED_BODY ::=
   --    protected body DEFINING_IDENTIFIER is
   --      {PROTECTED_OPERATION_ITEM}
   --    end [protected_IDENTIFIER];

   --  PROTECTED_BODY_STUB ::=
   --    protected body DEFINING_IDENTIFIER is separate;

   --  This routine scans out a protected declaration, protected body
   --  or a protected stub.

   --  The caller has checked that the initial token is PROTECTED and
   --  scanned past it, so Token is set to the following token.

   --  Error recovery: cannot raise Error_Resync

   function P_Protected return Node_Id is
      Name_Node      : Node_Id;
      Protected_Node : Node_Id;
      Protected_Sloc : Source_Ptr;

   begin
      Push_Scope_Stack;
      Scope.Table (Scope.Last).Etyp := E_Name;
      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Lreq := False;
      Protected_Sloc := Prev_Token_Ptr;

      if Token = Tok_Body then
         Scan; -- past BODY
         Name_Node := P_Defining_Identifier;
         Scope.Table (Scope.Last).Labl := Name_Node;
         TF_Is;

         --  Protected stub

         if Token = Tok_Separate then
            Scan; -- past SEPARATE
            Protected_Node := New_Node (N_Protected_Body_Stub, Protected_Sloc);
            Set_Defining_Identifier (Protected_Node, Name_Node);
            TF_Semicolon;
            Pop_Scope_Stack; -- remove unused entry

         --  Protected body

         else
            Protected_Node := New_Node (N_Protected_Body, Protected_Sloc);
            Set_Defining_Identifier (Protected_Node, Name_Node);
            Set_Declarations (Protected_Node, P_Protected_Operation_Items);
            End_Statements;
         end if;

         return Protected_Node;

      --  Otherwise we must have a protected declaration

      else
         if Token = Tok_Type then
            Scan; -- past TYPE
            Protected_Node :=
              New_Node (N_Protected_Type_Declaration, Protected_Sloc);
            Name_Node := P_Defining_Identifier;
            Set_Defining_Identifier (Protected_Node, Name_Node);
            Scope.Table (Scope.Last).Labl := Name_Node;
            Set_Discriminant_Specifications
              (Protected_Node, P_Known_Discriminant_Part_Opt);

         else
            Protected_Node :=
              New_Node (N_Single_Protected_Declaration, Protected_Sloc);
            Name_Node := P_Defining_Identifier;
            Set_Defining_Identifier (Protected_Node, Name_Node);

            if Token = Tok_Left_Paren then
               Error_Msg_SC ("discriminant part not allowed here!");
               Resync_Past_Right_Paren_Or_EOL;
            end if;

            Scope.Table (Scope.Last).Labl := Name_Node;
         end if;

         T_Is;
         Set_Protected_Definition (Protected_Node, P_Protected_Definition);

         return Protected_Node;
      end if;
   end P_Protected;

   --------------------------------
   -- 9.4  Protected Declaration --
   --------------------------------

   --  Parsed by P_Protected (9.5)

   -------------------------------------
   -- 9.4  Protected Type Declaration --
   -------------------------------------

   --  Parsed by P_Protected (9.5)

   ---------------------------------------
   -- 9.4  Single Protected Declaration --
   ---------------------------------------

   --  Parsed by P_Protected (9.5)

   -------------------------------
   -- 9.4  Protected Definition --
   -------------------------------

   --  PROTECTED_DEFINITION ::=
   --      {PROTECTED_OPERATION_DECLARATION}
   --    [private
   --      {PROTECTED_ELEMENT_DECLARATION}]
   --    end [protected_IDENTIFIER]

   --  PROTECTED_ELEMENT_DECLARATION ::=
   --    PROTECTED_OPERATION_DECLARATION | COMPONENT_DECLARATION

   --  The caller has already established the scope stack entry

   --  Error recovery: cannot raise Error_Resync

   function P_Protected_Definition return Node_Id is
      Def_Node  : Node_Id;
      Item_Node : Node_Id;

   begin
      Def_Node := New_Node (N_Protected_Definition, Token_Ptr);

      --  Get rid of active SIS entry from outer scope. This means we will
      --  miss some nested cases, but it doesn't seem worth the effort. See
      --  discussion in Par for further details

      SIS_Entry_Active := False;

      --  Loop to scan visible declarations (protected operation declarations)

      Set_Visible_Declarations (Def_Node, New_List);

      loop
         Item_Node := P_Protected_Operation_Declaration_Opt;
         exit when No (Item_Node);
         Append (Item_Node, Visible_Declarations (Def_Node));
      end loop;

      --  Deal with PRIVATE part (including graceful handling
      --  of multiple PRIVATE parts).

      Private_Loop : while Token = Tok_Private loop
         if Private_Declarations (Def_Node) = No_List then
            Set_Private_Declarations (Def_Node, New_List);
         else
            Error_Msg_SC ("duplicate private part");
         end if;

         Scan; -- past PRIVATE

         Declaration_Loop : loop
            if Token = Tok_Identifier then
               P_Component_Declarations (Private_Declarations (Def_Node));
            else
               Item_Node := P_Protected_Operation_Declaration_Opt;
               exit Declaration_Loop when No (Item_Node);
               Append (Item_Node, Private_Declarations (Def_Node));
            end if;
         end loop Declaration_Loop;
      end loop Private_Loop;

      End_Statements;
      return Def_Node;
   end P_Protected_Definition;

   ------------------------------------------
   -- 9.4  Protected Operation Declaration --
   ------------------------------------------

   --  PROTECTED_OPERATION_DECLARATION ::=
   --    SUBPROGRAM_DECLARATION | ENTRY_DECLARATION

   --  Error recovery: cannot raise Error_Resync

   --  Note: a pragma can also be returned in this position

   function P_Protected_Operation_Declaration_Opt return Node_Id is
   begin
      --  This loop runs more than once only when a junk declaration
      --  is skipped.

      loop
         if Token = Tok_Pragma then
            return P_Pragma;

         elsif Token = Tok_Entry then
            return P_Entry_Declaration;

         elsif Token = Tok_Function or else Token = Tok_Procedure then
            return P_Subprogram (Pf_Decl);

         elsif Token = Tok_Identifier
           or else Token in Token_Class_Declk
         then
            Error_Msg_SC ("Illegal declaration in protected definition!");
            Resync_Past_Semicolon;

         else
            return Empty;
         end if;
      end loop;
   end P_Protected_Operation_Declaration_Opt;

   ----------------------------------------
   -- 9.4  Protected Element Declaration --
   ----------------------------------------

   --  Parsed by P_Protected_Definition (9.4)

   -------------------------
   -- 9.4  Protected Body --
   -------------------------

   --  Parsed by P_Protected (9.5)

   ------------------------------------
   -- 9.4  Protected Operation Items --
   ------------------------------------

   --  PROTECTED_OPERATION_ITEM ::=
   --    SUBPROGRAM_DECLARATION | SUBPROGRAM_BODY | ENTRY_BODY

   --  This procedure parses and returns a list of protected operation items

   function P_Protected_Operation_Items return List_Id is
      Item_List : List_Id;

   begin
      Item_List := New_List;

      loop
         if Token = Tok_Entry then
            Append (P_Entry_Body, Item_List);

         elsif Token = Tok_Function or else Token = Tok_Procedure then
            Append (P_Subprogram (Pf_Pbod), Item_List);

         elsif Token = Tok_Identifier then
            Error_Msg_SC
              ("all components must be declared in spec!");

            Resync_Past_Semicolon;

         elsif Token in Token_Class_Declk then
            Error_Msg_SC ("illegal declaration in protected definition!");
            Resync_Past_Semicolon;

         else
            exit;
         end if;
      end loop;

      return Item_List;
   end P_Protected_Operation_Items;

   ------------------------------
   -- 9.5.2  Entry Declaration --
   ------------------------------

   --  ENTRY_DECLARATION ::=
   --    entry DEFINING_IDENTIFIER [(DISCRETE_SUBTYPE_DEFINITION)]
   --      PARAMETER_PROFILE

   --  The caller has checked that the initial token is ENTRY

   --  Error recovery: cannot raise Error_Resync

   function P_Entry_Declaration return Node_Id is
      Decl_Node  : Node_Id;
      Scan_State : Saved_Scan_State;

   begin
      Decl_Node := New_Node (N_Entry_Declaration, Token_Ptr);
      Scan; -- past ENTRY

      Set_Defining_Identifier (Decl_Node, P_Defining_Identifier);

      --  If left paren, could be (Discrete_Subtype_Definition) or Formal_Part

      if Token = Tok_Left_Paren then
         Scan; -- past (

         --  If identifier after left paren, could still be either

         if Token = Tok_Identifier then
            Save_Scan_State (Scan_State); -- at Id
            Scan; -- past Id

            --  If comma or colon after Id, must be Formal_Part

            if Token = Tok_Comma or else Token = Tok_Colon then
               Restore_Scan_State (Scan_State); -- to Id
               Set_Parameter_Specifications (Decl_Node, P_Formal_Part);

            --  Else if Id wi no comma or colon, must be discrete subtype defn

            else
               Restore_Scan_State (Scan_State); -- to Id
               Set_Discrete_Subtype_Definition
                 (Decl_Node, P_Discrete_Subtype_Definition);
               T_Right_Paren;
               Set_Parameter_Specifications (Decl_Node, P_Formal_Part_Opt);
            end if;

         --  If no Id, must be discrete subtype definition

         else
            Set_Discrete_Subtype_Definition
              (Decl_Node, P_Discrete_Subtype_Definition);
            T_Right_Paren;
            Set_Parameter_Specifications (Decl_Node, P_Formal_Part_Opt);
         end if;
      end if;

      --  Error recovery check for illegal return

      if Token = Tok_Return then
         Error_Msg_SC ("entry cannot have return value!");
         Resync_Past_Semicolon;
      else
         TF_Semicolon;
      end if;

      return Decl_Node;
   end P_Entry_Declaration;

   -----------------------------
   -- 9.5.2  Accept Statement --
   -----------------------------

   --  ACCEPT_STATEMENT ::=
   --    accept entry_DIRECT_NAME
   --      [(ENTRY_INDEX)] PARAMETER_PROFILE [do
   --        HANDLED_SEQUENCE_OF_STATEMENTS
   --    end [entry_IDENTIFIER]];

   --  The caller has checked that the initial token is ACCEPT

   --  Error recovery: cannot raise Error_Resync. If an error occurs, the
   --  scan is resynchronized past the next semicolon and control returns.

   function P_Accept_Statement return Node_Id is
      Scan_State  : Saved_Scan_State;
      Accept_Node : Node_Id;

   begin
      Push_Scope_Stack;
      Scope.Table (Scope.Last).Sloc := Token_Ptr;
      Scope.Table (Scope.Last).Ecol := Start_Column;

      Accept_Node := New_Node (N_Accept_Statement, Token_Ptr);
      Scan; -- past ACCEPT
      Scope.Table (Scope.Last).Labl := Token_Node;

      Set_Accept_Name (Accept_Node, P_Identifier);

      --  Left paren could be (Entry_Index) or Formal_Part, determine which

      if Token = Tok_Left_Paren then
         Save_Scan_State (Scan_State); -- at left paren
         Scan; -- past left paren

         --  If first token after left paren not identifier, then Entry_Index

         if Token /= Tok_Identifier then
            Set_Entry_Index (Accept_Node, P_Expression);
            T_Right_Paren;
            Set_Parameter_Specifications (Accept_Node, P_Formal_Part_Opt);

         --  First token after left paren is identifier, could be either case

         else -- Token = Tok_Identifier
            Scan; -- past identifier

            --  If identifier followed by comma or colon, must be Formal_Part

            if Token = Tok_Comma or else Token = Tok_Colon then
               Restore_Scan_State (Scan_State); -- to left paren
               Set_Parameter_Specifications (Accept_Node, P_Formal_Part_Opt);

            --  If identifier not followed by comma/colon, must be entry index

            else
               Restore_Scan_State (Scan_State); -- to left paren
               Scan; -- past left paren (again!)
               Set_Entry_Index (Accept_Node, P_Expression);
               T_Right_Paren;
               Set_Parameter_Specifications (Accept_Node, P_Formal_Part_Opt);
            end if;
         end if;
      end if;

      --  Scan out DO if present

      if Token = Tok_Do then
         Scope.Table (Scope.Last).Etyp := E_Name;
         Scope.Table (Scope.Last).Lreq := False;
         Scan; -- past DO
         Set_Handled_Statement_Sequence
           (Accept_Node, P_Handled_Sequence_Of_Statements);
         End_Statements;

      else
         Pop_Scope_Stack; -- discard unused entry
         TF_Semicolon;
      end if;

      return Accept_Node;

   --  If error, resynchronize past semicolon

   exception
      when Error_Resync =>
         Resync_Past_Semicolon;
         return Error;

   end P_Accept_Statement;

   ----------------------
   -- 9.7  Entry Index --
   ----------------------

   --  Parsed by P_Expression (4.4)

   ---------------------
   -- 9.7  Entry Body --
   ---------------------

   --  ENTRY_BARRIER ::= when CONDITION

   --  ENTRY_BODY ::=
   --    entry DEFINING_IDENTIFIER ENTRY_BODY_FORMAL_PART ENTRY_BARRIER is
   --      DECLARATIVE_PART
   --    begin
   --      HANDLED_SEQUENCE_OF_STATEMENTS
   --    end [entry_IDENTIFIER];

   --  ENTRY_BARRIER ::= when CONDITION

   --  The caller has checked that the initial token is ENTRY

   --  Error_Recovery: cannot raise Error_Resync

   function P_Entry_Body return Node_Id is
      Entry_Node     : Node_Id;
      Iterator_Node  : Node_Id;
      Name_Node      : Node_Id;

   begin
      Push_Scope_Stack;
      Entry_Node := New_Node (N_Entry_Body, Token_Ptr);
      Scan; -- past ENTRY

      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Lreq := False;
      Scope.Table (Scope.Last).Etyp := E_Name;

      Name_Node := P_Defining_Identifier;
      Set_Defining_Identifier (Entry_Node, Name_Node);
      Scope.Table (Scope.Last).Labl := Name_Node;

      Set_Entry_Body_Formal_Part (Entry_Node, P_Entry_Body_Formal_Part);

      if Token = Tok_When then
         Scan; -- past WHEN;
         Set_Condition (Entry_Node, P_Expression_No_Right_Paren);
         T_Is;
      else
         T_When; -- to give error message
         Set_Condition (Entry_Node, Error);
         TF_Is;
      end if;

      P_Decls_Begin_End (Entry_Node);
      return Entry_Node;
   end P_Entry_Body;

   ---------------------------------
   -- 9.7  Entry Body Formal Part --
   ---------------------------------

   --  ENTRY_BODY_FORMAL_PART ::=
   --    [(ENTRY_INDEX_SPECIFICATION)] [FORMAL_PART]

   --  Error_Recovery: cannot raise Error_Resync

   function P_Entry_Body_Formal_Part return Node_Id is
      Fpart_Node : Node_Id;
      Scan_State : Saved_Scan_State;

   begin
      Fpart_Node := New_Node (N_Entry_Body_Formal_Part, Token_Ptr);

      --  See if entry index specification present, and if so parse it

      if Token = Tok_Left_Paren then
         Save_Scan_State (Scan_State); -- at left paren
         Scan; -- past left paren

         if Token = Tok_For then
            Set_Entry_Index_Specification
              (Fpart_Node, P_Entry_Index_Specification);
            T_Right_Paren;
         else
            Restore_Scan_State (Scan_State); -- to left paren
         end if;
      end if;

      Set_Parameter_Specifications (Fpart_Node, P_Formal_Part_Opt);
      return Fpart_Node;
   end P_Entry_Body_Formal_Part;

   ------------------------
   -- 9.7  Entry Barrier --
   ------------------------

   --  Parsed by P_Entry_Body (9.7)

   --------------------------------------
   -- 9.5.2  Entry Index Specification --
   --------------------------------------

   --  ENTRY_INDEX_SPECIFICATION ::=
   --    for DEFINING_IDENTIFIER in DISCRETE_SUBTYPE_DEFINITION

   --  Error recovery: can raise Error_Resync

   function P_Entry_Index_Specification return Node_Id is
      Iterator_Node : Node_Id;

   begin
      Iterator_Node := New_Node (N_Entry_Index_Specification, Token_Ptr);
      T_For; -- past FOR
      Set_Defining_Identifier (Iterator_Node, P_Defining_Identifier);
      T_In;
      Set_Discrete_Subtype_Definition
        (Iterator_Node, P_Discrete_Subtype_Definition);
      return Iterator_Node;
   end P_Entry_Index_Specification;

   ---------------------------------
   -- 9.5.3  Entry Call Statement --
   ---------------------------------

   --  Parsed by Call_Name (4.1). Within a select, an entry call is parsed
   --  by P_Select_Statement (9.7)

   ------------------------------
   -- 9.5.4  Requeue Statement --
   ------------------------------

   --  REQUEUE_STATEMENT ::= requeue entry_NAME [with abort];

   --  The caller has checked that the initial token is requeue

   --  Error recovery: can raise Error_Resync

   function P_Requeue_Statement return Node_Id is
      Requeue_Node : Node_Id;

   begin
      Requeue_Node := New_Node (N_Requeue_Statement, Token_Ptr);
      Scan; -- past REQUEUE
      Set_Name (Requeue_Node, P_Name);

      if Token = Tok_With then
         Scan; -- past WITH
         T_Abort;
         Set_Abort_Present (Requeue_Node, True);
      end if;

      TF_Semicolon;
      return Requeue_Node;
   end P_Requeue_Statement;

   --------------------------
   -- 9.6  Delay Statement --
   --------------------------

   --  DELAY_STATEMENT ::=
   --    DELAY_UNTIL_STATEMENT
   --  | DELAY_RELATIVE_STATEMENT

   --  DELAY_UNTIL_STATEMENT ::= delay until delay_EXPRESSION;

   --  DELAY_RELATIVE_STATEMENT ::= delay delay_EXPRESSION;

   --  The caller has checked that the initial token is DELAY

   --  Error recovery: cannot raise Error_Resync

   function P_Delay_Statement return Node_Id is
      Delay_Node : Node_Id;
      Delay_Sloc : Source_Ptr;
      Expr_Node  : Node_Id;

   begin
      Delay_Sloc := Token_Ptr;
      Scan; -- past DELAY

      --  The following check for delay until misused in Ada 83 doesn't catch
      --  all cases, but it's good enough to catch most of them!

      if Token_Name = Name_Until then
         Check_9X_Keyword (Tok_Until, Tok_Left_Paren);
         Check_9X_Keyword (Tok_Until, Tok_Identifier);
      end if;

      if Token = Tok_Until then
         Delay_Node := New_Node (N_Delay_Until_Statement, Delay_Sloc);
         Scan; -- past UNTIL
      else
         Delay_Node := New_Node (N_Delay_Relative_Statement, Delay_Sloc);
      end if;

      Expr_Node := P_Expression_No_Right_Paren;

      if Nkind (Delay_Node) = N_Delay_Relative_Statement then
         Check_Simple_Expression_In_Ada_83 (Expr_Node);
      end if;

      Set_Expression (Delay_Node, Expr_Node);
      TF_Semicolon;
      return Delay_Node;
   end P_Delay_Statement;

   --------------------------------
   -- 9.6  Delay Until Statement --
   --------------------------------

   --  Parsed by P_Delay_Statement (9.6)

   -----------------------------------
   -- 9.6  Delay Relative Statement --
   -----------------------------------

   --  Parsed by P_Delay_Statement (9.6)

   ---------------------------
   -- 9.7  Select Statement --
   ---------------------------

   --  SELECT_STATEMENT ::=
   --    SELECTIVE_ACCEPT
   --  | CONDITIONAL_ENTRY_CALL
   --  | TIMED_ENTRY_CALL
   --  | ASYNCHRONOUS_SELECT

   --  SELECTIVE_ACCEPT ::=
   --    select
   --      [GUARD]
   --        SELECT_ALTERNATIVE
   --    {or
   --      [GUARD]
   --        SELECT_ALTERNATIVE
   --    [else
   --      SEQUENCE_OF_STATEMENTS]
   --    end select;

   --  GUARD ::= when CONDITION =>

   --  Note: the guard preceding a select alternative is included as part
   --  of the node generated for a selective accept alternative.

   --  SELECT_ALTERNATIVE ::=
   --    ACCEPT_ALTERNATIVE | DELAY_ALTERNATIVE | TERMINATE_ALTERNATIVE

   --  TIMED_ENTRY_CALL ::=
   --    select
   --      ENTRY_CALL_ALTERNATIVE
   --    or
   --      DELAY_ALTERNATIVE
   --    end select;

   --  CONDITIONAL_ENTRY_CALL ::=
   --    select
   --      ENTRY_CALL_ALTERNATIVE
   --    else
   --      SEQUENCE_OF_STATEMENTS
   --    end select;

   --  ENTRY_CALL_ALTERNATIVE ::=
   --    ENTRY_CALL_STATEMENT [SEQUENCE_OF_STATEMENTS]

   --  ASYNCHRONOUS_SELECT ::=
   --    select
   --      TRIGGERING_ALTERNATIVE
   --    then abort
   --      ABORTABLE_PART
   --    end select;

   --  TRIGGERING_ALTERNATIVE ::=
   --    TRIGGERING_STATEMENT [SEQUENCE_OF_STATEMENTS]

   --  TRIGGERING_STATEMENT ::= ENTRY_CALL_STATEMENT | DELAY_STATEMENT

   --  The caller has checked that the initial token is SELECT

   --  Error recovery: can raise Error_Resync

   function P_Select_Statement return Node_Id is
      Select_Node    : Node_Id;
      Select_Sloc    : Source_Ptr;
      Stmnt_Sloc     : Source_Ptr;
      Ecall_Node     : Node_Id;
      Alternative    : Node_Id;
      Statement_List : List_Id;
      Alt_List       : List_Id;
      Cond_Expr      : Node_Id;
      Delay_Stmnt    : Node_Id;

   begin
      Push_Scope_Stack;
      Scope.Table (Scope.Last).Etyp := E_Select;
      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Sloc := Token_Ptr;
      Scope.Table (Scope.Last).Labl := Error;

      Select_Sloc := Token_Ptr;
      Scan; -- past SELECT
      Stmnt_Sloc := Token_Ptr;

      --  If first token after select is designator, then we have an entry
      --  call, which must be the start of a conditional entry call, timed
      --  entry call or asynchronous select

      if Token in Token_Class_Desig then

         --  Scan entry call statement

         begin
            Ecall_Node := P_Call_Name;

            if Expr_Form = EF_Name then
               Error_Msg_SP ("entry name expected");
            end if;

            TF_Semicolon;

         exception
            when Error_Resync =>
               Resync_Past_Semicolon;
               return Error;
         end;

         Statement_List := P_Sequence_Of_Statements (SS_Eltm_Ortm_Tatm);

         --  OR follows, we have a timed entry call

         if Token = Tok_Or then
            Scan; -- past OR

            Select_Node := New_Node (N_Timed_Entry_Call, Select_Sloc);
            Set_Entry_Call_Alternative (Select_Node,
              Make_Entry_Call_Alternative (Stmnt_Sloc,
                Entry_Call_Statement => Ecall_Node,
                Statements => Statement_List));

            --  Only possibility is delay alternative. If we have anything
            --  else, give message, and treat as conditional entry call.

            if Token /= Tok_Delay then
               Error_Msg_SC
                  ("only allowed alternative in timed entry call is delay!");
               Discard_Junk_List (P_Sequence_Of_Statements (SS_Sreq));
               Set_Delay_Alternative (Select_Node, Error);

            else
               Set_Delay_Alternative (Select_Node, P_Delay_Alternative);
            end if;

         --  ELSE follows, we have a conditional entry call

         elsif Token = Tok_Else then
            Scan; -- past ELSE

            Select_Node := New_Node (N_Conditional_Entry_Call, Select_Sloc);

            Set_Entry_Call_Alternative (Select_Node,
              Make_Entry_Call_Alternative (Stmnt_Sloc,
                Entry_Call_Statement => Ecall_Node,
                Statements => Statement_List));

            Set_Else_Statements
              (Select_Node, P_Sequence_Of_Statements (SS_Sreq));

         --  Only remaining case is THEN ABORT (asynchronous select)

         elsif Token = Tok_Abort then
            Select_Node :=
              Make_Asynchronous_Select (Select_Sloc,
                Triggering_Alternative =>
                  Make_Triggering_Alternative (Stmnt_Sloc,
                    Triggering_Statement => Ecall_Node,
                    Statements => Statement_List),
                Abortable_Part => P_Abortable_Part);

         --  Else error

         else
            if Ada_83 then
               Error_Msg_BC ("OR or ELSE expected");
            else
               Error_Msg_BC ("OR or ELSE or THEN ABORT expected");
            end if;

            Select_Node := Error;
         end if;

         End_Statements;

      --  Here we have a selective accept or an an asynchronous select (first
      --  token after SELECT is other than a designator token).

      else

         --  If we have delay with no guard, could be asynchronous select

         if Token = Tok_Delay then
            Delay_Stmnt := P_Delay_Statement;
            Statement_List := P_Sequence_Of_Statements (SS_Eltm_Ortm_Tatm);

            --  Asynchronous select

            if Token = Tok_Abort then
               Select_Node :=
                 Make_Asynchronous_Select (Select_Sloc,
                   Triggering_Alternative =>
                     Make_Triggering_Alternative (Stmnt_Sloc,
                       Triggering_Statement => Delay_Stmnt,
                       Statements => Statement_List),
                     Abortable_Part => P_Abortable_Part);

               End_Statements;
               return Select_Node;

            --  Delay which was not an asyncrhonous select. Must be a selective
            --  accept, and since at least one accept statement is required,
            --  we must have at least one OR phrase present.

            else
               Alt_List := New_List_1 (
                 Make_Delay_Alternative (Stmnt_Sloc,
                   Delay_Statement => Delay_Stmnt,
                   Statements      => Statement_List));
               T_Or;
            end if;

         --  If not a delay statement, then must be another possibility
         --  for a selective accept alternative, or perhaps a guard is present

         else
            Alt_List := New_List;
         end if;

         Select_Node := New_Node (N_Selective_Accept, Select_Sloc);
         Set_Selective_Accept_Alternatives (Select_Node, Alt_List);

         --  Scan out selective accept alternatives

         loop
            if Token = Tok_When then
               Scan; --  past WHEN
               Cond_Expr := P_Expression_No_Right_Paren;
               T_Arrow;
            else
               Cond_Expr := Empty;
            end if;

            if Token = Tok_Accept then
               Alternative := P_Accept_Alternative;

               --  Check for junk attempt at asynchronous select using
               --  an Accept alternative as the triggering statement

               if Token = Tok_Abort
                 and then Is_Empty_List (Alt_List)
                 and then No (Cond_Expr)
               then
                  Error_Msg
                    ("triggering statement must be entry call or delay",
                     Sloc (Alternative));
                  Scan; -- past junk ABORT
                  Discard_Junk_List (P_Sequence_Of_Statements (SS_Sreq));
                  End_Statements;
                  return Error;
               end if;

            elsif Token = Tok_Delay then
               Alternative := P_Delay_Alternative;

            elsif Token = Tok_Terminate then
               Alternative := P_Terminate_Alternative;

            end if;


            --  THEN ABORT at this stage is just junk

            if Token = Tok_Abort then
               Error_Msg_SP ("misplaced `THEN ABORT`");
               Scan; -- past junk ABORT
               Discard_Junk_List (P_Sequence_Of_Statements (SS_Sreq));
               End_Statements;
               return Error;

            else
               exit when Token /= Tok_Or;
            end if;

            Append (Alternative, Alt_List);
            T_Or;
         end loop;

         if Token = Tok_Else then
            Scan; -- past ELSE
            Set_Else_Statements
              (Select_Node, P_Sequence_Of_Statements (SS_Ortm_Sreq));

            if Token = Tok_Or then
               Error_Msg_SC ("select alternative cannot follow else part!");
            end if;
         end if;

         End_Statements;
      end if;

      return Select_Node;
   end P_Select_Statement;

   -----------------------------
   -- 9.7.1  Selective Accept --
   -----------------------------

   --  Parsed by P_Select_Statement (9.7.1)

   ------------------
   -- 9.7.1  Guard --
   ------------------

   --  Parsed by P_Select_Statement (9.7.1)

   -------------------------------
   -- 9.7.1  Select Alternative --
   -------------------------------

   --  SELECT_ALTERNATIVE ::=
   --    ACCEPT_ALTERNATIVE | DELAY_ALTERNATIVE | TERMINATE_ALTERNATIVE

   --  Note: the guard preceding a select alternative is included as part
   --  of the node generated for a selective accept alternative.

   --  Error recovery: cannot raise Error_Resync

   -------------------------------
   -- 9.7.1  Accept Alternative --
   -------------------------------

   --  ACCEPT_ALTERNATIVE ::=
   --    ACCEPT_STATEMENT [SEQUENCE_OF_STATEMENTS]

   --  Error_Recovery: Cannot raise Error_Resync

   function P_Accept_Alternative return Node_Id is
      Accept_Alt_Node : Node_Id;

   begin
      Accept_Alt_Node := New_Node (N_Accept_Alternative, Token_Ptr);
      Set_Accept_Statement (Accept_Alt_Node, P_Accept_Statement);

      --  Note: the reason that we accept THEN ABORT as a terminator for
      --  the sequence of statements is for error recovery which allows
      --  for misuse of an accept statement as a triggering statememt.

      Set_Statements
        (Accept_Alt_Node, P_Sequence_Of_Statements (SS_Eltm_Ortm_Tatm));
      return Accept_Alt_Node;
   end P_Accept_Alternative;

   ------------------------------
   -- 9.7.1  Delay Alternative --
   ------------------------------

   --  DELAY_ALTERNATIVE ::=
   --    DELAY_STATEMENT [SEQUENCE_OF_STATEMENTS]

   --  Error_Recovery: Cannot raise Error_Resync

   function P_Delay_Alternative return Node_Id is
      Delay_Alt_Node : Node_Id;

   begin
      Delay_Alt_Node := New_Node (N_Delay_Alternative, Token_Ptr);
      Set_Delay_Statement (Delay_Alt_Node, P_Delay_Statement);

      --  Note: the reason that we accept THEN ABORT as a terminator for
      --  the sequence of statements is for error recovery which allows
      --  for misuse of an accept statement as a triggering statememt.

      Set_Statements
        (Delay_Alt_Node, P_Sequence_Of_Statements (SS_Eltm_Ortm_Tatm));
      return Delay_Alt_Node;
   end P_Delay_Alternative;

   ----------------------------------
   -- 9.7.1  Terminate Alternative --
   ----------------------------------

   --  TERMINATE_ALTERNATIVE ::= terminate;

   --  Error_Recovery: Cannot raise Error_Resync

   function P_Terminate_Alternative return Node_Id is
      Terminate_Alt_Node : Node_Id;

   begin
      Terminate_Alt_Node := New_Node (N_Terminate_Alternative, Token_Ptr);
      Scan; -- past TERMINATE
      TF_Semicolon;
      return Terminate_Alt_Node;
   end P_Terminate_Alternative;

   -----------------------------------
   -- 9.7.2  Conditional Entry Call --
   -----------------------------------

   --  Parsed by P_Select_Statement (9.7)

   -----------------------------------
   -- 9.7.2  Entry_Call_Alternative --
   -----------------------------------

   --  Parsed by P_Select_Statement (9.7)

   -----------------------------
   -- 9.7.3  Timed Entry Call --
   -----------------------------

   --  Parsed by P_Select_Statement (9.7)

   --------------------------------
   -- 9.7.4  Asynchronous Select --
   --------------------------------

   --  Parsed by P_Select_Statement (9.7)

   -----------------------------------
   -- 9.7.4  Triggering Alternative --
   -----------------------------------

   --  Parsed by P_Select_Statement (9.7)

   ---------------------------------
   -- 9.7.4  Triggering Statement --
   ---------------------------------

   --  Parsed by P_Select_Statement (9.7)

   ---------------------------
   -- 9.7.4  Abortable Part --
   ---------------------------

   --  ABORTABLE_PART ::= SEQUENCE_OF_STATEMENTS

   --  The caller has verified that THEN ABORT is present, and Token is
   --  pointing to the ABORT on entry (or if not, then we have an error)

   --  Error recovery: cannot raise Error_Resync

   function P_Abortable_Part return Node_Id is
      Abortable_Part_Node : Node_Id;

   begin
      Abortable_Part_Node := New_Node (N_Abortable_Part, Token_Ptr);
      T_Abort; -- scan past ABORT

      if Ada_83 then
         Error_Msg_SP ("asynchronous select not allowed in Ada-83!");
      end if;

      Set_Statements (Abortable_Part_Node, P_Sequence_Of_Statements (SS_Sreq));
      return Abortable_Part_Node;
   end P_Abortable_Part;

   --------------------------
   -- 9.8  Abort Statement --
   --------------------------

   --  ABORT_STATEMENT ::= abort task_NAME {, task_NAME};

   --  The caller has checked that the initial token is ABORT

   --  Error recovery: cannot raise Error_Resync

   function P_Abort_Statement return Node_Id is
      Abort_Node : Node_Id;

   begin
      Abort_Node := New_Node (N_Abort_Statement, Token_Ptr);
      Scan; -- past ABORT
      Set_Names (Abort_Node, New_List);

      loop
         Append (P_Name, Names (Abort_Node));
         exit when Token /= Tok_Comma;
         Scan; -- past comma
      end loop;

      TF_Semicolon;
      return Abort_Node;
   end P_Abort_Statement;

end Ch9;
