------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P A R . P R A G                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.45 $                             --
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

--  This function performs syntax checking on pragmas. Exactly what is syntax
--  and what is semantics is not always clear. A documentation line called
--  syntax check identifies the checks that are made in each case. The
--  remaining semantic checks are in the subunit Sem.Prag. Note that in
--  all cases (except pragma Restrictions), the syntax check includes
--  checking for appropriate presence of identifiers in pragma arguments.
--  Note that the checks here do *not* include any placement checks, since
--  this routine is called at a point where the pragma node has not been
--  linked into the tree.

separate (Par)
function Prag (Pragma_Node : Node_Id; Semi : Source_Ptr) return Node_Id is
   Pragma_Id_Node : constant Node_Id    := Identifier (Pragma_Node);
   Pragma_Sloc    : constant Source_Ptr := Sloc (Pragma_Node);
   Nargs          : Nat;
   Arg_Node       : Node_Id;
   Expr_Node      : Node_Id;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Arg1 return Node_Id;
   function Arg2 return Node_Id;
   function Arg3 return Node_Id;
   --  Obtain specified Pragma_Argument_Association

   procedure Check_Ada_83_Warning;
   --  Issues a warning message for the current pragma if operating in Ada 83
   --  mode (used for language pragmas that are not a standard part of Ada 83).
   --  This procedure does not raise Error_Resync.

   procedure Check_Ada_9X_Warning;
   --  Issues a warning message for the current pragma if operating in Ada 9X
   --  mode (used for pragmas that are now obsolete in Ada 9X). This procedure
   --  does not raise Error_Resync.

   procedure Check_Arg_Count (Required : Int);
   --  Check argument count for pragma = Required.
   --  If not give error and raise Error_Resync.

   procedure Check_At_Least_One_Argument;
   --  Check there is at least one argument.
   --  If not give error and raise Error_Resync.

   procedure Check_Expression_Is_Convention (Arg : Node_Id);
   --  Check the expression of the specified argument to make sure that it
   --  is a valid convention name. If not give error and raise Error_Resync.
   --  This procedure also checks for the possible allowed presence of the
   --  identifier Convention for this argument.

   procedure Check_Expression_Is_Entity (Arg : Node_Id);
   --  Check the expression of the specified argument to make sure that it
   --  is a valid entity name (i.e. either an identifier or an operator
   --  symbol). If not, give an error and raise Error_Resync. This procedure
   --  also checks for the possible allowed presence of the identifier
   --  Entity for this argument.

   procedure Check_Expression_Is_Identifier (Arg : Node_Id);
   --  Check the expression of the specified argument to make sure that it
   --  is an identifier. If not give error and raise Error_Resync.

   procedure Check_Expression_Is_Integer_Literal (Arg : Node_Id);
   --  Check the expression of the specified argument to make sure that it
   --  is an integer literal. If not give error and raise Error_Resync.

   procedure Check_Expression_Is_Library_Unit_Name (Arg : Node_Id);
   --  Check the expression of the specified argument to make sure that it
   --  is of the form of a library unit name, i.e. that it is an identifier
   --  or a selected component with a selector name that is itself an
   --  identifier. If not of this form, give error and raise Error_Resync.

   procedure Check_Expression_Is_Locking_Policy (Arg : Node_Id);
   --  Check the expression of the specified argument to make sure that it is
   --  a valid locking policy name. If not give error and raise Error_Resync.

   procedure Check_Expression_Is_Queuing_Policy (Arg : Node_Id);
   --  Check the expression of the specified argument to make sure that it is
   --  a valid queuing policy name. If not give error and raise Error_Resync.

   procedure Check_Expression_Is_Task_Dispatching_Policy (Arg : Node_Id);
   --  Check the expression of the specified argument to make sure that it is
   --  a valid task dispatching policy name. If not give error and raise
   --  Error_Resync.

   procedure Check_Expression_Is_One_Of (Arg : Node_Id; N1, N2 : Name_Id);
   --  Check the expression of the specified argument to make sure that it
   --  is an identifier whose name matches either N1 or N2. If not, then
   --  issue an error message and raise Error_Resync.

   procedure Check_Library_Unit_Pragma;
   --  Library unit pragmas (10.1.5) have at most one argument, which must
   --  be the current compilation unit.

   procedure Check_No_Identifier (Arg : Node_Id);
   --  Checks that the given argument does not have an identifier. If an
   --  identifier is present, then an error message is issued, and
   --  Error_Resync is raised.

   procedure Check_No_Identifiers;
   --  Checks that none of the arguments to the pragma has an identifier.
   --  If any argument has an identifier, then an error message is issued,
   --  and Error_Resync is raised.

   procedure Check_Optional_Identifier (Arg : Node_Id; Id : Name_Id);
   --  Checks if the given argument has an identifier, and if so, requires
   --  it to match the given identifier name. If there is a non-matching
   --  identifier, then an error message is given and Error_Resync raised.

   --------------
   -- Arg1,2,3 --
   --------------

   function Arg1 return Node_Id is
   begin
      return First (Pragma_Argument_Associations (Pragma_Node));
   end Arg1;

   function Arg2 return Node_Id is
   begin
      return Next (Arg1);
   end Arg2;

   function Arg3 return Node_Id is
   begin
      return Next (Arg2);
   end Arg3;

   --------------------------
   -- Check_Ada_83_Warning --
   --------------------------

   procedure Check_Ada_83_Warning is
   begin
      if Ada_83 then
         Error_Msg ("pragma& is not standard in Ada 83?", Pragma_Sloc);
      end if;

      --  Put back the node for subsequent error messages, because this is a
      --  situation where we do not raise Error_Resync and get out immediately

      Error_Msg_Node_1 := Pragma_Id_Node;
   end Check_Ada_83_Warning;

   --------------------------
   -- Check_Ada_9X_Warning --
   --------------------------

   procedure Check_Ada_9X_Warning is
   begin
      if Ada_9X then
         Error_Msg ("pragma& is obsolete in Ada 9X?", Pragma_Sloc);
      end if;

      --  Put back the node for subsequent error messages, because this is a
      --  situation where we do not raise Error_Resync and get out immediately

      Error_Msg_Node_1 := Pragma_Id_Node;
   end Check_Ada_9X_Warning;

   ---------------------
   -- Check_Arg_Count --
   ---------------------

   procedure Check_Arg_Count (Required : Int) is
   begin
      if Nargs /= Required then
         Error_Msg ("wrong number of arguments for pragma&", Pragma_Sloc);
         raise Error_Resync;
      end if;
   end Check_Arg_Count;

   ---------------------------------
   -- Check_At_Least_One_Argument --
   ---------------------------------

   procedure Check_At_Least_One_Argument is
   begin
      if Nargs = 0 then
         Error_Msg ("pragma& requires at least one argument", Pragma_Sloc);
         raise Error_Resync;
      end if;
   end Check_At_Least_One_Argument;

   ------------------------------------
   -- Check_Expression_Is_Convention --
   ------------------------------------

   procedure Check_Expression_Is_Convention (Arg : Node_Id) is
   begin
      Check_Expression_Is_Identifier (Arg);
      Check_Optional_Identifier (Arg, Name_Convention);

      if not Is_Convention_Name (Chars (Expression (Arg))) then
         Error_Msg
           ("argument of pragma& is not valid convention name",
             Sloc (Expression (Arg)));
         raise Error_Resync;
      end if;
   end Check_Expression_Is_Convention;

   --------------------------------
   -- Check_Expression_Is_Entity --
   --------------------------------

   procedure Check_Expression_Is_Entity (Arg : Node_Id) is
   begin
      if Nkind (Expression (Arg)) /= N_Operator_Symbol then
         Check_Expression_Is_Identifier (Arg);
      end if;

      Check_Optional_Identifier (Arg, Name_Entity);
   end Check_Expression_Is_Entity;

   ------------------------------------
   -- Check_Expression_Is_Identifier --
   ------------------------------------

   procedure Check_Expression_Is_Identifier (Arg : Node_Id) is
   begin
      if Nkind (Expression (Arg)) /= N_Identifier then
         Error_Msg
           ("argument for pragma& must be identifier",
             Sloc (Expression (Arg)));
         raise Error_Resync;
      end if;
   end Check_Expression_Is_Identifier;

   -----------------------------------------
   -- Check_Expression_Is_Integer_Literal --
   -----------------------------------------

   procedure Check_Expression_Is_Integer_Literal (Arg : Node_Id) is
   begin
      if Nkind (Expression (Arg)) /= N_Integer_Literal then
         Error_Msg
           ("argument for pragma& must be integer literal",
             Sloc (Expression (Arg)));
         raise Error_Resync;
      end if;
   end Check_Expression_Is_Integer_Literal;

   -------------------------------------------
   -- Check_Expression_Is_Library_Unit_Name --
   -------------------------------------------

   procedure Check_Expression_Is_Library_Unit_Name (Arg : Node_Id) is
      Argx : constant Node_Id := Expression (Arg);

   begin
      if Nkind (Argx) /= N_Identifier
        and then (Nkind (Argx) /= N_Selected_Component
          or else Nkind (Selector_Name (Argx)) /= N_Identifier)
      then
         Error_Msg
           ("argument for pragma& must be library unit name", Sloc (Argx));
         raise Error_Resync;
      end if;
   end Check_Expression_Is_Library_Unit_Name;

   ----------------------------------------
   -- Check_Expression_Is_Locking_Policy --
   ----------------------------------------

   procedure Check_Expression_Is_Locking_Policy (Arg : Node_Id) is
   begin
      Check_Expression_Is_Identifier (Arg);

      if not Is_Locking_Policy_Name (Chars (Expression (Arg))) then
         Error_Msg
           ("argument of pragma& is not valid locking policy name",
             Sloc (Expression (Arg)));
         raise Error_Resync;
      end if;
   end Check_Expression_Is_Locking_Policy;

   ----------------------------------------
   -- Check_Expression_Is_Queuing_Policy --
   ----------------------------------------

   procedure Check_Expression_Is_Queuing_Policy (Arg : Node_Id) is
   begin
      Check_Expression_Is_Identifier (Arg);

      if not Is_Queuing_Policy_Name (Chars (Expression (Arg))) then
         Error_Msg
           ("argument of pragma& is not valid queuing policy name",
             Sloc (Expression (Arg)));
         raise Error_Resync;
      end if;
   end Check_Expression_Is_Queuing_Policy;

   -------------------------------------------------
   -- Check_Expression_Is_Task_Dispatching_Policy --
   -------------------------------------------------

   procedure Check_Expression_Is_Task_Dispatching_Policy (Arg : Node_Id) is
   begin
      Check_Expression_Is_Identifier (Arg);

      if not Is_Task_Dispatching_Policy_Name (Chars (Expression (Arg))) then
         Error_Msg
           ("argument of pragma& is not valid task dispatching policy name",
             Sloc (Expression (Arg)));
         raise Error_Resync;
      end if;
   end Check_Expression_Is_Task_Dispatching_Policy;

   --------------------------------
   -- Check_Expression_Is_One_Of --
   --------------------------------

   procedure Check_Expression_Is_One_Of (Arg : Node_Id; N1, N2 : Name_Id) is
      Argx : constant Node_Id := Expression (Arg);

   begin
      Check_Expression_Is_Identifier (Arg);

      if Chars (Argx) /= N1 and then Chars (Argx) /= N2 then
         Error_Msg_Name_1 := N1;
         Error_Msg_Name_2 := N2;

         Error_Msg
           ("argument for pragma& must be% or%", Sloc (Argx));
         raise Error_Resync;
      end if;
   end Check_Expression_Is_One_Of;

   -------------------------------
   -- Check_Library_Unit_Pragma --
   -------------------------------

   procedure Check_Library_Unit_Pragma is
   begin
      Check_Ada_83_Warning;

      if Nargs /= 0 then
         Check_No_Identifiers;
         Check_Arg_Count (1);
         Check_Expression_Is_Library_Unit_Name (Arg1);
      end if;
   end Check_Library_Unit_Pragma;

   -------------------------
   -- Check_No_Identifier --
   -------------------------

   procedure Check_No_Identifier (Arg : Node_Id) is
   begin
      if Present (Identifier (Arg)) then
         Error_Msg ("pragma& does not permit named arguments",
           Sloc (Identifier (Arg_Node)));
         raise Error_Resync;
      end if;
   end Check_No_Identifier;

   --------------------------
   -- Check_No_Identifiers --
   --------------------------

   procedure Check_No_Identifiers is
   begin
      if Nargs > 0 then
         Arg_Node := Arg1;
         while Present (Arg_Node) loop
            Check_No_Identifier (Arg_Node);
            Arg_Node := Next (Arg_Node);
         end loop;
      end if;
   end Check_No_Identifiers;

   -------------------------------
   -- Check_Optional_Identifier --
   -------------------------------

   procedure Check_Optional_Identifier (Arg : Node_Id; Id : Name_Id) is
   begin
      if Present (Arg) and then Present (Identifier (Arg)) then
         if Chars (Identifier (Arg)) /= Id then
            Error_Msg_Name_1 := Id;
            Error_Msg ("pragma& argument expects identifier%",
              Sloc (Identifier (Arg)));
            raise Error_Resync;
         end if;
      end if;
   end Check_Optional_Identifier;

   ----------
   -- Prag --
   ----------

begin
   Error_Msg_Node_1 := Pragma_Id_Node;

   --  Count number of arguments. This loop also checks if any of the arguments
   --  are Error, indicating a syntax error as they were parsed. If so, we
   --  simply return, because we get into trouble with cascaded errors if we
   --  try to perform our error checks on junk arguments.

   Nargs := 0;

   if List_Present (Pragma_Argument_Associations (Pragma_Node)) then
      Arg_Node := Arg1;

      while Arg_Node /= Empty loop
         Nargs := Nargs + 1;

         if Expression (Arg_Node) = Error then
            return Error;
         end if;

         Arg_Node := Next (Arg_Node);
      end loop;
   end if;

   --  Remaining processing is pragma dependent

   case Get_Pragma_Id (Chars (Pragma_Id_Node)) is

      ------------------------
      -- Abort_Defer (GNAT) --
      ------------------------

      --  pragma Abort_Defer;

      --  This pragma is implementation (GNAT) defined. It must appear at
      --  the start of the statement sequence of a handled sequence of
      --  statements (right after the begin). It has the effect of deferring
      --  aborts for the sequence of statements (but not for the declarations
      --  or handlers, if any, associated with this statement sequence).

      --  Syntax check: no arguments

      when Pragma_Abort_Defer =>
         Check_Arg_Count (0);

      -------------------                                                      
      -- Ada_83 (GNAT) --                                                      
      -------------------                                                      

      --  pragma Ada_83;                                                       

      --  This pragma is implementation (GNAT) defined. Its effect is to       
      --  establish Ada 83 mode for the remainder of current unit, regardless  
      --  of the mode set by the command line switches.                        

      --  Syntax check: no arguments                                           

      when Pragma_Ada_83 =>                                                    
         Check_Arg_Count (0);                                                  
         Ada_83 := True;                                                       
         Ada_9X := False;                                                      

      -------------------
      -- Ada_9X (GNAT) --
      -------------------

      --  pragma Ada_9X;

      --  This pragma is implementation (GNAT) defined. Its effect is to
      --  establish Ada 9X for the remainder of the current unit, regardless
      --  of the mode set by the command line switches.

      --  Syntax check: no arguments

      when Pragma_Ada_9X =>
         Check_Arg_Count (0);
         Ada_83 := False;
         Ada_9X := True;

      ------------------------------
      -- All_Calls_Remote (I.2.3) --
      ------------------------------

      --  pragma All_Calls_Remote (library_package_NAME);

      --  Syntax check: one argument

      when Pragma_All_Calls_Remote =>
         Check_Ada_83_Warning;
         Check_No_Identifiers;
         Check_Arg_Count (1);

      -------------------
      -- Assert (GNAT) --
      -------------------

      --  pragma Assert (Boolean_EXPRESSION [,PROCEDURE_CALL_STATEMENT]);

      --  This pragma is implementation (GNAT) defined. Its effect depends
      --  on the setting of the Assertions_Enabled flag in Opt. If this
      --  flag is off (False), then the pragma has no effect. If the flag
      --  is on (True), then the semantics of the pragma is equivalent to:

      --    if not Boolean_EXPRESSION then
      --       [PROCEDURE_CALL_STATEMENT;]
      --       raise System.Assertions.Assert_Failure;
      --    end if;

      --  Syntax check: one or two arguments. The second argument, if present,
      --  is of the form of a procedure call, which is parsed either as a name
      --  or as a function call. It is then converted to the corresponding
      --  procedure call.

      when Pragma_Assert =>
         Check_No_Identifiers;

         if Nargs /= 2 then
            Check_Arg_Count (1);

         else
            declare
               Expr : constant Node_Id := New_Copy (Expression (Arg2));

            begin
               if Nkind (Expr) /= N_Indexed_Component
                 and then Nkind (Expr) /= N_Function_Call
                 and then Nkind (Expr) /= N_Identifier
                 and then Nkind (Expr) /= N_Selected_Component
               then
                  Error_Msg 
                    ("2nd arg of pragma& is not procedure call", Sloc (Expr));
                  raise Error_Resync;
               else
                  Set_Debug_Statement
                    (Pragma_Node, P_Statement_Call_Name (Expr));
               end if;
            end;
         end if;

      --------------------------
      -- Asynchronous (I.4.1) --
      --------------------------

      --  pragma All_synchronous (DIRECT_NAME);

      --  Syntax check: one argument, which must be an identifier

      when Pragma_Asynchronous =>
         Check_Ada_83_Warning;
         Check_No_Identifiers;
         Check_Arg_Count (1);
         Check_Expression_Is_Identifier (Arg1);

      ------------------
      -- Atomic (G.5) --
      ------------------

      --  pragma Atomic (DIRECT_NAME);

      --  Syntax check: one argument, which must be an identifier

      when Pragma_Atomic =>
         Check_Ada_83_Warning;
         Check_No_Identifiers;
         Check_Arg_Count (1);
         Check_Expression_Is_Identifier (Arg1);

      -----------------------------
      -- Atomic_Components (G.5) --
      -----------------------------

      --  pragma Atomic_Components (array_DIRECT_NAME);

      --  Syntax check: one argument, which must be an identifier

      when Pragma_Atomic_Components =>
         Check_Ada_83_Warning;
         Check_No_Identifiers;
         Check_Arg_Count (1);
         Check_Expression_Is_Identifier (Arg1);

      ----------------------------
      -- Attach_Handler (G.3.1) --
      ----------------------------

      --  pragma Attach_Handler (handler_NAME, EXPRESSION);

      --  Syntax check: two arguments

      when Pragma_Attach_Handler =>
         Check_Ada_83_Warning;
         Check_No_Identifiers;
         Check_Arg_Count (2);

      --------------------------
      -- Controlled (13.11.3) --
      --------------------------

      --  pragma Controlled (first_subtype_DIRECT_NAME);

      --  Syntax check: one argument, which must be an identifier

      when Pragma_Controlled =>
         Check_No_Identifiers;
         Check_Arg_Count (1);
         Check_Expression_Is_Identifier (Arg1);

      ----------------------
      -- Convention (M.1) --
      ----------------------

      --  pragma Convention ([Convention =>] convention_IDENTIFIER,
      --    [Entity =>] DIRECT_NAME);

      when Pragma_Convention =>
         Check_Ada_83_Warning;
         Check_Arg_Count (2);
         Check_Expression_Is_Convention (Arg1);
         Check_Expression_Is_Entity (Arg2);

      ------------------
      -- Debug (GNAT) --
      ------------------

      --  pragma Debug (PROCEDURE_CALL_STATEMENT);

      --  This pragma is implementation (GNAT) defined. Its effect depends
      --  on the setting of the Assertions_Enabled flag in Opt. If this
      --  flag is off (False), then the pragma has no effect. If the flag
      --  is on (True), then the semantics of the pragma is equivalent to
      --  the procedure call.

      --  Syntax check: one argument which must be of the form of a procedure
      --  call, parsed either as a name or as a function call. It is then 
      --  converted to the corresponding procedure call.

      when Pragma_Debug =>
         Check_No_Identifiers;
         Check_Arg_Count (1);

         declare
            Expr : constant Node_Id := New_Copy (Expression (Arg1));

         begin
            if Nkind (Expr) /= N_Indexed_Component
              and then Nkind (Expr) /= N_Function_Call
              and then Nkind (Expr) /= N_Identifier
              and then Nkind (Expr) /= N_Selected_Component
            then
               Error_Msg
                 ("argument of pragma& is not procedure call", Sloc (Expr));
               raise Error_Resync;
            else
               Set_Debug_Statement
                 (Pragma_Node, P_Statement_Call_Name (Expr));
            end if;
         end;

      ---------------------
      -- Elaborate (N.4) --
      ---------------------

      --  pragma Elaborate (library_unit_NAME {, library_unit_NAME});

      --  Syntax check: at least one argument, all arguments of the form
      --   of either identifiers, or selected components with the selector
      --   name being an identifier.

      when Pragma_Elaborate =>
         Check_No_Identifiers;
         Check_At_Least_One_Argument;

         Arg_Node := Arg1;

         while Present (Arg_Node) loop
            Check_Expression_Is_Library_Unit_Name (Arg_Node);
            Arg_Node := Next (Arg_Node);
         end loop;

      ----------------------------
      -- Elaborate_All (10.2.1) --
      ----------------------------

      --  pragma Elaborate_All (library_unit_NAME {, library_unit_NAME});

      --  Syntax check: at least one argument, all arguments of the form
      --   of either identifiers, or selected components with the selector
      --   name being an identifier.

      when Pragma_Elaborate_All =>
         Check_Ada_83_Warning;
         Check_No_Identifiers;
         Check_At_Least_One_Argument;

         Arg_Node := Arg1;
         while Present (Arg_Node) loop
            Check_Expression_Is_Library_Unit_Name (Arg_Node);
            Arg_Node := Next (Arg_Node);
         end loop;

      -----------------------------
      -- Elaborate_Body (10.2.1) --
      -----------------------------

      --  pragma Elaborate_Body [(library_unit_NAME)];

      --  Syntax check: at most one argument, which, if present, is the
      --  current compilation unit name

      when Pragma_Elaborate_Body =>
         Check_Library_Unit_Pragma;

      ------------------
      -- Export (M.1) --
      ------------------

      --  pragma Export ([Convention =>] convention_IDENTIFIER,
      --    [Entity =>] DIRECT_NAME [, [Link_Name =>] STRING_EXPRESSION]);

      --  Syntax check: 2 or 3 arguments. 1st argument must be a 
      --  convention, 2nd argument must be an entity

      when Pragma_Export =>
         Check_Ada_83_Warning;

         if Nargs = 3 then
            Check_Optional_Identifier (Arg3, Name_Link_Name);
         else
            Check_Arg_Count (2);
         end if;

         Check_Expression_Is_Convention (Arg1);
         Check_Expression_Is_Entity (Arg2);

      ------------------
      -- Import (M.1) --
      ------------------

      --  pragma Import ([Convention =>] convention_IDENTIFIER,
      --    [Entity =>] DIRECT_NAME [, [Link_Name =>] STRING_EXPRESSION]);

      --  Syntax check: 2 or 3 arguments. 1st argument must be a convention,
      --  2nd argument must be an entity


      when Pragma_Import =>
         Check_Ada_83_Warning;

         if Nargs = 3 then
            Check_Optional_Identifier (Arg3, Name_Link_Name);
         else
            Check_Arg_Count (2);
         end if;

         Check_Expression_Is_Convention (Arg1);
         Check_Expression_Is_Entity (Arg2);

      --------------------
      -- Improve (GNAT) --
      --------------------

      --  pragma Improve (Time | Space, IDENTIFIER);

      --  This pragma is implementation (GNAT) defined. It has is ignored
      --  semantically, and is provided only for compatibility with Alsys,
      --  where it is used to excercise control over variant record layout.

      --  Syntax check: Two arguments, the first is either Time or Space,
      --  and the second must be an identifier.

      when Pragma_Improve =>
         Check_No_Identifiers;
         Check_Arg_Count (2);
         Check_Expression_Is_One_Of (Arg1, Name_Time, Name_Space);
         Check_Expression_Is_Identifier (Arg2);

      --------------------
      -- Inline (6.3.2) --
      --------------------

      --  pragma Inline (NAME {, NAME});

      --  Syntax check: at least one argument, and the arguments are either
      --  of the form of identifiers, or of selected components.

      when Pragma_Inline =>
         Check_No_Identifiers;
         Check_At_Least_One_Argument;

         Arg_Node := Arg1;
         while Present (Arg_Node) loop
            Expr_Node := Expression (Arg_Node);

            if Nkind (Expr_Node) /= N_Identifier
              and then Nkind (Expr_Node) /= N_Selected_Component
              and then Nkind (Expr_Node) /= N_Operator_Symbol
            then
               Error_Msg
                 ("argument of pragma& is not subprogram name",
                   Sloc (Expr_Node));
            end if;

            Arg_Node := Next (Arg_Node);
         end loop;

      ------------------------------
      -- Inspection_Point (L.2.2) --
      ------------------------------

      --  pragma Inspection_Point [(object_NAME {, object_NAME})];

      --  Syntax check: None

      when Pragma_Inspection_Point =>
         Check_Ada_83_Warning;
         Check_No_Identifiers;

      ------------------------
      -- Interface (Ada 83) --
      ------------------------

      --  pragma Interface (convention_IDENTIFIER, DIRECT_NAME);

      --  Syntax check: two arguments, first is a convention name

      when Pragma_Interface =>
         Check_No_Identifiers;
         Check_Arg_Count (2);
         Check_Expression_Is_Convention (Arg1);

      ---------------------------
      -- Interface_Name (GNAT) --
      ---------------------------

      --  pragma Interface_Name ([Entity =>] DIRECT_NAME,
      --    [Link_Name =>] STRING_EXPRESSION);

      --  This pragma is implementation (GNAT) defined. It is an alternative
      --  way of specifying the interface name for an interfaced subprogram,
      --  and is provided for compatibility with Ada 83 compilers that use
      --  the pragma for this purpose.

      --  Syntax check: two arguments, first is an entity name

      when Pragma_Interface_Name =>
         Check_Arg_Count (2);
         Check_Expression_Is_Entity (Arg1);
         Check_Optional_Identifier (Arg2, Name_Link_Name);

      -------------------------------
      -- Interrupt_Handler (G.3.1) --
      -------------------------------

      --  pragma Interrupt_Handler (handler_NAME);

      --  Syntax check: one argument

      when Pragma_Interrupt_Handler =>
         Check_Ada_83_Warning;
         Check_Arg_Count (1);
         Check_No_Identifiers;

      ------------------------------
      -- Interrupt_Priority (H.1) --
      ------------------------------

      --  pragma Interrupt_Priority [(EXPRESSION)];

      --  Syntax check: one argument

      when Pragma_Interrupt_Priority =>
         Check_Ada_83_Warning;

         if Nargs /= 0 then
            Check_Arg_Count (1);
            Check_No_Identifiers;
         end if;

      -----------------
      -- List (2.18) --
      -----------------

      --  pragma List (Off | On)

      --  Syntax check: one argument, which must be On or Off

      when Pragma_List =>
         Check_No_Identifiers;
         Check_Arg_Count (1);
         Check_Expression_Is_One_Of (Arg1, Name_On, Name_Off);

         --  We unconditionally make a List_On entry for the pragma, so that
         --  in the List (Off) case, the pragma will print even in a region
         --  of code with listing turned off (this is required!)

         List_Pragmas.Increment_Last;
         List_Pragmas.Table (List_Pragmas.Last) :=
           (Ptyp => List_On, Ploc => Sloc (Pragma_Node));

         --  Now generate the list off entry for pragma List (Off)

         if Chars (Expression (Arg1)) = Name_Off then
            List_Pragmas.Increment_Last;
            List_Pragmas.Table (List_Pragmas.Last) :=
              (Ptyp => List_Off, Ploc => Semi);
         end if;

      --------------------------
      -- Locking_Policy (H.3) --
      --------------------------

      --  pragma Locking_Policy (policy_IDENTIFIER);

      --  Syntax check: one argument which is a locking policy identifier

      when Pragma_Locking_Policy =>
         Check_Ada_83_Warning;
         Check_Arg_Count (1);
         Check_No_Identifiers;
         Check_Expression_Is_Locking_Policy (Arg1);

      -------------------------------
      -- Normalize_Scalars (L.1.1) --
      -------------------------------

      --  pragma Normalize_Scalars;

      --  Syntax check: no arguments

      when Pragma_Normalize_Scalars =>
         Check_Ada_83_Warning;
         Check_Arg_Count (0);

      --------------------------
      -- Memory_Size (Ada 83) --
      --------------------------

      --  pragma Memory_Size (NUMERIC_LITERAL)

      --  Syntax check: one argument, which must be a integer literal

      when Pragma_Memory_Size =>
         Check_Ada_9X_Warning;
         Check_No_Identifiers;
         Check_Arg_Count (1);
         Check_Expression_Is_Integer_Literal (Arg1);

      --------------------
      -- Optimize (2.8) --
      --------------------

      --  pragma Optimize (Time | Space)

      --  Syntax check: one argument, which must be Time or Space

      when Pragma_Optimize =>
         Check_No_Identifiers;
         Check_Arg_Count (1);
         Check_Expression_Is_One_Of (Arg1, Name_Time, Name_Space);

      -----------------
      -- Pack (13.2) --
      -----------------

      --  pragma Pack (first_subtype_DIRECT_NAME);

      --  The DIRECT_NAME must be an IDENTIFIER

      --  Syntax check: one argument, which must be an identifier

      when Pragma_Pack =>
         Check_No_Identifiers;
         Check_Arg_Count (1);
         Check_Expression_Is_Identifier (Arg1);

      ----------------
      -- Page (2.8) --
      ----------------

      --  pragma Page;

      --  Syntax check: no arguments

      when Pragma_Page =>
         Check_No_Identifiers;
         Check_Arg_Count (0);
         List_Pragmas.Increment_Last;
         List_Pragmas.Table (List_Pragmas.Last) := (Page, Semi);

      ---------------------------
      -- Preelaborate (10.2.1) --
      ---------------------------

      --  pragma Preelaborate [(library_unit_NAME)];

      --  Syntax check: at most one argument, which, if present, is the
      --  current compilation unit name

      when Pragma_Preelaborate =>
         Check_Library_Unit_Pragma;

      --------------------
      -- Priority (H.1) --
      --------------------

      --  pragma Priority (EXPRESSION);

      --  Syntax check: one argument

      when Pragma_Priority =>
         Check_No_Identifiers;
         Check_Arg_Count (1);

      -------------------
      -- Pure (10.2.1) --
      -------------------

      --  pragma Pure [(library_unit_NAME)];

      --  Syntax check: at most one argument, which, if present, is the
      --  current compilation unit name.

      when Pragma_Pure =>
         Check_Library_Unit_Pragma;

      --------------------------
      -- Queuing_Policy (H.4) --
      --------------------------

      --  pragma Queuing_Policy (policy_IDENTIFIER);

      --  Syntax check: one argument which is a queuing policy identifier

      when Pragma_Queuing_Policy =>
         Check_Ada_83_Warning;
         Check_Arg_Count (1);
         Check_No_Identifiers;
         Check_Expression_Is_Queuing_Policy (Arg1);

      -----------------------------------
      -- Remote_Call_Interface (I.2.3) --
      -----------------------------------

      --  Pragma Remote_Call_Interface [(library_package_NAME)];

      --  Syntax check: at most one argument, which, if present, is the
      --  current compilation unit name

      when Pragma_Remote_Call_Interface =>
         Check_Library_Unit_Pragma;

      --------------------------
      -- Remote_Types (I.2.2) --
      --------------------------

      --  Pragma Remote_Types [(library_package_NAME)];

      --  Syntax check: at most one argument, which, if present, is the
      --  current compilation unit name

      when Pragma_Remote_Types =>
         Check_Library_Unit_Pragma;

      --------------------------
      -- Restrictions (13.12) --
      --------------------------

      --  pragma Restrictions (RESTRICTION {, RESTRICTION});

      --  RESTRICTION ::=
      --    restriction_IDENTIFIER
      --  | restriction_parameter_IDENTIFIER => EXPRESSION

      --  Syntax: at least one argument

      when Pragma_Restrictions =>
         Check_Ada_83_Warning;
         Check_At_Least_One_Argument;

      ------------------------
      -- Reviewable (L.2.1) --
      ------------------------

      --  pragma Reviewable;

      --  Syntax: no arguments

      when Pragma_Reviewable =>
         Check_Ada_83_Warning;
         Check_Arg_Count (0);

      ---------------------
      -- Shared (Ada 83) --
      ---------------------

      --  pragma Shared (DIRECT_NAME)

      --  Syntax check: one argument, which must be an identifier

      when Pragma_Shared =>
         Check_Ada_9X_Warning;
         Check_No_Identifiers;
         Check_Arg_Count (1);
         Check_Expression_Is_Identifier (Arg1);

      ----------------------------
      -- Shared_Passive (I.2.1) --
      ----------------------------

      --  Pragma Shared_Passive [(library_package_NAME)];

      --  Syntax check: at most one argument, which, if present, is the
      --  current compilation unit name

      when Pragma_Shared_Passive =>
         Check_Library_Unit_Pragma;

      ---------------------------
      -- Storage_Unit (Ada 83) --
      ---------------------------

      --  pragma Storage_Unit (NUMERIC_LITERAL);

      --  Syntax check: one argument, which must be the integer literal 8

      when Pragma_Storage_Unit =>
         Check_Ada_9X_Warning;
         Check_No_Identifiers;
         Check_Arg_Count (1);
         Check_Expression_Is_Integer_Literal (Arg1);

         if Intval (Expression (Arg1)) /= 8 then
            Error_Msg
              ("the only allowed argument for pragma& is 8", Pragma_Sloc);
         end if;

      ---------------------
      -- Suppress (11.5) --
      ---------------------

      --  pragma Suppress (IDENTIFIER [, [On =>] NAME]);

      --  Syntax check: first argument must be an identifier which is a
      --  valid check name. Second argument must be named On if name given.

      when Pragma_Suppress =>
         Check_No_Identifier (Arg1);
         Check_Optional_Identifier (Arg2, Name_On);
         Check_At_Least_One_Argument;
         Check_Expression_Is_Identifier (Arg1);

         if not Is_Check_Name (Chars (Expression (Arg1))) then
            Error_Msg
              ("argument of pragma& is not valid check name",
                Sloc (Expression (Arg1)));
         end if;

      --------------------------
      -- System_Name (Ada 83) --
      --------------------------

      --  pragma System_Name (DIRECT_NAME);

      --  Syntax check: one argument, which must be the identifier GNAT

      when Pragma_System_Name =>
         Check_Ada_9X_Warning;
         Check_No_Identifiers;
         Check_Arg_Count (1);
         Check_Expression_Is_One_Of (Arg1, Name_Gcc, Name_Gnat);

      -------------------------------------
      -- Task_Dispatching_Policy (H.2.2) --
      -------------------------------------

      --  pragma Task_Dispatching_Policy (DIRECT_NAME);

      --  Syntax check: one argument which is a task dispatching
      --  policy identifier

      when Pragma_Task_Dispatching_Policy =>
         Check_Ada_83_Warning;
         Check_Arg_Count (1);
         Check_No_Identifiers;
         Check_Expression_Is_Task_Dispatching_Policy (Arg1);

      ----------------------------
      -- Task_Stack_Size (GNAT) --
      ----------------------------

      --  pragma Task_Stack_Size (EXPRESSION);

      --  This pragma is implementation (GNAT) defined. It must occur within
      --  a task definition, and specifies the task stack size for the task
      --  in a manner similar to the use of an attribute definition clause
      --  specifying the Storage_Size of a task type. The expression may
      --  reference discriminants of the task type, allowing individual task
      --  objects to specify the task stack size.

      --  Syntax check: one argument

      when Pragma_Task_Stack_Size =>
         Check_No_Identifiers;
         Check_Arg_Count (1);

      --------------------
      -- Volatile (G.5) --
      --------------------

      --  pragma Volatile (DIRECT_NAME);

      --  Syntax check: one argument, which must be an identifier

      when Pragma_Volatile =>
         Check_Ada_83_Warning;
         Check_No_Identifiers;
         Check_Arg_Count (1);
         Check_Expression_Is_Identifier (Arg1);

      -------------------------------
      -- Volatile_Components (G.5) --
      -------------------------------

      --  pragma Volatile_Components (array_DIRECT_NAME);

      --  Syntax check: one argument, which must be an identifier

      when Pragma_Volatile_Components =>
         Check_Ada_83_Warning;
         Check_No_Identifiers;
         Check_Arg_Count (1);
         Check_Expression_Is_Identifier (Arg1);

   end case;

   return Pragma_Node;

   --------------------
   -- Error Handling --
   --------------------

exception
   when Error_Resync =>
      return Error;

end Prag;
