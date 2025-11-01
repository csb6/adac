------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ P R A G                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.60 $                             --
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

--  This unit contains the semantic processing for all pragmas, both language
--  and implementation defined. To add a new pragma, see packages Par.Prag,
--  and Sem_Prag. A certain amount of syntax checking has been done in each
--  case by the routines in Par.Prag. This procedure carries out the remaining
--  semantic checks.

with Atree;    use Atree;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Tbuild;   use Tbuild;

package body Sem_Prag is

   --------------------
   -- Analyze_Pragma --
   --------------------

   procedure Analyze_Pragma (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      function Arg1 return Node_Id;
      function Arg2 return Node_Id;
      function Arg3 return Node_Id;
      --  Obtain specified Pragma_Argument_Association

      procedure Error_Pragma (Msg : Str);
      --  Outputs error message for current pragma. The message contains an &
      --  that will be replaced with the pragma name, and the flag is placed
      --  on the pragma itself.

      procedure Error_Pragma_Argument (Msg : Str; Arg : Node_Id);
      --  Outputs error message for current pragma. The message contains an &
      --  that will be replaced with the pragma name, and the flag is placed
      --  on the pragma argument specified by the Arg operand.

      procedure Find_Program_Unit_Name (Id : Node_Id);
      --  If the pragma is a compilation unit pragma,  the id must denote the
      --  compilation unit in the same compilation,  and the pragma must appear
      --  in the list of preceding or trailing pragmas. If it is a program
      --  unit pragma that is not a compilation unit pragma, then the
      --  identifier must be visible.

      procedure Pragma_Misplaced;
      --  Issue fatal error message for misplaced pragma

      procedure Pragma_Not_Implemented;
      --  Issue warning message for unimplemented pragma

      function Valid_Unit_Pragma (Is_Library_Unit : Boolean := True) 
                                                         return Boolean;
      --  Common legality checks for program and library unit  pragmas.

      Nargs : Int;
      --  Number of pragma argument associations

      --------------
      -- Arg1,2,3 --
      --------------

      function Arg1 return Node_Id is
      begin
         return First (Pragma_Argument_Associations (N));
      end Arg1;

      function Arg2 return Node_Id is
      begin
         return Next (Arg1);
      end Arg2;

      function Arg3 return Node_Id is
      begin
         return Next (Arg2);
      end Arg3;

      ------------------
      -- Error_Pragma --
      ------------------

      procedure Error_Pragma (Msg : Str) is
      begin
         Error_Msg_Node_1 := Identifier (N);
         Error_Msg (Msg, Loc);
      end Error_Pragma;

      ---------------------------
      -- Error_Pragma_Argument --
      ---------------------------

      procedure Error_Pragma_Argument (Msg : Str; Arg : Node_Id) is
      begin
         Error_Msg_Node_1 := Identifier (N);
         Error_Msg (Msg, Sloc (Arg));
      end Error_Pragma_Argument;

      ----------------------------
      -- Find_Program_Unit_Name --
      ----------------------------

      procedure Find_Program_Unit_Name (Id : Node_Id) is
         Unit_Name : Entity_Id;
         Unit_Kind : Node_Kind;
         P         : constant Node_Id := Parent (N);

      begin
         if Nkind (P) = N_Compilation_Unit then
            Unit_Kind := Nkind (Unit (P));

            if Unit_Kind = N_Subprogram_Declaration
              or else Unit_Kind = N_Package_Declaration
              or else Unit_Kind in N_Generic_Declaration
            then
               Unit_Name :=
                 Defining_Unit_Simple_Name (Specification (Unit (P)));

               if Chars (Id) = Chars (Unit_Name) then
                  Set_Entity (Id, Unit_Name);
                  Set_Etype (Id, Etype (Unit_Name));
               else
                  Error_Pragma
                    ("cannot find program unit referenced by pragma&");
                  Set_Etype (Id, Any_Type);
               end if;

            else
               Error_Pragma ("pragma& inapplicable to this unit");
                  Set_Etype (Id, Any_Type);
            end if;

         else
            Find_Name (Id);
         end if;

      end Find_Program_Unit_Name;

      ----------------------
      -- Pragma_Misplaced --
      ----------------------

      procedure Pragma_Misplaced is
      begin
         Error_Pragma ("incorrect placement of pragma&");
      end Pragma_Misplaced;

      ----------------------------
      -- Pragma_Not_Implemented --
      ----------------------------

      procedure Pragma_Not_Implemented is
      begin
         Error_Pragma ("pragma& not implemented?");
      end Pragma_Not_Implemented;

   -----------------------
   -- Valid_Unit_Pragma --
   -----------------------

   function Valid_Unit_Pragma (Is_Library_Unit : Boolean := True) 
     return Boolean
   is
      Decl        : Node_Id;
      Plist       : List_Id;
      Parent_Node : Node_Id;
      Unit_Name   : Entity_Id;
      Valid       : Boolean := True;

   begin
      if not Is_List_Member (N) then
         Pragma_Misplaced;
         Valid := False;

      else
         Plist := List_Containing (N);
         Parent_Node := List_Parent (Plist);

         if Parent_Node = Empty then
            Pragma_Misplaced;
            Valid := False;

         elsif Nkind (Parent_Node) = N_Compilation_Unit then

            --  Pragma must appear after a compilation_unit, and must have
            --  an argument with the right name.

            if Plist /= Following_Pragmas (Parent_Node) then
               Pragma_Misplaced;
               Valid := False;

            elsif Nargs > 0 then
               if not Same_Name (Expression (Arg1),
                  Defining_Unit_Name (Specification (Unit (Parent_Node))))
               then
                  Error_Pragma_Argument
                    ("pragma& argument is not current unit name", Arg1);
                  Valid := False;
               end if;

            else 
               Error_Msg_N ("missing argument in unit pragma", N);
               Valid := False;
            end if; 

         elsif N = First (Plist) then

            --  Name is optional, pragma applies to enclosing unit.

            if Nargs > 0 then
               Find_Name (Expression (Arg1));
               if Entity (Expression (Arg1)) /= Current_Scope then
                  Error_Msg_N 
                   ("argument of unit pragma must be enclosing unit", N);
                  Valid := False;
               end if;
            end if; 

         --  This is always illegal for library unit pragmas, because the
         --  name, if any, will denote a local unit.

         elsif Is_Library_Unit then
            Error_Msg_N ("invalid context for library unit pragma", N);

         --  If not first in  declarative part, name is required.

         elsif Nargs > 0 then
            Find_Name (Expression (Arg1));
            Unit_Name := Entity (Expression (Arg1));

            if Scope (Unit_Name) /= Current_Scope then
               Error_Msg_N 
                  ("argument of pragma must be in current scope", Arg1);
               Valid := False;

            else
               Decl := Get_Declaration_Node (Unit_Name);   

               if Nkind (Decl) /= N_Package_Declaration
                 and then Nkind (Decl) not in N_Generic_Instantiation
                 and then Nkind (Decl) /= N_Generic_Subprogram_Declaration
                 and then Nkind (Decl) /= N_Subprogram_Declaration
               then
                  Error_Msg_N ("invalid name in unit pragma", N);
                  Valid := False;
               end if;
            end if;

         else 
            Error_Msg_N ("missing argument in unit pragma", N);
            Valid := False;
         end if; 

      end if;

      return Valid;

   end Valid_Unit_Pragma;

   --------------------------------------------
   -- Start of processing for Analyze_Pragma --
   --------------------------------------------

   begin
      --  Count number of arguments

      declare
         Arg_Node : Node_Id;

      begin
         Nargs := 0;

         if List_Present (Pragma_Argument_Associations (N)) then
            Arg_Node := Arg1;

            while Arg_Node /= Empty loop
               Nargs := Nargs + 1;
               Arg_Node := Next (Arg_Node);
            end loop;
         end if;
      end;

      --  An enumeration type defines the pragmas that are supported by the
      --  implementation. Get_Pragma_Id (in package Prag) transorms a name
      --  into the corresponding enumeration value for the following case.

      case Get_Pragma_Id (Chars (Identifier (N))) is

         -----------------
         -- Abort_Defer --
         -----------------

         when Pragma_Abort_Defer => Abort_Defer : begin

            --  The only required semantic processing is to check the
            --  placement. This pragma must appear at the start of the
            --  statement sequence of a handled sequence of statements.

            if Nkind (Parent (N)) /= N_Handled_Sequence_Of_Statements
              or else N /= First (Statements (Parent (N)))
            then
               Pragma_Misplaced;
            end if;
         end Abort_Defer;

         ------------
         -- Ada_83 --
         ------------

         when Pragma_Ada_83 =>
            Ada_83 := True;
            Ada_9X := False;

         ------------
         -- Ada_9X --
         ------------

         when Pragma_Ada_9X =>
            Ada_83 := False;
            Ada_9X := True;

         ----------------------
         -- All_Calls_Remote --
         ----------------------

         when Pragma_All_Calls_Remote =>
            Pragma_Not_Implemented;

         ------------
         -- Assert --
         ------------

         when Pragma_Assert => Assert : begin

            --  If we are not in debug mode then rewrite the pragma with
            --  a null statement and do not even analyze the pragma.

            if not Assertions_Enabled then                                    
               Rewrite_Substitute_Tree (N, Make_Null_Statement (Loc));

            --  If we are in debug mode, then rewrite the pragma with its     
            --  corresponding if statement, and then analyze the statement    
            --  The expansion transforms:                                     

            --    pragma Assert (condition [,procedure_call]);                

            --  into                                                          

            --    if not condition then                                       
            --       [procedure_call;]                                        
            --       raise System.Assertions.Assert_Failure;                  
            --    end if;                                                     

            else                                                              
               declare                                                        
                  Stmts : List_Id;                                            

               begin                                                          
                  if Nargs = 2 then                                           
                     Stmts := New_List_1 (New_Copy (Debug_Statement (N)));    
                  else                                                        
                     Stmts := New_List;                                       
                  end if;                                                     

                  Append_To (Stmts,                                           
                    Make_Raise_Statement (Loc,                                
                      Name =>                                                 
                        New_Reference_To (RTE (RE_Assert_Failure), Loc)));    

                  Rewrite_Substitute_Tree (N,                                 
                    Make_If_Statement (Loc,                                   
                      Condition =>                                            
                        Make_Op_Not (Loc,                                     
                          Right_Opnd => Expression (Arg1)),                   
                      Then_Statements => Stmts));                             

                  Analyze (N);                                                
               end;                                                           
            end if;                                                           

         end Assert;

         ------------------
         -- Asynchronous --
         ------------------

         when Pragma_Asynchronous =>
            Pragma_Not_Implemented;

         ------------
         -- Atomic --
         ------------

         when Pragma_Atomic =>
            Pragma_Not_Implemented;

         -----------------------
         -- Atomic_Components --
         -----------------------

         when Pragma_Atomic_Components =>
            Pragma_Not_Implemented;

         --------------------
         -- Attach_Handler --
         --------------------

         when Pragma_Attach_Handler =>
            Pragma_Not_Implemented;

         ----------------
         -- Controlled --
         ----------------

         when Pragma_Controlled =>
            Pragma_Not_Implemented;

         ----------------
         -- Convention --
         ----------------

         when Pragma_Convention => Convention : begin
            declare
               Id          : Node_Id;
               Proc_Def_Id : Entity_Id;

            begin
               if Chars ( Expression (Arg1)) = Name_Intrinsic then
                  Id := Expression (Arg2);
                  Find_Program_Unit_Name (Id);

                  --  Remaining processing is needed only if we found the name.

                  if Etype (Id) /= Any_Type then
                     Proc_Def_Id := Entity (Id);

                     if Ekind (Proc_Def_Id) /= E_Procedure
                       and then Ekind (Proc_Def_Id) /= E_Function
                       and then Ekind (Proc_Def_Id) /= E_Generic_Procedure
                       and then Ekind (Proc_Def_Id) /= E_Generic_Function
                     then
                        Error_Pragma_Argument
                          ("second argument of pragma& must be a subprogram",
                            Id);

                     elsif Scope (Proc_Def_Id) /= Current_Scope then
                        Error_Pragma_Argument
                          ("pragma& must be in same declarative part", Id);
                     else

                        while Present (Proc_Def_Id)
                          and then Scope (Proc_Def_Id) = Current_Scope loop
                           Set_Is_Intrinsic   (Proc_Def_Id);
                           Proc_Def_Id := Homonym (Proc_Def_Id);
                        end loop;

                     end if;
                  end if;
               else
                  Pragma_Not_Implemented;
               end if;
            end;
         end Convention;

         -----------
         -- Debug --
         -----------

         when Pragma_Debug => Debug : begin

            --  If we are not in debug mode then rewrite the pragma with    
            --  a null statement and do not even analyze the pragma.        

            if not Assertions_Enabled then                                  
               Rewrite_Substitute_Tree (N, Make_Null_Statement (Loc));

            --  If we are in debug mode, then rewrite the pragma with its     
            --  corresponding procedure call, and then analyze the call.      

            else                                                              
               Rewrite_Substitute_Tree (N, New_Copy (Debug_Statement (N)));   
               Analyze (N);                                                   
               Resolve_Complete_Context (N, Standard_Void_Type);              
            end if;                                                           

         end Debug;

         ---------------
         -- Elaborate --
         ---------------

         when Pragma_Elaborate => Elaborate : begin
            declare
               Plist       : List_Id;
               Parent_Node : Node_Id;
               Arg         : Node_Id;
               Temp_Node   : Node_Id;

            begin
               --  Pragma must be in context items list of a compilation unit

               if not Is_List_Member (N) then
                  Pragma_Misplaced;
                  return;

               else
                  Plist := List_Containing (N);
                  Parent_Node := List_Parent (Plist);

                  if Parent_Node = Empty
                    or else Nkind (Parent_Node) /= N_Compilation_Unit
                    or else Context_Items (Parent_Node) /= Plist
                  then
                     Pragma_Misplaced;
                     return;
                  end if;
               end if;

               --  There can be no items following it on the list except
               --  other pragmas (it cannot appear before a with or use clause)

               Temp_Node := Next (N);

               while Present (Temp_Node) loop
                  if Nkind (Temp_Node) /= N_Pragma then
                     Pragma_Misplaced;
                     return;
                  end if;

                  Temp_Node := Next (Temp_Node);
               end loop;

               --  Finally, the arguments must all be units mentioned in a with
               --  clause in the same context clause. Note we already checked
               --  (in Par.Prag) that the arguments are either identifiers or

               Arg := Arg1;
               Outer : while Present (Arg) loop
                  Temp_Node := First (Plist);

                  Inner : while Temp_Node /= N loop
                     if Nkind (Temp_Node) = N_With_Clause
                       and then Same_Name (Name (Temp_Node), Expression (Arg))
                     then
                        Set_Elaborate_Present (Temp_Node, True);
                        exit Inner;
                     end if;

                     Temp_Node := Next (Temp_Node);
                  end loop Inner;

                  if Temp_Node = N then
                     Error_Pragma_Argument
                       ("Argument of pragma& is not with'ed unit", Arg);
                  end if;

                  Arg := Next (Arg);
               end loop Outer;
            end;
         end Elaborate;

         -------------------
         -- Elaborate_All --
         -------------------

         when Pragma_Elaborate_All => Elaborate_All : begin
            declare
               Plist       : List_Id;
               Parent_Node : Node_Id;
               Arg         : Node_Id;
               Temp_Node   : Node_Id;

            begin
               --  Pragma must be in context items list of a compilation unit

               if not Is_List_Member (N) then
                  Pragma_Misplaced;
                  return;

               else
                  Plist := List_Containing (N);
                  Parent_Node := List_Parent (Plist);

                  if Parent_Node = Empty
                    or else Nkind (Parent_Node) /= N_Compilation_Unit
                    or else Context_Items (Parent_Node) /= Plist
                  then
                     Pragma_Misplaced;
                     return;
                  end if;
               end if;

               --  Note: unlike pragma Elaborate, pragma Elaborate_All does not
               --  have to appear at the end of the context clause, but may
               --  appear mixed in with other items.

               --  Final check: the arguments must all be units mentioned in
               --  a with clause in the same context clause. Note that we
               --  already checked (in Par.Prag) that all the arguments are
               --  either identifiers or selected components.

               Arg := Arg1;
               Outr : while Present (Arg) loop
                  Temp_Node := First (Plist);

                  Innr : while Temp_Node /= N loop
                     if Nkind (Temp_Node) = N_With_Clause
                       and then Same_Name (Name (Temp_Node), Expression (Arg))
                     then
                        Set_Elaborate_All_Present (Temp_Node, True);
                        exit Innr;
                     end if;

                     Temp_Node := Next (Temp_Node);
                  end loop Innr;

                  if Temp_Node = N then
                     Error_Pragma_Argument
                       ("Argument of pragma& is not with'ed unit", Arg);
                  end if;

                  Arg := Next (Arg);
               end loop Outr;
            end;
         end Elaborate_All;

         --------------------
         -- Elaborate_Body --
         --------------------

         when Pragma_Elaborate_Body => Elaborate_Body : begin
            declare
               Plist      : List_Id;
               Cunit_Node : Node_Id;

            begin
               if Valid_Unit_Pragma then

                  Plist := List_Containing (N);
                  Cunit_Node := List_Parent (Plist);

                  --  Case of pragma appearing in declarative part. Only
                  --  legal if it is in a package specification.

                  if Nkind (Cunit_Node) /= N_Compilation_Unit then
                     if Nkind (Cunit_Node) = N_Package_Specification then
                        Cunit_Node := Parent (Parent (Cunit_Node));
                     else
                        Pragma_Misplaced;
                        return;
                     end if;
                  end if;

                  Set_Elaborate_Body_Present (Cunit_Node, True);
                  Set_Body_Required (Cunit_Node, True);
               end if;
            end;
         end Elaborate_Body;

         ------------
         -- Export --
         ------------

         when Pragma_Export =>
            Pragma_Not_Implemented;

         ------------
         -- Import --
         ------------

         --  The parser has verified that the first argument is a valid
         --  language convention name, and for the moment, the only such
         --  defined name is Convention_C, so we just assume that we are
         --  talking about C. All that needs to be done it to set a flag in
         --  the defining occurence of the subprogram to indicate that it is
         --  interfaced (and thus needs no body).

         when Pragma_Import => Import : begin
            declare
               Id          : Node_Id;
               Proc_Def_Id : Entity_Id;

            begin
               Id := Expression (Arg2);
               Find_Program_Unit_Name (Id);

               --  Remaining processing is needed only if we found the name

               if Etype (Id) /= Any_Type then
                  Proc_Def_Id := Entity (Id);

                  if not Is_Subprogram (Proc_Def_Id) then
                     Error_Pragma_Argument
                       ("second argument of pragma& must be a subprogram", Id);

                  else
                     --  If name is overloaded, pragma applies to all the
                     --  denoted entities in the same declarative part.
                     --  Ignore inherited subprograms, because the pragma will
                     --  apply to the parent operation which is the one called.

                     while Present (Proc_Def_Id) loop

                        if Present (Alias (Proc_Def_Id)) then
                           null;

                        elsif Parent (Get_Declaration_Node (Proc_Def_Id)) /=
                              Parent (N)
                        then
                           exit;

                        else
                           Set_Is_Imported (Proc_Def_Id);

                           --  If convention intrinsic, set instrinsic flag

                           if Chars (Expression (Arg1)) = Name_Intrinsic then
                              Set_Is_Intrinsic (Proc_Def_Id);
                           end if;

                           --  All interfaced procedures need an external
                           --  symbol created for them since they are
                           --  always referenced from another object file.

                           Set_Is_Public (Proc_Def_Id);
                           Set_Has_Completion (Proc_Def_Id);

                           if Nargs = 3 then
                              Set_Interface_Name
                                (Proc_Def_Id, Expression (Arg3));
                           end if;
                        end if;

                        Proc_Def_Id := Homonym (Proc_Def_Id);
                     end loop;
                  end if;
               end if;
            end;
         end Import;

         -------------
         -- Improve --
         -------------

         --  We don't do anything with this pragma, except to make sure it
         --  is of the right form (this is for nice Alsys compatibility).
         --  Maybe we should later at least check that the second argument is
         --  a record type name.

         when Pragma_Improve =>
            null;

         ------------
         -- Inline --
         ------------

         when Pragma_Inline => Inline : begin
            declare
               Assoc    : Node_Id := Arg1;
               Decl     : Node_Id;
               Subp_Id  : Node_Id;
               Subp     : Entity_Id;

               procedure Make_Inline (Subp : Entity_Id);
               --  Subp is the defining unit name of the subprogram
               --  declaration. Set the flag, as well as the flag in the
               --  corresponding boy, if there is one present.

               procedure Make_Inline (Subp : Entity_Id) is
                  Kind : Entity_Kind := Ekind (Subp);

               begin
                  if Etype (Subp) = Any_Type then
                     return;

                  elsif Present (Scope (Subp))
                    and then Scope (Subp) /= Current_Scope
                  then
                     Pragma_Misplaced;
                     return;
                  end if;

                  if Kind = E_Procedure or else Kind = E_Function then
                     Set_Is_Inlined (Subp, True);

                     Decl := Parent (Parent (Subp));

                     if Nkind (Decl) = N_Subprogram_Declaration
                       and then Present (Corresponding_Body (Decl))
                     then
                        Set_Is_Inlined (Corresponding_Body (Decl), True);
                     end if;

                  elsif Kind = E_Generic_Procedure
                    or else Kind = E_Generic_Function
                  then
                     Set_Is_Inlined (Subp, True);
                     Decl := Parent (Parent (Parent (Subp)));

                     if Present (Corresponding_Body (Decl)) then
                        Set_Is_Inlined (Corresponding_Body (Decl), True);
                     end if;

                  else
                     Error_Pragma ("expect subprogram name for pragma&");
                  end if;
               end Make_Inline;

            begin
               while Present (Assoc) loop
                  Subp_Id := Expression (Assoc);
                  Find_Name (Subp_Id);
                  Subp := Entity (Subp_Id);

                  if Subp = Any_Id then
                     null;
                  else
                     Make_Inline (Subp);

                     while Present (Homonym (Subp))
                       and then Scope (Homonym (Subp)) = Current_Scope
                     loop
                        Make_Inline (Homonym (Subp));
                        Subp := Homonym (Subp);
                     end loop;
                  end if;

                  Assoc := Next (Assoc);
               end loop;
            end;
         end Inline;

         ----------------------
         -- Inspection_Point --
         ----------------------

         when Pragma_Inspection_Point =>
            Pragma_Not_Implemented;

         ---------------
         -- Interface --
         ---------------

         --  The parser has verified that the first argument is a valid
         --  language convention name, and for the moment, the only such
         --  defined name is Convention_C, so we just assume that we are
         --  talking about C. All that needs to be done it to set a flag in
         --  the defining occurence of the subprogram to indicate that it is
         --  interfaced (and thus needs no body).

         when Pragma_Interface => Interface : begin
            declare
               Id : Node_Id;
               Proc_Def_Id : Entity_Id;

            begin
               Id := Expression (Arg2);
               Find_Program_Unit_Name (Id);

               --  Remaining processing is needed only if we found the name

               if Etype (Id) /= Any_Type then
                  Proc_Def_Id := Entity (Id);

                  if not Is_Subprogram (Proc_Def_Id) then
                     Error_Pragma_Argument
                       ("second argument of pragma& must be a subprogram", Id);

                  else
                     while Present (Proc_Def_Id) 
                     loop

                        if Present (Alias (Proc_Def_Id)) then
                           null;

                        elsif Parent (Get_Declaration_Node (Proc_Def_Id)) 
                                                            /= Parent (N) then
                           exit;

                        else
                           Set_Is_Imported (Proc_Def_Id);

                           Set_Is_Public (Proc_Def_Id);
                           Set_Has_Completion (Proc_Def_Id);
                        end if;

                        Proc_Def_Id := Homonym (Proc_Def_Id);
                     end loop;
                  end if;
               end if;
            end;
         end Interface;

         --------------------
         -- Interface_Name --
         --------------------

         when Pragma_Interface_Name => Interface_Name : begin
            declare
               Id          : constant Node_Id := Expression (Arg1);
               Link_Name   : constant Node_Id := Expression (Arg2);
               Proc_Def_Id : Entity_Id;

            begin
               Find_Name (Id);

               --  Remaining processing is needed only if we found the name.
               --  Check that name represents a subprogram for which a pragma
               --  Interface has been given. Then associate the defining
               --  identifier with the N_String_Literal (the external name)
               --  given in the pragma.

               if Etype (Id) /= Any_Type then
                  Proc_Def_Id := Entity (Id);

                  if not Is_Subprogram (Proc_Def_Id) then
                     Error_Pragma_Argument
                       ("argument of pragma& is not subprogram", Id);

                  elsif not Is_Imported (Proc_Def_Id) then
                     Error_Pragma_Argument
                       ("argument of pragma& is not imported subprogram", Id);
                  else
                     Set_Interface_Name (Proc_Def_Id, Link_Name);
                  end if;
               end if;
            end;
         end Interface_Name;

         -----------------------
         -- Interrupt_Handler --
         -----------------------

         when Pragma_Interrupt_Handler =>
            Pragma_Not_Implemented;

         ------------------------
         -- Interrupt_Priority --
         ------------------------

         when Pragma_Interrupt_Priority => Interrupt_Priority : begin
            declare
               P : constant Node_Id := Parent (N);

            begin
               if Nargs /= 0 then
                  Analyze (Expression (Arg1));
                  Resolve_Complete_Context
                    (Expression (Arg1), RTE (RE_Priority));
               end if;

               if Nkind (P) /= N_Task_Definition
                 and then Nkind (P) /= N_Protected_Definition
               then
                  Pragma_Misplaced;
                  return;

               elsif Has_Priority_Pragma (P) then
                  Error_Pragma ("duplicate pragma& not allowed");

               else
                  Set_Has_Priority_Pragma (P, True);
               end if;
            end;
         end Interrupt_Priority;

         ----------
         -- List --
         ----------

         --  There is nothing to do here, since we did all the processing
         --  for this pragma in Par.Prag (so that it works properly even in
         --  syntax only mode)

         when Pragma_List =>
            null;

         --------------------
         -- Locking_Policy --
         --------------------

         when Pragma_Locking_Policy =>
            Pragma_Not_Implemented;

         -----------------
         -- Memory_Size --
         -----------------

         when Pragma_Memory_Size =>
            Pragma_Not_Implemented;

         -----------------------
         -- Normalize_Scalars --
         -----------------------

         when Pragma_Normalize_Scalars =>
            Pragma_Not_Implemented;

         --------------
         -- Optimize --
         --------------

         --  Nothing to do, since all checks done in Par.Prag and we don't
         --  actually pay any attention to this pragma (does anyone?)

         when Pragma_Optimize =>
            null;

         ----------
         -- Pack --
         ----------

         when Pragma_Pack => Pack : begin
            declare
               Assoc   : Node_Id := Arg1;
               Type_Id : Node_Id := Expression (Assoc);
               Typ     : Entity_Id;

            begin
               Find_Type (Type_Id);
               Typ := Entity (Type_Id);

               if Typ = Any_Type then
                  return;

               elsif not Is_Composite_Type (Typ) then
                  Error_Pragma ("pragma& does not specify composite type");

               elsif Scope (Typ) /= Current_Scope then
                  Error_Pragma
                    ("pragma& does not specify type in same declarative part");

               else
                  if Is_Array_Type (Typ) then
                     Error_Pragma
                       ("?pragma& not implemented yet for array types");
                  end if;

                  Set_Is_Packed (Typ);
               end if;
            end;
         end Pack;

         ----------
         -- Page --
         ----------

         --  There is nothing to do here, since we did all the processing
         --  for this pragma in Par.Prag (so that it works properly even in
         --  syntax only mode)

         when Pragma_Page =>
            null;

         ------------------
         -- Preelaborate --
         ------------------

         when Pragma_Preelaborate => Preelaborate : begin
            if Valid_Unit_Pragma then
               Pragma_Not_Implemented;
            end if;
         end Preelaborate;

         --------------
         -- Priority --
         --------------

         when Pragma_Priority => Priority : begin
            declare
               P : constant Node_Id := Parent (N);

            begin
               Analyze (Expression (Arg1));
               Resolve_Complete_Context
                 (Expression (Arg1), RTE (RE_Any_Priority));

               if Nkind (P) /= N_Task_Definition
                 and then Nkind (P) /= N_Protected_Definition
                 and then Nkind (P) /= N_Subprogram_Body
               then
                  Pragma_Misplaced;
                  return;

               else
                  if Nkind (P) = N_Subprogram_Body
                    and then not Is_Static_Expression (Expression (Arg1))
                  then
                     Error_Pragma
                       ("static expression required " &
                        "for pragma& in subprogram body");
                  end if;

                  if Has_Priority_Pragma (P) then
                     Error_Pragma ("duplicate pragma& not allowed");
                  else
                     Set_Has_Priority_Pragma (P, True);
                  end if;
               end if;
            end;
         end Priority;

         ----------
         -- Pure --
         ----------

         when Pragma_Pure => Pure : begin
            if Valid_Unit_Pragma then
               Pragma_Not_Implemented;
            end if;
         end Pure;

         --------------------
         -- Queuing_Policy --
         --------------------

         when Pragma_Queuing_Policy =>
            Pragma_Not_Implemented;

         ---------------------------
         -- Remote_Call_Interface --
         ---------------------------

         when Pragma_Remote_Call_Interface =>
            Pragma_Not_Implemented;

         ------------------
         -- Remote_Types --
         ------------------

         when Pragma_Remote_Types =>
            Pragma_Not_Implemented;

         ------------------
         -- Restrictions --
         ------------------

         when Pragma_Restrictions =>
            Pragma_Not_Implemented;

         ----------------
         -- Reviewable --
         ----------------

         when Pragma_Reviewable =>
            Pragma_Not_Implemented;

         ------------
         -- Shared --
         ------------

         when Pragma_Shared =>
            Pragma_Not_Implemented;

         --------------------
         -- Shared_Passive --
         --------------------

         when Pragma_Shared_Passive =>
            Pragma_Not_Implemented;

         ------------------
         -- Storage_Unit --
         ------------------

         --  Nothing to do here, since this pragma was completely
         --  handled in Par

         when Pragma_Storage_Unit =>
            null;

         --------------
         -- Suppress --
         --------------

         when Pragma_Suppress => Suppress : begin
            declare
               C : constant Check_Id :=
                 Get_Check_Id (Chars (Expression (Arg1)));

            begin
               if Nargs = 1 then
                  case C is
                     when Access_Check =>
                        Scope_Suppress.Access_Checks := True;

                     when Accessibility_Check =>
                        Scope_Suppress.Accessibility_Checks := True;

                     when Discriminant_Check =>
                        Scope_Suppress.Discriminant_Checks := True;

                     when Division_Check =>
                        Scope_Suppress.Division_Checks := True;

                     when Elaboration_Check =>
                        Scope_Suppress.Elaboration_Checks := True;

                     when Index_Check =>
                        Scope_Suppress.Index_Checks := True;

                     when Length_Check =>
                        Scope_Suppress.Length_Checks := True;

                     when Overflow_Check =>
                        Scope_Suppress.Overflow_Checks := True;

                     when Range_Check =>
                        Scope_Suppress.Range_Checks := True;

                     when Storage_Check =>
                        Scope_Suppress.Storage_Checks := True;

                     when Tag_Check =>
                        Scope_Suppress.Tag_Checks := True;

                     when All_Checks =>
                        Scope_Suppress := (others => True);

                  end case;

               --  Case of two arguments present, where the check is
               --  suppressed for a specified entity (given as the second
               --  argument of the pragma)

               else
                  declare
                     E_Id      : Node_Id;
                     E         : Entity_Id;
                     Effective : Boolean;

                     procedure Suppress_Echeck (E : Entity_Id; C : Check_Id);
                     --  Used to suppress a single check on the given entity

                     procedure Suppress_Echeck (E : Entity_Id; C : Check_Id) is
                     begin

                        --  First set appropriate suppress flags in the entity

                        case C is
                           when Access_Check =>
                              Effective := Suppress_Access_Checks (E);
                              Set_Suppress_Access_Checks (E, True);

                           when Accessibility_Check =>
                              Effective := Suppress_Accessibility_Checks (E);
                              Set_Suppress_Accessibility_Checks (E, True);

                           when Discriminant_Check =>
                              Effective := Suppress_Discriminant_Checks  (E);
                              Set_Suppress_Discriminant_Checks (E, True);

                           when Division_Check =>
                              Effective := Suppress_Division_Checks (E);
                              Set_Suppress_Division_Checks (E, True);

                           when Elaboration_Check =>
                              Effective := Suppress_Elaboration_Checks (E);
                              Set_Suppress_Elaboration_Checks (E, True);

                           when Index_Check =>
                              Effective := Suppress_Index_Checks (E);
                              Set_Suppress_Index_Checks (E, True);

                           when Length_Check =>
                              Effective := Suppress_Length_Checks (E);
                              Set_Suppress_Length_Checks (E, True);

                           when Overflow_Check =>
                              Effective := Suppress_Overflow_Checks (E);
                              Set_Suppress_Overflow_Checks (E, True);

                           when Range_Check =>
                              Effective := Suppress_Range_Checks (E);
                              Set_Suppress_Range_Checks (E, True);

                           when Storage_Check =>
                              Effective := Suppress_Storage_Checks (E);
                              Set_Suppress_Storage_Checks (E, True);

                           when Tag_Check =>
                              Effective := Suppress_Tag_Checks (E);
                              Set_Suppress_Tag_Checks (E, True);

                           when All_Checks =>
                              Suppress_Echeck (E, Access_Check);
                              Suppress_Echeck (E, Accessibility_Check);
                              Suppress_Echeck (E, Discriminant_Check);
                              Suppress_Echeck (E, Division_Check);
                              Suppress_Echeck (E, Elaboration_Check);
                              Suppress_Echeck (E, Index_Check);
                              Suppress_Echeck (E, Length_Check);
                              Suppress_Echeck (E, Overflow_Check);
                              Suppress_Echeck (E, Range_Check);
                              Suppress_Echeck (E, Storage_Check);
                              Suppress_Echeck (E, Tag_Check);
                        end case;

                        --  If the entity is not declared in the current
                        --  scope, then we make an entry in the
                        --  Entity_Suppress table so that the flag will be
                        --  removed on exit. This entry is only made if the
                        --  suppress did something (i.e. the flag was not
                        --  already set).

                        if Effective and then Scope (E) /= Current_Scope then
                           Entity_Suppress.Increment_Last;
                           Entity_Suppress.Table
                             (Entity_Suppress.Last).Entity := E;
                           Entity_Suppress.Table
                             (Entity_Suppress.Last).Check  := C;
                        end if;
                     end Suppress_Echeck;

                  --  Start of processing for two argument pragma Suppress case

                  begin
                     E_Id := Expression (Arg2);
                     Find_Name (E_Id);
                     E := Entity (E_Id);

                     if E = Any_Id then
                        return;
                     else
                        Suppress_Echeck (E, C);

                        while Present (Homonym (E)) loop
                           E := Homonym (E);
                           Suppress_Echeck (E, C);
                        end loop;
                     end if;
                  end;
               end if;

            end;
         end Suppress;

         -----------------
         -- System_Name --
         -----------------

         --  Nothing to do here, since this pragma was completely
         --  handled in Par

         when Pragma_System_Name =>
            null;

         -----------------------------
         -- Task_Dispatching_Policy --
         -----------------------------

         when Pragma_Task_Dispatching_Policy =>
            Pragma_Not_Implemented;

         ---------------------
         -- Task_Stack_Size --
         ---------------------

         when Pragma_Task_Stack_Size => Task_Stack_Size : begin
            declare
               P : constant Node_Id := Parent (N);

            begin
               Analyze (Expression (Arg1));
               Resolve_Complete_Context
                 (Expression (Arg1), RTE (RE_Size_Type));

               if Nkind (P) /= N_Task_Definition then
                  Pragma_Misplaced;
                  return;

               else
                  if Has_Task_Stack_Size_Pragma (P) then
                     Error_Pragma ("duplicate pragma& not allowed");
                  else
                     Set_Has_Task_Stack_Size_Pragma (P, True);
                  end if;
               end if;
            end;
         end Task_Stack_Size;

         --------------
         -- Volatile --
         --------------

         when Pragma_Volatile =>
            Pragma_Not_Implemented;

         -------------------------
         -- Volatile_Components --
         -------------------------

         when Pragma_Volatile_Components =>
            Pragma_Not_Implemented;

      end case;
   end Analyze_Pragma;

end Sem_Prag;
