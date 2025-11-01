------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P A R . C H 1 0                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.39 $                             --
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

with Uname;    use Uname;

separate (Par)
package body Ch10 is

   --  Local functions, used only in this chapter

   function P_Context_Clause    return List_Id;
   function P_Subunit           return Node_Id;

   -------------------------
   -- 10.1.1  Compilation --
   -------------------------

   --  COMPILATION ::= {COMPILATION_UNIT}

   --  There is no specific parsing routine for a compilation, since we only
   --  permit a single compilation in a source file, so there is no explicit
   --  occurrence of compilations as such (our representation of a compilation
   --  is a series of separate source files).

   ------------------------------
   -- 10.1.1  Compilation unit --
   ------------------------------

   --  COMPILATION_UNIT ::=
   --    CONTEXT_CLAUSE LIBRARY_ITEM
   --  | CONTEXT_CLAUSE  SUBUNIT

   --  LIBRARY_ITEM ::=
   --    private LIBRARY_UNIT_DECLARATION | LIBRARY_UNIT_BODY

   --  LIBRARY_UNIT_DECLARATION ::=
   --    SUBPROGRAM_DECLARATION | PACKAGE_DECLARATION
   --  | GENERIC_DECLARATION    | GENERIC_INSTANTIATION

   --  LIBRARY_UNIT_RENAMING_DECLARATION ::=
   --    PACKAGE_RENAMING_DECLARATION | SUBPROGRAM_RENAMING_DECLARATION

   --  LIBRARY_UNIT_BODY ::= SUBPROGRAM_BODY | PACKAGE_BODY

   --  Error recovery: cannot raise Error_Resync. If an error occurs, tokens
   --  are skipped up to the next possible beginning of a compilation unit.

   function P_Compilation_Unit return Node_Id is
      Scan_State         : Saved_Scan_State;
      Context_Item_List  : List_Id;
      Body_Node          : Node_Id;
      Spec_Node          : Node_Id;
      Specification_Node : Node_Id;
      Unit_Node          : Node_Id;
      Comp_Unit_Node     : Node_Id;
      Name_Node          : Node_Id;
      File_Name          : Name_Id;

      Cunit_Error_Flag   : Boolean := False;
      --  This flag is set True if we have to scan for a compilation unit
      --  token. It is used to ensure clean termination in such cases by
      --  not insisting on being at the end of file, and, in the sytax only
      --  case by not scanning for additional compilation units.

   begin
      Comp_Unit_Node := New_Node (N_Compilation_Unit, No_Location);
      File.Table (Scan_Unit).Cunit := Comp_Unit_Node;
      Set_Context_Items (Comp_Unit_Node, P_Context_Clause);

      --  Allow PRIVATE in 9X mode

      if Token = Tok_Private then
         File.Table (Scan_Unit).Keyword_Casing := Determine_Token_Casing;

         if GNAT_Style_Check then
            GNAT_Column_Check (Token_Ptr);
         end if;

         if Ada_83 then
            Error_Msg_SC ("private units not allowed in Ada 83!");
         end if;

         Save_Scan_State (Scan_State); -- at PRIVATE
         Scan; -- past PRIVATE

         if Token = Tok_Separate then
            Error_Msg_SP ("cannot have private subunits!");

         elsif Token = Tok_Package then
            Scan; -- past PACKAGE

            if Token = Tok_Body then
               Restore_Scan_State (Scan_State); -- to PRIVATE
               Error_Msg_SC ("cannot have private package body!");
               Scan; -- ignore PRIVATE
            else
               Restore_Scan_State (Scan_State); -- to PRIVATE
               Scan; -- past PRIVATE
               Set_Private_Present (Comp_Unit_Node, True);
            end if;
         end if;
      end if;

      --  Loop to find our way to a compilation unit token

      loop
         exit when Token in Token_Class_Cunit and then Token /= Tok_With;

         exit when Bad_Spelling_Of (Tok_Procedure)
           or else Bad_Spelling_Of (Tok_Package)
           or else Bad_Spelling_Of (Tok_Function)
           or else Bad_Spelling_Of (Tok_Generic);

         if Token = Tok_With then
            Error_Msg_SC ("misplaced WITH");
            Append_List (P_Context_Clause, Context_Items (Comp_Unit_Node));

         elsif Bad_Spelling_Of (Tok_With) then
            Append_List (P_Context_Clause, Context_Items (Comp_Unit_Node));

         else
            Error_Msg_SC ("~compilation unit expected");
            Cunit_Error_Flag := True;
            Resync_Cunit;

            --  If we are at an end of file, then just quit, the above error
            --  message was complaint enough.

            if Token = Tok_EOF then
               return Error;
            end if;
         end if;
      end loop;

      --  We have a compilation unit token, so that's a reasonable choice for
      --  determining the standard casing convention used for keywords in case
      --  it hasn't already been done on seeing a WITH or PRIVATE.

      File.Table (Scan_Unit).Keyword_Casing := Determine_Token_Casing;

      if GNAT_Style_Check then
         GNAT_Column_Check (Token_Ptr);
      end if;

      --  Remaining processing depends on particular type of compilation unit

      if Token = Tok_Package then
         Set_Unit (Comp_Unit_Node, P_Package (Pf_Decl_Gins_Pbod_Rnam));

      elsif Token = Tok_Generic then
         Set_Unit (Comp_Unit_Node, P_Generic);

      elsif Token = Tok_Separate then
         Set_Unit (Comp_Unit_Node, P_Subunit);

      elsif Token = Tok_Procedure or else Token = Tok_Function then
         Set_Unit (Comp_Unit_Node, P_Subprogram (Pf_Decl_Gins_Pbod_Rnam));

         --  A little bit of an error recovery check here. If we just scanned
         --  a subprogram declaration (as indicated by an SIS entry being
         --  active), then if the following token is BEGIN or an identifier,
         --  or a token which can reasonably start a declaration but cannot
         --  start a compilation unit, then we assume that the semicolon in
         --  the declaration should have been IS.

         if SIS_Entry_Active then

            if Token = Tok_Begin
               or else Token = Tok_Identifier
               or else Token in Token_Class_Deckn
            then
               Push_Scope_Stack;
               Scope.Table (Scope.Last).Etyp := E_Name;
               Scope.Table (Scope.Last).Sloc := SIS_Sloc;
               Scope.Table (Scope.Last).Ecol := SIS_Ecol;
               Scope.Table (Scope.Last).Lreq := False;
               SIS_Entry_Active := False;
               Error_Msg_SP ("semicolon should be IS!");

               Body_Node := Unit (Comp_Unit_Node);
               Specification_Node := Specification (Body_Node);
               Change_Node (Body_Node, N_Subprogram_Body);
               Set_Specification (Body_Node, Specification_Node);
               P_Decls_Begin_End (Body_Node);
               Set_Unit (Comp_Unit_Node, Body_Node);
            end if;

         end if;

      --  Shouldn't be any other possibilities for compilation units

      else
         Error_Msg_SC ("Internal error: expected compilation unit token here");
         Compiler_Abort;
      end if;

      --  Here is where we set the Sloc field of the N_Compilation_Unit node,
      --  which must point to the name of the unit, which is a bit hard to
      --  find in some cases (which is why we set it in the node!)

      Unit_Node := Unit (Comp_Unit_Node);

      --  Only try this if we got an OK unit!

      if Unit_Node /= Error then
         if Nkind (Unit_Node) = N_Subunit then
            Unit_Node := Proper_Body (Unit_Node);
         end if;

         if Nkind (Unit_Node) in N_Generic_Declaration then
            Unit_Node := Specification (Unit_Node);
         end if;

         if Nkind (Unit_Node) = N_Package_Declaration
           or else Nkind (Unit_Node) = N_Subprogram_Declaration
           or else Nkind (Unit_Node) = N_Subprogram_Body
           or else Nkind (Unit_Node) = N_Subprogram_Renaming_Declaration
         then
            Unit_Node := Specification (Unit_Node);
         end if;

         if Nkind (Unit_Node) = N_Task_Body
           or else Nkind (Unit_Node) = N_Protected_Body
         then
            Name_Node := Defining_Identifier (Unit_Node);
         else
            Name_Node := Defining_Unit_Name (Unit_Node);
         end if;

         Set_Sloc (Comp_Unit_Node, Sloc (Name_Node));

         --  Set Entity field in file table. Easier now that we have name!
         --  Note that this is also skipped if we had a bad unit

         if Nkind (Name_Node) = N_Defining_Program_Unit_Name then
            File.Table (Scan_Unit).Entity := Defining_Identifier (Name_Node);
         else
            File.Table (Scan_Unit).Entity := Name_Node;
         end if;

         --  Set unit name

         File.Table (Scan_Unit).Unit_Name :=
           Get_Unit_Name (Unit (Comp_Unit_Node));

      --  If we had a bad unit, make sure the fatal flag is set in the file
      --  table entry, since this is surely a fatal error and also set our
      --  flag to inhibit the requirement that we be at end of file.

      else
         Cunit_Error_Flag := True;
         File.Table (Scan_Unit).Fatal_Error := True;
      end if;

      --  Clear away any missing semicolon indication, we are done with that
      --  unit, so what's done is done, and we don't want anything hanging
      --  around from the attempt to parse it!

      SIS_Entry_Active := False;

      --  Any final pragmas get appended to the context clause

      if Token = Tok_Pragma then
         Set_Following_Pragmas (Comp_Unit_Node, New_List);
         P_Pragmas_Opt (Following_Pragmas (Comp_Unit_Node));
      end if;

      --  If -db debug switch is set, then add a "with System" to the context
      --  clause to allow references by the declarations of _V used to address
      --  version strings in traceback information. Don't do this in System
      --  itself, or we get a circular with dependency! Note that we put the
      --  with of System at the start of the context clause, because it can't
      --  go after a pragma Elaborate.

      if Debug_Flag_B
        and then Unit_Node /= Error
        and then Present (File.Table (Scan_Unit).Entity)
        and then Chars (File.Table (Scan_Unit).Entity) /= Name_System
      then
         Prepend_To (Context_Items (Comp_Unit_Node),
           Make_With_Clause (Sloc (Name_Node),
             Name       => Make_Identifier (Sloc (Name_Node), Name_System)));
      end if;

      --  And now we should be at the end of file, except that if we had to
      --  scan for a compilation unit, then we don't check this, since it
      --  seems in practice to often make things worse, and we already gave
      --  a serious error message.

      if Token /= Tok_EOF and then not Cunit_Error_Flag then

         --  If we are not at end of file, then fatal error unless we are
         --  syntax checking only mode, where we do allow additional units
         --  by making a recursive call to this routine. Skip this message
         --  if we already had some fatal error.

         if Operating_Mode = Check_Syntax then
            return P_Compilation_Unit;

         else
            if not File.Table (Scan_Unit).Fatal_Error then

               if Token in Token_Class_Cunit then
                  Error_Msg_SC
                    ("~end of file expected, " &
                     "file can have only one compilation unit");

               else
                  Error_Msg_SC ("~end of file expected");
               end if;
            end if;

            return Error;
         end if;

      --  This is the normal return

      else
         return Comp_Unit_Node;
      end if;

   exception
      --  An error resync is a serious bomb, so indicate result unit no good

      when Error_Resync =>
         File.Table (Scan_Unit).Fatal_Error := True;
         return Error;

   end P_Compilation_Unit;

   --------------------------
   -- 10.1.1  Library Item --
   --------------------------

   --  Parsed by P_Compilation_Unit (10.1.1)

   --------------------------------------
   -- 10.1.1  Library Unit Declaration --
   --------------------------------------

   --  Parsed by P_Compilation_Unit (10.1.1)

   -----------------------------------------------
   -- 10.1.1  Library Unit Renaming Declaration --
   -----------------------------------------------

   --  Parsed by P_Compilation_Unit (10.1.1)

   -------------------------------
   -- 10.1.1  Library Unit Body --
   -------------------------------

   --  Parsed by P_Compilation_Unit (10.1.1)

   ----------------------------
   -- 10.1.2  Context Clause --
   ----------------------------

   --  CONTEXT_CLAUSE ::= {CONTEXT_ITEM}

   --  CONTEXT_ITEM ::= WITH_CLAUSE | USE_CLAUSE

   --  WITH_CLAUSE ::=
   --    with library_unit_NAME {,library_unit_NAME};

   --  Error recovery: Cannot raise Error_Resync

   function P_Context_Clause return List_Id is
      Item_List  : List_Id;
      Scan_State : Saved_Scan_State;
      With_Node  : Node_Id;
      Id_Node    : Node_Id;
      First_Flag : Boolean;

   begin
      Item_List := New_List;

      --  Get keyword casing from WITH keyword in case not set yet

      if Token = Tok_With then
         File.Table (Scan_Unit).Keyword_Casing := Determine_Token_Casing;
      end if;

      --  Loop through context items

      loop
         if GNAT_Style_Check then
            GNAT_Column_Check (Token_Ptr);
         end if;

         --  Gather any pragmas appearing in the context clause

         P_Pragmas_Opt (Item_List);

         --  Processing for WITH clause

         if Token = Tok_With then
            Scan; -- past WITH
            First_Flag := True;

            --  Loop through names in one with clause, generating a separate
            --  N_With_Clause node for each nam encountered.

            loop
               With_Node := New_Node (N_With_Clause, Token_Ptr);
               Append (With_Node, Item_List);

               if Ada_9X then
                  Set_Name (With_Node, P_Qualified_Simple_Name);

               else -- Ada-83
                  Save_Scan_State (Scan_State); -- at Id
                  Id_Node := P_Identifier;

                  if Token = Tok_Dot then
                     Restore_Scan_State (Scan_State); -- to Id
                     Error_Msg_SC ("child units not allowed in Ada 83!");
                     Set_Name (With_Node, P_Qualified_Simple_Name);

                  else
                     Set_Name (With_Node, Id_Node);
                  end if;
               end if;

               Set_First_Name (With_Node, First_Flag);
               First_Flag := False;
               exit when Token /= Tok_Comma;
               Scan; -- past comma
            end loop;

            Set_Last_Name (With_Node, True);
            TF_Semicolon;


         --  Processing for USE clause

         elsif Token = Tok_Use then
            Append (P_Use_Clause, Item_List);

         --  Anything else is end of context clause

         else
            exit;
         end if;
      end loop;

      return Item_List;
   end P_Context_Clause;

   -------------------------
   -- 10.1.2  With Clause --
   -------------------------

   --  Parsed by P_Context_Clause (10.1.1)

   -----------------------
   -- 10.1.3  Body Stub --
   -----------------------

   --  Function stub parsed by P_Subprogram (6.1)
   --  Procedure stub parsed by P_Subprogram (6.1)
   --  Task stub parsed by P_Task (9.1)
   --  Protected stub parsed by P_Protected (9.4)

   ---------------------
   -- 10.1.3  Subunit --
   ---------------------

   --  SUBUNIT ::=
   --    separate (parent_unit_NAME) PROPER_BODY

   --  The caller has checked that the initial token is SEPARATE

   --  Error recovery: cannot raise Error_Resync

   function P_Subunit return Node_Id is
      Subunit_Node : Node_Id;
      Body_Node    : Node_Id;

   begin
      Subunit_Node := New_Node (N_Subunit, Token_Ptr);
      Body_Node := Error; -- in case no good body found
      Scan; -- past SEPARATE;

      T_Left_Paren;
      Set_Name (Subunit_Node, P_Qualified_Simple_Name);
      T_Right_Paren;

      if Token = Tok_Function or else Token = Tok_Procedure then
         Body_Node := P_Subprogram (Pf_Pbod);

      elsif Token = Tok_Package then
         Body_Node := P_Package (Pf_Pbod);

      elsif Token = Tok_Protected then
         Scan; -- past PROTECTED

         if Token = Tok_Body then
            Body_Node := P_Protected;
         else
            Error_Msg_AP ("~BODY expected");
            return Error;
         end if;


      elsif Token = Tok_Task then
         Scan; -- past TASK

         if Token = Tok_Body then
            Body_Node := P_Task;
         else
            Error_Msg_AP ("~BODY expected");
            return Error;
         end if;

      else
         Error_Msg_SC ("~proper body expected");
         return Error;
      end if;

      Set_Proper_Body  (Subunit_Node, Body_Node);
      return Subunit_Node;

   end P_Subunit;

end Ch10;
