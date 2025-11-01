------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 0                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.80 $                             --
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
with Debug;        use Debug;
with Einfo;        use Einfo;
with Errcount;     use Errcount;
with Errout;       use Errout;
with Lib;          use Lib;
with Namet;        use Namet;
with Nmake;        use Nmake;
with Opt;          use Opt;
with Output;       use Output;
with Sem;          use Sem;
with Sem_Ch6;      use Sem_Ch6;
with Sem_Ch7;      use Sem_Ch7;
with Sem_Ch8;      use Sem_Ch8;
with Sem_Util;     use Sem_Util;
with Stand;        use Stand;
with Sinfo;        use Sinfo;
with Sinfo.Change; use Sinfo.Change;
with Snames;       use Snames;
with Stringt;      use Stringt;
with Tbuild;       use Tbuild;
with Uname;        use Uname;

package body Sem_Ch10 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Context (N : Node_Id);
   --  Analyzes items in the context clause of compilation unit

   function Ancestor (Lib_Unit : Node_Id) return Entity_Id;
   --  Return the root ancestor of a child unit.

   procedure Implicit_With_On_Parent (Child_Unit : Node_Id; Item : Node_Id);
   --  When a child unit appears in a context clause,  the implicit withs on
   --  parents is made explicit, and with clauses are inserted in the context
   --  clause after that for the child.

   procedure Insert_Version_Declaration (N : Node_Id);
   --  This procedure inserts the declaration of Version_S (for a spec)
   --  or Version_B for a body in the declarative region of the library
   --  library unit if either of the the db (traceback) or dv (version)
   --  debug switches are set. One might thing of this as expansion
   --  activity, but since it is a real variable, the program can reference
   --  it, and thus it must be put in early. Furthermore, the expander model
   --  is wrong for this insertion, since it is inserted at the outer level
   --  and referenced at inner levels, and expansion works inside out.
   --  The declaration is added to subprogram bodies, package specs,
   --  package bodies, generic versions of these, and to subunits.

   procedure Install_Context (N : Node_Id);
   --  Installs the entities from the context clause of the given compilation
   --  unit into the visibility chains. This is done before analyzing a unit.

   procedure Install_Withed_Unit (Unit_Name : Entity_Id);
   --  This procedure installs the entities of a singled withed unit into the
   --  visibility chains. The argument is the entity for the unit involved.
   --  The caller ensures that the unit is not already currently installed.

   procedure Install_Parents (Lib_Unit : Node_Id);
   --  This procedure establishes the context for the compilation of a child
   --  unit. If Lib_Unit is a child library spec then the context of the parent
   --  is installed, and the parent itself made directly visible, so that
   --  the child unit is processed in the declarative region of the parent.
   --  Install_Parents makes a recursive call to itself to ensure that all
   --  parents are loaded in the nested case. If Lib_Unit is a library body,
   --  the only effect of Install_Parents is to install the private decls of
   --  the parents, because the visible parent declarations will have been
   --  installed as part of the context of the corresponding spec.

   function Is_Child_Spec (Lib_Unit : Node_Id) return Boolean;
   --  Lib_Unit is a library unit which may be a spec or a body. Is_Child_Spec
   --  returns True if Lib_Unit is a library spec which is a child spec, i.e.
   --  a library spec that has a parent. If the call to Is_Child_Spec returns
   --  True, then Parent_Spec (Lib_Unit) is non-Empty and points to the
   --  compilation unit for the parent spec.

   procedure Remove_Context (N : Node_Id);
   --  Removes the entities from the context clause of the given compilation
   --  unit from the visibility chains. This is done on exit from a unit as
   --  part of cleaning up the visibility chains for the caller. A special
   --  case is that the call from the Main_Unit can be ignored, since at the
   --  end of the main unit the visibility table won't be needed in any case.

   procedure Remove_Parents (Lib_Unit : Node_Id);
   --  Remove_Parents checks if Lib_Unit is a child spec. If so then the parent
   --  contexts established by the corresponding call to Install_Parents are
   --  removed. Remove_Parents contains a recursive call to itself to ensure
   --  that all parents are removed in the nested case.

   procedure Remove_Withed_Unit (Unit_Name : Entity_Id);
   --  This procedure reverses the effects of a previous call to the install
   --  routine (Install_Withed_Unit). It removes the entities of a single
   --  withed unit from the visibility chains. The argument is the entity for
   --  the unit involved. The caller has ensured that the unit is installed.
   --  Also called to remove the parent of  a child unit.

   procedure Remove_Entity (E : Entity_Id);
   --  This procedure removes a single entity from the visibility chain.
   --  The caller has ensured that the entity is on the chain.

   procedure Analyze_Proper_Body (N : Node_Id);
   --  Common processing for subprogram stubs and package stubs. Once the
   --  subunit name is established, load and analyze.

   ------------------------------
   -- Analyze_Compilation_Unit --
   ------------------------------

   procedure Analyze_Compilation_Unit (N : Node_Id) is
      Lib_Unit : constant Node_Id := Unit (N);
      Spec_Id  : Node_Id;

   begin
      --  If the unit is a subunit whose parent has not been analyzed (which
      --  indicates that the main unit is a subunit, either the current one or
      --  one of its descendents) then the subunit is compiled as part of the
      --  analysis of the parent, which we proceed to do. Basically this gets
      --  handled from the top down and we don't want to do anything at this
      --  level (i.e. this subunit will be handled on the way down from the
      --  parent), so at this level we immediately return.

      if Nkind (Lib_Unit) = N_Subunit
        and then not Analyzed (Library_Unit (N))
      then
         Semantics (Library_Unit (N));
         return;
      end if;

      Insert_Version_Declaration (N);

      --  Analyze context (this will call Sem recursively for with'ed units)

      Analyze_Context (N);

      --  If the unit is a package body, the spec is already loaded
      --  and must be analyzed first, before we analyze the body.

      if Nkind (Lib_Unit) = N_Package_Body then
         Semantics (Library_Unit (N));

         Spec_Id :=
           Defining_Unit_Simple_Name (Specification (Unit (Library_Unit (N))));

         --  The following check is an error defense, get out if as a result
         --  of errors we do not have a proper package spec around!

         if No (Spec_Id) or else
            (Ekind (Spec_Id) /= E_Package
             and then Ekind (Spec_Id) /= E_Generic_Package)
         then
            return;
         else
            Set_Is_Directly_Visible (Spec_Id, True);
         end if;

      --  If the unit is a subprogram body, then we similarly need to analyze
      --  its spec. However, things are a little simpler in this case, because
      --  here, this analysis is done only for error checking and consistency
      --  purposes, so there's nothing else to be done.

      elsif (Nkind (Lib_Unit) = N_Subprogram_Body
             and then not Acts_As_Spec (N))
      then
         Semantics (Library_Unit (N));

      --  If it is a subprogram declaration it does not need an elaboration
      --  procedure. A renamed package also needs no elaboration procedure.

      elsif Nkind (Lib_Unit) = N_Subprogram_Declaration
        or else Nkind (Lib_Unit) = N_Package_Renaming_Declaration
      then
         Set_Preelaborable (N, True);
      end if;

      --  If it is a child unit, the parent must be elaborated first

      if Is_Child_Spec (Lib_Unit) then
         Semantics (Parent_Spec (Lib_Unit));
      end if;

      --  With the analysis done, install the context. Note that we can't
      --  install the context from the with clauses as we analyze them,
      --  because each with clause must be analyzed in a clean visibility
      --  context, so we have to wait and install them all at once.

      Install_Context (N);

      --  All components of the context: with-clauses, library unit, ancestors
      --  if any, (and their context)  are analyzed and installed. Now analyze
      --  the unit itself, which is either a package, subprogram spec or body.

      Analyze (Lib_Unit);

      --  Treat compilation unit pragmas that appear after the library unit

      if List_Present (Following_Pragmas (N)) then
         declare
            Prag_Node : Node_Id := First (Following_Pragmas (N));

         begin
            while Present (Prag_Node) loop
               Analyze (Prag_Node);
               Prag_Node := Next (Prag_Node);
            end loop;
         end;
      end if;

      --  Last step is to deinstall the context we just installed
      --  as well as the unit just compiled, unless it is the main
      --  unit, or the specification for the main unit.

      if N /= File.Table (Main_Unit).Cunit then
         Remove_Context (N);

         if Nkind (Lib_Unit) = N_Package_Declaration then
            Remove_Withed_Unit
              (Defining_Unit_Simple_Name (Specification (Lib_Unit)));

         elsif Nkind (Lib_Unit) = N_Package_Body
           or else (Nkind (Lib_Unit) = N_Subprogram_Body
             and then not Acts_As_Spec (N))
         then
            --  Bodies that are not the main unit are compiled if they
            --  are generic or contain generic or inlined units. Their
            --  analysis brings in the context of the corresponding spec
            --  (unit declaration) which must be removed as well, to
            --  return the compilation environment to its proper state.
            Remove_Context (Library_Unit (N));
         end if;
      end if;

   end Analyze_Compilation_Unit;

   ----------------------------------
   -- Analyze_Concurrent_Body_Stub --
   ----------------------------------

   procedure Analyze_Concurrent_Body_Stub (N : Node_Id) is
   begin
      Unimplemented (N, "Concurrrent body stubs");
   end Analyze_Concurrent_Body_Stub;

   ---------------------
   -- Analyze_Context --
   ---------------------

   procedure Analyze_Context (N : Node_Id) is
      Item : Node_Id := First (Context_Items (N));

   begin
      --  Note: we do not analyze use clauses at this time, since we don't
      --  want to do any installing of use visible entities until we actually
      --  install the complete context (in Install_Context). Otherwise things
      --  can get installed in the wrong context.

      while Present (Item) loop
         if Nkind (Item) = N_With_Clause
           or else Nkind (Item) = N_Pragma
         then
            Analyze (Item);
         end if;

         Item := Next (Item);
      end loop;
   end Analyze_Context;

   -------------------------------
   -- Analyze_Package_Body_Stub --
   -------------------------------

   procedure Analyze_Package_Body_Stub (N : Node_Id) is
      Id : Entity_Id := Defining_Identifier (N);
      Nam  : Entity_Id;

   begin
      --  The package declaration must be in the current declarative part

      Nam := Current_Entity (Id);

      if No (Nam)
        or else Scope (Nam) /= Current_Scope
        or else
          (Ekind (Nam) /= E_Package and then Ekind (Nam) /= E_Generic_Package)
      then
         Error_Msg_N ("missing specification for package stub", N);

      else
         --  Indicate that the body of the package exists. If we are doing
         --  only semantic analysis, the stub stands for the body. If we are
         --  generating code, the existence of the body will be confirmed
         --  when we load the proper body.

         Set_Has_Completion (Nam);
         Analyze_Proper_Body (N);
      end if;
   end Analyze_Package_Body_Stub;

   -------------------------
   -- Analyze_Proper_Body --
   -------------------------

   --  If the subunit is already loaded, it means that the main unit
   --  was a subunit, and that the current unit is one of its parents
   --  which was being analyzed to provide the needed context for the
   --  analysis of the subunit. In this case we analyze the subunit
   --  and then raise Subunit_Found, since we don't need to analyze
   --  any more of the parent (only the part up to here is relevant
   --  to the desired analysis of the subunit).

   procedure Analyze_Proper_Body (N : Node_Id) is
      Unum         : Unit_Number_Type;
      Subunit_Name : constant Unit_Name_Type := Get_Unit_Name (N);

   begin
      if Is_Loaded (Subunit_Name) then
         Unum := Load (Subunit_Name, True, N); -- must work, to set Unum
         Compiler_State := Analyzing; -- reset after load
         Analyze_Subunit (File.Table (Unum).Cunit);
         Set_Library_Unit (N, File.Table (Unum).Cunit);
         raise Subunit_Found;

      --  If the main unit is a subunit, then we are just performing semantic
      --  analysis on that subunit, and any other subunits of any parent unit
      --  should be ignored.

      elsif Nkind (Unit (File.Table (Main_Unit).Cunit)) = N_Subunit
        and then Subunit_Name /= File.table (Main_Unit).Unit_Name
      then
         return;

      --  If the subunit is not already loaded, and we are generating code,
      --  then this is the case where compilation started from the parent,
      --  and we are generating code for an entire subunit tree. In that
      --  case we definitely need to load the subunit.

      elsif Operating_Mode = Generate_Code then
         Unum := Load (Subunit_Name, True, N);
         Compiler_State := Analyzing; -- reset after load

         if Unum /= No_Unit and then not File.Table (Unum).Fatal_Error then

            if Debug_Flag_L then
               Write_Str ("*** Loaded subunit from stub. Analyze");
               Write_Eol;
            end if;

            Analyze_Subunit (File.Table (Unum).Cunit);
            Set_Library_Unit (N, File.Table (Unum).Cunit);
         end if;

         --  The remaining case is when the subunit is not already loaded and
         --  we are not generating code. In this case we are just analyzing
         --  the parent to check it for errors, and we are not interested in
         --  the subunit, so there is nothing to be done, except enter the
         --  name if there was no previous declaration.

      else
         null;
      end if;
   end Analyze_Proper_Body;

   ----------------------------------
   -- Analyze_Protected_Body_Stub --
   ----------------------------------

   procedure Analyze_Protected_Body_Stub (N : Node_Id) is
   begin
      Unimplemented (N, "Protected body stubs");
   end Analyze_Protected_Body_Stub;

   ----------------------------------
   -- Analyze_Subprogram_Body_Stub --
   ----------------------------------

   --  A subprogram body stub can appear with or without a previous
   --  specification. If there is one, the analysis of the body will
   --  find it and verify conformance.  The formals appearing in the
   --  specification of the stub play no role, except for requiring
   --  an additional conformance check. However, if we are performing
   --  semantic checks only, the stub must be analyzed like a body,
   --  because it may be the declaration of the subprogram.

   procedure Analyze_Subprogram_Body_Stub (N : Node_Id) is
   begin
      if Operating_Mode /= Generate_Code then
         Analyze_Subprogram_Body (N);
      else
         Analyze_Proper_Body (N);
      end if;
   end Analyze_Subprogram_Body_Stub;

   ---------------------
   -- Analyze_Subunit --
   ---------------------

   --  A subunit is compiled either by itself (for semantic checking)
   --  or as part of compiling the parent (for code generation). In
   --  either case, by the time we actually process the subunit, the
   --  parent has already been installed and analyzed. The node N is
   --  a compilation unit, whose context needs to be treated here,
   --  because we come directly here from the parent without calling
   --  Analyze_Compilation_Unit.

   --  The compilation context includes the explicit context of the
   --  subunit, and the context of the parent, together with the parent
   --  itself. In order to compile the current context, we remove the
   --  one inherited from the parent, in order to have a clean visibility
   --  table. We restore the parent context before analyzing the proper
   --  body itself. On exit, we remove only the explicit context of the
   --  subunit.

   procedure Analyze_Subunit (N : Node_Id) is
      Lib      : constant Node_Id := Library_Unit (N);
      Lib_Spec : constant Node_Id := Library_Unit (Lib);

   begin
      if List_Present (Context_Items (N)) then
         Remove_Context (Lib);

         --  If the parent is a package body, remove the context of the spec
         --  as well. If it is a subprogram body, verify that there is a
         --  spec for it.

         if Present (Lib_Spec) then
            Remove_Context (Lib_Spec);
         end if;

         Analyze_Context (N);

         if Present (Lib_Spec) then
            Install_Context (Lib_Spec);
         end if;

         Install_Context (Lib);
         Install_Context (N);
      end if;

      Analyze (Proper_Body (Unit (N)));
      Remove_Context (N);

   end Analyze_Subunit;

   -------------------------
   -- Analyze_With_Clause --
   -------------------------

   --  Analyze the declaration of a unit in  a with clause. At end,
   --  label the with clause with the defining entity for the unit.

   procedure Analyze_With_Clause (N : Node_Id) is
      E_Name    : Entity_Id;
      Unit_Kind : Node_Kind := Nkind (Unit (Library_Unit (N)));

   begin

      if Unit_Kind in N_Generic_Declaration then
         E_Name := Defining_Unit_Simple_Name
                             (Specification (Unit (Library_Unit (N))));
         if Operating_Mode = Generate_Code
           and then not Is_Intrinsic (E_Name)
         then
            Load_Needed_Body (Library_Unit (N));

         else
            --  If not generating code, or if the unit is intrinsic
            --  (for now only Unchecked conversion,  which does not need an
            --  explicit body)  analyze the specification only.

            Semantics (Library_Unit (N));
         end if;

      else
         Semantics (Library_Unit (N));
         if        Unit_Kind = N_Package_Instantiation
           or else Unit_Kind = N_Procedure_Instantiation
           or else Unit_Kind = N_Function_Instantiation
         then
            --  Instantiation node is replaced with body of instance.
            --  Unit name is defining unit name in corresponding spec.
            E_Name := Corresponding_Spec (Unit (Library_Unit (N)));

         elsif Unit_Kind = N_Package_Renaming_Declaration then
            E_Name := Defining_Unit_Simple_Name (Unit (Library_Unit (N)));

         else
            E_Name := Defining_Unit_Simple_Name
                                (Specification (Unit (Library_Unit (N))));
         end if;
      end if;

      if Nkind (Name (N)) = N_Selected_Component then

         --  Child unit in a with clause

         Change_Selected_Component_To_Expanded_Name (Name (N));
      end if;

      Set_Entity_With_Style_Check (Name (N), E_Name);
   end Analyze_With_Clause;

   --------------
   -- Ancestor --
   --------------

   function Ancestor (Lib_Unit : Node_Id) return Entity_Id is
      P      : constant Node_Id := Parent_Spec (Lib_Unit);
      P_Name : Entity_Id;

   begin
      P_Name := Defining_Unit_Simple_Name (Specification (Unit (P)));

      while Scope (P_Name) /= Standard_Standard loop
         P_Name := Scope (P_Name);
      end loop;

      return P_Name;
   end Ancestor;

   ---------------------
   -- Install_Context --
   ---------------------

   procedure Install_Context (N : Node_Id) is
      Lib_Unit  : Node_Id := Unit (N);
      Item      : Node_Id;
      Unit_Name : Entity_Id;

   begin
      --  Loop through context clauses to find the with clauses

      Item := First (Context_Items (N));
      while Present (Item) loop

         if Nkind (Item) = N_With_Clause
            and then not Implicit_With (Item)
         then
            Unit_Name := Entity (Name (Item));
            if not Is_Directly_Visible (Unit_Name) then
               Install_Withed_Unit (Unit_Name);
               Set_Context_Installed (Item, True);
            else
               --  Unit has already been installed for an earlier context.
               null;
            end if;
            if Is_Child_Spec (Get_Declaration_Node (Unit_Name)) then
               Implicit_With_On_Parent
                                   (Get_Declaration_Node (Unit_Name), Item);
            end if;

         elsif Nkind (Item) = N_Use_Package_Clause then
            Analyze_Use_Package (Item);

         elsif Nkind (Item) = N_Use_Type_Clause then
            Analyze_Use_Type (Item);
         end if;

         Item := Next (Item);
      end loop;

      --  If the unit is a body, the context of the specification must also
      --  be installed.

      if Nkind (Lib_Unit) = N_Package_Body
        or else (Nkind (Lib_Unit) = N_Subprogram_Body
                  and then not Acts_As_Spec (N))
      then
         Install_Context (Library_Unit (N));

         --  If the unit is the body of a public child unit, the private
         --  declarations of the parent must be made visible. If the child
         --  unit is private, the private declarations have been installed
         --  already in the call to Install_Parents for the spec.
         declare
            Lib_Spec : Node_Id := Unit (Library_Unit (N));
            P        : Node_Id;
            P_Name   : Entity_Id;
         begin
            if Is_Child_Spec (Lib_Spec)
              and then not Is_Private_Descendant (
                   Defining_Unit_Simple_Name (Specification (Lib_Spec)))
            then
               P := Unit (Parent_Spec (Lib_Spec));
               P_Name := Defining_Unit_Simple_Name (Specification (P));
               Install_Private_Declarations (P_Name);
            end if;
         end;
      end if;

      Install_Parents (Lib_Unit);

   end Install_Context;

   -----------------------------
   -- Implicit_With_On_Parent --
   -----------------------------

   procedure Implicit_With_On_Parent (Child_Unit : Node_Id;
                                      Item       : Node_Id) is
      P : Node_Id := Parent_Spec (Child_Unit);
      P_Name : Entity_Id :=
             Defining_Unit_Simple_Name (Specification (Unit (P)));
      Withn : Node_Id := Make_With_Clause (Sloc (Item),
                    Name => New_Reference_To (P_Name, Sloc (Item)));
   begin

      Set_Library_Unit          (Withn, P);
      Set_Corresponding_Spec    (Withn, P_Name);
      Set_First_Name            (Withn, True);
      Set_Implicit_With         (Withn, True);

      Insert_After (Item, Withn);
      Mark_Rewrite_Insertion (Withn);

      if not Is_Directly_Visible (P_Name) then
         Install_Withed_Unit (P_Name);
         Set_Context_Installed (Withn, True);
      end if;

      if Is_Child_Spec (Unit (P)) then
         Implicit_With_On_Parent (Unit (P), Withn);
      end if;
   end Implicit_With_On_Parent;

   --------------------------------
   -- Insert_Version_Declaration --
   --------------------------------

   procedure Insert_Version_Declaration (N : Node_Id) is
      Unit_Node  : Node_Id := Unit (N);
      Unit_Num   : constant Unit_Number_Type := Get_Cunit_Unit_Number (N);
      Unit_Decls : List_Id;
      Str        : String_Id;
      Ver_Decl   : Node_Id;
      Loc        : constant Source_Ptr := Sloc (Unit_Node);
      Ver_Ent    : Entity_Id;
      V_Name     : Name_Id;

   begin
      --  Don't generate version declaration unless either B or V debug
      --  switch is set and we have not detected any erorrs.

      if not (Debug_Flag_B or Debug_Flag_V)
        or else Errors_Detected /= 0
      then
         return;
      end if;

      --  Figure out whether to generate spec or body version information

      if Nkind (Unit_Node) = N_Subunit then
         Unit_Node := Proper_Body (Unit_Node);
      end if;

      if Nkind (Unit_Node) = N_Package_Declaration
        or else Nkind (Unit_Node) = N_Generic_Package_Declaration
      then
         Unit_Decls := Visible_Declarations (Specification (Unit_Node));
         V_Name := Name_Version_S;

      elsif Nkind (Unit_Node) = N_Subprogram_Body
        or else Nkind (Unit_Node) = N_Package_Body
      then
         Unit_Decls := Declarations (Unit_Node);
         V_Name := Name_Version_B;

      else
         return;
      end if;

      --  Build string constant for declaration

      Start_String;
      Get_Name_String (File.Table (Unit_Num).File_Name);

      for I in 1 .. Name_Len loop
         Store_String_Char (Get_Char_Code (Name_Buffer (I)));
      end loop;

      --  The version number information is appended only if debug V is 
      --  set and a version number was collected from the source header.

      if Debug_Flag_V 
        and then File.Table (Unit_Num).Version (1) /= ' '
      then
         Store_String_Char (Get_Char_Code (' '));

         for J in Int range 1 .. 6 loop
            exit when File.Table (Unit_Num).Version (J) = ' ';
            Store_String_Char
              (Get_Char_Code (File.Table (Unit_Num).Version (J)));
         end loop;
      end if;

      Store_String_Char (Get_Char_Code (NUL));
      Str := End_String;

      --  Now generate the declaration:

      --    Version_x : aliased constant String := "file name [version]";

      --  where Version_x is Version_S for a spec, Version_B for a body


      Ver_Ent := Make_Defining_Identifier (Loc, V_Name);

      Ver_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Ver_Ent,
          Constant_Present    => True,
          Object_Definition   =>
            New_Reference_To (Standard_String, Loc),
          Expression => Make_String_Literal (Loc, Str));

      Prepend (Ver_Decl, Unit_Decls);

   end Insert_Version_Declaration;

   -------------------------
   -- Install_Withed_Unit --
   -------------------------

   procedure Install_Withed_Unit (Unit_Name : Entity_Id) is

      procedure Install_Package (Package_Id : Entity_Id) is
         Vis_Entity  : Entity_Id;
         Prev_Entity : Entity_Id;

      begin
         if Debug_Flag_I then
            Write_Str ("Relinking package ");
            Write_Name (Chars (Package_Id));
            Write_Eol;
         end if;

         Vis_Entity := First_Entity (Package_Id);

         while Present (Vis_Entity)
           and then Vis_Entity /= First_Private_Entity (Package_Id)
         loop
            if not Is_Internal (Vis_Entity) then
               Prev_Entity := Current_Entity (Vis_Entity);
               --  Set Selectively_visible flag.
            end if;

            Vis_Entity := Next_Entity (Vis_Entity);
         end loop;

         --  Install recursively any packages nested in the visible part
         --  of the current one.

         Vis_Entity := First_Entity (Package_Id);

         while Present (Vis_Entity)
           and then Vis_Entity /= First_Private_Entity (Package_Id)
         loop
            if Ekind (Vis_Entity) = E_Package then
               Install_Package (Vis_Entity);
            end if;
            Vis_Entity := Next_Entity (Vis_Entity);
         end loop;

      end Install_Package;

   --  Start of processing for Install_Withed_Unit

   begin
      --  Install the unit entity itself (it's directly visible of course
      --  so it goes on the front of the chain with no fiddling around)

      Set_Is_Directly_Visible (Unit_Name);

      --  For a subprogram, that's all that needs to be done. For a
      --  package, the entities within the package must be installed.

      if Ekind (Unit_Name) = E_Package then
         Install_Package (Unit_Name);
      end if;

      if Scope (Unit_Name) /= Standard_Standard then

         --  The unit is a child unit. Install all parents

         declare
            P : Entity_Id := Scope (Unit_Name);

         begin
            while P /= Standard_Standard loop
               Install_Withed_Unit (P);
               P := Scope (P);
            end loop;
         end;
      end if;

   end Install_Withed_Unit;

   -----------------------
   -- Load_Needed_Body --
   -----------------------

   --  N is a generic unit named in a with clause, or else it is
   --  a unit that contains a generic unit or an inlined function.
   --  In order to perform an instantiation, the body of the unit
   --  must be present. If the unit itself is generic, we assume
   --  that an instantiation follows, and  load and analyze the body
   --  unconditionally. This forces analysis of the spec as well.
   --  If the unit is not generic, but contains a generic unit, it
   --  is loaded on demand, at the point of instantiation (see ch12).

   procedure Load_Needed_Body (N : Node_Id) is
      Body_Name : Unit_Name_Type;
      Unum      : Unit_Number_Type;

   begin
      Body_Name := Get_Body_Name (Get_Unit_Name (Unit (N)));
      Unum := Load (Body_Name, True, N);
      Compiler_State := Analyzing; -- reset after load

      if Unum /= No_Unit
        and then not File.Table (Unum).Fatal_Error
      then
         if Debug_Flag_L then
            Write_Str ("*** Loaded generic body" ); Write_Eol;
         end if;

         Semantics (File.Table (Unum).Cunit);
      end if;
   end Load_Needed_Body;

   ----------------------
   --  Install_Parents --
   ----------------------

   procedure Install_Parents (Lib_Unit : Node_Id) is
      P      : Node_Id;
      P_Name : Entity_Id;

   begin
      if Is_Child_Spec (Lib_Unit) then

         P := Unit (Parent_Spec (Lib_Unit));
         P_Name := Defining_Unit_Simple_Name (Specification (P));

         if Ekind (P_Name) = E_Generic_Package
           and then Nkind (Lib_Unit) /= N_Generic_Subprogram_Declaration
           and then Nkind (Lib_Unit) /= N_Generic_Package_Declaration
         then
            Error_Msg_N
              ("child of a generic package must be generic unit", Lib_Unit);
         end if;

         --  This is the recursive call that ensures all parents are loaded

         Install_Parents (P);

         --  Now we can install the context for this parent

         Install_Context (Parent_Spec (Lib_Unit));

         --  The child unit is in the declarative region of the parent. The
         --  parent must therefore appear in the scope stack and be visible,
         --  as when compiling the corresponding body. If the child unit is
         --  private or it is a package body, private declarations must be
         --  accessible as well.

         Set_Is_Directly_Visible (P_Name, True);
         Set_Is_Private_Descendant (Lib_Unit,
                       Is_Private_Descendant (P_Name)
                       or else Private_Present (Parent (Lib_Unit)));

         New_Scope (P_Name);
         Install_Visible_Declarations (P_Name);

         if Private_Present (Parent (Lib_Unit)) then
            Install_Private_Declarations (P_Name);
         end if;

      --  If the unit is not a child unit, or is a body, nothing to do.

      else
         null;
      end if;
   end Install_Parents;

   -------------------
   -- Is_Child_Spec --
   -------------------

   function Is_Child_Spec (Lib_Unit : Node_Id) return Boolean is
      K : constant Node_Kind := Nkind (Lib_Unit);

   begin
      return (        K = N_Function_Instantiation
              or else K = N_Generic_Function_Renaming_Declaration
              or else K = N_Generic_Package_Declaration
              or else K = N_Generic_Package_Renaming_Declaration
              or else K = N_Generic_Procedure_Renaming_Declaration
              or else K = N_Generic_Subprogram_Declaration
              or else K = N_Package_Declaration
              or else K = N_Package_Instantiation
              or else K = N_Package_Renaming_Declaration
              or else K = N_Procedure_Instantiation
              or else K = N_Subprogram_Declaration
              or else K = N_Subprogram_Renaming_Declaration)
        and then Present (Parent_Spec (Lib_Unit));
   end Is_Child_Spec;

   --------------------
   -- Remove_Parents --
   --------------------

   procedure Remove_Parents (Lib_Unit : Node_Id) is
      P      : Node_Id;
      P_Name : Entity_Id;

   begin
      if Is_Child_Spec (Lib_Unit) then
         P := Unit (Parent_Spec (Lib_Unit));
         P_Name := Defining_Unit_Simple_Name (Specification (P));
         Remove_Context (Parent_Spec (Lib_Unit));
         End_Package_Scope (P_Name);
         Set_Is_Package_Body (P_Name, False);

         --  This is the recursive call to remove the context of any
         --  higher level parent. This recursion ensures that all parents
         --  are removed in the reverse order of their installation.

         Remove_Parents (P);
      end if;
   end Remove_Parents;

   --------------------
   -- Remove_Context --
   --------------------

   procedure Remove_Context (N : Node_Id) is
      Lib_Unit  : Node_Id := Unit (N);
      Item      : Node_Id;
      Unit_Name : Entity_Id;

   begin
      --  We can skip the removal if this is the main unit, since on exit from
      --  the main unit no one needs the visibility chains any more, so it
      --  doesn't matter if they have junk left around on them.

      if N /= File.Table (Main_Unit).Cunit then

         --  Otherwise loop through context items looking for with clauses

         Item := First (Context_Items (N));

         while Present (Item) loop

            --  We are interested only in with clauses which got installed
            --  on entry, as indicated by their Context_Installed flag set

            if Nkind (Item) = N_With_Clause
               and then Context_Installed (Item)
            then

               --  Remove items from one with'ed unit

               Unit_Name := Entity (Name (Item));
               Remove_Withed_Unit (Unit_Name);
               Set_Context_Installed (Item, False);
            end if;

            Item := Next (Item);
         end loop;

         Remove_Parents (Lib_Unit);

      end if;
   end Remove_Context;

   ------------------------
   -- Remove_Withed_Unit --
   ------------------------

   procedure Remove_Withed_Unit (Unit_Name : Entity_Id) is

      procedure Remove_Package (Package_Id : Entity_Id) is
         Vis_Entity  : Entity_Id;

      begin
         Vis_Entity := First_Entity (Package_Id);

         while Present (Vis_Entity)
           and then Vis_Entity /= First_Private_Entity (Package_Id)
         loop
            Remove_Entity (Vis_Entity);

            if Ekind (Vis_Entity) = E_Package then
               Remove_Package (Vis_Entity);
            end if;

            Vis_Entity := Next_Entity (Vis_Entity);
         end loop;
      end Remove_Package;

   --  Start of processing for Remove_Withed_Unit

   begin
      --  We definitely must remove the unit entity itself

      if Debug_Flag_I then
         Write_Str ("remove withed unit "); Write_Name (Chars (Unit_Name));
         Write_Eol;
      end if;

      Remove_Entity (Unit_Name);
      Set_Is_Directly_Visible (Unit_Name, False);
      Set_In_Use (Unit_Name, False);    -- unconditionally.

      --  For a subprogram, that's all we have to do. For a package, we must
      --  also remove all the entities within the package that were chained
      --  and recursively for inner packages.

      if Ekind (Unit_Name) = E_Package then
         Remove_Package (Unit_Name);
      end if;

   end Remove_Withed_Unit;

   -------------------
   -- Remove_Entity --
   -------------------

   procedure Remove_Entity (E : Entity_Id) is
   begin
      --  Internal entities are never visible

      if Is_Internal (E) then
         null;

      else
         --  Set selectively_Visible false.
         null;
      end if;

      Set_Is_Use_Visible (E, False);    -- unconditionally.
   end Remove_Entity;

end Sem_Ch10;
