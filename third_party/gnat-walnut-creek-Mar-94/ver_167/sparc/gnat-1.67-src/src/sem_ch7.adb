------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M . C H 7                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.77 $                             --
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
with Lib;      use Lib;
with Namet;    use Namet;
with Opt;      use Opt;
with Output;   use Output;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch13; use Sem_Ch13;
with Sem_Util; use Sem_Util;
with Stand;    use Stand;
with Sinfo;    use Sinfo;

package body Sem_Ch7 is

--  This package contains the routines to process package specifications and
--  bodies. The most important semantic aspects of package processing are the
--  handling of private and full declarations, and the construction of
--  dispatch tables for tagged types.

-----------------------------------
-- Handling private declarations --
-----------------------------------

--  The principle that each entity has a single defining occurrence clashes
--  with the presence of two separate definitions for private types: the first
--  is the private type declaration, and the second is the full type
--  declaration. It is important that all references to the type point to the
--  same defining occurence, namely the first one. To enforce the two separate
--  views of the entity, the corresponding information is swapped between the
--  two declarations. Outside of the package, the defining occurence only
--  contains the private declaration information, while in the private part
--  and the body of the package the defining occurrence contains the full
--  declaration. To simplify the swap, the defining occurrence that currently
--  holds the private declaration points to the full declaration. During
--  semantic processing the defining occurence also points to a list of
--  private dependents, that is to say access types or composite types whose
--  designated types or component types are subtypes or derived types of the
--  private type in question. After the full declaration has been seen, the
--  private dependents are updated to indicate that they have full definitions.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Private_Part  (Decls : List_Id; N : Entity_Id);

   procedure Exchange_Declarations (Id : Entity_Id);
   --  Exchange private and full declaration on entry/exit from a package
   --  declaration or body. The semantic links of the respective nodes
   --  are preserved in the exchange.

   ---------------------------------
   -- Analyze_Package_Declaration --
   ---------------------------------

   procedure Analyze_Package_Declaration (N : Node_Id) is
      Id : constant Node_Id := Defining_Unit_Simple_Name (Specification (N));

   begin
      Enter_Name (Id);
      Set_Ekind (Id, E_Package);
      Set_Etype (Id, Standard_Void_Type);
      New_Scope (Id);

      if Debug_Flag_C then
         Write_Str ("========  Compiling package spec ");
         Write_Name (Chars (Id));
         Write_Eol;
      end if;

      Analyze (Specification (N));
      End_Package_Scope (Id);

      --  For a compilation unit, indicate whether it needs a body.

      if Nkind (Parent (N)) = N_Compilation_Unit then
         Set_Body_Required (Parent (N), Unit_Requires_Body (Id));
      end if;
   end Analyze_Package_Declaration;

   -----------------------------------
   -- Analyze_Package_Specification --
   -----------------------------------

   procedure Analyze_Package_Specification (N : Node_Id) is
      E : Entity_Id;
      Id : constant Entity_Id := Defining_Unit_Simple_Name (N);
      Vis_Decls  : List_Id := Visible_Declarations (N);
      Priv_Decls : List_Id := Private_Declarations (N);

   begin
      if List_Present (Vis_Decls) then
         Analyze_Declarations (Vis_Decls);
      end if;

      --  Verify that incomplete types have received full declarations.

      E := First_Entity (Id);

      while Present (E) loop
         if Ekind (E) = E_Incomplete_Type
           and then No (Full_Declaration (E))
         then
            Error_Msg_NE
              ("incomplete type& has no full declaration in visible part",
                     E, E);
         end if;

         E := Next_Entity (E);
      end loop;

      --  If package is a public child unit, then make the private
      --  declarations of the parent visible.

      if Present (Parent_Spec ( Parent (N))) 
         and then not Is_Private_Descendant (Id)
      then
         declare
            Par : Entity_Id := Scope (Id);
         begin
            while Par /= Standard_Standard loop
               Install_Private_Declarations (Par);
               Par := Scope (Par);
            end loop;
         end;
      end if;

      if List_Present (Priv_Decls) then
         Analyze_Private_Part (Priv_Decls, Id);
      end if;

      End_Use (Vis_Decls);
      End_Use (Priv_Decls);

      if List_Present (Priv_Decls) then
         Append_List (Freeze_All, Priv_Decls);
      elsif List_Present (Vis_Decls) then
         Append_List (Freeze_All,  Vis_Decls);
      end if;
   end Analyze_Package_Specification;

   -----------------------
   -- End_Package_Scope --
   -----------------------

   procedure End_Package_Scope (P : Entity_Id) is
      Id     : Entity_Id := First_Entity (P);

   begin
      while Present (Id) and then Id /= First_Private_Entity (P) loop
         if Debug_Flag_E then
            Write_Str ("unlinking visible entity ");
            Write_Int (Int (Id));
            Write_Eol;
         end if;

         Set_Is_Use_Visible (Id, In_Use (P));
         Set_Is_Directly_Visible (Id, False);

         if Ekind (Id) in E_Private_Type .. E_Limited_Private_Type 
           and then No (Full_Declaration (Id))
           and then not Is_Generic_Type (Id) 
           and then not Is_Derived_Type (Id) 
         then
            Error_Msg_N ("missing full declaration for private type", Id);
         end if;

         Id := Next_Entity (Id);
      end loop;

      --  Make private entities invisible by removing them from their
      --  homonym chains, and exchange full and private declarations
      --  for private types.

      while Present (Id) loop
         if Debug_Flag_E then
            Write_Str ("unlinking private entity ");
            Write_Int (Int (Id));
            Write_Eol;
         end if;

         Set_Is_Directly_Visible (Id, False);

         if Ekind (Id) in E_Private_Type .. E_Limited_Private_Type then
            --  The entry in the private part points to the full declaration,
            --  which is currently visible. Exchange them so only the private
            --  type declaration remains accessible, and link private and
            --  full declaration in the opposite direction. Before the actual
            --  exchange, we make sure that the Size is copied back into the
            --  private declaration. This is the one attribute of the full
            --  type that must be available for the private type too.

            --  Are there other attributes that should be treated the same,
            --  for example Eaddress? TBSL.
            Set_Esize (Id, Esize (Full_Declaration (Id)));
            Set_Is_Use_Visible (Id, In_Use (P));
            Exchange_Declarations (Id);

         else
            Set_Is_Private (Id);
            Set_Is_Use_Visible (Id, False);
         end if;

         Id  := Next_Entity (Id);
      end loop;

      Pop_Scope;
   end End_Package_Scope;

   ---------------------------
   -- Exchange_Declarations --
   ---------------------------

   procedure Exchange_Declarations (Id : Entity_Id) is
      Full_Id : constant Entity_Id := Full_Declaration (Id);
      Next1   : Entity_Id := Next_Entity (Id);
      Next2   : Entity_Id;

   begin
      --  If missing full declaration for type, nothing to exchange

      if No (Full_Id) then
         return;
      end if;

      Next2 := Next_Entity (Full_Id);
      Exchange_Nodes (Id, Full_Id);
      Set_Full_Declaration (Full_Id, Id);
      Set_Next_Entity (Id,  Next1);
      Set_Next_Entity (Full_Id, Next2);
   end Exchange_Declarations;

   --------------------------------------
   -- Analyze_Private_Type_Declaration --
   --------------------------------------

   procedure Analyze_Private_Type_Declaration (N : Node_Id) is
      Id : Node_Id := Defining_Identifier (N);

   begin
      Enter_Name (Id);

      if Limited_Present (N) then
         Set_Ekind (Id, E_Limited_Private_Type);
         Set_Is_Limited_Type (Id);
      else
         Set_Ekind (Id, E_Private_Type);
      end if;

      Set_Is_Tagged_Type (Id, Tagged_Present (N));

      Set_Etype (Id, Id);

      Set_Is_Private_Type (Id);
      Set_Is_Delayed (Id);

      if Ekind (Current_Scope) /= E_Package
         and Ekind (Current_Scope) /= E_Generic_Package
      then
         Error_Msg_N
           ("invalid context for private declaration", N);
      end if;

      New_Scope (Id);

      if List_Present (Discriminant_Specifications (N)) then
         Process_The_Discriminants (N);
      end if;

      End_Scope;

      --  Initialize dispatch_table in tagged case

      if Tagged_Present (N) then
         Make_Class_Type (Id);
         Set_Primitive_Operations (Id, New_Elmt_List);
         Set_Is_Abstract (Id,  Abstract_Present (N));

      elsif Abstract_Present (N) then
         Error_Msg_N ("only a tagged type can be abstract", N);
      end if;
   end Analyze_Private_Type_Declaration;

   --------------------------
   -- Analyze_Package_Body --
   --------------------------

   procedure Analyze_Package_Body (N : Node_Id) is
      Body_Id          : constant Entity_Id := Defining_Unit_Simple_Name (N);
      Stat_Seq         : constant Node_Id   := Handled_Statement_Sequence (N);
      Spec_Id          : Entity_Id;
      Last_Spec_Entity : Entity_Id;
      Pack_Decl        : Node_Id;

   begin
      --  Find corresponding package specification, and establish the
      --  current scope. The visible defining entity for the package is the
      --  defining occurrence in the spec. On exit from the package body, all
      --  body declarations are attached to the defining entity for the body,
      --  but the later is never used for name resolution. In this fashion
      --  there is only one visible entity that denotes the package.

      if Debug_Flag_C then
         Write_Str ("=====  Compiling package body ");
         Write_Name (Chars (Body_Id));
         Write_Eol;
      end if;

      Set_Ekind (Body_Id, E_Package_Body);

      Spec_Id := Current_Entity (Body_Id);
      while Present (Spec_Id) loop
         exit when Scope (Spec_Id) = Current_Scope
           and then (Ekind (Spec_Id) = E_Package
             or else Ekind (Spec_Id) = E_Generic_Package);
         Spec_Id := Homonym (Spec_Id);
      end loop;

      if No (Spec_Id) then
         Error_Msg_N ("missing specification for package body", Body_Id);
         return;
      else
         Pack_Decl := Get_Declaration_Node (Spec_Id);

         if Present (Corresponding_Body (Pack_Decl)) then
            Error_Msg_N ("redefinition of package body", Body_Id);
            return;
         end if;
      end if;

      if Ekind (Spec_Id) = E_Package
        and then Current_Scope = Standard_Standard
         and then Parent (N) = File.Table (Main_Unit).Cunit
         and then not Unit_Requires_Body (Spec_Id)
      then
         if Ada_83 then
            Error_Msg_N ("optional package body (not allowed in Ada9X)?", N);
         else
            Error_Msg_N ("spec of this package does not allow a body", N);
         end if;
      end if;

      --  defining name for the package body is not a visible entity: Only
      --  the defining name for the declaration is visible.

      Set_Ekind (Body_Id, Ekind (Spec_Id));
      Set_Etype (Body_Id, Standard_Void_Type);
      Set_Scope (Body_Id, Current_Scope);
      Set_Corresponding_Spec (N, Spec_Id);
      Set_Corresponding_Body (Pack_Decl, Body_Id);

      --  Indicate that we are currently compiling the body of the package.

      Set_Is_Package_Body (Spec_Id);
      Set_Has_Completion (Spec_Id);
      Last_Spec_Entity := Last_Entity (Spec_Id);

      New_Scope (Spec_Id);
      Install_Visible_Declarations (Spec_Id);
      Install_Private_Declarations (Spec_Id);
      Set_Use (Visible_Declarations (Specification (Pack_Decl)));
      Set_Use (Private_Declarations (Specification (Pack_Decl)));

      if List_Present (Declarations (N)) then
         Analyze_Declarations (Declarations (N));
      end if;

      if Present (Stat_Seq) then
         Analyze (Stat_Seq);
      end if;

      End_Use (Declarations (N));
      End_Use (Private_Declarations (Specification (Pack_Decl)));
      End_Use (Visible_Declarations (Specification (Pack_Decl)));
      Check_Completion;

      if Ekind (Spec_Id) = E_Package then
         End_Package_Scope (Spec_Id);
      else
         --  The local declarations of a generic package are not visible

         End_Scope;
      end if;

      --  Chain the body declarations to the defining occurrence in the package

      if Present (Last_Spec_Entity) then
         Set_First_Entity (Body_Id, Next_Entity (Last_Spec_Entity));
         Set_Next_Entity (Last_Spec_Entity, Empty);
      else
         Set_First_Entity (Body_Id, First_Entity (Spec_Id));
         Set_First_Entity (Spec_Id, Empty);
      end if;


      Set_Is_Package_Body (Spec_Id, False);
   end Analyze_Package_Body;

   ----------------------------------
   -- Install_Visible_Declarations --
   ----------------------------------

   procedure Install_Visible_Declarations (P : Entity_Id) is
      Id : Entity_Id;

   begin
      Id := First_Entity (P);
      while Present (Id)
        and then Id /= First_Private_Entity (P)
      loop
         Install_Package_Entity (Id);
         Id := Next_Entity (Id);
      end loop;
   end Install_Visible_Declarations;

   ----------------------------------
   -- Install_Private_Declarations --
   ----------------------------------

   procedure Install_Private_Declarations (P : Entity_Id) is
      Id : Entity_Id;

   begin
      --  First exchange declarations for private types, so that the
      --  full declaration is visible.
      Id := First_Entity (P);
      while Present (Id) and then Id /= First_Private_Entity (P) loop
         if Ekind (Id) in E_Private_Type .. E_Limited_Private_Type then
            Exchange_Declarations (Id);
            Set_Is_Directly_Visible (Id);
         end if;
         Id := Next_Entity (Id);
      end loop;

      --  Next make other declarations in the private part visible as well.
      Id := First_Private_Entity (P);
      while Present (Id) loop
         Install_Package_Entity (Id);
         Id := Next_Entity (Id);
      end loop;
   end Install_Private_Declarations;

   -----------------------------
   --  Install_Package_Entity --
   -----------------------------

   procedure Install_Package_Entity (Id : Entity_Id) is
   begin
      if not Is_Internal (Id) then
         if Debug_Flag_E then
            Write_Str ("Install: ");
            Write_Name (Chars (Id));
            Write_Eol;
         end if;
         Set_Is_Directly_Visible (Id);
      end if;
   end Install_Package_Entity;

   --------------------------
   -- Analyze_Private_Part --
   --------------------------

   procedure Analyze_Private_Part (Decls : List_Id; N : Entity_Id) is
      L : Entity_Id := Last_Entity  (N);

   begin
      Set_In_Private_Part (N);
      Analyze_Declarations (Decls);

      --  The first private entity is the immediate follower of the last
      --  visible entity, if there was one.

      if Present (L) then
         Set_First_Private_Entity (N, Next_Entity (L));
      else
         Set_First_Private_Entity (N, First_Entity (N));
      end if;

      Set_In_Private_Part (N, False);
   end Analyze_Private_Part;

   ------------------------
   -- Unit_Requires_Body --
   ------------------------

   function Unit_Requires_Body (P : Entity_Id) return Boolean is
      E : Entity_Id := First_Entity (P);

   begin
      while Present (E) loop
         if (Is_Overloadable (E)
           and then Ekind (E) /= E_Enumeration_Literal
           and then Ekind (E) /= E_Operator
           and then not Has_Completion (E))
           or else
             (Ekind (E) = E_Package and then E /= P
             and then not Has_Completion (E)
             and then Unit_Requires_Body (E))
           or else
             (Ekind (E) = E_Incomplete_Type
             and then No (Full_Declaration (E)))
           or else
             (((Ekind (E) = E_Task_Type)
              or else Ekind (E) = E_Protected_Type)
              and then not Has_Completion (E))
         then
            return True;
         else
            null;
         end if;

         E := Next_Entity (E);
      end loop;

      return False;
   end Unit_Requires_Body;

   ----------------------
   -- Is_Fully_Visible --
   ----------------------

   --  The full declaration of a private type is visible in the private
   --  part of the package declaration, and in the package body, at which
   --  point the full declaration must have been given.

   function Is_Fully_Visible (Type_Id : Entity_Id) return Boolean is
      S : Entity_Id := Scope (Type_Id);

   begin
      if Is_Generic_Type (Type_Id) then return false;
      elsif In_Private_Part (S) then
         return Present (Full_Declaration (Type_Id));
      else return Is_Package_Body (S);
      end if;
   end Is_Fully_Visible;

   ---------------------------------
   -- Mark_Implicit_Private_Decls --
   ---------------------------------

   procedure Mark_Implicit_Private_Decls (Impl_List : List_Id) is
      N : Node_Id := First (Impl_List);
      Type_Id : Entity_Id;

   begin
      while Present (N) loop
         if Nkind (N) = N_Implicit_Type then
            Type_Id := Defining_Identifier (N);
            Set_Is_Private_Type (Type_Id, Has_Private_Component (Type_Id));
         end if;

         N := Next (N);
      end loop;
   end Mark_Implicit_Private_Decls;

end Sem_Ch7;
