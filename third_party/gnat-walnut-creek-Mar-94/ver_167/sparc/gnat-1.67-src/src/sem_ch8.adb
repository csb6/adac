------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M . C H 8                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.120 $                            --
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
with Comperr;      use Comperr;
with Debug;        use Debug;
with Einfo;        use Einfo;
with Errout;       use Errout;
with Namet;        use Namet;
with Nmake;        use Nmake;
with Opt;          use Opt;
with Output;       use Output;
with Sem;          use Sem;
with Sem_Attr;     use Sem_Attr;
with Sem_Ch3;      use Sem_Ch3;
with Sem_Ch4;      use Sem_Ch4;
with Sem_Ch5;      use Sem_Ch5;
with Sem_Ch6;      use Sem_Ch6;
with Sem_Ch8;      use Sem_Ch8;
with Sem_Res;      use Sem_Res;
with Sem_Util;     use Sem_Util;
with Sem_Type;     use Sem_Type;
with Stand;        use Stand;
with Sinfo;        use Sinfo;
with Sinfo.Change; use Sinfo.Change;
with Snames;       use Snames;
with Tbuild;       use Tbuild;

package body Sem_Ch8 is

   ------------------------------------
   -- Visibility and Name Resolution --
   ------------------------------------

   --  This package handles name resolution and the collection of
   --  interpretations for overloaded names, prior to overload resolution.

   --  Name resolution is the process that establishes a mapping between source
   --  identifiers and the entities they denote at each point in the program.
   --  Each entity is represented by a defining occurrence. Each identifier
   --  that denotes an entity points to the corresponding defining occurrence.
   --  This is the entity of the applied occurrence. Each occurrence holds
   --  an index into the names table, where source identifiers are stored.

   --  Each entry in the names table for an identifier or designator uses the
   --  Info pointer to hold a link to the currently visible entity that has
   --  this name (see subprograms Get_Name_Entity_Id and Set_Name_Entity_Id
   --  in package Einfo). The visibility is initialized at the beginning of
   --  semantic rocessing to make entities in package Standard directly
   --  visible. The visibility table is used in a more subtle way when
   --  compiling subunits (see below).

   --  Entities that have the same name (i.e. homonyms) are chained. In the
   --  case of overloaded entities, this chain holds all the possible meanings
   --  of a given identifier. The process of overload resolution uses type
   --  information to select from this chain the unique meaning of a given
   --  identifier.

   --  Entities are also chained in their scope, through the Next_Entity link.
   --  As a consequence, the name space is organized as a sparse matrix, where
   --  each row corresponds to a scope, and each column to a source identifier.
   --  Open scopes, that is to say scopes currently being compiled, have their
   --  corresponding rows of entities in order, innermost scope first.
   --  The scopes of packages that are mentioned in  context clauses appear in
   --  no particular order, interspersed among open scopes. This is because
   --  in the course of analyzing the context of a compilation, a package
   --  declaration is first an open scope,  and subsequently an element of the
   --  context. If subunits or child units are present, a parent unit may
   --  appear under various guises at various times in the compilation.
   --
   --  When the compilation of the innermost scope is complete, the entities
   --  defined therein are no longer visible. If the scope is not a package
   --  declaration, these entities are never visible subsequently,  and can be
   --  removed from visibility chains. If the scope is a package declaration,
   --  its visible declarations may still be accessible. Therefore the entities
   --  defined in such a scope are left on the visibility chains, and only
   --  their visibility (directly-visible or use-visible) is affected. The
   --  name resolution routines rely on the fact that directly visible entities
   --  are ordered on their chains, but cannot assume any relative ordering
   --  among entities made accessible by context clauses. In particular, the
   --  entities defined in the current scope are not necessarily the first ones
   --  on their chain, because a package entity may have been inserted first.

   --  When compiling a child unit, entities in the parent scope are directly
   --  visible. When compiling the body of a child unit, private entities in
   --  the parent must also be made visible. There are separate routine to
   --  make the visible and private declarations directly visible at various
   --  times (see sem_ch7).
   --

   --              +--------+         +-----+
   --              | In use |-------->| EU1 |-------------------------->
   --              +--------+         +-----+
   --                                    |                      |
   --      +--------+                 +-----+                +-----+
   --      | Stand. |---------------->| ES1 |--------------->| ES2 |--->
   --      +--------+                 +-----+                +-----+
   --                                    |                      |
   --              +---------+           |                   +-----+
   --              | with'ed |------------------------------>| EW2 |--->
   --              +---------+           |                   +-----+
   --                                    |                      |
   --      +--------+                 +-----+                +-----+
   --      | Scope2 |---------------->| E12 |--------------->| E22 |--->
   --      +--------+                 +-----+                +-----+
   --                                    |                      |
   --      +--------+                 +-----+                +-----+
   --      | Scope1 |---------------->| E11 |--------------->| E12 |--->
   --      +--------+                 +-----+                +-----+
   --          ^                         |                      |
   --          |                         |                      |
   --          |   +---------+           |                      |
   --          |   | with'ed |----------------------------------------->
   --          |   +---------+           |                      |
   --          |                         |                      |
   --      Scope stack                   |                      |
   --      (innermost first)             |                      |
   --                                 +----------------------------+
   --      Names  table =>            | Id1 |     |    |     | Id2 |
   --                                 +----------------------------+

   --  Name resolution must deal with several syntactic forms: simple names,
   --  qualified names, indexed names, and various forms of calls.

   --  Each identifier points to an entry in the names table. The resolution
   --  of a simple name consists in traversing the homonym chain, starting
   --  from the names table. If an entry is directly visible, it is the one
   --  designated by the identifier. If only use-visible entities are on the
   --  chain, we must verify that they do not hide each other. If the entity
   --  we find is overloadable, we collect all other overloadable entities on
   --  the chain as long as they are not hidden.
   --
   --  To resolve expanded names, we must find the entity at the intersection
   --  of the entity chain for the scope (the prefix) and the homonym chain
   --  for the selector. In general, homonym chains will be much shorter than
   --  entity chains, so it is preferable to start from the names table as
   --  well. If the entity found is overloadable, we must collect all other
   --  interpretations that are defined in the scope denoted by the prefix.

   --  For records, protected types, and tasks, their local entities are
   --  removed from visibility chains on exit from the corresponding scope.
   --  From the outside, these entities are always accessed by selected
   --  notation, and the entity chain for the record type, protected type,
   --  etc. is traversed sequentially in  order to find the designated entity.

   --  The discriminants of a type and the operations of a protected type or
   --  task are unchained on  exit from the first view of the type, (such as
   --  a private or incomplete type declaration, or a protected type speci-
   --  fication) and rechained when compiling the second view.

   --  In the case of operators,  we do not make operators on derived types
   --  explicit. As a result, the notation P."+" may denote either a user-
   --  defined function with name "+", or else an implicit declaration of the
   --  operator "+" in package P. The resolution of expanded names always
   --  tries to resolve an operator name as such an implicitly defined entity,
   --  in addition to looking for explicit declarations.

   --  All forms of names that denote entities (simple names, expanded names,
   --  character literals in some cases) have a Entity attribute, which
   --  identifies the entity denoted by the name.

   ---------------------
   -- The Scope Stack --
   ---------------------

   --  The Scope stack keeps track of the scopes currently been compiled.
   --  Every entity that contains declarations (including records) is placed
   --  on the scope stack while it is being processed, and removed at the end.
   --  Whenever a non-package scope is exited, the entities defined therein
   --  are removed from the visibility table, so that entities in outer scopes
   --  become visible (see previous description). On entry to Sem, the scope
   --  stack only contains the package Standard. As usual, subunits complicate
   --  this picture ever so slightly.

   ------------------------
   -- Compiling subunits --
   ------------------------

   --  Subunits must be compiled in the environment of the corresponding
   --  stub, that is to say with the same visibility into the parent (and its
   --  context) that is available at the point of the stub declaration, but
   --  with the additional visibility provided by the context clause of the
   --  subunit itself. As a result, compilation of a subunit forces compilation
   --  of the parent (see description in lib-). At the point of the stub
   --  declaration, Analyze is called recursively to compile the proper body
   --  of the subunit, but without reinitializing the names table, nor the
   --  scope stack (i.e. standard is not pushed on the stack). In this fashion
   --  the context of the subunit is added to the context of the parent, and
   --  the subunit is compiled in the correct environment. Note that in the
   --  course of processing the context of a subunit, Standard will appear
   --  twice on the scope stack: once for the parent of the subunit, and
   --  once for the unit in the context clause being compiled. However, the
   --  two sets of entities are not linked by homonym chains, so that the
   --  compilation of any context unit happens in a fresh visibility
   --  environment.

   -------------------------------
   -- Processing of USE Clauses --
   -------------------------------

   --  Every defining occurrence has a flag to indicate whether it is directly
   --  visible. Resolution of simple names examines this flag. The processing
   --  of use clauses consists in setting this flag on all visible entities
   --  defined in the corresponding package. On exit from the scope of the use
   --  clause, the corresponding flag must be reset. However, a package may
   --  appear in several nested use clauses (pathological but legal, alas!)
   --  which forces us to use a slightly more involved scheme:

   --    a) The defining occurrence for a package hold a flag -In_Use- to
   --    indicate that it is currently in the scope of a use clause. If a
   --    redundant use clause is encountered, then the corresponding occurence
   --    of the package name is flagged -Redundant_Use-.

   --    b) On exit from a scope, the use clauses in its declarative part are
   --    scanned. The visibility flag is reset in all entities declared in
   --    package named in a use clause, as long as the package is not flagged
   --    as being in a redundant use clause (in which case the outer use
   --    clause is still in effect, and the direct visibility of its entities
   --    must be retained).

   --  Note that entities are not removed from their homonym chains on exit
   --  from the package specification. A subsequent use clause does not need
   --  to rechain the visible entities, but only to establish their direct
   --  visibility.

   -----------------------------------
   -- Handling private declarations --
   -----------------------------------

   --  The principle that each entity has a single defining occurrence clashes
   --  with the presence of two separate definitions for private types: the
   --  first is the private type declaration, and second is the full type
   --  declaration. It is important that all references to the type point to
   --  the same defining occurence, namely the first one. To enforce the two
   --  separate views of the entity, the corresponding information is swapped
   --  between the two declarations. Outside of the package, the defining
   --  occurence only contains the private declaration information, while in
   --  the private part and the body of the package the defining occurrence
   --  contains the full declaration. To simplify the swap, the defining
   --  occurrence that currently holds the private declaration points to the
   --  full declaration. During semantic processing the defining occurence
   --  also points to a list of private dependents, that is to say access
   --  types or composite types whose designated types or component types are
   --  subtypes or derived types of the private type in question. After the
   --  full declaration has been seen, the private dependents are updated to
   --  indicate that they have full definitions.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Attribute_Renaming (N : Node_Id);
   --  Analyze renaming of attribute as function.

   function Entity_Matches_Spec (Old_S,  New_S : Entity_Id) return Boolean;
   --  Used to resolve subprogram renaming. Old_S is a possible interpretation
   --  of the entity being renamed, New_S has an explicit signature. If Old_S
   --  is a subprogram, type conformance is required.

   procedure Find_Function_Call (N : Node_Id);
   --  Prefix in index_component is an overloadable entity: the node
   --  is a function call. Reformat it as such. Procedure calls are
   --  statements syntactically, and are handled elsewhere.

   procedure Find_Index_Or_Call (N : Node_Id);
   --  An indexed name can denote one of the following constructs:
   --    a) An indexed component of an array
   --    b) A function call
   --    c) A conversion
   --    d) A slice
   --  The resolution of the construct requires some semantic information
   --  on the prefix and the indices.

   procedure Find_Index_Or_Slice (N : Node_Id);
   --  An indexed component with a single index may designate a slice if
   --  the index is a subtype mark. Resolution of the index is required
   --  for disambiguation.

   procedure Find_Slice (N : Node_Id);
   --  Analyze subtype indication and prefix of a slice, and verify that
   --  prefix is a one-dimensional array.

   procedure Find_Explicit_Dereference (N : Node_Id);
   --  Analyze prefix of an explicit dereference, and verify that it is
   --  an access object.

   function Get_Full_Declaration (T_Name : Entity_Id) return Entity_Id;
   --  If T_Name is an incomplete type and the full declaration has been
   --  seen, or is the name of a private type and the current context is
   --  one in which the full declaration is visible, return the full
   --  declaration.

   procedure Use_One_Package (P : Entity_Id);
   --  Make visible entities declarated in  package P use-visible in
   --  the current context.

   -------------------------------
   -- Dump Semantic Information --
   -------------------------------

   procedure Write_Info;
   --  Write information on entities declared in current scope

   ---------------
   -- New_Scope --
   ---------------

   procedure New_Scope (S : Entity_Id) is
   begin
      Scope_Stack.Increment_Last;
      Scope_Stack.Table (Scope_Stack.Last).Entity               := S;
      Scope_Stack.Table (Scope_Stack.Last).Save_Scope_Suppress  :=
        Scope_Suppress;
      Scope_Stack.Table (Scope_Stack.Last).Save_Entity_Suppress :=
        Entity_Suppress.Last;

   end New_Scope;

   ---------------
   -- Pop_Scope --
   ---------------

   procedure Pop_Scope is
      E : Entity_Id;

   begin
      if Debug_Flag_E then
         Write_Info;
      end if;

      Scope_Suppress :=
        Scope_Stack.Table (Scope_Stack.Last).Save_Scope_Suppress;

      while Entity_Suppress.Last >
                 Scope_Stack.Table (Scope_Stack.Last).Save_Entity_Suppress
      loop
         E := Entity_Suppress.Table (Entity_Suppress.Last).Entity;

         case Entity_Suppress.Table (Entity_Suppress.Last).Check is

            when Access_Check        =>
               Set_Suppress_Access_Checks        (E, False);

            when Accessibility_Check =>
               Set_Suppress_Accessibility_Checks (E, False);

            when Discriminant_Check  =>
               Set_Suppress_Discriminant_Checks  (E, False);

            when Division_Check      =>
               Set_Suppress_Division_Checks      (E, False);

            when Elaboration_Check   =>
               Set_Suppress_Elaboration_Checks   (E, False);

            when Index_Check         =>
               Set_Suppress_Index_Checks         (E, False);

            when Length_Check        =>
               Set_Suppress_Length_Checks        (E, False);

            when Overflow_Check      =>
               Set_Suppress_Overflow_Checks      (E, False);

            when Range_Check         =>
               Set_Suppress_Range_Checks         (E, False);

            when Storage_Check       =>
               Set_Suppress_Storage_Checks       (E, False);

            when Tag_Check           =>
               Set_Suppress_Tag_Checks           (E, False);

            --  All_Checks should not appear here (since it is entered as a
            --  series of its separate checks). Bomb if it is encountered

            when All_Checks =>
               Compiler_Abort;
         end case;

         Entity_Suppress.Decrement_Last;
      end loop;

      Scope_Stack.Decrement_Last;
   end Pop_Scope;

   --------------------
   -- In_Open_Scopes --
   --------------------

   function In_Open_Scopes (S : Entity_Id) return Boolean is
   begin
      --  Since there are several scope stacks maintained by Scope_Stack each
      --  delineated by Standard (see comments by definition of Scope_Stack)
      --  it is necessary to end the search when Standard is reached.
      for I in reverse 0 .. Scope_Stack.Last loop
         if Scope_Stack.Table (I).Entity = S then
            return True;
         end if;
         exit when Scope_Stack.Table (I).Entity = Standard_Standard;
      end loop;

      return False;
   end In_Open_Scopes;

   ---------------
   -- Find_Name --
   ---------------

   procedure Find_Name (N : Node_Id) is
   begin
      case Nkind (N) is
         when N_Identifier
            | N_String_Literal
            | N_Operator_Symbol
            | N_Defining_Identifier  => Find_Simple_Name (N);

         when N_Selected_Component   => Find_Selected_Component (N);

         when N_Expanded_Name        => Find_Selected_Component (N);

         when N_Indexed_Component    => Find_Index_Or_Call (N);

         when N_Slice                => Find_Slice (N);

         when N_Attribute_Reference  => Analyze_Attribute (N);

         when N_Explicit_Dereference => Find_Explicit_Dereference (N);

         when N_Type_Conversion      => Analyze_Conversion (N);

         when N_Function_Call        => null;

         --  Find_Name should only be called on appropriate nodes

         when others                 => Compiler_Abort;
      end case;
   end Find_Name;

   ----------------------
   -- Find_Simple_Name --
   ----------------------

   procedure Find_Simple_Name (N : Node_Id) is
   begin
      --  If the entity pointer is already set, this is an internal node, or
      --  a node that is analyzed more than once, after a tree modification.
      --  In such a case there is no resolution to perform, just set the type.

      if Present (Entity (N)) then
         if Is_Type (Entity (N)) then
            Set_Etype (N, Entity (N));
         else
            Set_Etype (N, Etype (Entity (N)));
         end if;

         return;
      end if;

      --  Here if Entity pointer was not set, we need full visibility analysis

      declare
         E     : Entity_Id := Current_Entity (N);
         Outer : Entity_Id;
         Found : Boolean := False;

         function Inner (E1, E2 : Entity_Id) return Boolean is
         begin
            for I in reverse 0 .. Scope_Stack.Last loop
               if Scope_Stack.Table (I).Entity = Scope (E2) then
                  return False;
               elsif Scope_Stack.Table (I).Entity = Scope (E1) then
                  return True;
               end if;
               exit when Scope_Stack.Table (I).Entity = Standard_Standard;
            end loop;
         end Inner;

         function Find_Innermost (E : Entity_Id) return Entity_Id is
            E1     : Entity_Id := Homonym (E);
            Result : Entity_Id := E;
         begin
            while Present (E1) loop
               if Is_Directly_Visible (E1) and Inner (E1, E) then
                  Result := E1;
               end if;
               E1 := Homonym (E1);
            end loop;

            return Result;
         end Find_Innermost;

      begin
         --  Set the defaults in case of error

         Set_Entity (N, Any_Id);
         Set_Etype (N, Any_Type);

         if Debug_Flag_E then
            Write_Str ("Looking for "); Write_Name (Chars (N)); Write_Eol;
         end if;

         while Present (E) loop
            if Is_Directly_Visible (E) then
               E := Find_Innermost (E);
               Found := True; -- found current entity with given name
               exit;

            elsif Is_Use_Visible (E) then
               Found := True;

               --  Verify that it is not hidden by other use-visible entities

               Outer := Homonym (E);

               while Present (Outer) loop
                  if Is_Use_Visible (Outer)
                    and then (Is_Overloadable (Outer) /= Is_Overloadable (E)
                                or else not Is_Overloadable (E))
                  then
                     if Debug_Flag_E then
                        Write_Str ("ambiguity between the following");
                        Write_Eol;
                        Write_Entity_Info (E, " ");
                        Write_Entity_Info (Outer, " ");
                     end if;

                     Found := False;

                     E := Homonym (Outer);
                     --  these use-visible entities hide each other,  but
                     --  another use- or directly-visible entity may be
                     --  present on the chain.
                     exit;

                  elsif Is_Directly_Visible (Outer) then
                     E := Find_Innermost (Outer);
                     Found := True;
                     exit;
                  end if;

                  Outer := Homonym (Outer);
               end loop;

               exit when Found;

            else
               E := Homonym (E);
            end if;
         end loop;

         if Found then
            Set_Entity (N, E);

            if Is_Type (E) then
               Set_Etype (N, E);
            else
               Set_Etype (N, Get_Full_Declaration (Etype (E)));
            end if;

            if Debug_Flag_E then
               Write_Str (" found  ");
               Write_Entity_Info (E, "      ");
            end if;

            if Ekind (E) = E_Void then
               Error_Msg_N ("premature usage of&!", N);

            --  If the entity is overloadable, collect all interpretations
            --  of the name for subsequent overload resolution.

            elsif Is_Overloadable (E) and then Present (Homonym (E)) then
               Collect_Interps (N);

            else
               Set_Entity_With_Style_Check (N, E);
            end if;

         elsif No (Current_Entity (N)) then
            Error_Msg_N ("& is undefined!", N);

            --  Make entry with this name in standard (avoid cascaded errors)

            declare
               Id : Entity_Id :=
                 Extend_Node (New_Node (N_Defining_Identifier, Sloc (N)));

            begin
               Set_Chars (Id, Chars (N));
               Set_Ekind (Id, E_Variable);
               Set_Etype (Id, Any_Type);
               Set_Is_Directly_Visible (Id);
               Set_Current_Entity (Id);
               Append_Entity (Id, Standard_Standard);

               Set_Entity (N, Id);
               Set_Etype (N, Any_Type);
            end;

         else
            if Is_Use_Visible (Current_Entity (N)) then
               Error_Msg_N
                 ("& is ambiguous (several use-visible meanings)!", N);
            else
               Error_Msg_N
                 ("& is not directly visible (needs USE clause)!", N);
            end if;
         end if;
      end;
   end Find_Simple_Name;

   -----------------------------
   -- Find_Selected_Component --
   -----------------------------

   procedure Find_Selected_Component (N : Node_Id) is
      P : Node_Id := Prefix (N);

      P_Name : Entity_Id;
      --  Entity denoted by prefix

      P_Type : Entity_Id;
      --  and its type

   begin
      Find_Name (P);

      if Nkind (P) = N_Error then
         return;

      elsif Is_Name (P) then
         P_Name := Entity (P);
         P_Type := Etype (P);

         if Debug_Flag_E then
            Write_Str ("Found prefix type to be ");
            Write_Entity_Info (P_Type, "      "); Write_Eol;
         end if;

         if Is_Appropriate_For_Record (P_Type) then

            --  Selected component of record. Type checking will validate
            --  name of selector.

            Analyze_Selected_Component (N);

         elsif Is_Appropriate_For_Entry_Prefix (P_Type)
           and then not In_Open_Scopes (P_Name)
         then
            --  Call to protected operation or entry. Type checking is
            --  needed on the prefix.
            Analyze_Selected_Component (N);

         elsif In_Open_Scopes (P_Name)
           and then (Ekind (P_Name) /= E_Void
           and then not Is_Overloadable (P_Name))
         then
            --  Prefix denotes an enclosing loop, block, or task, i.e. an
            --  enclosing construct that is not a subprogram or accept.

            Find_Expanded_Name (N);

         elsif Ekind (P_Name) = E_Package then
            Find_Expanded_Name (N);

         elsif Is_Overloadable (P_Name) then

            --  Prefix  can be an enclosing subprogram or accept statement.
            --  it can also be a call to a parameterless function that yields
            --  a record value.

            if In_Open_Scopes (P_Name) then
               if In_Open_Scopes (Homonym (P_Name)) then
                  Error_Msg_N ("ambiguous expanded name&", P);
                  Set_Entity   (N, Any_Id);
                  Set_Etype         (N, Any_Type);
               else
                  Find_Expanded_Name (N);
               end if;
            else
               --  If no interpretation as an expanded name is possible, it
               --  must be a selected component of a record returned by a
               --  function call. Reformat tree as a function call, the rest
               --  is done by type resolution.

               Find_Expanded_Name (N);
            end if;

         --  Remaining cases generate various error messages

         else
            --  Format node as expanded name, to avoid cascaded errors

            Change_Node       (N, N_Expanded_Name);
            Set_Prefix        (N, P);

            --  Set_Selector_Name (N, Empty); ????

            Set_Entity   (N, Any_Id);
            Set_Etype         (N, Any_Type);

            if P_Name = Any_Id  then
               null; -- object was undeclared (error message emitted already)

            elsif Ekind (P_Name) = E_Void then
               Error_Msg_N ("premature usage of&", P);

            else
               Error_Msg_N (
                "invalid prefix in selected component&", P);
            end if;
         end if;  -- prefix is simple or expanded name.

      else
         --  If prefix is not the name of an entity, it must be an expression,
         --  whose type is appropriate for a record. This is determined by
         --  type resolution.

         Analyze_Selected_Component (N);
      end if;
   end Find_Selected_Component;

   -------------------------------
   -- Is_Appropriate_For_Record --
   -------------------------------

   function Is_Appropriate_For_Record (T : Entity_Id) return Boolean is
   begin
      return
        Present (T)
          and then (Is_Record_Type (T)
            or else (Is_Access_Type (T)
              and then Is_Record_Type (Designated_Type (T))));
   end Is_Appropriate_For_Record;

   -------------------------------------
   -- Is_Appropriate_For_Entry_Prefix --
   -------------------------------------

   function Is_Appropriate_For_Entry_Prefix (T : Entity_Id) return Boolean is
      P_Type : Entity_Id := T;
   begin
      if Is_Access_Type (P_Type) then
         P_Type := Designated_Type (P_Type);
      end if;
      return Is_Task_Type (P_Type) or else Is_Protected_Type (P_Type);
   end Is_Appropriate_For_Entry_Prefix;

   ------------------------
   -- Find_Expanded_Name --
   ------------------------

   procedure Find_Expanded_Name (N : Node_Id) is
      P_Name   : Entity_Id := Entity (Prefix (N));
      Selector : Node_Id   := Selector_Name (N);
      Id       : Entity_Id;

   begin
      --  We search the homonym chain of the entity until we find an entity
      --  declared in the scope denoted by the prefix. If the entity is
      --  private, it may nevertheless be directly visible, if we are in the
      --  scope of its declaration.
      --  If the prefix is a renamed package, look for the entity in the
      --  original package.
      if Ekind (P_Name) = E_Package
        and then Present (Renamed_Object (P_Name))
      then
         P_Name := Renamed_Object (P_Name);
      end if;

      Id := Current_Entity (Selector);

      while Present (Id) loop
         exit when  Scope (Id) = P_Name
           and then (not Is_Private (Id) or else Is_Directly_Visible (Id));
         Id := Homonym (Id);
      end loop;

      if No (Id) or else Chars (Id) /=  Chars (Selector) then
         if (Nkind (Selector) = N_Operator_Symbol
           and then Has_Implicit_Operator (N))
         then
            --  There is an implicit instance of the predefined operator
            --  in the given scope. Find the predefined operator in scope
            --  Standard.
            Id := Current_Entity (Selector);
            while Present (Id) and then Scope (Id) /= Standard_Standard loop
               Id := Homonym (Id);
            end loop;

         else
            Error_Msg_Node_2 := P_Name;
            Error_Msg_NE ("& not declared in&", N, Selector);
            Id := Any_Id;
         end if;
      end if;

      Change_Selected_Component_To_Expanded_Name (N);
      Set_Entity_With_Style_Check (N, Id);
      if Is_Type (Id) then
         Set_Etype (N, Id);
      else
         Set_Etype (N, Get_Full_Declaration (Etype (Id)));
      end if;

      if Is_Overloadable (Id) and then Present (Homonym (Id))
        and then Scope (Id) = Scope (Homonym (Id))
      then
         Collect_Interps (N);
      end if;

      if (Nkind (Selector_Name (N)) = N_Operator_Symbol
        and then Has_Implicit_Operator (N)
        and then Scope (Id) /= Standard_Standard)
      then

         --  In addition to user-defined operators in the given scope,
         --  there is also an implicit instance of the predefined
         --  operator. Find the predefined operator in scope
         --  Standard, and add it as well to the interpretations.
         --  Procedure Add_One_Interp will determine which hides which.

         Id := Current_Entity (Selector);
         while Present (Id) and then Scope (Id) /= Standard_Standard loop
            Id := Homonym (Id);
         end loop;
         Add_One_Interp (N, Id,  Etype (Id));
      end if;

      --  If the expanded name represents a named number replace it with
      --  an the appropriate literal node.

      if Is_Named_Number (Id) then
         Rewrite_Named_Number (N, Etype (N));
      end if;
   end Find_Expanded_Name;

   ------------------------
   -- Find_Index_Or_Call --
   ------------------------

   procedure Find_Index_Or_Call (N : Node_Id) is
      P   : Node_Id := Prefix (N);
      E   : Node_Id;
      U_N : Entity_Id;

   begin
      Find_Name (P); -- name of array, function, or type.

      if List_Present (Expressions (N)) then
         E := First (Expressions (N));

         while Present (E) loop
            Analyze_Expression (E); -- array indices, or actuals in  call
            E := Next (E);
         end loop;
      end if;

      if Is_Name (P) then
         U_N := Entity (P);

         if Ekind (U_N) in  Type_Kind then

            --  Reformat node as a type conversion.

            E := Remove_Head ( Expressions (N));

            if Present (First (Expressions (N))) then
               Error_Msg_N
                ("argument of type conversion must be single expression", N);
            end if;

            Change_Node (N, N_Type_Conversion);
            Set_Subtype_Mark (N, P);
            Set_Etype (N, U_N);
            Set_Expression (N, E);
            Analyze_Conversion (N);

         elsif Is_Overloadable (U_N) then
            Find_Function_Call (N);

         elsif Ekind (Etype (P)) = E_Subprogram_Type
           or else (Ekind (Etype (P)) = E_Access_Subprogram_Type
                      and then
                    Ekind (Designated_Type (Etype (P))) = E_Subprogram_Type)
         then
            --  Call to access_to-subprogram with possible implicit dereference

            Find_Function_Call (N);

         else
            Find_Index_Or_Slice (N);
         end if;

      --  If not an entity name, prefix is an expression that may denote
      --  an array or an access-to-subprogram.

      else
         if (Ekind (Etype (P)) = E_Subprogram_Type)
           or else (Ekind (Etype (P)) = E_Access_Subprogram_Type
                     and then
                    Ekind (Designated_Type (Etype (P))) = E_Subprogram_Type)
         then
            Find_Function_Call (N);

         else
            Find_Index_Or_Slice (N);
         end if;
      end if;
   end Find_Index_Or_Call;

   ------------------------
   -- Find_Function_Call --
   ------------------------

   procedure Find_Function_Call (N : Node_Id) is
      P : constant Node_Id := Prefix (N);
      E : constant List_Id := Expressions (N);
      S : constant Source_Ptr := Sloc (N);

   begin
      if Is_Name (P)
        and then Is_Overloadable (Entity (P))
        and then Needs_No_Actuals (Entity (P))
      then
         --  An amusing syntactic ambiguity appears if parameterless
         --  functions that return an array type appear in a call. In
         --  this case the expression must be parsed eventually as an
         --  indexing on the result of the function call. If both
         --  parameterless and parametered functions are present, then the
         --  tree itself is ambiguous, and both parsings must be carried, to
         --  be resolved by overload resolution. ??? TBSL
         null;
      end if;

      Change_Node (N, N_Function_Call);
      Set_Name (N, P);
      Set_Parameter_Associations (N, E);
      Set_Sloc (N, S);
      Analyze_Call (N);
   end Find_Function_Call;

   -------------------------
   -- Find_Index_Or_Slice --
   -------------------------

   procedure Find_Index_Or_Slice (N : Node_Id) is
      P : constant Node_Id := Prefix (N);
      E : constant Node_Id := First (Expressions (N));

   begin
      --  If more than one index is present, this is an indexed component
      --  of a multidimensional array.

      if Present (Next (E)) then
         Analyze_Indexed_Component (N);

      elsif Is_Name (E) and then Ekind (Entity (E)) in Type_Kind then

         --  The node denotes a slice if E is a subtype mark or a discrete
         --  range. The discrete range  already appears syntactically as a
         --  N_Slice node, so we only need to consider the case of a
         --  subtype mark here.

         Change_Node (N, N_Slice);
         Set_Prefix (N, P);
         Set_Discrete_Range (N, E);

      else
         --  Single expression indexes a one-dimensional array.

         Analyze_Indexed_Component (N);
      end if;
   end Find_Index_Or_Slice;

   ----------------
   -- Find_Slice --
   ----------------

   procedure Find_Slice (N : Node_Id) is
      P : Node_Id := Prefix (N);
      D : Node_Id := Discrete_Range (N);
      Array_Type : Entity_Id;

   begin
      if No (Etype (P)) then

         --  The caller may have analyzed the prefix already

         Analyze (P);
      end if;

      Analyze (D);
      Array_Type := Etype (P);
      Set_Etype (N, Any_Type);

      if Is_Access_Type (Array_Type) then
         Array_Type := Designated_Type (Array_Type);
      end if;

      if not Is_Array_Type (Array_Type) then
         Error_Msg_N ("prefix of slice must be appropriate for array", P);

      elsif Number_Dimensions (Array_Type) /= 1 then
         Error_Msg_N
           ("type is not one-dimensional array in slice prefix", N);

      elsif not Has_Compatible_Type (D, Etype (First_Index (Array_Type))) then
         Error_Msg_N ("invalid index type in slice ", N);

      else
         Set_Etype (N, Array_Type);
      end if;
   end Find_Slice;

   -------------------------------
   -- Find_Explicit_Dereference --
   -------------------------------

   procedure Find_Explicit_Dereference (N : Node_Id) is
      P : Node_Id := Prefix (N);

   begin
      Find_Name (P);

      if Is_Access_Type (Etype (P)) then
         if Ekind (Designated_Type (Etype (P))) = E_Subprogram_Type
            and then Nkind (Parent (N)) /= N_Indexed_Component
         then
            --  Name is a function call with no actuals.
            Change_Node (N,  N_Function_Call);
            Set_Name (N,  P);
            Set_Parameter_Associations (N, New_List);
            Analyze_Call (N);
         else
            Set_Etype (N, Designated_Type (Etype (P)));
         end if;
      else
         Error_Msg_N ("attempt to dereference non-access type&", N);
         Set_Etype (N, Any_Type);
      end if;
   end Find_Explicit_Dereference;

   ---------------
   -- Find_Type --
   ---------------

   procedure Find_Type (N : Node_Id) is
      C      : Entity_Id;
      T      : Entity_Id;
      T_Name : Entity_Id;

   begin
      if Nkind (N) = N_Attribute_Reference then
         if Chars (Identifier (N)) = Name_Class then
            Find_Type (Prefix (N));
            T := Base_Type (Entity (Prefix (N)));

            if not Is_Tagged_Type (T) then
               if Ekind (T) = E_Incomplete_Type then
                  --  It is legal to denote the class type of an incomplete
                  --  type. The full type will have to be tagged, of course.
                  Set_Is_Tagged_Type (T);
                  Make_Class_Type (T);
                  Set_Entity (N, Last_Entity (Current_Scope));
                  Set_Etype  (N, Last_Entity (Current_Scope));
               else
                  Error_Msg_N
                    ("type& is not a tagged type", Prefix (N));
                  Set_Entity (N, Any_Type);
               end if;

            else
               C := Classwide_Type (T);
               Set_Entity_With_Style_Check (N, C);
               Set_Etype (N, C);
            end if;

         else
            Error_Msg_N ("invalid attribute in subtype mark", N);
         end if;

      else
         Find_Name (N);
         T_Name := Entity (N);

         if T_Name  = Any_Id or else Etype (N) = Any_Type then

            --  Undefined id. Make it into a valid type

            Set_Entity (N, Any_Type);

         elsif not Is_Type (T_Name)
           and then T_Name /= Standard_Void_Type
         then
            Error_Msg_N ("subtype mark required in this context", N);
            Set_Entity (N, Any_Type);

         else
            Set_Entity (N, Get_Full_Declaration (T_Name));
            if In_Open_Scopes (T_Name) then
               if Ekind (Base_Type (T_Name)) = E_Task_Type then
                  Error_Msg_N ("task type cannot be used as type mark " &
                     "within its own body", N);
               else
                  Error_Msg_N ("Type declaration cannot refer to itself", N);
               end if;
            end if;
         end if;
      end if;
   end Find_Type;

   --------------------------
   -- Get_Full_Declaration --
   --------------------------

   function Get_Full_Declaration (T_Name : Entity_Id) return Entity_Id is
   begin
      if (Ekind (T_Name) = E_Incomplete_Type
          and then Present (Full_Declaration (T_Name)))
      then
         return Full_Declaration (T_Name);

      elsif Is_Class_Type (T_Name) 
        and then Ekind (Etype (T_Name)) = E_Incomplete_Type 
        and then Present (Full_Declaration (Etype (T_Name))) 
      then
         return Classwide_Type (Full_Declaration (Etype (T_Name)));

      else
         return T_Name;
      end if;
   end Get_Full_Declaration;

   ---------------------------
   -- Has_Implicit_Operator --
   ---------------------------

   function Has_Implicit_Operator (N : Node_Id) return Boolean is
      Op_Id : constant Name_Id    := Chars (Selector_Name (N));
      P     : constant Entity_Id  := Entity (Prefix (N));
      Id    : Entity_Id;

   begin

      Id := First_Entity (P);

      --  Boolean operators: implicit declaration exists if scope contains
      --  declaration for derived boolean type, or for array of boolean type.

      if Op_Id = Name_Op_And
        or else Op_Id = Name_Op_Not
        or else Op_Id = Name_Op_Or
        or else Op_Id = Name_Op_Xor
      then
         while Present (Id) loop
            if Valid_Boolean_Arg (Id) then
               return true;
            end if;

            Id := Next_Entity (Id);
         end loop;

      --  Equality: look for any non-limited type.

      elsif Op_Id = Name_Op_Eq
        or else Op_Id = Name_Op_Ne
      then
         while Present (Id) loop
            if Is_Type (Id) and not Is_Limited_Type (Id) then
               return true;
            end if;

            Id := Next_Entity (Id);
         end loop;

      --  Comparison operators: scalar type, or array of scalar.

      elsif Op_Id = Name_Op_Lt
        or else Op_Id = Name_Op_Le
        or else Op_Id = Name_Op_Gt
        or else Op_Id = Name_Op_Ge
      then
         while Present (Id) loop
            if Is_Scalar_Type (Id)
              or else (Is_Array_Type (Id)
                        and then Is_Scalar_Type (Component_Type (Id)))
            then
               return true;
            end if;

            Id := Next_Entity (Id);
         end loop;

      --  Arithmetic operators: any numeric type

      elsif Op_Id = Name_Op_Abs
        or else Op_Id = Name_Op_Add
        or else Op_Id = Name_Op_Mod
        or else Op_Id = Name_Op_Rem
        or else Op_Id = Name_Op_Subtract
        or else Op_Id = Name_Op_Multiply
        or else Op_Id = Name_Op_Divide
        or else Op_Id = Name_Op_Expon
      then
         while Present (Id) loop
            if Is_Numeric_Type (Id) then
               return True;
            end if;

            Id := Next_Entity (Id);
         end loop;

      --  Concatenation: any one-dimensional array type

      elsif Op_Id = Name_Op_Concat then
         while Present (Id) loop
            if Is_Array_Type (Id) and then Number_Dimensions (Id) = 1 then
               return true;
            end if;

            Id := Next_Entity (Id);
         end loop;
      else
         return False;
      end if;
   end Has_Implicit_Operator;

   ---------------
   -- End_Scope --
   ---------------

   procedure End_Scope is
      Id    : Entity_Id;
      Prev  : Entity_Id;
      Outer : Entity_Id;

   begin
      Id := First_Entity (Current_Scope);

      while Present (Id) loop
         --  An entity in the current scope is not necessarily the first one
         --  on its homonym chain. Find its predecessor if any,
         --  If it is an internal entity, it will not be in the visibility
         --  chain altogether,  and there is nothing to unchain.

         if Id /= Current_Entity (Id) then
            Prev := Current_Entity (Id);
            while Present (Prev)
              and then Present (Homonym (Prev))
              and then Homonym (Prev) /= Id
            loop
               Prev := Homonym (Prev);
            end loop;

            if No (Prev) or else Homonym (Prev) /= Id then
               --  Id is not in the visibility chain.
               goto Next_Ent;
            end if;

         else
            Prev := Empty;
         end if;

         Outer := Homonym (Id);

         while Present (Outer) and then Scope (Outer) = Current_Scope loop
            Outer := Homonym (Outer);
         end loop;

         if No (Prev) then
            Set_Name_Entity_Id (Chars (Id), Outer);
         else
            Set_Homonym (Prev,  Outer);
         end if;

         <<Next_Ent>> Id  := Next_Entity (Id);
      end loop;

      Pop_Scope;
   end End_Scope;

   -------------------------
   -- Analyze_Use_Package --
   -------------------------

   --  Resolve the package names in the use clause, and make all the visible
   --  entities defined in the package use-visible. If the package is
   --  already in use because of a previous use clause, its visible entities
   --  are already use-visible. In that case, mark the occurrence as a
   --  redundant use. If the package is an open scope, i.e. if the use
   --  clause occurs within the package itself, ignore it.

   procedure Analyze_Use_Package (N : Node_Id) is
      Pack_Name : Node_Id;
      Pack      : Entity_Id;

      --  Internal function used to scan the context clause to find the
      --  previous occurence of package name in with-clause, or report error.

      function Find_Package_Unit return Entity_Id is
         Item : Node_Id := First (Context_Items (Parent (N)));

      begin
         while Present (Item) and then Item /= N loop
            if Nkind (Item) = N_With_Clause
              and then Same_Name (Name (Item), Pack_Name)
            then
               return Entity (Name (Item));
            end if;

            Item := Next (Item);
         end loop;

         Error_Msg_N ("unknown package in USE clause", N);
         return Standard_Standard;
      end Find_Package_Unit;

   --  Start processing for Use_Package_Clause

   begin
      --  Loop through package names to identify referenced packages

      Pack_Name := First (Names (N));
      while Present (Pack_Name) loop

         --  If this use clause appears in the context clause of a compilation
         --  unit, then we have to find the corresponding with clause to
         --  identify the referenced package.

         if Nkind (Parent (N)) = N_Compilation_Unit then
            Pack := Find_Package_Unit;

            if Nkind (Pack_Name) = N_Selected_Component then
               Change_Selected_Component_To_Expanded_Name (Pack_Name);
            end if;

            Set_Entity (Pack_Name, Pack);

         --  If the use clause is appearing in a normal declaration context,
         --  then it references a package name that is visible by normal rules.

         else
            Find_Name (Pack_Name);
         end if;

         Pack_Name := Next (Pack_Name);
      end loop;

      --  Loop through package names to mark all entities as use visible

      Pack_Name := First (Names (N));
      while Present (Pack_Name) loop
         Pack := Entity (Pack_Name);

         if Ekind (Pack) /= E_Package
           and then Etype (Pack) /= Any_Type
         then
            Error_Msg_N ("& is not a usable package", Pack_Name);

         else

            if In_Open_Scopes (Pack) then
               null;

            elsif not In_Use (Pack)
              or else Nkind (Parent (N)) = N_Compilation_Unit
            then
               Use_One_Package (Pack);

            else
               Set_Redundant_Use (Pack_Name, True);
            end if;
         end if;

         Pack_Name := Next (Pack_Name);
      end loop;

   end Analyze_Use_Package;

   ---------------------
   -- Use_One_Package --
   ---------------------

   procedure Use_One_Package (P : Entity_Id) is
      Id : Entity_Id;
      Prev : Entity_Id;
   begin
      Set_In_Use (P);
      --  If unit is a package renaming,  indicate that the renamed
      --  package is also in use (the flags on both entities must
      --  remain consistent).
      if Present (Renamed_Object (P)) then
         Set_In_Use (Renamed_Object (P));
      end if;


      --  Loop through entities in one package making them use visible

      Id := First_Entity (P);
      while Present (Id)
        and then Id /= First_Private_Entity (P)
      loop
         Prev := Current_Entity (Id);

         while Present (Prev) loop
            if Is_Directly_Visible (Prev)
              and then (not Is_Overloadable (Prev)
              or else not Is_Overloadable (Id)
              or else (Type_Conformant (Id, Prev)))
            then
               --  use-visible entity remains hidden
               goto Next_Usable_Entity;
            end if;
            Prev := Homonym (Prev);
         end loop;
         --  On exit,  we know entity is not hidden.
         Set_Is_Use_Visible (Id);

         <<Next_Usable_Entity>> Id := Next_Entity (Id);
      end loop;
   end Use_One_Package;

   ----------------------
   -- Analyze_Use_Type --
   ----------------------

   procedure Analyze_Use_Type (N : Node_Id) is
      Id : Entity_Id;

   begin
      Id := First (Subtype_Marks (N));
      while Present (Id) loop
         Find_Type (Id);

         if Entity (Id) /= Any_Type then
            Set_Is_Use_Visible (Entity (Id));

            --  If the base type is anonymous this indicates that the entity
            --  is really a first named subtype and we need to make the
            --  base type of the entity "use type" visible as well. Otherwise,
            --  operators which are defined on the type,  and which the user
            --  wanted to make directly visible by means of this use clause,
            --  would in fact not become visible.

            if Is_Internal (Base_Type (Entity (Id))) then
               Set_Is_Use_Visible (Base_Type (Entity (Id)));
            end if;
         end if;

         Id := Next (Id);
      end loop;
   end Analyze_Use_Type;

   -------------
   -- End_Use --
   -------------

   procedure End_Use (L : List_Id) is
      Decl    : Node_Id;
      Pack_Name : Node_Id;
      Pack    : Entity_Id;
      Id      : Entity_Id;

   begin
      if List_Present (L) then
         Decl := First (L);

         while Present (Decl) loop
            if Nkind (Decl) = N_Use_Package_Clause then
               Pack_Name := First (Names (Decl));

               while Present (Pack_Name) loop
                  Pack := Entity (Pack_Name);

                  if Ekind (Pack) = E_Package then
                     if not Redundant_Use (Pack_Name) then
                        Set_In_Use (Pack, False);
                        Id := First_Entity (Pack);

                        while Present (Id) loop
                           Set_Is_Use_Visible (Id, False);
                           Id := Next_Entity (Id);
                        end loop;
                     end if;

                     if Present (Renamed_Object (Pack)) then
                        Set_In_Use (Renamed_Object (Pack), False);
                     end if;
                     --  TBSL : this may be a redundant use of the renamed
                     --  package!
                  end if;

                  Pack_Name := Next (Pack_Name);
               end loop;

            elsif Nkind (Decl) = N_Use_Type_Clause  then
               Id := First (Names (Decl));
               while Present (Id) loop
                  Find_Type (Id);
                  if Entity (Id) /= Any_Type then
                     Set_Is_Use_Visible (Entity (Id),  False);
                  end if;

                  Id := Next (Id);
               end loop;
            end if;

            Decl := Next (Decl);
         end loop;
      end if;

   end End_Use;

   -------------
   -- Set_Use --
   -------------

   procedure Set_Use (L : List_Id) is
      Decl    : Node_Id;
      Pack_Name : Node_Id;
      Pack    : Entity_Id;
      Id      : Entity_Id;

   begin
      if List_Present (L) then
         Decl := First (L);

         while Present (Decl) loop
            if Nkind (Decl) = N_Use_Package_Clause then
               Pack_Name := First (Names (Decl));

               while Present (Pack_Name) loop
                  Pack := Entity (Pack_Name);

                  if Ekind (Pack) = E_Package then
                     if not In_Use (Pack) then
                        Use_One_Package (Pack);

                     else
                        Set_Redundant_Use (Pack_Name, True);
                     end if;
                  end if;

                  Pack_Name := Next (Pack_Name);
               end loop;

            elsif Nkind (Decl) = N_Use_Type_Clause  then
               Id := First (Names (Decl));
               while Present (Id) loop
                  if Entity (Id) /= Any_Type then
                     Set_Is_Use_Visible (Entity (Id),  True);
                  end if;

                  Id := Next (Id);
               end loop;
            end if;

            Decl := Next (Decl);
         end loop;
      end if;

   end Set_Use;

   -----------------------------
   -- Analyze_Object_Renaming --
   -----------------------------

   procedure Analyze_Object_Renaming (N : Node_Id) is
      Id  : constant Entity_Id := Defining_Identifier (N);
      Nam : constant Node_Id   := Name (N);
      S   : constant Entity_Id := Subtype_Mark (N);
      T   : Entity_Id;
      T2  : Entity_Id;

   begin
      Enter_Name (Id);
      Find_Name (Nam);
      T2 := Etype (Nam);
      Find_Type (S);
      T := Entity (S);
      Resolve_Complete_Context (Nam, T);
      Set_Ekind (Id, E_Variable);

      if T = Any_Type or else Etype (Nam) = Any_Type then
         return;

      elsif not Is_Constrained (T) then

         --  The constraints are inherited from the renamed object, they
         --  are not affected by the given subtype mark.

         Set_Etype (Id, T2);

      else
         Set_Etype (Id, T);
      end if;

      if not Is_Variable (Nam) then
         Set_Ekind (Id, E_Constant);
      end if;

      Set_Renamed_Object (Id, Nam);
   end Analyze_Object_Renaming;

   --------------------------------
   -- Analyze_Exception_Renaming --
   --------------------------------

   --  The language only allows a single identifier, but the tree holds
   --  an identifier list. The parser has already issued an error message
   --  if there is more than one element in the list.

   procedure Analyze_Exception_Renaming (N : Node_Id) is
      Id  : constant Node_Id   := Defining_Identifier (N);
      Nam : constant Node_Id   := Name (N);

   begin
      Enter_Name (Id);
      Set_Ekind (Id,  E_Exception);
      Set_Etype (Id, Standard_Exception_Type);
      Find_Name (Nam);

      if Ekind (Entity (Nam)) /= E_Exception then
         Error_Msg_N ("invalid exception name in renaming", Nam);
      else
         Set_Renamed_Object (Id, Entity (Nam));
      end if;
   end Analyze_Exception_Renaming;

   ------------------------------
   -- Analyze_Package_renaming --
   ------------------------------

   procedure Analyze_Package_Renaming (N : Node_Id) is
      New_P : Entity_Id := Defining_Unit_Simple_Name (N);
      Old_P : Entity_Id;
   begin
      Find_Name (Name (N));
      Old_P := Entity (Name (N));

      if Etype (Old_P) = Any_Type then
         null;

      elsif Ekind (Old_P) /= E_Package then
         Error_Msg_N ("invalid package name", Name (N));

      else
         --  Entities in the old package are accessible through the
         --  renaming entity. The simplest implementation is to have
         --  both packages share the entity list.
         Enter_Name (New_P);
         Set_Ekind (New_P, E_Package);
         Set_Etype (New_P, Standard_Void_Type);
         Set_Renamed_Object (New_P,  Old_P);
         Set_Has_Completion (New_P);

         Set_First_Entity (New_P,  First_Entity (Old_P));
         Set_Last_Entity  (New_P,  Last_Entity  (Old_P));
         Set_First_Private_Entity (New_P,
             First_Private_Entity (Old_P));
      end if;
   end Analyze_Package_Renaming;

   ---------------------------------
   -- Analyze_Subprogram_Renaming --
   ---------------------------------

   procedure Analyze_Subprogram_Renaming (N : Node_Id) is
      Nam   : Node_Id := Name (N);
      Spec  : Node_Id := Specification (N);
      New_S : Entity_Id := Defining_Unit_Simple_Name (Spec);
      Old_S : Entity_Id;
      Prev  : Entity_Id;
      I     : Interp_Index;
      It    : Interp;

   begin
      --  The renaming defines a new overloaded entity, which is analyzed
      --  like a subprogram declaration.

      New_S := Analyze_Spec (Spec);
      Prev := Find_Corresponding_Spec (N);
      if Present (Prev) then
         --  Renaming_As_Body. Renaming declaration is the completion of
         --  the declaration of Prev.
         if not Type_Conformant (Prev, New_S) then
            Error_Msg_N
                  ("renamed entity not type conformant with declaration", N);
         elsif Ada_83 then
            Error_Msg_N ("in Ada83 a renaming cannot serve as a body", N);
         end if;
         New_S := Prev;
      else
         New_Overloaded_Entity (New_S);
         Set_Has_Completion (New_S);
      end if;

      Find_Name (Nam);

      if Etype (Nam) = Any_Type then
         null;

      elsif Nkind (Nam) = N_Attribute_Reference then
         Attribute_Renaming (N);

      elsif not Is_Overloadable (Entity (Nam)) then
         Error_Msg_N ("expect valid subprogram name in renaming", N);

      else

         --  Now we must resolve the subprogram renaming

         Old_S := Any_Id;

         if not Is_Overloaded (Name (N)) then
            if Entity_Matches_Spec (Entity (Name (N)), New_S) then
               Old_S := Entity (Name (N));
            end if;

         else
            Get_First_Interp (Name (N),  I,  It);

            while Present (It.Nam) loop

               if Entity_Matches_Spec (It.Nam, New_S) then
                  if Old_S /= Any_Id then
                     Old_S := Disambiguate (It.Nam, Old_S, Etype (Old_S));
                     if Old_S = Any_Id then
                        Error_Msg_N ("ambiguous renaming", N);
                        exit;
                     end if;
                  else
                     Old_S := It.Nam;
                  end if;
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end if;

         if Old_S /= Any_Id then

            --  The subprogram being renamed must be mode conformant
            --  with the given specification.

            if Ekind (Old_S) /= E_Operator then
               if not Mode_Conformant (New_S,  Old_S) then
                  Error_Msg_N
                  ("renamed entity not mode conformant with declaration", N);
               end if;
            end if;

            if Present (Prev) then

               --  Rewrite renaming declaration as a subprogram body, whose
               --  single statement is a call to Old_S.

               declare
                  Actuals   : List_Id := New_List;
                  Call_Node : Node_Id;
                  Body_Node : Node_Id;
                  Formal    : Entity_Id;
                  Loc : Source_Ptr := Sloc (N);
               begin
                  --  Reset completion flag, so this body is seen as the
                  --  proper completion.
                  Set_Has_Completion (New_S, False);
                  Formal := First_Formal (New_S);
                  while Present (Formal) loop
                     Append (New_Reference_To (Formal, Loc), Actuals);
                     Formal := Next_Formal (Formal);
                  end loop;

                  if Ekind (Old_S) = E_Function then
                     Call_Node := Make_Return_Statement (Loc,
                        Expression =>
                          Make_Function_Call (Loc,
                            Name => New_Reference_To (Old_S,  Loc),
                            Parameter_Associations => Actuals));
                  else
                     Call_Node := Make_Procedure_Call_Statement (Loc,
                       Name => New_Reference_To (Old_S,  Loc),
                       Parameter_Associations => Actuals);
                  end if;

                  Body_Node := Make_Subprogram_Body (Loc,
                    Specification => Spec,
                    Declarations => New_List,
                    Handled_Statement_Sequence =>
                      Make_Handled_Sequence_Of_Statements (Loc,
                        Statements => New_List_1 (Call_Node)));

                  Analyze (Body_Node);
                  Rewrite_Substitute_Tree (N,  Body_Node);
               end;
            else
               Set_Alias (New_S, Old_S);
            end if;
         else
            Error_Msg_N
              ("No visible subprogram matches this specification", Spec);
         end if;
      end if;
   end Analyze_Subprogram_Renaming;

   -------------------------
   -- Entity_Matches_Spec --
   -------------------------

   function Entity_Matches_Spec (Old_S,  New_S : Entity_Id) return Boolean is

   begin
      --  Simple case: same entity kinds, type conformance is required.
      --  A parameterless function can also rename a literal.

      if Ekind (Old_S) = Ekind (New_S)
        or else (Ekind (New_S) = E_Function
                   and then Ekind (Old_S) = E_Enumeration_Literal)
      then
         return Type_Conformant (Old_S,  New_S);

      elsif Ekind (New_S) = E_Function and then Ekind (Old_S) = E_Operator then
         return Operator_Matches_Spec (Old_S, New_S);

      elsif Ekind (New_S) = E_Procedure and then Ekind (Old_S) = E_Entry then
         Unimplemented (Old_S, "entries renamed as procedures");
         return False;

      else
         return False;
      end if;
   end Entity_Matches_Spec;

   ------------------------
   -- Attribute_Renaming --
   ------------------------

   procedure Attribute_Renaming (N : Node_Id) is
   begin
      Unimplemented (N, "Attribute renaming");
   end Attribute_Renaming;

   ----------------
   -- Write_Info --
   ----------------

   procedure Write_Info is
      Id : Entity_Id := First_Entity (Current_Scope);

   begin
      if Current_Scope = Standard_Standard then
         --  No point in dumping standard entities.
         return;
      end if;

      Write_Str ("========================================================");
      Write_Eol;
      Write_Str ("        Defined Entities in ");
      Write_Name (Chars (Current_Scope));
      Write_Eol;
      Write_Str ("========================================================");
      Write_Eol;

      if No (Id) then
         Write_Str ("-- none --");
         Write_Eol;

      else
         while Present (Id) loop
            Write_Entity_Info (Id, " ");
            Id := Next_Entity (Id);
         end loop;
      end if;

      if Scope (Current_Scope) = Standard_Standard then

         --  Print information on the current unit itself

         Write_Entity_Info (Current_Scope,  " ");
      end if;

      Write_Eol;
   end Write_Info;

end Sem_Ch8;
