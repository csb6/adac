------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 8                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.6 $                              --
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

with Types; use Types;
package Sem_Ch8  is

   procedure Analyze_Object_Renaming                    (N : Node_Id);
   procedure Analyze_Exception_Renaming                 (N : Node_Id);
   procedure Analyze_Package_Renaming                   (N : Node_Id);
   procedure Analyze_Subprogram_Renaming                (N : Node_Id);
   procedure Analyze_Use_Package                        (N : Node_Id);
   procedure Analyze_Use_Type                           (N : Node_Id);

   procedure End_Scope;
   --  Called at end of scope. On exit On exit from blocks and bodies
   --  (subprogram, package, task, and protected bodies), the name of the
   --  current scope must be removed from the scope stack, and the local
   --  entities must be removed from their homonym chains. On exit from
   --  record declarations, from package specifications, and from tasks
   --  and protected type specifications, more specialized procedures
   --  are invoked.

   procedure End_Use (L : List_Id);
   --  Find use clauses that are declarative items, and reset
   --  the direct visibility flags of the imported entities.

   --  procedure Establish_Transient_Scope;
   --  If no transient scope is active, then a new transient scope is
   --  established and pushed onto the top of the scope stack. See section
   --  "Handling of Transient Data" in package body for details.

   procedure Find_Expanded_Name (N : Node_Id);
   --  Selected component is known to be expanded name. Verify legality
   --  of selector given the scope denoted by prefix.

   procedure Find_Name (N : Node_Id);
   --  Top level routine for name resolution.

   procedure Find_Selected_Component (N : Node_Id);
   --  Resolve various cases of selected components,recognize expanded
   --  names.

   procedure Find_Simple_Name (N : Node_Id);
   --  Scan the homonym chain, starting from the names table, to find the
   --  current entity that corresponds to this name. In most cases it will
   --  be the innermost entity with that name, defined in the current scope.
   --  However, entities in packages may appear on the chain without being
   --  directly visible, and we have to skip past then and scan the homonym
   --  homonym chain until a directly visible entity is found.

   procedure Find_Type (N : Node_Id);
   --  Perform name resolution, and verify that the name found is that of a
   --  type. If it is an incomplete type whose full declaration has been
   --  senn, return the entity in the full declaration. Similarly, if the
   --  type type is private, it has receivd a full declaration, and we are
   --  in the private part or body of the package, return the full
   --  declaration as well. Special processing for Class types as well.

   function Has_Implicit_Operator (N : Node_Id) return Boolean;
   --  N is an expanded name whose selector is an operator name (eg P."+").
   --  A declarative part contains an implicit declaration of an operator
   --  if it has a declaration of a type to which one of the predefined
   --  operators apply. The existence of this routine is an artifact of
   --  our implementation: a more straightforward but more space-consuming
   --  choice would be to make all inherited operators explicit in the
   --  symbol table.

   function In_Open_Scopes (S : Entity_Id) return Boolean;
   --  S is the entity of a scope. This function determines if this scope
   --  is currently open (i.e. it appears somewhere in the scope stack).

   function Is_Appropriate_For_Record (T : Entity_Id) return Boolean;
   --  Prefix is appropriate for record if it is of a record type, or
   --  an access to such.

   function Is_Appropriate_For_Entry_Prefix (T : Entity_Id) return Boolean;
   --  True if it is of a task type, a protected type,  or else
   --  an access to such.

   procedure New_Scope (S : Entity_Id);
   --  Make new scope stack entry, pushing S, the entity for a scope
   --  onto the top of the scope table. The current setting of the scope
   --  suppress flags is saved for restoration on exit.

   procedure Pop_Scope;
   --  Remove top entry from scope stack, restoring the saved setting
   --  of the scope suppress flags.

   procedure Set_Use (L : List_Id);
   --  Find use clauses that are declarative items in a package
   --  declaration, and  set the use-visible flags of imported entities
   --  before analyzing corresponding package body.

end Sem_Ch8;
