------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ U T I L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.20 $                             --
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


--  Package containing utility procedures used throughout the semantics

with Einfo; use Einfo;
with Types; use Types;
package Sem_Util is

   function Access_Checks_Suppressed        (E : Entity_Id) return Boolean;
   function Accessibility_Checks_Suppressed (E : Entity_Id) return Boolean;
   function Discriminant_Checks_Suppressed  (E : Entity_Id) return Boolean;
   function Division_Checks_Suppressed      (E : Entity_Id) return Boolean;
   function Elaboration_Checks_Suppressed   (E : Entity_Id) return Boolean;
   function Index_Checks_Suppressed         (E : Entity_Id) return Boolean;
   function Length_Checks_Suppressed        (E : Entity_Id) return Boolean;
   function Overflow_Checks_Suppressed      (E : Entity_Id) return Boolean;
   function Range_Checks_Suppressed         (E : Entity_Id) return Boolean;
   function Storage_Checks_Suppressed       (E : Entity_Id) return Boolean;
   function Tag_Checks_Suppressed           (E : Entity_Id) return Boolean;
   --  These functions check to see if the named check is suppressed,
   --  either by an active scope suppress setting, or because the check
   --  has been specifically suppressed for the given entity. If no entity
   --  is relevant for the current check, then Empty is used as an argument.

   --  Note: the reason we insist on specifying Empty is to force the
   --  caller to think about whether there is any relevant entity that
   --  should be checked.

   pragma Inline (Access_Checks_Suppressed);
   pragma Inline (Accessibility_Checks_Suppressed);
   pragma Inline (Discriminant_Checks_Suppressed);
   pragma Inline (Division_Checks_Suppressed);
   pragma Inline (Elaboration_Checks_Suppressed);
   pragma Inline (Index_Checks_Suppressed);
   pragma Inline (Length_Checks_Suppressed);
   pragma Inline (Overflow_Checks_Suppressed);
   pragma Inline (Range_Checks_Suppressed);
   pragma Inline (Storage_Checks_Suppressed);
   pragma Inline (Tag_Checks_Suppressed);

   procedure Apply_Access_Check (N : Node_Id; Typ : Entity_Id);
   --  Determines whether an expression node should be flagged as needing
   --  a runtime access check. If the node requires such a check, the
   --  Do_Access_Check flag is turned on.

   procedure Apply_Discriminant_Check (N : Node_Id; Typ : Entity_Id);
   --  Determines whether an expression node should be flagged as needing
   --  a runtime discriminant check. If the node requires such a check,
   --  the Do_Discriminant_Check flag is turned on.

   procedure Apply_Range_Check (N : Node_Id; Target_Type : Entity_Id);
   --  Determines whether an expression node should be flagged as needing
   --  a runtime range check. If the node requires such a check, the
   --  Do_Range_Check flag is turned on.

   function Current_Entity (N : Node_Id) return Entity_Id;
   pragma Inline (Current_Entity);
   --  Find the currently visible definition for a given identifier

   function Current_Scope return Entity_Id;
   --  Get entity representing current scope

   function Defining_Unit_Simple_Name (N : Node_Id) return Entity_Id;
   --  N is the declaration of a program unit (compilation or otherwise).
   --  If the unit is a child unit, return the final defining identifier;
   --  Otherwise, the program unit name itself.

   procedure Enter_Name (Def_Id : Node_Id);
   --  Insert new name in symbol table of current scope with check for
   --  duplications (error message is issued if a conflict is found)

   procedure Get_Index_Bounds (I : Node_Id; L, H : out Node_Id);
   --  This procedure assigns to L and H respectively the values of the
   --  low and high bounds of node I, which must be a range, subtype
   --  indication, or the name of a scalar subtype.

   function Get_Declaration_Node (Unit_Id : Entity_Id) return Node_Id;
   --  Unit_Id is the simple name of a unit, this function returns the
   --  corresponding _Declaration node for the entity, or the subprogram
   --  body node for it. 
   --  The unit may be a child unit with any number of ancestors. 

   function Has_Private_Component (Type_Id : Entity_Id) return Boolean;
   --  Check if a type has a  (sub)component of a private type that has not
   --  yet received a full declaration. 

   function Has_Tagged_Component (Typ : Entity_Id) return Boolean;
   --  Typ must be a composite type (array or record). This function is used
   --  to check if '=' has to be expanded into a bunch component comparaisons.

   function In_Subrange_Of (T1 : Entity_Id; T2 : Entity_Id) return Boolean;
   --  Checks whether the range of values for type T1 is always in the range
   --  given for type T2.

   function Is_Name (N : Node_Id) return Boolean;
   pragma Inline (Is_Name);
   --  Test if the node kind of N is either N_Identifier or N_Expanded_Name,
   --  i.e. the name of an entity that has a defined unique name.

   function New_External_Entity (Kind       : Entity_Kind; 
                                 Scope_Id   : Entity_Id;
                                 Sloc_Value : Source_Ptr;
                                 Related_Id : Entity_Id;
                                 Suffix     : Str;
                                 Index      : Nat := 0;
                                 Prefix     : Str := "") return Entity_Id;
   --  New_Internal_Id creates an N_Defining_Identifier node for an internal
   --  created entity, such as an implicit type or subtype, or a record
   --  initialization procedure. The entity name is constructed with a call
   --  to New_External_Name (Related_Id, Suffix, Index, Prefix), so that
   --  the generated name may be referenced as a public entry.

   function New_Implicit_Type (Sloc_Value : Source_Ptr; 
                               Related_Id : Entity_Id := Empty;
                               Suffix     : Str := "";
                               Index      : Nat := 0;
                               Scope_Id   : Entity_Id := Current_Scope) 
     return Entity_Id;
   --  This is used in cases where there is a new underlying type or subtype
   --  introduced implicitly. Examples include:

   --   a. the implicit unconstrained array type type introduced by a
   --      constrained array type declaration.

   --   b. the implicit subtype introduced by a subtype indication given as
   --      "Integer range 1 .. 2".

   --   For each implicit type or subtype that is created we also create
   --   an N_Implicit_Type node which is inserted in the declaration list
   --   to reflect the present of the created type. The only information
   --   it contains is a pointer to the created entity for the type.
   --   (which is in the Defining_Identifier field). This list is collected
   --   for all such implicit entities generated for a given declaration
   --   and is inserted before the declaration in Analyze_Declarations.

   --   Related_Id is present only if the implicit type name may be referenced
   --   as a public symbol, and thus needs a unique external name. The name
   --   has the form:

   --     "ityp__" & Related_Id_Name & "___" & Suffix & Postfix'Image.

   --   where Postfix'Image is present only if Postfix is non-zero, i.e. the
   --   name is created by a call to:

   --     New_External_Name (Chars (Related_Id), Suffix, Index, "ityp)

   --   If the implicit type does not need an external name, the last 
   --   three parameters are omitted. In this case, the created name of
   --   the implicit type has the form:

   --     "ityp__" & a_serial_number

   --   i.e. the name is created by a call to New_Internal_Name ("ityp")

   --   Note that in all cases, the name starts with "ityp__". This is used
   --   to identify implicit types in the error message handling circuits.

   function New_Internal_Entity (Kind       : Entity_Kind;
                                 Scope_Id   : Entity_Id;
                                 Sloc_Value : Source_Ptr;
                                 Id_Str     : Str) return Entity_Id;
   --  New_Internal_Entity is similar to New_External_Entity, except that
   --  the name is constructed by New_Internal_Name (Id_Str). This is used
   --  when the resulting entity does not have to be referenced as a
   --  public entity.

   function Normalize_Actuals (N : Node_Id; S : Entity_Id; Report : Boolean)
     return Boolean;
   --  Reorders lists of actuals according to names of formals, returned
   --  value indicates sucess of reordering. For more details, see body.
   --  Errors are reported only if Report is set to True.

   procedure Rewrite_Named_Number (N : Node_Id; Typ : Entity_Id);
   --  Replace an identifier or expanded name that is a named number by
   --  an appropriate literal node representing its value, and emit a
   --  constraint check if needed. The appropriate tree Rewrite procedures
   --  are used so that the original reference to the named number is
   --  preserved for debugging purposes, etc.

   function Same_Name (N1, N2 : Node_Id) return Boolean;
   --  Determine if two (possibly expanded) names are the same name

   procedure Set_Current_Entity (E : Entity_Id);
   pragma Inline (Set_Current_Entity);
   --  Establish the entity E as the currently visible definition of its
   --  associated name (i.e. the Node_Id associated with its name)

   procedure Set_Entity_With_Style_Check (N : Node_Id; Val : Node_Id);
   --  This procedure has the same calling sequence as Set_Entity,
   --  but it performs the additional check if Gnat_Style_Check is on
   --  that if the defining entity and reference are both user declared
   --  identifiers, then the spelling (capitalization) must match.

   procedure Set_Public_Status (Id : Entity_Id);
   --  If an entity (visible or otherwise) is defined in a library
   --  package, or a package that is itself public, then this subprogram
   --  labels the entity public as well.

   function Static_Integer (N : Node_Id) return Uint;
   --  This function analyzes the given expression node and then resolves it
   --  as any integer type. If the result is static, then the value of the
   --  universal expression is returned, otherwise an error message is output
   --  and a value of No_Uint is returned.

   procedure Trace_Scope (N : Node_Id; E : Entity_Id; Msg : Str);
   --  Print debugging information on entry to each unit being analyzed.

   procedure Unimplemented (N : Node_Id; Feature : Str);
   --  An indication that here and there we are not finished yet. Prints out
   --  a message on the given node indicating that the feature described by
   --  the given string is not implemented yet.

end Sem_Util;
