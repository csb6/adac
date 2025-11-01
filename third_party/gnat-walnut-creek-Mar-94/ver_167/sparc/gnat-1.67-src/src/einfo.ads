------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                E I N F O                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.155 $                            --
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
package Einfo is

--  This package defines the annotations to the abstract syntax tree that
--  are are needed to support semantic processing of an Ada compilation.

--  These annotations are for the most part attributes of declared entities,
--  and they correspond to conventional symbol table information. Other
--  attributes include sets of meanings for overloaded names, possible
--  types for overloaded expressions, flags to indicate deferred constants,
--  incomplete types, etc. These attributes are stored in available fields
--  in tree nodes (i.e. fields not used by the parser, as defined by the
--  Sinfo package specification), and accessed by means of a set of
--  subprograms which define an abstract interface.

--  There are two kinds of semantic information

--    First, the tree nodes with the following Nkind values:

--      N_Defining_Identifier
--      N_Defining_Character_Literal
--      N_Defining_Operator_Symbol

--    are called Entities, and constitute the information that would often
--    be stored separately in a symbol table. These nodes are all extended
--    to provide extra space, and contain fields which depend on the entity
--    kind, as defined by the contents of the Ekind field. The use of the
--    Ekind field, and the associated fields in the entity, are defined
--    in this package, as are the access functions to these fields.

--    Second, in some cases semantic information is stored directly in other
--    kinds of nodes, e.g. the Etype field, used to indicate the type of an
--    expression. The access functions to these fields are defined in the
--    Sinfo package, but their full documentation is to be found in
--    the Einfo package specification.

--  Declaration processing places information in the nodes of their defining
--  identifiers. Name resolution places in all other occurences of an
--  identifier a pointer to the corresponding defining occurrence.

--  ------------------------------
--  -- Common Entity Attributes --
--  ------------------------------

--  All entities have the following common attributes

--    Chars (Name1)
--       Source identifier or designator of entity. It is an entry into the
--       names table, and it is set by the parser.

--    Ekind
--       An element of the type Entity_Kind defined below.

--    Etype (Node5)
--       Declared type of an entity. Also an entity. For a type entity,
--       this pointer is self-referential.

--    Has_Address_Clause (Flag28)
--       Flag set if Address clause given for object or subprogram. Always
--       False for other entity kinds.

--    Has_Homonym (Flag12)
--       A flag to indicate that an entity has a homonym in the same scope.
--       scope. Used by Gigi to generate unique expanded names for such
--       entities.

--    Has_Master_Entity (Flag21)
--       A flag present on entities that can appear in the scope stack (see
--       Sem spec). It is set if a task master entity (_Master) has been
--       declared and initialized in the corresponding scope.

--    Has_Size_Clause (Flag29)
--       Flag set if Size clause given for object or type. Always false for
--       other kinds of entities. This is used to prevent multiple Size
--       clauses for a given entity, and is always initially cleared for a
--       derived type (even though the Size, if set, is inherited by the
--       derived type).

--    Has_Storage_Size_Clause (Flag23)
--       Flag set if Storage_Size clause given for a task type. Always False
--       for other kinds of entities. This is used to prevent multiple
--       Storage_Size clauses for a given task type.

--    Has_Tasks (Flag30)
--       A flag that is set on task types themselves, and also (recursively)
--       on any composite type which has a component that has this flag set.
--       The meaning is that an allocator of such an object must create
--       the required tasks. Note that the flag is not set on access types,
--       even if they designate tasks.

--    Homonym (Node4)
--       A link to chain entities that are homonyms and that are declared in
--       the same or enclosing scopes (Homonyms in the same scope are
--       overloaded) 

--    Is_Delayed (Flag18)
--       Indicates that the elaboration of the declaration for the entity
--       must be deferred in Gigi, until the completion of some other entity
--       (typically a private type) is encountered.

--    Is_Directly_Visible (Flag7)
--       Indicates whether entity is defined in some currently open scope.

--    Is_Imported (Flag24)
--       Set if the entity is imported. For now we only allow the import of
--       functions and procedures, but that may well change later on, which
--       is why this field is present in all entity types.

--    Is_Internal (Flag2)
--       Set to indicate an entity created during semantic processing (e.g.
--       an implicit type).

--    Is_Private (Flag13)
--       Indicates  whether entity is defined in the private part of a
--       package.

--    Is_Public (FLag10)
--       To indicate that an external symbol must be created for this
--       entity, because it may be referenced from another object file.

--    Is_Use_Visible (Flag9)
--       Indicates whether entity is defined in a package that appears in
--       currently active use clause.

--    Next_Entity (Node6)
--       The entities of a scope are chained, with the head of the list 
--       being in the First_Entity field of the scope entity. All entities
--       use the Next_Entity field as a forward pointer for this list, with
--       Empty indicating the end of the list.

--    Parent
--       As usual, this field points to the syntactic parent of the node, 
--       which for an entity is the declaration that declares the entity. 

--    Scope (Node3)
--       Points to the entity for the scope (block, loop, subprogram,
--       package etc.) in which the entity is declared.

--  --------------------------------
--  -- Entity-Specific Attributes --
--  --------------------------------

--  These fields only exist for certain kinds of entities, so access to
--  these fields should check for appropriate Ekind values first. This
--  list only includes fields that are directly defined in the entities.
--  For syntactic access functions, see specs of the functions themselves.

--    Alias (Node7)
--       Present in overloadable entities (enumeration literals, functions,
--       procedures). Points to parent subprogram of a derived subprogram.

--    Component_Type (Node10) 
--       Present in array types and subtypes, and also in the special 
--       enumeration table type created for enumeration type. References
--       the entity for the component type.

--    Corresponding_Task_Type (Node7)
--       Present in record types that are constructed by the expander to
--       represent task types (Is_Task_Record_Type flag True). Points to the
--       entity for the corresponding task type. Note that such records are
--       untagged, so it is safe to overlap the Primitive_Operations field
--       which is only used for tagged record types.

--    Directly_Designated_Type (Node10)
--       Present in access types. This field points to the type that is
--       directly designated by the access type. In the case of an access
--       type to an incomplete type, this field references the incomplete
--       type. Note that in the semantic processing, what is useful in
--       nearly all cases is the full type designated by the access type.
--       The function Designated_Type obtains this full type in the case of
--       access to an incomplete type.

--    Default_Value (Node10)
--       Present in IN parameters. Points to the node representing the
--       expression for the default value for the parameter. Empty if the
--       parameter has no default value.

--    Discriminal (Node9)
--       Present in discriminants (Discriminant formal: GNAT's first
--       coinage). The entity used as a formal parameter that corresponds
--       to a discriminant. See section "Use of Discriminants" for details.

--    Discriminant_Default_Value (Node10)
--       Present in discriminants. Points to the node representing the
--       expression for the default value of the discriminant. Set to
--       Empty if the discriminant has no default value.

--    Discriminant_Constraint (Elist2)
--       Present in entities that can have discriminant constraints (task
--       types and subtypes, record types and subtypes, private types,
--       limited private types and incomplete types). Points to an element
--       list containing the discriminant constraint (list of discriminant
--       associations) for a record (sub)type.

--    Discriminant_Checking_Func (Node10)
--       Present in components. Points to the defining identifier of the
--       function built by the expander returns a Boolean indicating whether
--       the given record component exists for the current discriminant 
--       values.

--    Enumeration_Pos (Uint11)
--       Present in enumeration literals. Contains the position number
--       corresponding to the value of the enumeration literal.

--    Enumeration_Rep (Uint12)
--       Present in enumeration literals. Contains the representation that
--       corresponds to the value of the the enumeration literal. Note that
--       this is normally the same as Enumeration_Pos except in the presence
--       of representation clauses, where Pos will still represent the
--       position of the literal within the type and Rep will have be the
--       value given in the representation clause.

--    Entry_Index (Uint11)
--       Present in entries and entry families. For an entry, contains the
--       entry index, which is the ordinal position of the entry among the
--       entries declared in the task definition, excluding any entry
--       families (the first entry is numbered 1). In an entry family, this
--       field contains the entry index of the first entry among any of the
--       entry families of a task definition (i.e. the value is the same
--       for all entry families in one task definition, and is one greater
--       than the largest entry index value for any non-family entry, or 1
--       if only entry families are present.

--    Equivalent_Type (Node7)
--       Present in class subtypes. Points to a type entity created by the
--       expander which gives Gigi an easily understandable equivalent of
--       the class subtype with a known size (given by an initial value).
--       See Exp_Util.Expand_Class_Subtype for further details.

--    Esize (Node12)
--       Present in all types and subtypes, an also for components, tags,
--       constants and variables. Contains the size of a type or object.
--       A value of zero is used in the case of composite types or objects 
--       for which no size clause has been given (i.e. those types for which
--       only Gigi knows the allocated size)

--    First_Entity (Node9)
--       Present in all entities which act as scopes to which a list of
--       associated entities is attached (blocks, class subtypes and types,
--       entries, functions, loops, packages, procedures, protected objects,
--       record types and subtypes, task types and subtypes). Points to a 
--       list of associated entities chained through the Next_Entity field
--       with Empty marking the end of the list.

--    First_Private_Entity (Node11)
--       Present in all entities containing private parts (packages,
--       protected types and subtypes, task types and subtypes). The 
--       entities on the entity chain are in order of declaration, so the
--       entries for private entities are at the end of the chain. This
--       field points to the first entity for the private part. It is
--       Empty if there are no entities declared in the private part or
--       if there is no private part.

--    Full_Declaration (Node11)
--       Present in incomplete types, private types, limited private types,
--       and deferred constants. Points to the entity for the corresponding
--       full type declaration.

--    Has_Completion (Flag26)
--       Present in all entities that require a completion (functions,
--       procedures, private types, limited private types, incomplete types,
--       and packages that require a body). Set if the completion has been
--       encountered and analyzed.

--    Has_Discriminants (Flag5)
--       Present in all entities that can have discriminants (record types
--       and subtypes, task types and subtypes, protected types and subtypes,
--       private types, limited private types, and incomplete types). Set if
--       known discriminants are present.

--    Interface_Name (Node12)
--       Present in functions and procedures, contains Empty unless a pragma
--       Interface_Name or Import has been given, in which case it points to
--       the N_String_Literal node for the specified link name.

--    Internal_Access_Disp_Table (Node11)
--       The entity which represents the dispatch table access associated
--       with the given tagged type. This field is not directly in the 
--       corresponding Record_Type entity because there is not enough room,
--       it rather is in the Tag component (whose name is Name_uTag) of this
--       record.

--    Is_Abstract (Flag19)
--       Present in all types, and also for functions and procedures. Set
--       for abstract types and abstract subprograms.

--    Is_Aliased (Flag15)
--       Present on objects whose declarations carry the keyword aliased,
--       and on record components that have the keyword. For arrays with an
--       aliased component declaration, the flag is on the array type itself.

--    Is_Intrinsic (Flag27)
--       Present in functions and procedures. Set if a pragma Convention 
--       Intrinsic has been given for the subprogram.

--    Init_Proc (Node8)
--       Present in all type entities. Points to the entity for the
--       Initialization procedure if one is required. If this field is
--       Empty, then no Initialization proc is defined for the type. Note
--       that subtyupes and implicit types do not have this field set.
--       Subtype objects are initialized using the initialization procedure
--       of the base type. A special case arises for task types. The
--       Init_Proc field of task types is Empty, but initialization is
--       required. The initialization procedure used is the one for the
--       corresponding record type (see Base_Init_Proc).

--    Is_Inlined (Flag11)
--       Present in functions and procedures. Set if a pragma Inline applies
--       to the subprogram. For subprograms created during expansion, this
--       flag may be set directly by the expander to request inlining.

--    Is_Limited_Type (Flag25)
--       Present in all type entities. Set if the type is a limited type
--       (private type, limited private type, task type, protected type,
--       composite containing a limited component, or subtypes of any of
--       these types).

--    Is_Package_Body (Flag5)
--       Need documentation for this field ???

--    Is_Private_Type (Flag14)
--       Present in all type entities. Set if the type is private or if it
--       depends on a private type.

--    Is_Private_Descendant (Flag14)
--       Present in entities that can represent library units (packages,
--       functions, procedures). Set if the library unit is itself a private
--       child unit, or if it is the descendent of a private child unit.

--    Is_Pure (Flag3)
--       Present in functions and procedures. Set by the semantic processing 
--       to indicate that the subprogram is known to be pure, i.e. to have no
--       side effects.

--    Is_Task_Record_Type (Flag20)
--       Present in record types and subtypes. Set if the type was created
--       by the expander to represent a task type. For every task type,
--       type, such as record type is constructed, and task objects are
--       instances of this record type at runtime (Gigi will replace
--       declarations of the task type by declarations of the corresponding
--       record type. See package Exp_Ch9 for further details.

--    Is_Volatile (Flag16)
--       Present in all type entities, and also in constants, components and
--       variables. Set if a pragma Volatile applies to the entity.

--    Last_Entity (Node10)
--       Present in all entities which act as scopes to which a list of
--       associated entities is attached (blocks, class subtypes and types,
--       entries, functions, loops, packages, procedures, protected objects,
--       record types and subtypes, task types and subtypes). Points to the
--       last entry in the list of associated entities chained through the
--       Next_Entity field. Empty if no entities are chained.

--    Lit_Name_Table (Node7)
--       Present in enumeration types and subtypes. Points to the entity
--       (which is of the special type E_Enum_Table_Type) for a table of
--       accesses to strings, generated by Gigi for each enumeration type.
--       The table is an array whose index values are 'Pos values and whose
--       entries are access to string, where each string is the 'Image value.

--    Master_Id (Node9)
--       Present in access types and subtypes. . Empty unless Has_Tasks is
--       set for the designated type, in which case it points to the entity
--       for the Master_Id for the access type master.

--    Needs_Discr_Check (Flag5)
--       Present in components. Set if the references to this component need
--       a discriminant check.

--    Original_Record_Component (Node8)
--       Present for components. When this component is inherited in a record
--       extension, it points to the original component (the entity of the 
--       ancestor component which is not itself inherited) otherwise it points
--       to itself. Gigi uses this attribute to implement the automatic 
--       dereference in the extension and to apply the transformation:
--         Rec_Ext.Comp ===> Rec_Ext.Parent. ... .Parent.Comp

--    Parent_Subtype (Node11)
--       Defined for all scalar types and subtypes. Always Empty for types.
--       For subtypes, references the entity for the type or subtype from
--       which the subtype was immediately obtained. This attribute is used
--       in recognizing some cases of range checking which can be eliminated
--       at compile time.

--    Primitive_Operations (Elist7)
--       Present in tagged record types and subtypes and in tagged private
--       types. Points to an element list of entities for primitive operations
--       for the tagged type. Not present in untagged types.

--    Renamed_Object (Node7)
--       Present in constants and variables. References the entity for the
--       renamed object.

--    Scalar_Range (Node10)
--       Present in all scalar types (including modular types, where the
--       bounds are 0 .. modulus - 1). References a node in the tree that
--       contains the bounds for the range. Note that this information
--       could be obtained by rummaging around the tree, but it is more
--       convenient to have it immediately at hand in the entity.

--    String_Literal_Length (Uint11)
--       Present in string literal subtypes (which are created to correspond
--       to string literals in the program). Contains the length of the string
--       literal.

--    Slice_Range (Node11)
--       Present in slice subtypes, which are created to represent the type
--       of slices appearing in the program. References the N_Discrete_Range
--       node of the slice for which this subtype was created.

--    Table_High_Bound (Node11)
--       Present in the special Enum_Table_Type entities created to
--       represent the Lit_Name_Table created by Gigi. Contains the high
--       bound (i.e. number of entries minus one) of the created table.
--       Equal to Enum_Type'Pos (Enum_Type'Last).

--    Task_Value_Type (Node7)
--       Present in task types and subtypes. References the entity for the
--       corresponding record type constructed by the expander (see Exp_Ch9).
--       This type is used to represent values of the task type.

--  Each identifier which is not a defining occurrence holds a pointer
--  to its defining occurrence (its Entity).

--  Type checking and overload resolution decorate each identifier and each
--  expression with a type as well.


   ------------------
   -- Access Kinds --
   ------------------

   --  The following three entity kinds are introduced by the corresponding
   --  type definitions:
   --    E_Access_Type,  E_General_Access_Type,  E_Anonymous_Access_Type.

   --  In  addition, we define the kind E_Allocator_Type to label allocators
   --  and attribute references with 'Access. This is because special 
   --  resolution rules apply to these constructs. Eventually the constructs
   --  are labelled with the access type imposed by the context.  Gigi should
   --  never see the type E_Allocator.

   --  Finally, the type Any_Access is used to label -null- during type
   --  resolution. Any_Access is also replaced by the context type after
   --  resolution.

   --------------------------------------------------------------
   -- Description of Defined Access Functions for Entity_Kinds --
   --------------------------------------------------------------

   --  For each enumeration value defined in Entity_Kind we list all
   --  the access functions defined in Einfo which can legally be applied
   --  to an entity of that kind. The implementation of the access functions
   --  themselves is given in the Einfo body. Many of these access functions
   --  are simply references to fields in the entity, and in this case the
   --  corresponding field is given in the description. If no field is given,
   --  then the access function involves more processing than simply accessing
   --  a single field or flag.

   --  The following list of access functions applies to all entities for
   --  types and subtypes. References to this list appear subsequently as
   --  as "(plus type functions)" for each appropriate Entity_Kind.

   --    Base_Type
   --    Esize                      (Uint12)
   --    Init_Proc                  (Node8)
   --    Is_Abstract                (Flag19)
   --    Is_Access_Type
   --    Is_Array_Type
   --    Is_Boolean_Type
   --    Is_Character_Type
   --    Is_Composite_Type
   --    Is_Discrete_Type
   --    Is_Elementary_Type
   --    Is_Enumeration_Type
   --    Is_Fixed_Type
   --    Is_Float_Type
   --    Is_Frozen                  (Flag4)
   --    Is_Generic_Type            (Flag6)
   --    Is_Incomplete_Type
   --    Is_Integer_Type
   --    Is_Internal                (Flag2)
   --    Is_Limited_Type
   --    Is_Numeric_Type
   --    Is_Packed
   --    Is_Private_Type
   --    Is_Protected_Type
   --    Is_Real_Type
   --    Is_Record_Type
   --    Is_Scalar_Type
   --    Is_String_Type
   --    Is_Task_Type
   --    Is_Tagged_Type             (Flag8)
   --    Is_Type
   --    Is_Volatile                (Flag16)
   --    Next_Index
   --    Root_Type

   --  Applicable access functions by entity kind

   --  E_Access_Subprogram_Type
   --    Directly_Designated_Type   (Node10)
   --    (plus type functions)

   --  E_Access_Type
   --  E_Access_Subtype
   --    Master_Id                  (Node9)
   --    Directly_Designated_Type   (Node10)
   --    (plus type functions)

   --  E_Allocator_Type
   --    Directly_Designated_Type   (Node10)
   --    (plus type functions)

   --  E_Array_Type
   --  E_Array_Subtype
   --    Component_Type             (Node10)
   --    First_Index                (Node9)
   --    Is_Constrained             (Flag3)
   --    Number_Dimensions
   --    (plus type functions)

   --  E_Anonymous_Access_Type
   --    Directly_Designated_Type   (Node10)
   --    (plus type functions)

   --  E_Block
   --    First_Entity               (Node9)
   --    Last_Entity                (Node10)

   --  E_Boolean_Type
   --    First_Literal              (Node9)
   --    Parent_Subtype             (Node11)
   --    Scalar_Range               (Node10)
   --    Type_Low_Bound
   --    Type_High_Bound
   --    (plus type functions)

   --  E_Character_Type
   --    First_Literal              (Node9)
   --    Parent_Subtype             (Node11)
   --    Scalar_Range               (Node10)
   --    Type_Low_Bound
   --    Type_High_Bound
   --    (plus type functions)

   --  E_Class_Subtype
   --    First_Component
   --    Equivalent_Type            (Node7)
   --    First_Entity               (Node9)
   --    Last_Entity                (Node10)
   --    (plus type functions)

   --  E_Class_Type
   --    First_Component
   --    First_Entity               (Node9)
   --    Last_Entity                (Node10)
   --    (plus type functions)

   --  E_Component
   --    Discriminant_Checking_Func (Node10)
   --    Internal_Access_Disp_Table (Node11)
   --    Esize                      (Uint12)
   --    Is_Volatile                (Flag16)
   --    Needs_Discr_Check          (Flag5)
   --    Next_Component
   --    Original_Record_Component  (Node8)

   --  E_Constant
   --    Esize                      (Uint12)
   --    Full_Declaration           (Node11)
   --    Is_Volatile                (Flag16)
   --    Renamed_Object             (Node7)

   --  E_Decimal_Type
   --  E_Decimal_Subype
   --    To be specified ???

   --  E_Discriminant
   --    Discriminant_Default_Value (Node10)
   --    Next_Discriminant
   --    Discriminal

   --  E_Entry
   --    First_Formal
   --    First_Entity               (Node9)
   --    Last_Entity                (Node10)
   --    Entry_Index                (Uint11)

   --  E_Entry_Family
   --    Entry_Index                (Uint11)

   --  E_Enumeration_Literal
   --    Alias                      (Node7)
   --    Enumeration_Pos            (Uint11)
   --    Enumeration_Rep            (Uint12)
   --    Next_Literal

   --  E_Enumeration_Type
   --  E_Enumeration_Subtype
   --    First_Literal              (Node9)
   --    Lit_Name_Table             (Node7)
   --    Parent_Subtype             (Node11)
   --    Scalar_Range               (Node10)
   --    Type_Low_Bound
   --    Type_High_Bound
   --    (plus type functions)

   --  E_Enum_Table_Type
   --    Component_Type             (Node10)
   --    Table_High_Bound           (Node11)

   --  E_Exception
   --  E_Exception_Type
   --    (no additional fields)

   --  E_Fixed_Type
   --  E_Fixed_Subtype
   --    Parent_Subtype             (Node11)
   --    Scalar_Range               (Node10)
   --    Type_Low_Bound
   --    Type_High_Bound
   --    (plus type functions)

   --  E_Float_Type
   --  E_Float_Subtype
   --    Parent_Subtype             (Node11)
   --    Scalar_Range               (Node10)
   --    Type_Low_Bound
   --    Type_High_Bound
   --    (plus type functions)

   --  E_Formal_Derived_Type

   --  E_Formal_Generic_Package

   --  E_Function
   --  E_Generic_Function
   --    Alias                      (Node7)
   --    First_Entity               (Node9)
   --    First_Formal
   --    Has_Completion             (Flag26)
   --    Has_Subprogram_Body        (Flag5)
   --    Interface_Name             (Node12)
   --    Is_Intrinsic               (Flag27)
   --    Is_Dispatching_Operation   (Flag6)
   --    Is_Inlined                 (Flag11)
   --    Is_Private_Descendant      (Flag14)
   --    Is_Pure                    (Flag3)
   --    Is_Abstract                (Flag19)
   --    Last_Entity

   --  E_General_Access_Type
   --    Master_Id                  (Node9)
   --    Directly_Designated_Type   (Node10)
   --    (plus type functions)

   --  E_Incomplete_Type
   --    Discriminant_Constraint    (Elist2)
   --    First_Discriminant
   --    Full_Declaration           (Node11)
   --    Has_Discriminants          (Flag5)
   --    (plus type functions)

   --  E_In_Parameter
   --  E_Generic_In_Parameter
   --    Default_Value              (Node10)
   --    Parameter_Mode

   --  E_In_Out_Parameter
   --  E_Generic_In_Out_Parameter
   --    Parameter_Mode

   --  E_Integer_Type
   --  E_Integer_Subtype
   --    Parent_Subtype             (Node11)
   --    Scalar_Range               (Node10)
   --    Type_Low_Bound
   --    Type_High_Bound
   --    (plus type functions)

   --  E_Label
   --    Reachable                  (Flag5)

   --  E_Limited_Private_Type
   --    Discriminant_Constraint    (Elist2)
   --    First_Discriminant
   --    Full_Declaration           (Node11)
   --    Has_Completion             (Flag26)
   --    Has_Discriminants          (Flag5)
   --    (plus type functions)

   --  E_Limited_Type

   --  E_Loop
   --    Has_Exit                   (Flag5)

   --  E_Modular_Type
   --  E_Modular_Subtype
   --    Modulus                    (Uint9)
   --    Scalar_Range               (Node10)
   --    Parent_Subtype             (Node11)
   --    Type_Low_Bound
   --    Type_High_Bound
   --    (plus type functions)

   --  E_Named_Integer
   --    (no additional fields)

   --  E_Named_Real
   --    (no additional fields)

   --  E_Out_Parameter
   --    Parameter_Mode

   --  E_Package
   --  E_Generic_Package
   --    First_Entity               (Node9)
   --    First_Private_Entity       (Node11)
   --    Has_Completion             (Flag26)
   --    In_Private_Part            (Flag4)
   --    In_Use                     (Flag8)
   --    Is_Package_Body            (Flag5)
   --    Is_Private_Descendant      (Flag14)
   --    Last_Entity                (Node10)

   --  E_Package_Body
   --    (no additional fields)

   --  E_Private_Type
   --    Discriminant_Constraint    (Elist2)
   --    First_Discriminant
   --    Full_Declaration           (Node11)
   --    Has_Completion             (Flag26)
   --    Has_Discriminants          (Flag5)
   --    Primitive_Operations       (Elist7)    [if Is_Tagged]
   --    (plus type functions)

   --  E_Procedure
   --  E_Generic_Procedure
   --    Alias                      (Node7)
   --    First_Entity               (Node9)
   --    Interface_Name             (Node12)
   --    First_Formal
   --    Has_Completion             (Flag26)
   --    Has_Subprogram_Body        (Flag5)
   --    Is_Intrinsic               (Flag27)
   --    Is_Dispatching_Operation   (Flag6)
   --    Is_Inlined                 (Flag11)
   --    Is_Private_Descendant      (Flag14)
   --    Is_Pure                    (Flag3)
   --    Is_Abstract                (Flag19)
   --    Last_Entity

   --  E_Protected_Body
   --    (no additional fields)

   --  E_Protected_Object
   --    First_Entity               (Node9)
   --    Last_Entity                (Node10)

   --  E_Protected_Type
   --  E_Protected_Subtype
   --    First_Private_Entity       (Node11)
   --    Has_Discriminants          (Flag5)

   --  E_Record_Type
   --  E_Record_Subtype
   --    Discriminant_Constraint    (Elist2)
   --    Access_Disp_Table
   --    First_Component
   --    First_Discriminant
   --    First_Entity               (Node9)
   --    Has_Discriminants          (Flag5)
   --    Is_Constrained             (Flag3)
   --    Is_Task_Record_Type        (Flag20)
   --    Last_Entity                (Node10)
   --    Primitive_Operations       (Elist7)    [if Is_Tagged]
   --    Corresponding_Task_Type    (Node7)     [if not Is_Tagged]
   --    Tag_Component
   --    (plus type functions)

   --  E_Slice_Subtype
   --    Component_Type             (Node10)
   --    Slice_Range                (Node11)
   --    (plus type functions)

   --  E_String_Type
   --  E_String_Subtype
   --    Component_Type             (Node10)
   --    First_Index                (Node9)
   --    Is_Constrained             (Flag3)
   --    Number_Dimensions
   --    (plus type functions)

   --  E_String_Literal_Subtype
   --    Component_Type             (Node10)
   --    String_Literal_Length      (Uint11)
   --    (plus type functions)

   --  E_Subprogram_Body
   --    (no additional fields)

   --  E_Subprogram_Type
   --    Directly_Designated_Type   (Node10)
   --    First_Formal
   --    (plus type functions)

   --  E_Task_Body
   --    (no additional fields)

   --  E_Task_Type
   --  E_Task_Subtype
   --    Discriminant_Constraint    (Elist2)
   --    First_Entity               (Node9)
   --    Has_Discriminants          (Flag5)
   --    Last_Entity                (Node10)
   --    Task_Value_Type            (Node7)
   --    First_Private_Entity       (Node11)
   --    (plus type functions)

   --  E_Variable
   --    Esize                      (Uint12)
   --    Is_Volatile                (Flag16)
   --    Renamed_Object             (Node7)

   --  E_Void

   --------------------------------
   -- Classification of Entities --
   --------------------------------

   --  The classification of program entities which follows is a refinement of
   --  the list given in RM 3.1(1). E.g., separate entities denote subtypes of
   --  different type classes. Ada9X entities include class_wide types,
   --  protected types, subprogram types, generalized access types,  generic
   --  formal derived types and generic formal packages.

   --  The order chosen for these kinds allows us to classify related entities
   --  sp that they are contiguous. As a result, they do not appear in the
   --  exact same order as their order of first appearance in the LRM (For
   --  example, private types are listed before packages). The contiguity
   --  allows us to define useful subtypes (see below) such as type entities,
   --  overloaded entities, etc.

   --  Each entity (explicitly or implicitly declared) has a kind, which is
   --  a value of the following type:

   type Entity_Kind is (
      E_Void,                 
      E_Variable,
      E_Component,
      E_Constant,
      E_Named_Integer,        
      E_Named_Real,

      --  Type entities

      E_Enumeration_Type,
      E_Enumeration_Subtype,
      E_Character_Type,
      E_Boolean_Type,

      --  Numeric types

      E_Integer_Type,
      E_Integer_Subtype,
      E_Modular_Type,
      E_Modular_Subtype,
      E_Float_Type,
      E_Float_Subtype,
      E_Fixed_Type,
      E_Fixed_Subtype,
      E_Decimal_Type,
      E_Decimal_Subtype,

      --  Access types

      E_Access_Type,
      E_Access_Subtype,
      E_Allocator_Type,
      E_General_Access_Type,   
      E_Access_Subprogram_Type,
      E_Anonymous_Access_Type,

      --  Composite types

      E_Array_Type,
      E_Array_Subtype,
      E_String_Type,
      E_String_Subtype,
      E_String_Literal_Subtype,
      E_Enum_Table_Type,
      E_Slice_Subtype,
      E_Record_Type,
      E_Class_Subtype,          -- Ada9x (Internal)
      E_Class_Type,             -- Ada9X
      E_Record_Subtype,
      E_Incomplete_Type,
      E_Private_Type,
      E_Limited_Private_Type,
      E_Limited_Type,
      E_Task_Type,
      E_Task_Subtype,
      E_Protected_Type,
      E_Protected_Subtype,

      E_Exception_Type,
      E_Subprogram_Type,        

      E_Discriminant,
      E_Loop,
      E_Loop_Parameter,
      E_Block,
      E_Label,

      --  Overloadable entities

      E_Enumeration_Literal,
      E_Function,
      E_Operator,
      E_Procedure,
      E_Entry,

      --  Parameter entities

      E_In_Parameter,
      E_Out_Parameter,
      E_In_Out_Parameter,

      --  Other entities

      E_Package,
      E_Package_Body,
      E_Protected_Body,
      E_Protected_Object,
      E_Entry_Family,
      E_Exception,
      E_Generic_Package,
      E_Generic_Function,
      E_Generic_Procedure,
      E_Generic_In_Parameter,
      E_Generic_In_Out_Parameter,
      E_Formal_Derived_Type,
      E_Formal_Generic_Package,
      E_Task_Body,
      E_Subprogram_Body
   );

   subtype Type_Kind        is Entity_Kind range
       E_Enumeration_Type ..
   --  E_Enumeration_Subtype
   --  E_Boolean_Type
   --  E_Integer_Type
   --  E_Integer_Subtype
   --  E_Modular_Type
   --  E_Modular_Subtype
   --  E_Float_Type
   --  E_Float_Subtype
   --  E_Fixed_Type
   --  E_Fixed_Subtype
   --  E_Decimal_Type
   --  E_Decimal_Subtype
   --  E_Access_Type
   --  E_Access_Subprogram_Type,
   --  E_Allocator_Type,
   --  E_General_Access_Type
   --  E_Anonymous_Access_Type
   --  E_Array_Type
   --  E_Array_Subtype
   --  E_Record_Type
   --  E_Class_Subtype
   --  E_Class_Type
   --  E_Record_Subtype
   --  E_Incomplete_Type
   --  E_Private_Type
   --  E_Limited_Private_Type
   --  E_Limited_Type
   --  E_Task_Type
   --  E_Task_Subtype
   --  E_Protected_Type
   --  E_Protected_Subtype
   --  E_Exception_Type
       E_Subprogram_Type;

   subtype Boolean_Kind        is Entity_Kind range
       E_Boolean_Type ..
       E_Boolean_Type;

   subtype Integer_Kind        is Entity_Kind range
       E_Integer_Type ..
   --  E_Integer_Subtype
   --  E_Modular_Type
       E_Modular_Subtype;

   subtype Signed_Integer_Kind is Entity_Kind range
       E_Integer_Type ..
       E_Integer_Subtype;

   subtype Modular_Kind        is Entity_Kind range
       E_Modular_Type ..
       E_Modular_Subtype;

   subtype Discrete_Kind       is Entity_Kind range
       E_Enumeration_Type ..
   --  E_Enumeration_Subtype
   --  E_Integer_Type
   --  E_Integer_Subtype
   --  E_Modular_Type
       E_Modular_Subtype;

   subtype Enumeration_Kind    is Entity_Kind range
       E_Enumeration_Type ..
   --  E_Enumeration_Subtype
   --  E_Character_Type
       E_Boolean_Type;

   subtype Real_Kind           is Entity_Kind range
       E_Float_Type ..
   --  E_Float_Subtype
   --  E_Fixed_Type
   --  E_Fixed_Subtype
   --  E_Decimal_Type
       E_Decimal_Subtype;

   subtype Float_Kind          is Entity_Kind range
       E_Float_Type ..
       E_Float_Subtype;

   subtype Fixed_Kind          is Entity_Kind range
       E_Fixed_Type ..
   --  E_Fixed_Subtype
   --  E_Decimal_Type
       E_Decimal_Subtype;

   subtype Protected_Kind      is Entity_Kind range
       E_Protected_Type ..
       E_Protected_Subtype;

   subtype Task_Kind           is Entity_Kind range
       E_Task_Type ..
       E_Task_Subtype;

   subtype Numeric_Kind        is Entity_Kind range
       E_Integer_Type ..
   --  E_Integer_Subtype
   --  E_Modular_Type
   --  E_Modular_Subtype
   --  E_Float_Type
   --  E_Float_Subtype
   --  E_Fixed_Type
   --  E_Fixed_Subtype
   --  E_Decimal_Type
       E_Decimal_Subtype;

   subtype Scalar_Kind         is Entity_Kind range
       E_Enumeration_Type ..
   --  E_Enumeration_Subtype
   --  E_Boolean_Type
   --  E_Integer_Type
   --  E_Integer_Subtype
   --  E_Modular_Type
   --  E_Modular_Subtype
   --  E_Float_Type
   --  E_Float_Subtype
   --  E_Fixed_Type
   --  E_Fixed_Subtype
   --  E_Decimal_Type
       E_Decimal_Subtype;

   subtype Access_Kind         is Entity_Kind range
       E_Access_Type ..
   --  E_Access_Subtype
   --  E_Allocator_Type
   --  E_General_Access_Type
   --  E_Access_Subprogram_Type
       E_Anonymous_Access_Type;

   subtype Incomplete_Kind     is Entity_Kind range
       E_Incomplete_Type ..
   --  E_Private_Type
   --  E_Limited_Private_Type
       E_Limited_Type;

   subtype Private_Kind        is Entity_Kind range
       E_Private_Type ..
       E_Limited_Private_Type;

   subtype Overloaded_Kind     is Entity_Kind range
       E_Enumeration_Literal ..
   --  E_Function
   --  E_Procedure
       E_Entry;

   subtype Subprogram_Kind     is Entity_Kind range
       E_Function ..
       E_Procedure;

   subtype Formal_Kind         is Entity_Kind range
       E_In_Parameter ..
   --  E_Out_Parameter
       E_In_Out_Parameter;

   subtype Named_Kind          is Entity_Kind range
       E_Named_Integer ..
       E_Named_Real;

   subtype Array_Kind          is Entity_Kind range
       E_Array_Type ..
   --  E_Array_Subtype
   --  E_String_Type
   --  E_String_Subtype
   --  E_String_Literal_Subtype
       E_Slice_Subtype;

   subtype Record_Kind         is Entity_Kind range
       E_Record_Type ..
   --  E_Class_Type
   --  E_Class_Subtype
       E_Record_Subtype;

   subtype String_Kind         is Entity_Kind range
       E_String_Type ..
   --  E_String_Subtype
       E_String_Literal_Subtype;

   subtype Composite_Kind      is Entity_Kind range
       E_Array_Type ..
   --  E_Array_Subtype
   --  E_String_Type
   --  E_String_Subtype
   --  E_String_Literal_Subtype
   --  E_Slice_Subtype
   --  E_Record_Type
   --  E_Class_Type
   --  E_Class_Subtype
   --  E_Record_Subtype
   --  E_Incomplete_Type
   --  E_Private_Type
   --  E_Limited_Private_Type
   --  E_Limited_Type
   --  E_Task_Type
   --  E_Task_Subtype,
   --  E_Protected_Type,
       E_Protected_Subtype;

   ---------------
   -- Iterators --
   ---------------

   --  In addition to attributes that are stored as plain data, other
   --  attributes are procedural, and require some small amount of
   --  computation. Of course, from the point of view of a user of this
   --  package, the distinction is not visible (even the field information
   --  provided below should be disregarded, as it is subject to  change
   --  without notice!). A number of  attributes appear as lists: lists of
   --  formals,  lists of actuals, of discriminants, etc. For these, pairs
   --  of functions are defined, which take the form:

   --      function First_Thing (E : Enclosing_Construct) return Thing;
   --      function Next_Thing (T : Thing) return Thing;

   --  The end of iteration is always signalled by a value of Empty, so that
   --  loops over these chains invariably have the form:

   --      This : Thing;
   --      ...
   --      This := First_Thing (E);

   --      while Present (This) loop
   --         Do_Something_With (This);
   --        ...
   --        This := Next_Thing (This);
   --      end loop;

   -----------------------------------
   -- Handling of Check Suppression --
   -----------------------------------

   --  There are three ways that checks can be suppressed:

   --    1.  At the command line level. Package Opt contains global Boolean
   --        flags with names Suppress_Options.xxx_Checks, where xxx is the
   --        name of one of the checks that can be suppressed (excluding
   --        All_Checks, which is simply reflected by setting all the
   --        individual flags)

   --    2.  At the scope level. The body of Sem contains flags with names
   --        Suppress.xxx_Checks which are set to indicate that the given
   --        check is suppressed for the current scope. These flags are
   --        saved in the scope stack on entry to a scope and restored on
   --        exit from the scope.

   --    3.  At the entity level. Each entity contains a set of flags named
   --        Suppress_xxx_Checks which suppress the given check for that
   --        particularly entity (of course not all flags are meaningful for
   --        all entities).

   -------------------------------
   -- Handling of Discriminants --
   -------------------------------

   --  During semantic processing, discriminants are separate entities which
   --  reflect the semantic properties and allowed usage of discriminants in
   --  the language.

   --  In the case of discriminants used as bounds, the references are handled
   --  directly, since special processing is needed in any case. However, there
   --  are two circumstances in which discriminants are referenced in a quite
   --  general manner, like any other variables:

   --     In initialization expressions for records. Note that the expressions
   --     in Priority and Task_Stack_Size pragmas are effectively in this
   --     category, since these pragmas are converted to initialized record
   --     fields in the Task_Value_Type.

   --     In task and protected bodies, where the discriminant values may be
   --     referenced freely within these bodies.

   --  In both these cases, the discriminants must be treated essentially as
   --  objects. The following approach is used to simplify and minimize the
   --  special processing that is required.

   --  When a record type with discriminants is processed, the semantic
   --  processing creates the entities for the discriminants. It also creates
   --  an additional set of entities, called discriminals, one for each of
   --  the discriminants, and the Discriminal field of the discriminant entity
   --  points to this additional entity, which is initially created as an
   --  uninitialized (E_Void) entity.

   --  During expansion of expressions, any discriminant reference is replaced
   --  by a reference to the corresponding discriminal. When the initialization
   --  procedure for the record is created (there will always be one, since
   --  discriminants are present, see Exp_Ch3 for further details), the
   --  discriminals are used as the entities for the formal parameters of
   --  this initialization procedure. The references to these discriminants
   --  have already been replaced by references to these discriminals, which
   --  are now the formal parameters corresponding to the required objects.

   --  In the case of a task or protected body, the semantics similarly
   --  creates a set of discriminals for the discriminants of the task or
   --  protected type. When the procedure is created for the task body,
   --  the parameter passed in is a reference to the task value type, which
   --  contains the required discriminant values. The expander creates a
   --  set of declarations of the form:

   --      discriminal : constant dtype renames _Task.discriminant;

   --  where disriminal is the discriminal entity referenced by the task
   --  discriminant, and _Task is the task value passed in as the parameter.
   --  Again, any references to discriminants in the task body have been
   --  replaced by the discriminal reference, which is now an object that
   --  contains the required value.

   --  This approach for tasks means that two sets of discriminals are needed
   --  for a task type, one for the initialization procedure, and one for the
   --  task body. This works out nicely, since the semantics allocates one set
   --  for the task itself, and one set for the corresponding record.

   --  The one bit of trickiness arises in making sure that the right set of
   --  discriminals is used at the right time. First the task definition is
   --  processed. Any references to discriminants here are replaced by the
   --  the corresponding *task* discriminals (the record type doesn't even
   --  exist yet, since it is constructed as part of the expansion of the
   --  task declaration, which happens after the semantic processing of the
   --  task definition).

   --  Just before the record initialization routine is constructed, the
   --  expander exchanges the task and record discriminals. This has two
   --  effects. First the generation of the record initialization routine
   --  uses the discriminals that are now on the record, which is the set
   --  that used to be on the task, which is what we want.

   --  Second, a new set of (so far unused) discriminals is now on the task
   --  discriminants, and it is this set that will be used for expanding the
   --  task body, and also for the discriminal declarations at the start of
   --  the task body.

   -----------------------
   --  Access Functions --
   -----------------------

   --  All attributes are manipulated through a procedural interface. This
   --  section contains the functions used to obtain attribute values.

   function Corresponding_Task_Type (Id : Entity_Id) return Entity_Id;

   function Homonym (Id : Entity_Id) return Entity_Id;

   function Scope (Id : Entity_Id) return Entity_Id;

   function Next_Entity  (Id : Entity_Id) return Entity_Id;

   function First_Entity (Scope_Id : Entity_Id) return Entity_Id;

   function Last_Entity  (Scope_Id : Entity_Id) return Entity_Id;

   function Master_Id    (Access_Id : Entity_Id) return Entity_Id;

   function Is_Directly_Visible (Id : Entity_Id) return Boolean;
   function Is_Use_Visible      (Id : Entity_Id) return Boolean;

   function Has_Homonym (Id : Entity_Id) return Boolean;

   function Has_Completion (Id : Entity_Id) return Boolean;

   function Has_Master_Entity (Id : Entity_Id) return Boolean;

   function Has_Tasks (Id : Entity_Id) return Boolean;

   function Constant_Value (Constant_Id : Entity_Id) return Node_Id;

   function Renamed_Object (Id : Entity_Id) return Node_Id;

   function Root_Type (Type_Id : Entity_Id) return Entity_Id;

   function Esize (Type_Id : Entity_Id) return Uint;

   function Is_Delayed (E : Entity_Id) return Boolean;

   function Is_Generic_Type (Type_Id : Entity_Id) return Boolean;

   function Is_Frozen (Type_Id : Entity_Id) return Boolean;

   function First_Literal  (Type_Id : Entity_Id) return Entity_Id;
   function Next_Literal   (Id : Entity_Id) return Entity_Id;

   function Lit_Name_Table (Id : Entity_Id) return Entity_Id;

   function Scalar_Range (Type_Id : Entity_Id) return Node_Id;

   function Modulus (Type_Id : Entity_Id) return Uint;

   function Type_Low_Bound  (Type_Id : Entity_Id) return Node_Id;
   function Type_High_Bound (Type_Id : Entity_Id) return Node_Id;
   --  These functions return the low and high bounds of a scalar type.
   --  The returned values are literals for a base type, but maybe expressions
   --  in the case of a subtype with dynamic bounds.

   function Number_Dimensions (Array_Id : Entity_Id) return Pos;

   function Component_Type    (Array_Id : Entity_Id) return Entity_Id;

   function First_Index (Array_Id : Entity_Id) return Node_Id;
   --  By introducing implicit subtypes for the index constraints,
   --  we obtain the same structure for constrained and unconstrained
   --  arrays: subtype marks and discrete ranges are both lists of
   --  subtypes. First_Index returns the tree node corresponding to the
   --  Subtype_Marks (index_subtype_definition) in the case of
   --  E_Array_Type and Discrete_Subtype_Definitions (index constraint)
   --  in the case of an E_Array_Subtype. To get the defining occurence
   --  representing the type or subtype corresponding to the first index
   --  it is necessary to retrieve the Etype (First_Index (Array_Id)).

   function Next_Index (Index_Id : Entity_Id) return Node_Id;
   --  Return the tree node (in a way similar to First_Index) of the next
   --  index_subtype_definition or index_constraint.

   function Is_Constrained   (Type_Id  : Entity_Id) return Boolean;

   function Has_Discriminants (Type_Id  : Entity_Id) return Boolean;

   function First_Discriminant (Type_Id : Entity_Id) return Entity_Id;
   --  The discriminants are always the first entities declared in a
   --  record type except in the case of tagged types. For tagged
   --  types, the tag is prepended to the front of the entity chain
   --  during Expander phase of compilation. Therefore the First_Entity
   --  needs to be checked if it is a tag and if so the Next_Entity
   --  must be returned. This check must be explicit since even for
   --  tagged types the tag will not exist during the Sem phase.
   --  The caller of First_Discriminant should always make sure
   --  that the record type has discriminants (either by the explicit
   --  context or by calling Has_Discriminants). A debug check is made
   --  to trap violations of this rule.

   function Next_Discriminant  (Id     : Entity_Id) return Entity_Id;
   --  Follow the chain of declared entities as long as the kind of
   --  the entity corresponds to a discriminant. Note that the
   --  discriminants might be the only components of the record.

   function Init_Proc (Type_Id : Entity_Id) return Entity_Id;

   function Base_Init_Proc (Type_Id : Entity_Id) return Entity_Id;
   --  Gets the Init_Proc field from the base type of Type_Id, and also
   --  deals with going indirect through the Task_Value_Type field for
   --  tasks (tasks are initialized using the initialization routine
   --  for the associated record type).

   function Primitive_Operations (Id : Entity_Id) return Elist_Id;

   function Discriminant_Constraint (Type_Id : Entity_Id) return Elist_Id;

   function Internal_Access_Disp_Table (Component_Id : Entity_Id) 
     return Entity_Id;
   --  Only used on the component "_Tag" of a tagged type by Access_Disp_Table.

   function Access_Disp_Table (Rec_Id : Entity_Id) return Entity_Id;
   --  Gives the pointer to the Dispatch Table associated with a tagged type

   function Is_Dispatching_Operation (Overloaded_Id : Entity_Id)
     return Boolean;

   function Tag_Component (Rec_Id : Entity_Id) return Entity_Id;

   function First_Component (Rec_Id : Entity_Id) return Entity_Id;
   --  Follow the chain of declared entities for the record until the
   --  first component is found (one with an Ekind of E_Variable).
   --  The discriminants are skipped over. Note that the record might
   --  be null, in which case Empty will be returned.

   function Next_Component (Id : Entity_Id) return Entity_Id;
   --  Follow the chain of declared entities until one is found
   --  which corresponds to a component (Ekind is E_Variable). Any
   --  internal types generated from the subtype indications of the
   --  record components are skipped.

   function Directly_Designated_Type (Access_Id : Entity_Id) return Entity_Id;

   function Designated_Type (Access_Id : Entity_Id) return Entity_Id;
   --  Differs from Directly_Designated_Type in that if the access type refers
   --  to an incomplete type, and the full type is available, then this full
   --  type is returned instead of the incomplete type.

   function Full_Declaration (Private_Id : Entity_Id) return Entity_Id;

   function Discriminant_Checking_Func (Id : Entity_Id) return Entity_Id;

   function Discriminant_Default_Value (Id : Entity_Id) return Node_Id;
   function Discriminal                (Id : Entity_Id) return Node_Id;

   function Has_Address_Clause      (Id : Entity_Id) return Boolean;
   function Has_Size_Clause         (Id : Entity_Id) return Boolean;
   function Has_Storage_Size_Clause (Id : Entity_Id) return Boolean;

   function Is_Elementary_Type  (Type_Id : Entity_Id) return Boolean;
   function Is_Scalar_Type      (Type_Id : Entity_Id) return Boolean;
   function Is_Discrete_Type    (Type_Id : Entity_Id) return Boolean;
   function Is_Enumeration_Type (Type_Id : Entity_Id) return Boolean;
   function Is_Boolean_Type     (Type_Id : Entity_Id) return Boolean;
   function Is_Character_Type   (Type_Id : Entity_Id) return Boolean;
   function Is_Integer_Type     (Type_Id : Entity_Id) return Boolean;
   function Is_Real_Type        (Type_Id : Entity_Id) return Boolean;
   function Is_Fixed_Type       (Type_Id : Entity_Id) return Boolean;
   function Is_Float_Type       (Type_Id : Entity_Id) return Boolean;
   function Is_Numeric_Type     (Type_Id : Entity_Id) return Boolean;
   function Is_Composite_Type   (Type_Id : Entity_Id) return Boolean;
   function Is_Array_Type       (Type_Id : Entity_Id) return Boolean;
   function Is_String_Type      (Type_Id : Entity_Id) return Boolean;
   function Is_Record_Type      (Type_Id : Entity_Id) return Boolean;
   function Is_Access_Type      (Type_Id : Entity_Id) return Boolean;
   function Is_Task_Type        (Type_Id : Entity_Id) return Boolean;
   function Is_Tagged_Type      (Type_Id : Entity_Id) return Boolean;
   function Is_Task_Record_Type (Type_Id : Entity_Id) return Boolean;
   function Is_Protected_Type   (Type_Id : Entity_Id) return Boolean;
   function Is_Limited_Type     (Type_Id : Entity_Id) return Boolean;
   function Is_Incomplete_Type  (Type_Id : Entity_Id) return Boolean;
   function Is_Internal         (Type_Id : Entity_Id) return Boolean;
   function Is_Packed           (Type_Id : Entity_Id) return Boolean;
   function Is_Private_Type     (Type_Id : Entity_Id) return Boolean;
   function Is_Subprogram       (Type_Id : Entity_Id) return Boolean;
   function Is_Named_Number     (Type_Id : Entity_Id) return Boolean;

   function Is_Abstract (Id : Entity_Id) return Boolean;

   function Is_Aliased  (Id : Entity_Id) return Boolean;

   function Is_Imported (Id : Entity_Id) return Boolean;

   function Is_Inlined  (Id : Entity_Id) return Boolean;

   function Is_Private  (Id : Entity_Id) return Boolean;

   function Is_Public   (Id : Entity_Id) return Boolean;

   function Is_Pure     (Proc_Id : Entity_Id) return Boolean;

   function Is_Type     (Id : Entity_Id) return Boolean;

   function Is_Volatile (Id : Entity_Id) return Boolean;

   function Parent_Subtype (Id : Entity_Id) return Entity_Id;

   function Reachable (Label_Id : Entity_Id) return Boolean;

   function Slice_Range (Id : Entity_Id) return Node_Id;

   function String_Literal_Length (Id : Entity_Id) return Uint;

   function Subtype_Kind (K : Entity_Kind) return Entity_Kind;
   --  Given an entity_kind K this function returns the entity_kind
   --  corresponding to subtype kind of the type represented by K.
   --  So for example if K is E_Integer_Type then E_Integer_Subtype is
   --  returned. If K is already a subtype kind it itself is returned. An
   --  internal error is generated if no such correspondence exists for K.

   function Table_High_Bound (Id : Entity_Id) return Node_Id;

   function Needs_Discr_Check (Id : Entity_Id) return Boolean;

   function Has_Exit  (Loop_Id : Entity_Id) return Boolean;

   function First_Formal (Overloaded_Id : Entity_Id) return Entity_Id;
   --  Return first formal of a subprogram or subprogram type. The formals
   --  are the first entities declared in a subprogram, or in a subprogram
   --  type (the designated type of an Access_To_Subprogram definition.

   function Next_Formal (Formal_Id : Entity_Id) return Entity_Id;
   --  Returns next formal of a subprogram or subprogram type. Returns
   --  Empty if Formal_Id is the last formal.

   function Parameter_Mode   (Formal_Id    : Entity_Id) return Formal_Kind;

   function Next_Overloads   (Overloaded_Id : Entity_Id) return Entity_Id;

   function Alias (Overloaded_Id : Entity_Id) return Entity_Id;
   --  Return the parent subprogram of a derived subprogram

   function Needs_No_Actuals (Overloaded_Id : Entity_Id) return Boolean;

   function Interface_Name (Proc_Id : Entity_Id) return Node_Id;

   function Is_Intrinsic (Proc_Id : Entity_Id) return Boolean;

   function First_Actual (Node : Node_Id) return Node_Id;
   --  If positional parameters are present, then the first of these is
   --  returned, otherwise First_Named_Actual is used to find the first
   --  named parameter in declaration order. If there are no parameters,
   --  (i.e. the call is a parameterless call), then Empty is returned.
   --  Note that because the parameters are returned in declaration order,
   --  not in the call order, this does not correspond to simply taking the
   --  first entry in the Parameter_Associations list.

   function Next_Actual (Actual_Id : Node_Id) return Node_Id;
   --  Find next actual parameter in declaration order. Again since we use
   --  declaration order, rather than call order, this does not correspond
   --  to simply taking the next entry of the Parameter_Associations list.

   function Is_Overloadable (Id : Entity_Id) return Boolean;

   function Entry_Index (Id : Entity_Id) return Uint;

   function Enumeration_Pos (Enum_Id : Entity_Id) return Uint;

   function Enumeration_Rep (Enum_Id : Entity_Id) return Uint;

   function Has_Subprogram_Body (Subprogram_Id : Entity_Id) return Boolean;

   function Default_Value (In_Id : Entity_Id) return Node_Id;

   function Is_Package_Body (Package_Id : Entity_Id) return Boolean;

   function In_Private_Part (Package_Id : Entity_Id) return Boolean;

   function In_Use (Package_Id : Entity_Id) return Boolean;

   function Is_Private_Descendant (Id : Entity_Id) return Boolean;
   function Base_Type (Type_Id : Entity_Id) return Entity_Id;
   --  Return base type of a type or subtype. The base type of a type is
   --  the type itself. The base of a subtype is the type that it constrains

   function First_Private_Entity (Package_Id : Entity_Id) return Entity_Id;

   function Original_Record_Component (Id : Entity_Id) return Entity_Id;

   function Task_Value_Type    (Id : Entity_Id) return Entity_Id;

   function Suppress_Access_Checks        (E : Entity_Id) return Boolean;
   function Suppress_Accessibility_Checks (E : Entity_Id) return Boolean;
   function Suppress_Discriminant_Checks  (E : Entity_Id) return Boolean;
   function Suppress_Division_Checks      (E : Entity_Id) return Boolean;
   function Suppress_Elaboration_Checks   (E : Entity_Id) return Boolean;
   function Suppress_Index_Checks         (E : Entity_Id) return Boolean;
   function Suppress_Length_Checks        (E : Entity_Id) return Boolean;
   function Suppress_Overflow_Checks      (E : Entity_Id) return Boolean;
   function Suppress_Range_Checks         (E : Entity_Id) return Boolean;
   function Suppress_Storage_Checks       (E : Entity_Id) return Boolean;
   function Suppress_Tag_Checks           (E : Entity_Id) return Boolean;

   function Is_Class_Type (T : Entity_Id) return Boolean;
   --  true for E_Class_Type and E_Class_Subtype

   function Classwide_Type (T : Entity_Id) return Entity_Id;
   --  given  a tagged type,  find the implicitly defined classwide
   --  type for it.

   function Equivalent_Type (Sub_Class : Entity_Id) return Entity_Id;

   ------------------------------------------
   --  Procedures to Set Entity Attributes --
   ------------------------------------------

   procedure Set_Corresponding_Task_Type (Id : Entity_Id; T : Entity_Id);

   procedure Set_Scope (Id : Entity_Id; S : Entity_Id);

   procedure Set_Homonym (Id : Entity_Id; T : Entity_Id);

   procedure Set_Esize (Type_Id : Entity_Id; S : Uint);

   procedure Set_Is_Delayed (E : Entity_Id; Status : Boolean := True);

   procedure Set_Is_Generic_Type (Type_Id : Entity_Id;
                                  Status  : Boolean := True);

   procedure Set_Is_Frozen (Type_Id : Entity_Id;
                            Status  : Boolean := True);

   procedure Set_Has_Address_Clause (Id : Entity_Id; Status : Boolean := True);

   procedure Set_Has_Size_Clause (Id : Entity_Id; Status : Boolean := True);

   procedure Set_Has_Storage_Size_Clause (Id : Entity_Id; 
                                          Status : Boolean := True);

   procedure Set_Has_Homonym (Id : Entity_Id; Status : Boolean := True);

   procedure Set_Has_Completion (Id : Entity_Id; Status : Boolean := True);

   procedure Set_Has_Master_Entity (Id : Entity_Id; Status : Boolean := True);

   procedure Set_Has_Tasks (Id : Entity_Id; Status : Boolean := True);

   procedure Set_First_Entity (Scope_Id : Entity_Id; Id : Entity_Id);

   procedure Set_Next_Entity (Prev : Entity_Id; Id : Entity_Id);

   procedure Set_Renamed_Object (Id : Entity_Id; Object : Node_Id);

   procedure Set_Scalar_Range (Type_Id : Entity_Id; R_Node : Node_Id);

   procedure Set_Modulus (Type_Id : Entity_Id; Mod_Value : Uint);

   procedure Set_Lit_Name_Table (Type_Id : Entity_Id; Table : Entity_Id);

   procedure Set_First_Index (Array_Id : Entity_Id; Index : Node_Id);

   procedure Set_First_Literal (Type_Id : Entity_Id; Id : Entity_Id);

   procedure Set_Is_Constrained (Type_Id : Entity_Id;
                                 Status  : Boolean := True);

   procedure Set_Is_Internal (Type_Id : Entity_Id; Status : Boolean := True);

   procedure Set_Is_Limited_Type  (Type_Id : Entity_Id;
                                   Status  : Boolean := True);

   procedure Set_Is_Packed (Type_Id : Entity_Id; Status : Boolean := True);

   procedure Set_Is_Private_Type
                           (Type_Id : Entity_Id; Status : Boolean := True);

   procedure Set_Is_Private  (Id : Entity_Id; Status : Boolean := True);

   procedure Set_Is_Public   (Id : Entity_Id; Status : Boolean := True);

   procedure Set_Is_Abstract (Id : Entity_Id; Status : Boolean := True);

   procedure Set_Is_Aliased  (Id : Entity_Id; Status : Boolean := True);

   procedure Set_Is_Imported (Id : Entity_Id; Status : Boolean := True);

   procedure Set_Is_Inlined  (Id : Entity_Id; Status : Boolean := True);

   procedure Set_Is_Pure (Proc_Id : Entity_Id; Status : Boolean := True);

   procedure Set_Is_Volatile (Id : Entity_Id; Status : Boolean := True);

   procedure Set_Is_Tagged_Type
     (Type_Id : Entity_Id; Status : Boolean := True);

   procedure Set_Is_Task_Record_Type
     (Type_Id : Entity_Id; Status : Boolean := True);

   procedure Set_Has_Discriminants
     (Type_Id : Entity_Id; Status : Boolean := True);

   procedure Set_Discriminant_Constraint
     (Type_Id : Entity_Id; Elist  : Elist_Id);

   procedure Set_Primitive_Operations (Id : Entity_Id; Elist : Elist_Id);

   procedure Set_Internal_Access_Disp_Table 
     (Component_Id : Entity_Id; Id : Entity_Id);

   procedure Set_Access_Disp_Table (Rec_Id : Entity_Id; Id : Entity_Id);

   procedure Set_Is_Dispatching_Operation (Overloaded_Id : Entity_Id;
                                           Status        : Boolean := True);

   procedure Set_Init_Proc (Type_Id : Entity_Id; P : Entity_Id);

   procedure Set_Component_Type (Array_Id  : Entity_Id; Ct : Entity_Id);

   procedure Set_Directly_Designated_Type (Access_Id : Entity_Id;
                                           Dt        : Entity_Id);

   procedure Set_Parent_Subtype (Id : Entity_Id; Parent_Id : Entity_Id);

   procedure Set_Reachable (Label_Id : Entity_Id; Status : Boolean := True);

   procedure Set_Needs_Discr_Check (Id : Entity_Id; Status : Boolean := True);

   procedure Set_Has_Exit (Loop_Id : Entity_Id; Status : Boolean := True);

   procedure Set_Full_Declaration (Private_Id : Entity_Id; Fd : Entity_Id);

   procedure Set_Discriminant_Default_Value (Id : Entity_Id; Val : Node_Id);

   procedure Set_Discriminal (Id : Entity_Id; Formal : Entity_Id);

   procedure Set_Discriminant_Checking_Func (Id : Entity_Id;
                                             Proc_Id : Entity_Id);

   procedure Set_Default_Value (In_Id : Entity_Id; Val : Node_Id);

   procedure Set_Alias (Overloaded_Id : Entity_Id; Id : Entity_Id);

   procedure Set_Needs_No_Actuals (Overloaded_Id : Entity_Id;
                                   Status        : Boolean := True);

   procedure Set_Interface_Name (Proc_Id : Entity_Id; Lit_Node : Node_Id);
   procedure Set_Is_Intrinsic (Proc_Id : Entity_Id);

   procedure Set_Next_Actual (Ass1_Id : Node_Id; Ass2_Id : Node_Id);
   --  The arguments may be parameter associations, whose descendants
   --  are the optional formal name and the actual parameter. Positional
   --  parameters are already members of a list, and do not need to be
   --  chained separately.

   procedure Set_Entry_Index (Id : Entity_Id; Ev : Uint);

   procedure Set_Enumeration_Pos (Enum_Id : Entity_Id; Ev : Uint);

   procedure Set_Enumeration_Rep (Enum_Id : Entity_Id; Ev : Uint);

   procedure Set_Slice_Range (Id : Entity_Id; R_Node : Node_Id);

   procedure Set_String_Literal_Length (Id : Entity_Id; Val : Uint);

   procedure Set_Table_High_Bound (Id : Entity_Id; Val : Node_Id);

   procedure Set_Has_Subprogram_Body (Subprogram_Id : Entity_Id;
                                      Status        : Boolean := True);

   procedure Set_Is_Package_Body (Package_Id : Entity_Id;
                                  Status     : Boolean := True);

   procedure Set_In_Private_Part (Package_Id : Entity_Id;
                                  Status     : Boolean := True);

   procedure Set_In_Use (Package_Id : Entity_Id; Status : Boolean := True);

   procedure Set_Is_Private_Descendant (Id      : Entity_Id;
                                        Status  : Boolean := True);

   procedure Set_Last_Entity (Scope_Id : Entity_Id; Id : Entity_Id);

   procedure Set_Master_Id (Access_Id : Entity_Id; Id : Entity_Id);

   procedure Append_Entity (Id : Entity_Id; Scope_Id : Entity_Id);
   --  Add an entity to the list of entities declared in the given scope

   procedure Set_Is_Directly_Visible (Id     : Entity_Id;
                                      Status : Boolean := True);

   procedure Set_Is_Use_Visible (Id : Entity_Id; Status : Boolean := True);

   procedure Set_First_Private_Entity (Package_Id : Entity_Id; Id : Entity_Id);

   procedure Set_Original_Record_Component
     (Id : Entity_Id; Orig_Id : Entity_Id);

   procedure Set_Task_Value_Type (Id : Entity_Id; Ent : Entity_Id);

   procedure Write_Entity_Info (Id : Entity_Id; Prefix : Str);
   --  A debugging procedure to write out information about an entity

   procedure Set_Suppress_Access_Checks 
     (E : Entity_Id; V : Boolean := True);

   procedure Set_Suppress_Accessibility_Checks
     (E : Entity_Id; V : Boolean := True);

   procedure Set_Suppress_Discriminant_Checks 
     (E : Entity_Id; V : Boolean := True);

   procedure Set_Suppress_Division_Checks
     (E : Entity_Id; V : Boolean := True);

   procedure Set_Suppress_Elaboration_Checks
     (E : Entity_Id; V : Boolean := True);

   procedure Set_Suppress_Index_Checks 
     (E : Entity_Id; V : Boolean := True);

   procedure Set_Suppress_Length_Checks
     (E : Entity_Id; V : Boolean := True);

   procedure Set_Suppress_Overflow_Checks
     (E : Entity_Id; V : Boolean := True);

   procedure Set_Suppress_Range_Checks 
     (E : Entity_Id; V : Boolean := True);

   procedure Set_Suppress_Storage_Checks
     (E : Entity_Id; V : Boolean := True);

   procedure Set_Suppress_Tag_Checks
     (E : Entity_Id; V : Boolean := True);

   procedure Set_Equivalent_Type (Sub_Class : Entity_Id; Typ : Entity_Id);

   ---------------------
   --  Inline Pragmas --
   ---------------------

private
   pragma Inline (Access_Disp_Table);
   pragma Inline (Alias);
   pragma Inline (Component_Type);
   pragma Inline (Corresponding_Task_Type);
   pragma Inline (Default_Value);
   pragma Inline (Directly_Designated_Type);
   pragma Inline (Discriminal);
   pragma Inline (Discriminant_Checking_Func);
   pragma Inline (Discriminant_Constraint);
   pragma Inline (Discriminant_Default_Value);
   pragma Inline (Enumeration_Pos);
   pragma Inline (Enumeration_Rep);
   pragma Inline (Equivalent_Type);
   pragma Inline (Esize);
   pragma Inline (First_Entity);
   pragma Inline (First_Index);
   pragma Inline (First_Literal);
   pragma Inline (First_Private_Entity);
   pragma Inline (Has_Address_Clause);
   pragma Inline (Has_Completion);
   pragma Inline (Has_Discriminants);
   pragma Inline (Has_Exit);
   pragma Inline (Has_Homonym);
   pragma Inline (Has_Master_Entity);
   pragma Inline (Has_Size_Clause);
   pragma Inline (Has_Storage_Size_Clause);
   pragma Inline (Has_Subprogram_Body);
   pragma Inline (Has_Tasks);
   pragma Inline (Homonym);
   pragma Inline (In_Private_Part);
   pragma Inline (In_Use);
   pragma Inline (Init_Proc);
   pragma Inline (Interface_Name);
   pragma Inline (Internal_Access_Disp_Table);
   pragma Inline (Is_Abstract);
   pragma Inline (Is_Access_Type);
   pragma Inline (Is_Aliased);
   pragma Inline (Is_Array_Type);
   pragma Inline (Is_Boolean_Type);
   pragma Inline (Is_Character_Type);
   pragma Inline (Is_Composite_Type);
   pragma Inline (Is_Constrained);
   pragma Inline (Is_Delayed);
   pragma Inline (Is_Directly_Visible);
   pragma Inline (Is_Discrete_Type);
   pragma Inline (Is_Dispatching_Operation);
   pragma Inline (Is_Enumeration_Type);
   pragma Inline (Is_Fixed_Type);
   pragma Inline (Is_Float_Type);
   pragma Inline (Is_Frozen);
   pragma Inline (Is_Generic_Type);
   pragma Inline (Is_Imported);
   pragma Inline (Is_Incomplete_Type);
   pragma Inline (Is_Inlined);
   pragma Inline (Is_Integer_Type);
   pragma Inline (Is_Internal);
   pragma Inline (Is_Intrinsic);
   pragma Inline (Is_Limited_Type);
   pragma Inline (Is_Named_Number);
   pragma Inline (Is_Numeric_Type);
   pragma Inline (Is_Overloadable);
   pragma Inline (Is_Package_Body);
   pragma Inline (Is_Packed);
   pragma Inline (Is_Private);
   pragma Inline (Is_Private_Descendant);
   pragma Inline (Is_Private_Type);
   pragma Inline (Is_Protected_Type);
   pragma Inline (Is_Public);
   pragma Inline (Is_Pure);
   pragma Inline (Is_Real_Type);
   pragma Inline (Is_Record_Type);
   pragma Inline (Is_Scalar_Type);
   pragma Inline (Is_Subprogram);
   pragma Inline (Is_Tagged_Type);
   pragma Inline (Is_Task_Record_Type);
   pragma Inline (Is_Task_Type);
   pragma Inline (Is_Type);
   pragma Inline (Is_Use_Visible);
   pragma Inline (Is_Volatile);
   pragma Inline (Last_Entity);
   pragma Inline (Lit_Name_Table);
   pragma Inline (Master_Id);
   pragma Inline (Modulus);
   pragma Inline (Needs_Discr_Check);
   pragma Inline (Needs_No_Actuals);
   pragma Inline (Next_Entity);
   pragma Inline (Next_Index);
   pragma Inline (Next_Literal);
   pragma Inline (Next_Overloads);
   pragma Inline (Original_Record_Component);
   pragma Inline (Parameter_Mode);
   pragma Inline (Parent_Subtype);
   pragma Inline (Primitive_Operations);
   pragma Inline (Reachable);
   pragma Inline (Renamed_Object);
   pragma Inline (Scalar_Range);
   pragma Inline (Scope);
   pragma Inline (Slice_Range);
   pragma Inline (String_Literal_Length);
   pragma Inline (Suppress_Access_Checks);
   pragma Inline (Suppress_Accessibility_Checks);
   pragma Inline (Suppress_Discriminant_Checks);
   pragma Inline (Suppress_Division_Checks);
   pragma Inline (Suppress_Elaboration_Checks);
   pragma Inline (Suppress_Index_Checks);
   pragma Inline (Suppress_Length_Checks);
   pragma Inline (Suppress_Overflow_Checks);
   pragma Inline (Suppress_Range_Checks);
   pragma Inline (Suppress_Storage_Checks);
   pragma Inline (Suppress_Tag_Checks);
   pragma Inline (Table_High_Bound);
   pragma Inline (Task_Value_Type);
   pragma Inline (Type_High_Bound);
   pragma Inline (Type_Low_Bound);
end Einfo;
