------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S I N F O                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.128 $                            --
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

package Sinfo is

--  This package defines the structure of the abstract syntax tree. The Tree
--  package provides a basic tree structure. Sinfo describes how this
--  structure is used to represent the syntax of an Ada program.

--  Note: the grammar used here is taken from Version 3.0 of the AARM, dated
--  8 July 1993 14:33. The grammar in the AARM is followed very closely in
--  the tree design, and is repeated as part of this document. It also
--  incorporates changes (in the area of abstract types) made after the
--  recent WG9 meeting in Boston.

--  The tree contains not only the full syntactic representation of the
--  program, but also the results of semantic analysis. In particular, the
--  nodes for defining identifiers, defining character literals and defining
--  operator symbols, collectively referred to as entities, represent what
--  would normally be regarded as the symbol table information. In addition
--  a number of the tree nodes contain semantic information.

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in this C header file sinfo.h
--  which is created automatically from sinfo_.ada using xsinfo.spt.

   ---------------------------------
   -- Making Changes to This File --
   ---------------------------------

   --  If changes are made to this file, a number of related steps must be
   --  carried out to ensure consistency. First, if a field access function
   --  is added, it appears in six places:

   --    The documentation associated with the node
   --    The spec of the access function in sinfo.ads
   --    The body of the access function in sinfo.adb
   --    The spec of the set procedure in sinfo.ads
   --    The body of the set procedure in sinfo.adb
   --    The pragma Inline at the end of sinfo.ads

   --  The field chosen must be consistent in all places, and, for a node
   --  that is a subexpression, must not overlap any of the standard
   --  expression fields. In the body, the calls to the Dcheck_Node debug
   --  procedure will need cross-references adding in alphabetical order.

   --  If a new tree node is added, then the following changes are made

   --    Add it to the documentation in the appropriate place
   --    Add its fields to this documentation section
   --    Define it in the appropriate classification in Node_Kind
   --    In the body (sinfo), add entries to the Dcheck calls for all
   --     its fields (except standard expression fields) to include
   --     the new node in the debug cross-reference list
   --    Add an appropriate section to the case statement in sprint.adb
   --    Add an appropriate section to the case statement in sem.adb
   --    For a subexpression, add an appropriate sections to the case
   --     statement in sem_eval.adb
   --    For a subexpression, add an appropriate sections to the case
   --     statement in sem_res.adb

   --  Finally, four utility programs must be run:

   --    Run csinfo.spt to check that you have made the changes consistently.
   --     It checks most of the rules given above, with clear error messages.
   --     This utility reads sinfo.ads and sinfo.adb and generates a report
   --     to standard output.

   --    Run xsinfo.spt to create a-sinfo.h, the corresponding C header. This
   --     utility reads sinfo.ads and generates a-sinfo.h. Note that it
   --     does not need to read sinfo.adb, since the contents of the body
   --     are algorithmically determinable from the spec.

   --    Run xtreeprs.spt to create treeprs.ads, an updated version of
   --     the module that is used to drive the tree print routine. This
   --     utility reads (but does not modify) treeprs.adt, the template
   --     that provides the basic structure of the file, and then fills in
   --     the data from the comments in sinfo.ads.

   --    Run xnmake.spt to create nmake.ads and nmake.adb, the package body
   --     and spec of the Nmake package which contains functions for
   --     constructing nodes.

   --  Note: sometime we could write a utility that actually generated the
   --  body of sinfo from the spec instead of simply checking it, since, as
   --  noted above, the contents of the body can be determined from the spec.

   --------------------------------
   -- Implicit Nodes in the Tree --
   --------------------------------

   --  Generally the structure of the tree very closely follows the grammar
   --  as defined in the RM. However, certain nodes are omitted to save
   --  space and simplify semantic processing. Two general classes of such
   --  omitted nodes are as follows:

   --   If the only possibilities for a non-terminal are one or more other
   --   non terminals (i.e. the rule is a "skinny" rule), then usually the
   --   corresponding node is omitted from the tree, and the target construct
   --   appears directly. For example, a real type definition is either a
   --   floating point definition or a fixed point definition. No explicit
   --   node appears for real type definition. Instead either the floating
   --   point definition or fixed point definition appears directly.

   --   If a non-terminal corresponds to a list of some other non-terminal
   --   (possibly with separating punctuation), then usually it is omitted
   --   from the tree, and a list of components appears instead. For
   --   example, sequence of statements does not appear explicitly in the
   --   tree. Instead a list of statements appears directly.

   --  Some additional cases of omitted nodes occur and are documented
   --  individually. In particular, many nodes are omitted in the tree
   --  generated for an expression.

   -------------------------------------------
   -- Handling of Defining Identifier Lists --
   -------------------------------------------

   --  In several declarative forms in the syntax, lists of defining
   --  identifiers appear (object declarations, component declarations,
   --  number declarations etc.)

   --  The semantics of such statements are equivalent to a series of
   --  identical declarations of single defining identifiers (except that
   --  conformance checks require the same grouping of identifiers in the
   --  parameter case).

   --  To simplify semantic processing, the parser breaks down such multiple
   --  declaration cases into sequences of single declarations, duplicating
   --  type and initialization information as required. The flags More_Ids
   --  and Prev_Ids are used to record the original form of the source in
   --  the case where the original source used a list of names, More_Ids
   --  being set on all but the last name and Prev_Ids being set on all
   --  but the first name. These flags are used to reconstruct the original
   --  source (e.g. in the Sprint package), and also are included in the
   --  conformance checks, but otherwise have no semantic significance.

   --  Note: the reason that we use More_Ids and Prev_Ids rather than
   --  First_Name and Last_Name flags is so that the flags are off in the
   --  normal one identifier case, which minimizes tree print output.

   -----------------------
   -- Use of Node Lists --
   -----------------------

   --  With a few exceptions, if a construction of the form {non-terminal}
   --  appears in the tree, lists are used in the corresponding tree node
   --  (see package Atree for handling of node lists). In this case a field of
   --  the parent node points to a list of nodes for the non-terminal. The
   --  field name for such fields has a plural name which always ends in "s". 
   --  For example, a case statement has a field Alternatives pointing to a
   --  list of case statement alternative nodes.

   --  Only fields pointing to lists have names ending in "s", so generally
   --  the structure is strongly typed, fields not ending in s point to
   --  single nodes, and fields ending in s point to lists. 

   --  The following example shows how a traversal of a list is written. We
   --  suppose here that Stmt points to a N_Case_Statement node which has
   --  a list field called Alternatives:

   --   Alt := First (Alternatives (Decl));
   --   while Present (Alt) loop
   --      ..
   --      -- processing for case statement alternative Alt
   --      ..
   --      Alt := Next (Alt);
   --   end loop;

   --  The Present function tests for Empty, which in this case signals the
   --  end of the list. First returns Empty immediately if the list is empty.

   --  The exceptions to this rule occur with {DEFINING_IDENTIFIERS} in all
   --  contexts, which is handled as described in the previous section, and
   --  with {,library_unit_NAME} in the N_With_Clause mode, which is handled
   --  using the First_Name and Last_Name flags, as further detailed in the
   --  description of the N_With_Clause node.

   ---------------------
   -- Optional Fields --
   ---------------------

   --  Fields which correspond to a section of the syntax enclosed in square
   --  brackets are generally omitted (and the corresponding field set to
   --  Empty for a node, or No_List for a list). The documentation of such
   --  fields notes these cases. One exception to this rule occurs in the
   --  case of possibly empty statement sequences (such as the sequence of
   --  statements in an entry call alternative). Such cases appear in the
   --  syntax rules as [SEQUENCE_OF_STATEMENTS] and the fields corresponding
   --  to such optional statement sequences always contain an empty list (not
   --  No_List) if no statements are present.

   --  Note: the utility program that constructs the body and spec of the
   --  Nmake package relies on the format of the comments to determine if
   --  a field should have a default value in the corresponding make routine.
   --  The rule is that if the first line of the description of the field
   --  contains the string "(set to xxx if", then a default value of xxx is
   --  provided for this field in the corresponding Make_yyy routine.

   -----------------------------------
   -- Note on Body/Spec Terminology --
   -----------------------------------

   --  In informal discussions about Ada, it is customary to refer to package
   --  and subprogram specs and bodies. However, this is not technically
   --  correct, what is normally referred to as a spec or specification is in
   --  fact a package declaration or subprogram declaration. We are careful
   --  in GNAT to use the correct terminology and in particular, the full
   --  word specification is never used as an incorrect substitute for
   --  declaration. The structure and terminology used in the tree also
   --  reflects the grammar and thus uses declaration and specification in
   --  the technically correct manner.

   --  However, there are contexts in which the informal terminology is
   --  useful. We have the word "body" to refer to the entity declared by
   --  the declaration of a unit body, and in some contexts we need a
   --  similar term to refer to the entity declared by the package or
   --  subprogram declaration, and simply using declaration can be confusing
   --  since the body also has a declaration.

   --  An example of such a context is the link between the package body
   --  and its declaration. With_Declaration is confusing, since
   --  the package body itself is a declaration.

   --  To deal with this problem, we reserve the informal term Spec, i.e.
   --  the popular abbreviation used in this context, to refer to the entity
   --  declared by the package or subprogram declaration. So in the above
   --  example case, the field in the body is called With_Spec.

   --  Another important context for the use of the word Spec is in error
   --  messages, where a hyper-correct use of declaration would be confusing
   --  to a typical Ada programmer, and even for an expert programmer can
   --  cause confusion since the body has a declaration as well.

   --  So, to summarize:

   --     Declaration    always refers to the syntactic entity that is called
   --                    a declaration. In particular, subprogram declaration
   --                    and package declaration are used to describe the
   --                    syntactic entity that includes the semicolon.

   --     Specification  always refers to the syntactic entity that is called
   --                    a specification. In particular, the terms procedure
   --                    specification, function specification, package
   --                    specification, subprogram specification always refer
   --                    to the syntactic entity that has no semicolon.

   --     Spec           is an informal term, used to refer to the entity
   --                    that is declared by a task declaration, protected
   --                    declaration, generic declaration, subprogram
   --                    declaration or package declaration.

   --  This convention is followed throughout the GNAT documentation
   --  both internal and external, and in all error message text.

   ------------------------
   -- Internal Use Nodes --
   ------------------------

   --  These are Node_Kind settings used in the internal implementation
   --  which are not logically part of the specification.

   --  N_Unused_At_Start
   --  Completely unused entry at the start of the enumeration type. This
   --  is inserted so that no legitimate value is zero, which helps to get
   --  better debugging behavior, since zero is a likely uninitialized value).

   --  N_Unused_At_End
   --  Completely unused entry at the end of the enumeration type. This is
   --  handy so that arrays with Node_Kind as the index type have an extra
   --  entry at the end (see for example the use of the Pchar_Pos_Array in
   --  Treepr, where the extra entry provides the limit value when dealing
   --  with the last used entry in the array).

   ---------------------
   -- Syntactic Nodes --
   ---------------------

   --  In the following node definitions, all fields, both syntactic and
   --  semantic, are documented, as well as fields that are set by the
   --  library processing. The one exception is in the case of entities
   --  (defining indentifiers, character literals and operator symbols),
   --  where the usage of the fields depends on the entity kind.

   --  Some fields are marked -Lib, which means that they are the result
   --  of the library processing. The description of the usage of these
   --  fields appears in a following section in this file.

   --  In the node definitions, three common sets of fields are abbreviated
   --  to save both space in the documentation, and also space in the string
   --  (defined in Tree_Print_Strings) used to print trees. The following
   --  abbreviations are used:

   --    "plus fields for binary operator"
   --       Chars (Name1) contains the Name_Id for the operator
   --       Left_Opnd (Node2) is the left operand expression
   --       Right_Opnd (Node3) is the right operand expression
   --       Entity (Node4-Sem)
   --       Do_Overflow_Check (Flag2-Sem)

   --    "plus fields for unary operator"
   --       Chars (Name1) contains the Name_Id for the operator
   --       Right_Opnd (Node3) is the right operand expression
   --       Entity (Node4-Sem)
   --       Do_Overflow_Check (Flag2-Sem)

   --    "plus fields for expression"
   --       Parens (Flag1) set if parenthesized
   --       Etype (Node5-Sem) is the type of the expression
   --       Is_Overloaded (Flag5-Sem) used during overload resolution
   --       Is_Static (Flag6-Sem) set for static expression
   --       Is_Evaluated (Flag7-Sem) set if expression evaluated
   --       Has_No_Side_Effects (Flag8-Sem) set if known to be SE free
   --       Do_Range_Check (Flag9-Sem) set if a range check is required
   --       Evaluate_Once (Flag10-Sem) set to evaluate expression only once

   --  Node_Kind is the type used in the Nkind field to indicate the node
   --  kind. The actual definition of this type is given later (the reason
   --  for this is that we want the descriptions ordered by logical chapter
   --  in the RM, but the type definition is reordered to facilitate the
   --  definition of some subtype ranges. The individual descriptions of
   --  the nodes show how the various fields are used in each node kind,
   --  as well as providing logical names for the fields. Functions and
   --  procedures are provided for accessing and setting these fields
   --  using these logical names.

   ------------------------
   -- Common Flag Fields --
   ------------------------

   --  The following flag fields appear in all nodes

   --  Analyzed (Flag19-Sem)
   --    This flag is used to indicate that a node (and all its children
   --    have been analyzed. It is used to avoid reanalysis of a node that
   --    has already been analyzed, both for efficiency and functional
   --    correctness reasons.

   --  Error_Posted (Flag20)
   --    This flag is used to avoid multiple error messages being posted
   --    on or referring to the same node. This flag is set if an error
   --    message refers to a node or is posted on its source location,
   --    and has the effect of inhibiting further messages involving
   --    this same node.

   ------------------------------------
   -- Description of Semantic Fields --
   ------------------------------------

   --  The meaning of the syntactic fields is generally clear from their
   --  names without any further description, since the names are chosen
   --  to correspond very closely to the syntax in the reference manual.
   --  This section describes the usage of the semantic fields, which are
   --  used to contain additional information determined during semantic
   --  analysis.

   --  Activation_Chain_Entity (Node2-Sem)
   --    This is used in tree nodes representing task activators (blocks,
   --    subprogram bodies, package declarations, and task bodies). It is
   --    initially Empty, and then gets set to point to the entity for the
   --    declared Activation_Chain variable when the first task is declared.
   --    When tasks are declared in the corresponding declarative region
   --    this entity is located by name (its name is always _Chain) and
   --    the declared tasks are added to the chain.

   --  Acts_As_Spec (Flag4-Sem)
   --    A flag set in the N_Subprogram_Body node for a subprogram body
   --    which is acting as its own spec. This flag also appears in the
   --    compilation unit node at the library level for such a subprogram
   --    (see further description in spec of Lib package).

   --  Assignment_OK (Flag5-Sem)
   --     This flag is set in an assignment node to permit an assignment to
   --     a discriminant without generating a semantic error. Such assignments
   --     are not permitted at the Ada source level, but they occur during the
   --     creation of subsidiary routines for copying and initializing variant
   --     record values. This flag is also used to permit an assignment to
   --     a limited private type (used for initialization of task values).

   --  Body_Required (Flag3-Sem)
   --     A flag that appears in the N_Compilation_Unit node for a package
   --     spec to indicate that a body is required. In Ada 9X, if this flag
   --     is not set, then a body may not be present. In Ada 83, if this flag
   --     is not set, then a body is optional.

   --  Controlling_Argument (Node1-Sem)
   --     This field is set in procedure and function call nodes if the call
   --     is a dispatching call (it is Empty for a non-dispatching call).
   --     It indicates the source of the controlling tag for the call. For
   --     Procedure calls, the Controlling_Argument is one of the actuals.
   --     For a function that has a dispatching result, it is an entity in
   --     the context of the call that can provide a tag, or else it is the
   --     tag of the root type of the class.

   --  Corresponding_Body (Node5-Sem)
   --     This field is set in subprogram declarations, where it is needed
   --     if a pragma Inline is present and the subprogram is called, in
   --     generic declarations if the generic is instantiated, and also in
   --     package declarations that contain inlined subprograms that are
   --     called, or generic declarations that are instantiated. It points
   --     to the defining entity for the corresponding body.

   --  Corresponding_Spec (Node5-Sem)
   --     This field is set in subprogram and package body nodes, where it
   --     points to the defining entity for the corresponding spec, and also
   --     in the N_With_Clause node, where it points to the defining entity
   --     for the with'ed spec. It is Empty if there is no corresponding
   --     spec in the case where a subprogram body serves as its own spec.
   --     This field is also set in a subprogram renaming declaration when
   --     it is a Renaming_As_Body and in a task body (where it points to
   --     the task type eneity).

   --  Debug_Statement (Node3-Sem)
   --    This field is present in an N_Pragma node. It is used only for
   --    a Debug pragma or pragma Assert with a second parameter. The
   --    parameter is of the form of an expression, as required by the
   --    pragma syntax, but is actually a procedure call. To simplify
   --    semantic processing, the parser creates a copy of the argument
   --    rearranged into a procedure call statement and places it in the
   --    Debug_Statement field.

   --  Do_Access_Check (Flag2-Sem)
   --     This flag is set on N_Explicit_Deference, N_Indexed_Component or
   --     N_Selected_Component nodes to indicate that the Prefix field of
   --     the node points to a pointer whose value must be checked for null
   --     before carrying out the operation. The actual check is dealt with
   --     by Gigi (all the front end does is to set the flag).

   --  Do_Accessibility_Check (Flag3-Sem)
   --     This flag is set on N_Parameter_Specification nodesto indicate that
   --     that an accessibility check is required for the parameter. It is
   --     not yet decided who takes care of this check (TBD).

   --  Do_Discriminant_Check (Flag3-Sem)
   --     This flag is set on N_Selected_Component nodes to indicate that a
   --     discriminant check is required using the discriminant check routine
   --     associated with the selector. The actual check is dealt with by
   --     Gigi (all the front end does is to set the flag).

   --  Do_Division_Check (Flag3-Sem)
   --     This flag is set on a division operator (/ mod rem) to indicate
   --     that a zero divide check is required. The actual check is dealt
   --     with by the backend (all the front end does is to set the flag).

   --  Do_Elaboration_Check (Flag2-Sem)
   --     This flag is set in a call (N_Entry_Call_Statement, N_Function_Call
   --     or N_Procedure_Call_Statement) to indicate that an elaboration
   --     check is required before the call can proceed. It is not yet
   --     determined who deals with this flag (TBD).

   --  Do_Length_Check (Flag4-Sem)
   --     This flag is set in an N_Assignment_Statement, N_Op_And, N_Op_Or,
   --     N_Op_Xor, or N_Type_Conversion node to indicate that a length check
   --     is required. It is not determined who deals with this flag (TBD).

   --  Do_Overflow_Check (Flag2-Sem)
   --     This flag is set on an operator where an overflow check is required
   --     on the operation. The actual check is dealt with by the backend
   --     (all the front end does is to set the flag). The other case where
   --     this flag is used is on a Type_Conversion node. In this case it
   --     means that the type conversion is from one base type to another,
   --     and the value may not fit in the target base type. See also the
   --     description of Do_Range_Check for this case.

   --  Do_Range_Check (Flag9-Sem)
   --     This flag is set on an expression which appears in a context where
   --     a range check is required. The target type is clear from the
   --     context. The contexts in which this flag can appear are limited to
   --     the following.

   --       Right side of an assignment. In this case the target type is
   --       taken from the left side of the assignment, which is referenced
   --       by the Name of the N_Assignment_Statement node.

   --       Subscript expressions in an indexed component. In this case the
   --       target type is determined from the type of the array, which is
   --       referenced by the Prefix of the N_Indexed_Component node.

   --       Parameter expression for an IN parameter, appearing either
   --       directly in the Parameter_Associations list of a call or as
   --       the Expression of an N_Parameter_Association node that appears
   --       in this list. In either case, the check is against the type of
   --       the formal. Note that OUT and IN OUT parameters are handled by
   --       expanding assignments and explicit type conversions where a
   --       range check is required.

   --       Initialization expression for the initial value in an object
   --       declaration. In this case the Do_Range_Check flag is set on
   --       the initialization expression, and the check is against the
   --       range of the type of the object being declared.

   --       The expression of a type conversion. In this case the range check
   --       is against the target type of the conversion. See also the use of
   --       Do_Overflow_Check on a type conversion. The distinction is that
   --       the ovrflow check protects against a value that is outside the
   --       range of the target base type, whereas a range check checks that
   --       the resulting value (which is a value of the base type of the
   --       target type), satisfies the range constraint of the target type.

   --     Note: when a range check is required in contexts other than those
   --     listed above (e.g. in a return statement), an additional type
   --     conversion node is introduced to represent the required check.

   --  Do_Storage_Check (Flag2-Sem)
   --     This flag is set in an N_Allocator node to indicate that a storage
   --     check is required for the allocation, or in an N_Subprogram_Body
   --     node to indicate that a stack check is required in the subprogram
   --     prolog. The N_Allocator case is handled by the routine that expands
   --     the call to the runtime routine. The N_Subprogram_Body case is
   --     handled by the backend, and all the semantics does is set the flag.

   --  Do_Tag_Check (Flag3-Sem)
   --     This flag is set on an N_Assignment_Statement, N_Function_Call,
   --     N_Procedure_Call_Statement, N_Type_Conversion or N_Return_Statememt
   --     node to indicate that the tag check can be suppressed. It is not
   --     yet decided how this flag is used (TBD).

   --  Elaborate_Present (Flag4-Sem)
   --     This flag is set in the N_With_Clause node to indicate that a
   --     pragma Elaborate pragma appears for the with'ed units.

   --  Elaborate_All_Present (Flag1-Sem)
   --     This flag is set in the N_With_Clause node to indicate that a
   --     pragma Elaborate_All pragma appears for the with'ed units.

   --  Elaborate_Body_Present (Flag7-Sem)
   --     This flag is set in the N_Compilation_Unit node to indicate that
   --     a valid Elaborate_Body pragma appears for this unit.

   --  Enclosing_Variant (Node2-Sem)
   --     This field is present in the N_Variant node and identifies the
   --     Node_Id corresponding to the immediately enclosing variant when
   --     the variant is nested, and N_Empty otherwise. Set during semantic
   --     processing of the variant part of a record type.

   --  Entity (Node4-Sem)
   --     Appears in all direct names (identifier, character literal,
   --     operator symbol), as well as expanded names, and attributes that
   --     denote entities, such as 'Class. Points to the entity for the
   --     corresponding defining occurrence. Set after name resolution.
   --     In the case of identifiers in a WITH list, the corresponding
   --     defining occurrence is in a separately compiled file, and this
   --     pointer must be set using the library Load procedure. Note that
   --     during name resolution, the value in Entity may be temporarily
   --     incorrect (e.g. during overload resolution, Entity is
   --     initially set to the first possible correct interpretation, and
   --     then later modified if necessary to contain the correct value
   --     after resolution).

   --  Etype (Node5-Sem)
   --     Appears in all expression nodes, all direct names, and all
   --     entities. Points to the entity for the related type. Set after
   --     type resolution. Normally this is the actual subtype of the
   --     expression. However, in certain contexts such as the right side
   --     of an assignment, subscripts, arguments to calls, returned value
   --     in a function, initial value etc. it is the desired target type.
   --     In the event that this is different from the actual type, the
   --     Do_Range_Check flag will be set if a range check is required.

   --  Evaluate_Once (Flag10-Sem)
   --     This flag is set on an expression that may have more than one
   --     reference in the tree to ensure that the expression is only evaluated
   --     once, and that subsequence references use the same value. Otherwise,
   --     if an expression appears in the tree more than once it will be
   --     evaluated multiple times. Note that if an expression appears only
   --     once in the tree, Evaluate_Once has no semantic effect, but should
   --     be False from an efficiency point of view (since extra processing
   --     is required in Gigi to deal with the Evaluate_Once flag).

   --  First_Named_Actual (Node4-Sem)
   --     Present in procedure call statement and function call nodes. Set
   --     during semantic analysis to point to the first named  parameter
   --     where parameters are ordered by declaration order (as opposed to
   --     the actual order in the call which may be different due to named
   --     associations). Note that this field points to the actual parameter
   --     itself, not to the N_Parameter_Association node (its parent).

   --  Generic_Parent (Node5-Sem)
   --     Generic_parent is defined on declaration nodes that are instances.
   --     The value of Generic_Parent is the generic entity from which the
   --     instance is obtained.

   --  Has_No_Side_Effects (Flag8-Sem)
   --     A flag present in all expression nodes. Set by semantic analysis
   --     after determining that a given subexpression has no side effects.

   --  Has_Priority_Pragma (Flag6-Sem)
   --     A flag present in N_Subprogram_Body, N_Task_Definition and
   --     N_Protected_Definition nodes to flag the presence of either
   --     a Priority or Interrupt_Priority pragma in the declaration
   --     sequence (public or private in the task and protected cases)

   --  Has_Task_Stack_Size_Pragma (Flag5-Sem)
   --     A flag present in an N_Task_Definition node to flag the presence
   --     of a Task_Stack_Size pragma

   --  Implicit_Types (List2-Sem)
   --     Appears only in a N_Record_Definition node and is a list of
   --     N_Implicit_Type nodes which are generated if there were any
   --     implicit subtypes introduced by component declaration of the
   --     record type. If none were introduced, field is equal to No_List.

   --  Implicit_With (Flag2-Sem)
   --     This flag is set in the N_With_Clause node that is implicitly
   --     generated for runtime units that are loaded by the expander, and
   --     also for package System, if it is loaded implicitly by a use of
   --     the 'Address or 'Tag attribute

   --  Is_Overloaded (Flag5-Sem)
   --     A flag present in all expression nodes. Used temporarily during
   --     overloading determination. The setting of this flag is not
   --     relevant once overloading analysis is complete.

   --  Is_Task_Master (Flag5-Sem)
   --     A flag set in a Subprogram_Body, Block_Statement or Task_Body node
   --     to indicate that the construct is a task master (i.e. has declared
   --     tasks or declares an access to a task type).

   --  Next_Named_Actual (Node4-Sem)
   --     Present in parameter association node. Set during semantic
   --     analysis to point to the next named parameter, where parameters
   --     are ordered by declaration order (as opposed to the actual order
   --     in the call, which may be different due to named associations).
   --     Note that this field points to the actual parameter itself, not
   --     to the N_Parameter_Association node (its parent).

   --  Others_Discrete_Choices (List1-Sem)
   --     When a case statement or variant is analyzed, the semantic checks
   --     determine the actual list of choices that correspond to an others
   --     choice. This list is materialized for later use by the expander
   --     and the Others_Discrete_Choices field of an N_Others_Choice node
   --     points to this materialized list of choices, which is in standard
   --     format for a list of discrete choices, except that of course it
   --     cannot contain an N_Others_Choice entry.

   --  Preelaborable (Flag2-Sem)
   --     A flag that appears in the N_Compilation_Unit node to indicate
   --     whether or not elaboration code is present for this unit. It is
   --     set true if a pragma Preelaborate is present, or if the front end
   --     determines that no elaboration is required.

   --  Redundant_Use (Flag2-Sem)
   --     A flag present in Identifier nodes. Used only for identifiers
   --     referenced in use package clauses. Set to indicate that a use
   --     is redundant (and therefore need not be undone on scope exit)

   --  Unchecked_Conversion (Flag11-Sem)
   --     This flag is set in an N_Type_Conversion node that represents an
   --     unchecked conversion. Such nodes are generated during expander
   --     actions, and by the special casing of the expansion of calls to
   --     the bodies of instantiated unchecked conversion functions. The
   --     Size values of the input and output types are guaranteed to be
   --     equal, and neither of the types has a dynamic size.

   -----------------------------------
   -- Description of Library Fields --
   -----------------------------------

   --  The meaning of the syntactic fields is generally clear from their
   --  names without any further description, since the names are chosen
   --  to correspond very closely to the syntax in the reference manual.
   --  This section describes the usage of the library fields, which are
   --  used to contain additional information resulting from processing
   --  of library information (e.g. separate unit information)

   --  Acts_As_Spec
   --    A flag set in the N_Compilation_Unit node of a library subprogram
   --    which is acting as a spec (no separate source for the spec exists).

   --  Analyzed
   --    A flag in the N_Compilation_Unit node indicating that the unit has
   --    been semantically analyzed (used to avoid repeating this analysis
   --    on a given unit more than once).

   --  Expanded
   --    A flag in the N_Compilation_Unit node indicating that the unit has
   --    been expanded (used to avoid repeating this expansion on a given
   --    unit more than once).

   --  Library_Unit
   --    In a stub node, the Library_Unit field points to the compilation unit
   --    node of the corresponding subunit.
   --
   --    In a with clause node, the Library_Unit field points to the spec
   --    of the with'ed unit.
   --
   --    In a compilation unit node, the use of this field depends on
   --    the unit type:
   --
   --      For a subprogram body, the Library_Unit field points to the
   --      compilation unit node of the corresponding spec, unless
   --      Acts_As_Spec is set, in which case it points to itself.
   --
   --      For a package body, the Library_Unit field points to the
   --      compilation unit node of the corresponding spec.
   --
   --      For a subprogram spec to which pragma Inline applies, the
   --      Library_Unit field points to the compilation unit node of
   --      the corresponding body, if inlining is active.
   --
   --      For a generic declaration, the Library_Unit field points
   --      to the compilation unit node of the corresponding generic body.
   --
   --      For a subunit, the Library_Unit field points to the compilation
   --      unit node of the parent body.
   --
   --    Note that this field is not used to hold the parent pointer for a
   --    child unit (which might in any case need to use it for some other
   --    purpose as described above). Instead for a child unit, implicit
   --    with's are generated for all parents.

   --  Parent_Spec
   --     For a library unit that is a child unit spec (package or subprogram
   --     declaration, generic declaration or instantiation, or library level
   --     rename, this field points to the compilation unit node for the parent
   --     package specification. This field is Empty for library bodies (the
   --     parent spec in this case can be found from the corresponding spec).

      ---------------------
      -- 2.3  Identifier --
      ---------------------

      --  IDENTIFIER ::= LETTER {LETTER | DIGIT | UNDERLINE}

      --  An IDENTIFIER shall not contain two underlines in a row
      --  An IDENTIFIER shall not be a reserved word

      --  Note: in GNAT, a reserved word can be treated as an identifier
      --  in two cases. First, an incorrect use of a reserved word as an
      --  identifier is diagnosed and then treated as a normal identifier.
      --  Second, an attribute designator of the form of a reserved word
      --  (access, delta, digits, range) is treated as an identifier.

      --  Note: GNAT permits the use of a leading underline in identifiers
      --  when compiling the spec of package System. This allows handling of
      --  the declarations for Address and Tag, which are subtype renamings
      --  of the internal types _Address and _Tag in Standard.

      --  Note: The set of letters that is permitted in an identifier depends
      --  on the character set in use. See package Csets for full details.

      --  In the Ada grammar identifiers are the bottom level tokens which
      --  have very few semantics. Actual program identifiers are direct
      --  names. If we were being 100% honest with the grammar, then we would
      --  have a node called N_Direct_Name which would point to an identifier.
      --  However, that's too many extra nodes, so we just use the N_Identifier
      --  node directly as a direct name, and it contains the expression fields
      --  and Entity field that correspond to its use as a direct name.
      --  However, in those few cases where identifiers are used in contexts
      --  where they are not direct names (notably pragma and attribute
      --  identifiers, then only the Chars field is set).

      --  N_Identifier
      --  Sloc points to identifier
      --  Chars (Name1) contains the Name_Id for the identifier
      --  Entity (Node4-Sem)
      --  Redundant_Use (Flag2-Sem) set for redundant use clause
      --  plus fields for expression

      --------------------------
      -- 2.4  Numeric Literal --
      --------------------------

      --  NUMERIC_LITERAL ::= DECIMAL_LITERAL | BASED_LITERAL

      ----------------------------
      -- 2.4.1  Decimal Literal --
      ----------------------------

      --  DECIMAL_LITERAL ::= NUMERAL [.NUMERAL] [EXPONENT]

      --  NUMERAL ::= DIGIT {DIGIT | UNDERLINE}

      --  A NUMERAL shall not contain two underlines in a row,
      --  nor end in an underline.

      --  EXPONENT ::= E [+] NUMERAL | E - NUMERAL

      --  Decimal literals appear in the tree as either integer literal nodes
      --  or real literal nodes, depending on whether a period is present.

      --  N_Integer_Literal
      --  Sloc points to literal
      --  Intval (Uint3) contains integer value of literal
      --  plus fields for expression

      --  N_Real_Literal
      --  Sloc points to literal
      --  Numerator (Uint3) contains numerator of literal as rational value
      --  Denominator (Uint4) contains denominator of literal
      --  Decimal (Flag2) set for decimal literal case, in which case the
      --   denominator is a power of 10. Otherwise, the denominator is an
      --   integer value, representing the true value.
      --  plus fields for expression

      --------------------------
      -- 2.4.2  Based Literal --
      --------------------------

      --  BASED_LITERAL ::=
      --   BASE # BASED_NUMERAL [.BASED_NUMERAL] # [EXPONENT]

      --  BASE ::= NUMERAL

      --  BASED_NUMERAL ::=
      --    EXTENDED_DIGIT {EXTENDED_DIGIT | UNDERLINE}

      --  EXTENDED_DIGIT ::= DIGIT | LETTER

      --  A BASED_NUMERAL shall not contain two underlines in a row,
      --  nor end in an underline.

      --  Based literals appear in the tree as either integer literal nodes
      --  or real literal nodes, depending on whether a period is present.

      ----------------------------
      -- 2.5  Character Literal --
      ----------------------------

      --  CHARACTER_LITERAL ::= ' GRAPHIC_CHARACTER '

      --  N_Character_Literal
      --  Sloc points to literal
      --  Chars (Name1) contains the Name_Id for the identifier
      --  Char_Literal_Value (Char_Code2) contains the literal value
      --  Entity (Node4-Sem)
      --  plus fields for expression

      -------------------------
      -- 2.6  String Literal --
      -------------------------

      --  STRING LITERAL ::= "{STRING_ELEMENT}"

      --  A STRING_ELEMENT is either a pair of quotation marks ("), or a
      --  single GRAPHIC_CHARACTER other than a quotation mark.

      --  N_String_Literal
      --  Sloc points to literal
      --  Strval (Str3) contains Id of string value
      --  plus fields for expression

      ------------------
      -- 2.7  Comment --
      ------------------

      --  A COMMENT starts with two adjacent hyphens and extends up to the
      --  end of the line. A COMMENT may appear on any line of a program.

      --  Comments are skipped by the scanner and do not appear in the tree.
      --  It is possible to reconstruct the position of comments with respect
      --  to the elements of the tree by using the source position (Sloc)
      --  pointers that appear in every tree node.

      -----------------
      -- 2.8  Pragma --
      -----------------

      --  PRAGMA ::= pragma IDENTIFIER
      --    [(PRAGMA_ARGUMENT_ASSOCIATION {, PRAGMA_ARGUMENT_ASSOCIATION})];

      --  Note that a pragma may appear in the tree anywhere a declaration
      --  or a statement may appear, as well as in some other situations
      --  which are explicitly documented.

      --  N_Pragma
      --  Sloc points to PRAGMA
      --  Identifier (Node1)
      --  Pragma_Argument_Associations (List2) (set to No_List if none)
      --  Debug_Statement (Node3-Sem)

      --------------------------------------
      -- 2.8  Pragma Argument Association --
      --------------------------------------

      --  PRAGMA_ARGUMENT_ASSOCIATION ::=
      --    [pragma_argument_IDENTIFIER =>] NAME
      --  | [pragma_argument_IDENTIFIER =>] EXPRESSION

      --  N_Pragma_Argument_Association
      --  Sloc points to first token in association
      --  Identifier (Node1) (set to Empty if no pragma argument identifier)
      --  Expression (Node3)

      ------------------------
      -- 2.9  Reserved Word --
      ------------------------

      --  Reserved words are parsed by the scanner, and returned as the
      --  corresponding token types (e.g. PACKAGE is returned as Tok_Package)

      ----------------------------
      -- 3.1  Basic Declaration --
      ----------------------------

      --  BASIC_DECLARATION ::=
      --    TYPE_DECLARATION          | SUBTYPE_DECLARATION
      --  | OBJECT_DECLARATION        | NUMBER_DECLARATION
      --  | SUBPROGRAM_DECLARATION    | ABSTRACT_SUBPROGRAM_DECLARATION
      --  | PACKAGE_DECLARATION       | RENAMING_DECLARATION
      --  | EXCEPTION_DECLARATION     | GENERIC_DECLARATION
      --  | GENERIC_INSTANTIATION

      --  Basic declaration also includes IMPLICIT_LABEL_DECLARATION
      --  see further description in section on semantic nodes.

      --  Also, in the tree that is constructed, a pragma may appear
      --  anywhere that a declaration may appear.

      ------------------------------
      -- 3.1  Defining Identifier --
      ------------------------------

      --  DEFINING_IDENTIFIER ::= IDENTIFIER

      --  A defining identifier is an entity, which has additional fields
      --  depending on the setting of the Ekind field. These additional
      --  fields are defined (and access subprograms declared) in package
      --  Entity_Info.

      --  Note: N_Defining_Identifier is an extended node whose fields are
      --  deliberate layed out to match the layout of fields in an ordinary
      --  N_Identifier node allowing for easy alteration of an identifier
      --  node into a defining identifier node. For details, see procedure
      --  Sinfo.Change.Change_Identifier_To_Defining_Identifier.

      --  N_Defining_Identifier
      --  Sloc points to identifier
      --  Chars (Name1) contains the Name_Id for the identifier
      --  Etype (Node5-Sem)

      -----------------------------
      -- 3.2.1  Type Declaration --
      -----------------------------

      --  TYPE_DECLARATION ::=
      --    FULL_TYPE_DECLARATION
      --  | INCOMPLETE_TYPE_DECLARATION
      --  | PRIVATE_TYPE_DECLARATION
      --  | PRIVATE_EXTENSION_DECLARATION

      ----------------------------------
      -- 3.2.1  Full Type Declaration --
      ----------------------------------

      --  FULL_TYPE_DECLARATION ::=
      --    type DEFINING_IDENTIFIER [KNOWN_DISCRIMINANT_PART]
      --      is TYPE_DEFINITION;
      --  | TASK_TYPE_DECLARATION
      --  | PROTECTED_TYPE_DECLARATION

      --  The full type declaration node is used only for the first case. The
      --  second case (concurrent type declaration), is represented directly
      --  by a task type declaration or a protected type declaration.

      --  N_Full_Type_Declaration
      --  Sloc points to TYPE
      --  Defining_Identifier (Node1)
      --  Discriminant_Specifications (List2) (set to No_List if none)
      --  Type_Definition (Node3)

      ----------------------------
      -- 3.2.1  Type Definition --
      ----------------------------

      --  TYPE_DEFINITION ::=
      --    ENUMERATION_TYPE_DEFINITION  | INTEGER_TYPE_DEFINITION
      --  | REAL_TYPE_DEFINITION         | ARRAY_TYPE_DEFINITION
      --  | RECORD_TYPE_DEFINITION       | ACCESS_TYPE_DEFINITION
      --  | DERIVED_TYPE_DEFINITION

      --------------------------------
      -- 3.2.2  Subtype Declaration --
      --------------------------------

      --  SUBTYPE_DECLARATION ::=
      --    subtype DEFINING_IDENTIFIER is SUBTYPE_INDICATION;

      --  The subtype indication field is set to Empty for subtypes
      --  declared in package Standard (Positive, Natural).

      --  N_Subtype_Declaration
      --  Sloc points to SUBTYPE
      --  Defining_Identifier (Node1)
      --  Subtype_Indication (Node4)

      -------------------------------
      -- 3.2.2  Subtype Indication --
      -------------------------------

      --  SUBTYPE_INDICATION ::= SUBTYPE_MARK [CONSTRAINT]

      --  Note: if no constraint is present, the subtype indication appears
      --  directly in the tree as a subtype mark. The N_Subtype_Indication
      --  node is used only if a constraint is present.

      --  Note: the reason that this node has expression fields is that a
      --  subtype indication can appear as an operand of a membership test.

      --  N_Subtype_Indication
      --  Sloc points to first token of subtype mark
      --  Subtype_Mark (Node4)
      --  Constraint (Node3)
      --  Etype (Node5-Sem)

      --  Note: Etype is a copy of the Etype field of the Subtype_Mark. The
      --  reason for this redundancy is so that in a list of array index types,
      --  the Etype can be uniformly accessed to determine the subscript type.

      -------------------------
      -- 3.2.2  Subtype Mark --
      -------------------------

      --  SUBTYPE_MARK ::= subtype_NAME

      -----------------------
      -- 3.2.2  Constraint --
      -----------------------

      --  CONSTRAINT ::= SCALAR_CONSTRAINT | COMPOSITE_CONSTRAINT

      ------------------------------
      -- 3.2.2  Scalar Constraint --
      ------------------------------

      --  SCALAR_CONSTRAINT ::=
      --    RANGE_CONSTRAINT | DIGITS_CONSTRAINT | DELTA_CONSTRAINT

      ---------------------------------
      -- 3.2.2  Composite Constraint --
      ---------------------------------

      --  COMPOSITE_CONSTRAINT ::=
      --    INDEX_CONSTRAINT | DISCRIMINANT_CONSTRAINT

      -------------------------------
      -- 3.3.1  Object Declaration --
      -------------------------------

      --  OBJECT_DECLARATION ::=
      --    DEFINING_IDENTIFIER_LIST : [aliased] [constant]
      --      SUBTYPE_INDICATION [:= EXPRESSION];
      --  | DEFINING_IDENTIFIER_LIST : [aliased] [constant]
      --      ARRAY_TYPE_DEFINITION [:= EXPRESSION];
      --  | SINGLE_TASK_DECLARATION
      --  | SINGLE_PROTECTED_DECLARATION

      --  Note: aliased is not permitted in Ada 83 mode

      --  The N_Object_Declaration node is only for the first two cases.
      --  Single task declaration is handled by P_Task (9.1)
      --  Single protected declaration is handled by P_protected (9.5)

      --  Although the syntax allows multiple identifiers in the list, the
      --  semantics is as though successive declarations were given with
      --  identical type definition and expression components. To simplify
      --  semantic processing, the parser represents a multiple declaration
      --  case as a sequence of single declarations, using the More_Ids and
      --  Prev_Ids flags to preserve the original source form as described
      --  in the section on "Handling of Defining Identifier Lists".

      --  Note: if a range check is required for the initialization
      --  expression then the Do_Range_Check flag is set in the Expression,
      --  with the check being done against the type given by the object
      --  definition, which is also the Etype of the defining identifier.

      --  N_Object_Declaration
      --  Sloc points to first identifier
      --  Defining_Identifier (Node1) 
      --  Aliased_Present (Flag1) set if ALIASED appears
      --  Constant_Present (Flag2) set if CONSTANT appears
      --  Object_Definition (Node2) subtype indication/array type definition
      --  Expression (Node3) (set to Empty if not present)
      --  More_Ids (Flag5) (set to False if no more identifiers in list)
      --  Prev_Ids (Flag6) (set to False if no previous identifiers in list)

      -------------------------------------
      -- 3.3.1  Defining Identifier List --
      -------------------------------------

      --  DEFINING_IDENTIFIER_LIST ::=
      --    DEFINING_IDENTIFIER {, DEFINING_IDENTIFIER}

      -------------------------------
      -- 3.3.2  Number Declaration --
      -------------------------------

      --  NUMBER_DECLARATION ::=
      --    DEFINING_IDENTIFIER_LIST : constant := static_EXPRESSION;

      --  Although the syntax allows multiple identifiers in the list, the
      --  semantics is as though successive declarations were given with
      --  identical expressions. To simplify semantic processing, the parser
      --  represents a multiple declaration case as a sequence of single 
      --  declarations, using the More_Ids and Prev_Ids flags to preserve 
      --  the original source form as described in the section on "Handling
      --  of Defining Identifier Lists".

      --  N_Number_Declaration
      --  Sloc points to first identifier
      --  Defining_Identifier (Node1)
      --  Expression (Node3)
      --  More_Ids (Flag5) (set to False if no more identifiers in list)
      --  Prev_Ids (Flag6) (set to False if no previous identifiers in list)

      ----------------------------------
      -- 3.4  Derived Type Definition --
      ----------------------------------

      --  DERIVED_TYPE_DEFINITION ::=
      --    new [abstract] parent_SUBTYPE_INDICATION [RECORD_EXTENSION_PART]

      --  Note: ABSTRACT, record extension part not permitted in Ada 83 mode

      --  Note: a record extension part is required if ABSTRACT is present

      --  N_Derived_Type_Definition
      --  Sloc points to NEW
      --  Abstract_Present (Flag4)
      --  Subtype_Indication (Node4)
      --  Record_Extension_Part (Node3) (set to Empty if not present)

      ---------------------------
      -- 3.5  Range Constraint --
      ---------------------------

      --  RANGE_CONSTRAINT ::= range RANGE

      --  N_Range_Constraint
      --  Sloc points to RANGE
      --  Range_Expression (Node4)

      ----------------
      -- 3.5  Range --
      ----------------

      --  RANGE ::=
      --    RANGE_ATTRIBUTE_REFERENCE
      --  | SIMPLE_EXPRESSION .. SIMPLE_EXPRESSION

      --  Note: the case of a range given as a range attribute reference
      --  appears directly in the tree as an attribute reference.

      --  Note: the field name for a reference to a range is Range_Expression
      --  rather than Range, because range is a reserved keyword in Ada!

      --  Note: the reason that this node has expression fields is that a
      --  range can appear as an operand of a membership test.

      --  N_Range
      --  Sloc points to ..
      --  Low_Bound (Node1)
      --  High_Bound (Node2)
      --  plus fields for expression

      --  Note: if the range appears in a context, such as a subtype
      --  declaration, where range checks are required on one or both of
      --  the expression fields, then type conversion nodes are inserted
      --  to represent the required checks.

      ----------------------------------------
      -- 3.5.1  Enumeration Type Definition --
      ----------------------------------------

      --  ENUMERATION_TYPE_DEFINITION ::=
      --    (ENUMERATION_LITERAL_SPECIFICATION
      --      {, ENUMERATION_LITERAL_SPECIFICATION})

      --  Note: the Literals field in the node described below is null for
      --  the case of the standard types CHARACTER and WIDE_CHARACTER, for
      --  which special processing handles these types as special cases.

      --  N_Enumeration_Type_Definition
      --  Sloc points to left parenthesis
      --  Literals (List1) (Empty for CHARACTER or WIDE_CHARACTER)

      ----------------------------------------------
      -- 3.5.1  Enumeration Literal Specification --
      ----------------------------------------------

      --  ENUMERATION_LITERAL_SPECIFICATION ::=
      --    DEFINING_IDENTIFIER | DEFINING_CHARACTER_LITERAL

      ---------------------------------------
      -- 3.5.1  Defining Character Literal --
      ---------------------------------------

      --  DEFINING_CHARACTER_LITERAL ::= CHARACTER_LITERAL

      --  A defining character literal is an entity, which has additional
      --  fields depending on the setting of the Ekind field. These
      --  additional fields are defined (and access subprograms declared)
      --  in package Entity_Info.

      --  Note: N_Defining_Character_Literal is an extended node whose fields 
      --  are deliberate layed out to match the layout of fields in an ordinary
      --  N_Character_Literal node allowing for easy alteration of a character
      --  literal node into a defining character literal node. For details, see
      --  Sinfo.Change.Change_Character_Literal_To_Defining_Character_Literal.

      --  N_Defining_Character_Literal
      --  Sloc points to literal
      --  Chars (Name1) contains the Name_Id for the identifier
      --  Etype (Node5-Sem)

      ------------------------------------
      -- 3.5.4  Integer Type Definition --
      ------------------------------------

      --  Note: there is an error in this rule in the latest version of the
      --  grammar, so we have retained the old rule pending clarification.

      --  INTEGER_TYPE_DEFINITION ::=
      --    SIGNED_INTEGER_TYPE_DEFINITION
      --    MODULAR_TYPE_DEFINITION

      -------------------------------------------
      -- 3.5.4  Signed Integer Type Definition --
      -------------------------------------------

      --  SIGNED_INTEGER_TYPE_DEFINITION ::=
      --    range static_SIMPLE_EXPRESSION .. static_SIMPLE_EXPRESSION

      --  Note: the Low_Bound and High_Bound fields are set to Empty for
      --  integer types defined in package Standard.

      --  N_Signed_Integer_Type_Definition
      --  Sloc points to RANGE
      --  Low_Bound (Node1)
      --  High_Bound (Node2)

      -----------------------------------------
      -- 3.5.4  Unsigned Range Specification --
      -----------------------------------------

      --  MODULAR_TYPE_DEFINITION ::= mod static_EXPRESSION

      --  N_Modular_Type_Definition
      --  Sloc points to MOD
      --  Expression (Node3)

      ---------------------------------
      -- 3.5.6  Real Type Definition --
      ---------------------------------

      --  REAL_TYPE_DEFINITION ::=
      --    FLOATING_POINT_DEFINITION | FIXED_POINT_DEFINITION

      --------------------------------------
      -- 3.5.7  Floating Point Definition --
      --------------------------------------

      --  FLOATING_POINT_DEFINITION ::=
      --    digits static_SIMPLE_EXPRESSION [REAL_RANGE_SPECIFICATION]

      --  Note: The Digits_Expression and Real_Range_Specifications fields
      --  are set to Empty for floating-point types declared in Standard.

      --  N_Floating_Point_Definition
      --  Sloc points to DIGITS
      --  Digits_Expression (Node2)
      --  Real_Range_Specification (Node4) (set to Empty if not present)

      -------------------------------------
      -- 3.5.7  Real Range Specification --
      -------------------------------------

      --  REAL_RANGE_SPECIFICATION ::=
      --    range static_SIMPLE_EXPRESSION .. static_SIMPLE_EXPRESSION

      --  N_Real_Range_Specification
      --  Sloc points to RANGE
      --  Low_Bound (Node1)
      --  High_Bound (Node2)

      -----------------------------------
      -- 3.5.9  Fixed Point Definition --
      -----------------------------------

      --  FIXED_POINT_DEFINITION ::=
      --    ORDINARY_FIXED_POINT_DEFINITION | DECIMAL_FIXED_POINT_DEFINITION

      --------------------------------------------
      -- 3.5.9  Ordinary Fixed Point Definition --
      --------------------------------------------

      --  ORDINARY_FIXED_POINT_DEFINITION ::=
      --    delta static_EXPRESSION REAL_RANGE_SPECIFICATION
      --  | DECIMAL_FIXED_POINT_DEFINITION

      --  Note: In Ada 83, the EXPRESSION must be a SIMPLE_EXPRESSION

      --  Note: the Delta_Expression and Real_Range_Specification fields
      --  are set to Empty for fixed point types declared in Standard.

      --  N_Ordinary_Fixed_Point_Definition
      --  Sloc points to DELTA
      --  Delta_Expression (Node3)
      --  Real_Range_Specification (Node4)

      -------------------------------------------
      -- 3.5.9  Decimal Fixed Point Definition --
      -------------------------------------------

      --  DECIMAL_FIXED_POINT_DEFINITION ::=
      --    delta static_SIMPLE_EXPRESSION
      --      digits static_SIMPLE_EXPRESSION [REAL_RANGE_SPECIFICATION]

      --  Note: decimal types are not permitted in Ada 83 mode

      --  N_Decimal_Fixed_Point_Definition
      --  Sloc points to DELTA
      --  Delta_Expression (Node3)
      --  Digits_Expression (Node2)
      --  Real_Range_Specification (Node4) (set to Empty if not present)

      ------------------------------
      -- 3.5.9  Digits Constraint --
      ------------------------------

      --  DIGITS_CONSTRAINT ::=
      --    digits static_EXPRESSION [RANGE_CONSTRAINT]

      --  Note: in Ada 83, the EXPRESSION must be a SIMPLE_EXPRESSION
      --  Note: in Ada 9X, reduced accuracy subtypes are obsolescent

      --  N_Digits_Constraint
      --  Sloc points to DIGITS
      --  Digits_Expression (Node2)
      --  Range_Constraint (Node4) (set to Empty if not present)

      -----------------------------
      -- 3.5.9  Delta Constraint --
      -----------------------------

      --  Note: this is moved to section N.2 in the 9X document, since
      --  it is considered to be an "obsolescent" feature in Ada 9X.

      --  DELTA CONSTRAINT ::= DELTA STATIC_EXPRESSION [RANGE_CONSTRAINT]

      --  N_Delta_Constraint
      --  Sloc points to DELTA
      --  Delta_Expression (Node3)
      --  Range_Constraint (Node4) (set to Empty if not present)

      --------------------------------
      -- 3.6  Array Type Definition --
      --------------------------------

      --  ARRAY_TYPE_DEFINITION ::=
      --    UNCONSTRAINED_ARRAY_DEFINITION | CONSTRAINED_ARRAY_DEFINITION

      -----------------------------------------
      -- 3.6  Unconstrained Array Definition --
      -----------------------------------------

      --  UNCONSTRAINED_ARRAY_DEFINITION ::=
      --    array (INDEX_SUBTYPE_DEFINITION {, INDEX_SUBTYPE_DEFINITION}) of
      --      COMPONENT_DEFINITION

      --  Note: dimensionality of array is indicated by number of entries in
      --  the Subtype_Marks list, which has one entry for each dimension.

      --  N_Unconstrained_Array_Definition
      --  Sloc points to ARRAY
      --  Subtype_Marks (List2)
      --  Aliased_Present (Flag1) in component definition
      --  Subtype_Indication (Node4) in component definition

      -----------------------------------
      -- 3.6  Index Subtype Definition --
      -----------------------------------

      --  INDEX_SUBTYPE_DEFINITION ::= SUBTYPE_MARK range <>

      --  There is no explicit node in the tree for an index subtype
      --  definition since the N_Unconstrained_Array_Definition node
      --  incorporates the type marks which appear in this context.

      ---------------------------------------
      -- 3.6  Constrained Array Definition --
      ---------------------------------------

      --  CONSTRAINED_ARRAY_DEFINITION ::=
      --    ARRAY (DISCRETE_SUBTYPE_DEFINITION
      --      {, DISCRETE_SUBTYPE_DEFINITION}) OF COMPONENT_DEFINITION

      --  Note: dimensionality of array is indicated by number of entries
      --  in the Discrete_Subtype_Definitions list, which has one entry
      --  for each dimension.

      --  N_Constrained_Array_Definition
      --  Sloc points to ARRAY
      --  Discrete_Subtype_Definitions (List2)
      --  Aliased_Present (Flag1) in component definition
      --  Subtype_Indication (Node4) in component definition

      --------------------------------------
      -- 3.6  Discrete Subtype Definition --
      --------------------------------------

      --  DISCRETE_SUBTYPE_DEFINITION ::=
      --    discrete_SUBTYPE_INDICATION | RANGE

      -------------------------------
      -- 3.6  Component Definition --
      -------------------------------

      --  COMPONENT_DEFINITION ::= [aliased] SUBTYPE_INDICATION

      --  There is no explicit node in the tree for a component definition.
      --  Instead the subtype indication appears directly, and the ALIASED
      --  indication (Aliased_Present flag) is in the parent node.

      --  Note: although the syntax does not permit a component definition to
      --  be an anonymous array (and the parser will diagnose such an attempt
      --  with an appropriate message), it is possible for anonymous arrays
      --  to appear as component definitions. The semantics and back end handle
      --  this case properly, and the expander in fact generates such cases.


      -----------------------------
      -- 3.6.1  Index Constraint --
      -----------------------------

      --  INDEX_CONSTRAINT ::= (DISCRETE_RANGE {, DISCRETE_RANGE})

      --  It is not in general possible to distinguish between discriminant
      --  constraints and index constraints at parse time, since a simple
      --  name could be either the subtype mark of a discrete range, or an
      --  expression in a discriminant association with no name. Either
      --  entry appears simply as the name, and the semantic parse must
      --  distinguish between the two cases. Thus we use a common tree
      --  node format for both of these constraint types.

      --  See Discriminant_Constraint for format of node

      ---------------------------
      -- 3.6.1  Discrete Range --
      ---------------------------

      --  DISCRETE_RANGE ::= discrete_SUBTYPE_INDICATION | RANGE

      ----------------------------
      -- 3.7  Discriminant Part --
      ----------------------------

      --  DISCRIMINANT_PART ::=
      --    UNKNOWN_DISCRIMINANT_PART | KNOWN_DISCRIMINANT_PART

      ------------------------------------
      -- 3.7  Unknown Discriminant Part --
      ------------------------------------

      --  UNKNOWN_DISCRIMINANT_PART ::= (<>)

      --  Note: unknown discriminant parts are not permitted in Ada 83 mode

      --  There is no explicit node in the tree for an unknown discriminant
      --  part. Instead the Has_Unknown_Discriminants flag is set in the
      --  parent node.

      ----------------------------------
      -- 3.7  Known Discriminant Part --
      ----------------------------------

      --  KNOWN_DISCRIMINANT_PART ::=
      --    (DISCRIMINANT_SPECIFICATION {; DISCRIMINANT_SPECIFICATION})

      -------------------------------------
      -- 3.7  Discriminant Specification --
      -------------------------------------

      --  DISCRIMINANT_SPECIFICATION ::=
      --    DEFINING_IDENTIFIER_LIST : SUBTYPE_MARK
      --      [:= DEFAULT_EXPRESSION]
      --  | DEFINING_IDENTIFIER_LIST : ACCESS_DEFINITION
      --      [:= DEFAULT_EXPRESSION]

      --  Although the syntax allows multiple identifiers in the list, the
      --  semantics is as though successive specifications were given with
      --  identical type definition and expression components. To simplify
      --  semantic processing, the parser represents a multiple declaration
      --  case as a sequence of single specifications, using the More_Ids and
      --  Prev_Ids flags to preserve the original source form as described
      --  in the section on "Handling of Defining Identifier Lists".

      --  N_Discriminant_Specification
      --  Sloc points to first identifier
      --  Defining_Identifier (Node1) 
      --  Discriminant_Type (Node2) subtype mark or
      --    access parameter definition
      --  Expression (Node3) (set to Empty if no default expression)
      --  More_Ids (Flag5) (set to False if no more identifiers in list)
      --  Prev_Ids (Flag6) (set to False if no previous identifiers in list)

      -----------------------------
      -- 3.7  Default Expression --
      -----------------------------

      --  DEFAULT_EXPRESSION ::= EXPRESSION

      ------------------------------------
      -- 3.7.1  Discriminant Constraint --
      ------------------------------------

      --  DISCRIMINANT_CONSTRAINT ::=
      --    (DISCRIMINANT_ASSOCIATION {, DISCRIMINANT_ASSOCIATION})

      --  It is not in general possible to distinguish between discriminant
      --  constraints and index constraints at parse time, since a simple
      --  name could be either the subtype mark of a discrete range, or an
      --  expression in a discriminant association with no name. Either
      --  entry appears simply as the name, and the semantic parse must
      --  distinguish between the two cases. Thus we use a common tree
      --  node format for both of these constraint types.

      --  N_Index_Or_Discriminant_Constraint
      --  Sloc points to left paren
      --  Constraints (List1) points to list of discrete ranges or
      --    discriminant associations

      -------------------------------------
      -- 3.7.1  Discriminant Association --
      -------------------------------------

      --  DISCRIMINANT_ASSOCIATION ::=
      --    [discriminant_SELECTOR_NAME
      --      {| discriminant_SELECTOR_NAME} =>] EXPRESSION

      --  Note: a discriminant association that has no selector name list
      --  appears directly as an expression in the tree.

      --  N_Discriminant_Association
      --  Sloc points to first token of discriminant association
      --  Selector_Names (List1) (always non-empty, since if no selector
      --   names are present, this node is not used, see comment above)
      --  Expression (Node3)

      ---------------------------------
      -- 3.8  Record Type Definition --
      ---------------------------------

      --  RECORD_TYPE_DEFINITION ::=
      --    [abstract] [tagged] [limited] RECORD_DEFINITION

      --  Note: ABSTRACT, TAGGED, LIMITED are not permitted in Ada 83 mode

      --  There is no explicit node in the tree for a record type definition.
      --  Instead the flags for Tagged_Present and Limited_Present appear in
      --  the N_Record_Definition node for a record definition appearing in
      --  the context of a record type definition.

      ----------------------------
      -- 3.8  Record Definition --
      ----------------------------

      --  RECORD_DEFINITION ::=
      --    record
      --      COMPONENT_LIST
      --    end record
      --  | null record

      --  Note: the Abstract_Present, Tagged_Present and Limited_Present
      --  flags appear only for a record definition appearing in a record
      --  type definition.

      --  Note: the NULL RECORD case is not permitted in Ada 83

      --  N_Record_Definition
      --  Sloc points to RECORD or NULL
      --  Abstract_Present (Flag4)
      --  Tagged_Present (Flag1)
      --  Limited_Present (Flag2)
      --  Component_List (Node1) empty in null record case
      --  Implicit_Types (List2-Sem) list of implicit types (may be empty)
      --  Null_Present (Flag3) set in null record case

      -------------------------
      -- 3.8  Component List --
      -------------------------

      --  COMPONENT_LIST ::=
      --    COMPONENT_DECLARATION {COMPONENT_DECLARATION}
      --  | {COMPONENT_DECLARATION} VARIANT_PART
      --  | null;

      --  N_Component_List
      --  Sloc points to first token of component list
      --  Component_Declarations (List3)
      --  Variant_Part (Node4) (set to Empty if no variant part)
      --  Null_Present (Flag3)

      --------------------------------
      -- 3.8  Component Declaration --
      --------------------------------

      --  COMPONENT_DECLARATION ::=
      --    DEFINING_IDENTIFIER_LIST : COMPONENT_DEFINITION
      --      [:= DEFAULT_EXPRESSION]

      --  Note: although the syntax does not permit a component definition to
      --  be an anonymous array (and the parser will diagnose such an attempt
      --  with an appropriate message), it is possible for anonymous arrays
      --  to appear as component definitions. The semantics and back end handle
      --  this case properly, and the expander in fact generates such cases.

      --  Although the syntax allows multiple identifiers in the list, the
      --  semantics is as though successive declarations were given with the
      --  same component definition and expression components. To simplify
      --  semantic processing, the parser represents a multiple declaration
      --  case as a sequence of single declarations, using the More_Ids and
      --  Prev_Ids flags to preserve the original source form as described
      --  in the section on "Handling of Defining Identifier Lists".

      --  N_Component_Declaration
      --  Sloc points to first identifier
      --  Defining_Identifier (Node1)
      --  Aliased_Present (Flag1) in component definition
      --  Subtype_Indication (Node4) in component definition
      --  Expression (Node3) (set to Empty if no default expression)
      --  More_Ids (Flag5) (set to False if no more identifiers in list)
      --  Prev_Ids (Flag6) (set to False if no previous identifiers in list)

      -------------------------
      -- 3.8.1  Variant Part --
      -------------------------

      --  VARIANT_PART ::=
      --    case discriminant_DIRECT_NAME is
      --      VARIANT
      --      {VARIANT}
      --    end case;

      --  N_Variant_Part
      --  Sloc points to CASE
      --  Name (Node2)
      --  Variants (List1)

      --------------------
      -- 3.8.1  Variant --
      --------------------

      --  VARIANT ::= when DISCRETE_CHOICE_LIST => COMPONENT_LIST

      --  N_Variant
      --  Sloc points to WHEN
      --  Discrete_Choices (List4)
      --  Component_List (Node1)
      --  Enclosing_Variant (Node2-Sem)

      ---------------------------------
      -- 3.8.1  Discrete Choice List --
      ---------------------------------

      --  DISCRETE_CHOICE_LIST ::= DISCRETE_CHOICE {| DISCRETE_CHOICE}

      ----------------------------
      -- 3.8.1  Discrete Choice --
      ----------------------------

      --  DISCRETE_CHOICE ::= EXPRESSION | DISCRETE_RANGE | others

      --  Note: in Ada 83 mode, the expression must be a simple expression

      --  The only choice that appears explicitly is the OTHERS choice, as
      --  defined here. Other cases of discrete choice (expression and
      --  discrete range) appear directly. This production is also used
      --  for the OTHERS possibility of an exception choice.

      --  Note: in accordance with the syntax, the parser does not check that
      --  OTHERS appears at the end on its own in a choice list context. This
      --  is a semantic check.

      --  N_Others_Choice
      --  Sloc points to OTHERS
      --  Others_Discrete_Choices (List1-Sem)

      ----------------------------------
      -- 3.9.1  Record Extension Part --
      ----------------------------------

      --  RECORD_EXTENSION_PART ::= with RECORD_DEFINITION

      --  Note: record extension parts are not permitted in Ada 83 mode

      ----------------------------------
      -- 3.10  Access Type Definition --
      ----------------------------------

      --  ACCESS_TYPE_DEFINITION ::=
      --    ACCESS_TO_OBJECT_DEFINITION
      --  | ACCESS_TO_SUBPROGRAM_DEFINITION

      --  This is split into two separate cases for the normal subtype
      --  case and the access to subprogram case, in other words the
      --  modified grammar has:

      --  ACCESS_TYPE_DEFINITION ::=
      --    ACCESS_SUBTYPE_DEFINITION | ACCESS_SUBPROGRAM_DEFINITION

      ---------------------------------------
      -- 3.10  Access To Object Definition --
      ---------------------------------------

      --  ACCESS_TO_OBJECT_DEFINITION ::=
      --    access [GENERAL_ACCESS_MODIFIER] SUBTYPE_INDICATION

      --  N_Access_To_Object_Definition
      --  Sloc points to ACCESS
      --  All_Present (Flag1)
      --  Subtype_Indication (Node4)
      --  Constant_Present (Flag2)

      -----------------------------------
      -- 3.10  General Access Modifier --
      -----------------------------------

      --  GENERAL_ACCESS_MODIFIER ::= all | constant

      --  Note: general access modifiers are not permitted in Ada 83 mode

      --  There is no explicit node in the tree for general access modifier.
      --  Instead the All_Present or Constant_Present flags are set in the
      --  parent node.

      ----------------------------------------
      -- 3.10  Access Subprogram Definition --
      ----------------------------------------

      --  ACCESS_TO_SUBPROGRAM_DEFINITION
      --    access [protected] function PARAMETER_AND_RESULT_PROFILE
      --  | access [protected] procedure PARAMETER_PROFILE

      --  Note: access to subprograms are not permitted in Ada 83 mode

      --  N_Access_Function_Definition
      --  Sloc points to ACCESS
      --  Protected_Present (Flag1)
      --  Parameter_Specifications (List2) (set to No_List if no formal part)
      --  Subtype_Mark (Node4) result subtype

      --  N_Access_Procedure_Definition
      --  Sloc points to ACCESS
      --  Protected_Present (Flag1)
      --  Parameter_Specifications (List2) (set to No_List if no formal part)

      -----------------------------
      -- 3.10  Access Definition --
      -----------------------------

      --  ACCESS_DEFINITION ::= access SUBTYPE_MARK

      --  N_Access_Definition
      --  Sloc points to ACCESS
      --  Subtype_Mark (Node4)

      -----------------------------------------
      -- 3.10.1  Incomplete Type Declaration --
      -----------------------------------------

      --  INCOMPLETE_TYPE_DECLARATION ::=
      --    type DEFINING_IDENTIFIER [DISCRIMINANT_PART];

      --  N_Incomplete_Type_Declaration
      --  Sloc points to TYPE
      --  Defining_Identifier (Node1)
      --  Discriminant_Specifications (List2) (set to No_List if no
      --   discriminant part, or if the discriminant part is an
      --   unknown discriminant part)
      --  Has_Unknown_Discriminants (Flag3) set if (<>) discriminant

      ----------------------------
      -- 3.11  Declarative Part --
      ----------------------------

      --  DECLARATIVE_PART ::= {DECLARATIVE_ITEM}

      --  Note: although the parser enforces the syntactic requirement that
      --  a declarative part can contain only declarations, the semantic
      --  processing may add statements to the list of actions in a
      --  declarative part, so the code generator should be prepared
      --  to accept a statement in this position.

      ----------------------------
      -- 3.11  Declarative Item --
      ----------------------------

      --  DECLARATIVE_ITEM ::= BASIC_DECLARATIVE_ITEM | BODY

      ----------------------------------
      -- 3.11  Basic Declarative Item --
      ----------------------------------

      --  BASIC_DECLARATIVE_ITEM ::=
      --    BASIC_DECLARATION | REPRESENTATION_CLAUSE | USE_CLAUSE

      ----------------
      -- 3.11  Body --
      ----------------

      --  BODY ::= PROPER_BODY | BODY_STUB

      -----------------------
      -- 3.11  Proper Body --
      -----------------------

      --  PROPER_BODY ::=
      --    SUBPROGRAM_BODY | PACKAGE_BODY | TASK_BODY | PROTECTED_BODY

      ---------------
      -- 4.1  Name --
      ---------------

      --  NAME ::=
      --    DIRECT_NAME        | EXPLICIT_DEREFERENCE
      --  | INDEXED_COMPONENT  | SLICE
      --  | SELECTED_COMPONENT | ATTRIBUTE_REFERENCE
      --  | TYPE_CONVERSION    | FUNCTION_CALL
      --  | CHARACTER_LITERAL

      ----------------------
      -- 4.1  Direct Name --
      ----------------------

      --  DIRECT_NAME ::= IDENTIFIER | OPERATOR_SYMBOL

      -----------------
      -- 4.1  Prefix --
      -----------------

      --  PREFIX ::= NAME | IMPLICIT_DEREFERENCE

      -------------------------------
      -- 4.1  Explicit Dereference --
      -------------------------------

      --  EXPLICIT_DEREFERENCE ::= NAME . all

      --  N_Explicit_Dereference
      --  Sloc points to ALL
      --  Prefix (Node2)
      --  Do_Access_Check (Flag2-Sem)
      --  plus fields for expression

      -------------------------------
      -- 4.1  Implicit Dereference --
      -------------------------------

      --  IMPLICIT_DEREFERENCE ::= NAME

      ------------------------------
      -- 4.1.1  Indexed Component --
      ------------------------------

      --  INDEXED_COMPONENT ::= PREFIX (EXPRESSION {, EXPRESSION})

      --  Note: the parser may generate this node in some situations where it
      --  should be a function call. The semantic  pass must correct this
      --  misidentification (which is inevitable at the parser level).

      --  N_Indexed_Component
      --  Sloc points to first token of prefix
      --  Prefix (Node2)
      --  Expressions (List3)
      --  Do_Access_Check (Flag2-Sem)
      --  plus fields for expression

      --  Note: if any of the subscripts requires a range check, then the
      --  Do_Range_Check flag is set on the corresponding expression, with
      --  the index type being determined from the type of the Prefix, which
      --  references the array being indexed.

      ------------------
      -- 4.1.2  Slice --
      ------------------

      --  SLICE ::= PREFIX (DISCRETE_RANGE)

      --  N_Slice
      --  Sloc points to first token of prefix
      --  Prefix (Node2)
      --  Discrete_Range (Node4)
      --  Do_Access_Check (Flag2-Sem)
      --  plus fields for expression

      -------------------------------
      -- 4.1.3  Selected Component --
      -------------------------------

      --  SELECTED_COMPONENT ::= PREFIX . SELECTOR_NAME

      --  Note: selected components that are semantically expanded names get
      --  changed during semantic processing into the separate N_Expanded_Name
      --  node. See description of this node in the section on semantic nodes.

      --  N_Selected_Component
      --  Sloc points to period
      --  Prefix (Node2)
      --  Selector_Name (Node3)
      --  Do_Access_Check (Flag2-Sem)
      --  Do_Discriminant_Check (Flag3-Sem)
      --  plus fields for expression

      --------------------------
      -- 4.1.3  Selector Name --
      --------------------------

      --  SELECTOR_NAME ::= IDENTIFIER | CHARACTER_LITERAL | OPERATOR_SYMBOL

      --------------------------------
      -- 4.1.4  Attribute Reference --
      --------------------------------

      --  ATTRIBUTE_REFERENCE ::= PREFIX ' ATTRIBUTE_DESIGNATOR

      --  Note: the syntax is quite ambiguous at this point. Consider:

      --    A'Length (X)       X is part of the attribute designator
      --    A'Pos (X)          X is an actual parameter of function A'Pos
      --    A'Class (X)        X is the expression of a type conversion

      --  It would be possible for the parser to distinguish these cases
      --  by looking at the attribute identifier. However, that would mean
      --  more work in introducing new implementation defined attributes,
      --  and also it would mean that special processing for attributes
      --  would be scattered around, instead of being centralized in the
      --  semantic routine that handles an N_Attribute_Reference node.
      --  Consequently, the parser in all the above cases stores the
      --  expression (which appears as X in these examples) in the
      --  Expression field of the N_Attribute_Reference node.

      --  N_Attribute_Reference
      --  Sloc points to apostrophe
      --  Prefix (Node2)
      --  Identifier (Node1) from attribute designator
      --  Expression (Node3) (set to Empty if no expression in designator)
      --  Entity (Node4-Sem) used if the attribute yields a type
      --  Do_Access_Check (Flag2-Sem)
      --  plus fields for expression

      ---------------------------------
      -- 4.1.4  Attribute Designator --
      ---------------------------------

      --  ATTRIBUTE_DESIGNATOR ::=
      --    IDENTIFIER [(static_EXPRESSION)]
      --  | access | delta | digits

      --  There is no explicit node in the tree for an attribute designator.
      --  Instead the identifier and expression fields of the parent node
      --  (N_Attribute_Reference node) hold the information.

      --  Note: if ACCESS, DELTA or DIGITS appears in an attribute
      --  designator, then they are treated as identifiers internally
      --  rather than the keywords of the same name.

      --------------------------------------
      -- 4.1.4  Range Attribute Reference --
      --------------------------------------

      --  RANGE_ATTRIBUTE_REFERENCE ::= PREFIX ' RANGE_ATTRIBUTE_DESIGNATOR

      --  A range attribute reference is represented in the tree using the
      --  normal N_Attribute_Reference node.

      ---------------------------------------
      -- 4.1.4  Range Attribute Designator --
      ---------------------------------------

      --  RANGE_ATTRIBUTE_DESIGNATOR ::= PREFIX [(static_EXPRESSION)]

      --  A range attribute designator is represented in the tree using the
      --  normal N_Attribute_Reference node.

      --------------------
      -- 4.3  Aggregate --
      --------------------

      --  AGGREGATE ::=
      --    RECORD_AGGREGATE | EXTENSION_AGGREGATE | ARRAY_AGGREGATE

      -----------------------------
      -- 4.3.1  Record Aggregate --
      -----------------------------

      --  RECORD_AGGREGATE ::= (RECORD_COMPONENT_ASSOCIATION_LIST)

      --  N_Aggregate
      --  Sloc points to left parenthesis
      --  Expressions (List3) (Empty if none or null record case)
      --  Component_Associations (List4) (set to No_List if none)
      --  Null_Record_Present (Flag2)
      --  plus fields for expression

      --  Note: this structure is used for both record and array aggregates
      --  since the two cases are not separable by the parser. The parser
      --  makes no attempt to enforce consistency here, so it is up to the
      --  semantic phase to make sure that the aggregate is consistent (i.e.
      --  that it is not a "half-and-half" case that mixes record and array
      --  syntax. In particular, for a record aggregate, the expressions
      --  field will be set if there are positional associations.

      ----------------------------------------------
      -- 4.3.1  Record Component Association List --
      ----------------------------------------------

      --  RECORD_COMPONENT_ASSOCIATION_LIST ::=
      --     RECORD_COMPONENT_ASSOCIATION {, RECORD_COMPONENT_ASSOCIATION}
      --   | NULL RECORD

      --  There is no explicit node in the tree for a record component
      --  association list. Instead the Null_Record_Present flag is set in
      --  the parent node for the NULL RECORD case.

      -----------------------------------------
      -- 4.3.1  Record Component Association --
      -----------------------------------------

      --  RECORD_COMPONENT_ASSOCIATION ::=
      --    component_SELECTOR_NAME {| component_SELECTOR_NAME} => EXPRESSION
      --  | others => EXPRESSION

      --  N_Component_Association
      --  Sloc points to first selector name
      --  Choices (List1)
      --  Expression (Node3)

      --  Note: this structure is used for both record component associations
      --  and array component associations, since the two cases aren't always
      --  separable by the parser. The choices list may represent either a
      --  list of selector names in the record aggregate case, or a list of
      --  discrete choices in the array aggregate case.

      --------------------------------
      -- 4.3.2  Extension Aggregate --
      --------------------------------

      --  EXTENSION_AGGREGATE ::=
      --    (EXPRESSION with RECORD_COMPONENT_ASSOCIATION_LIST)

      --  Note: extension aggregates are not permitted in Ada 83 mode

      --  N_Extension_Aggregate
      --  Sloc points to left parenthesis
      --  Expression (Node3)
      --  Component_Associations (List4) (set to No_List if null record case)
      --  Null_Record_Present (Flag2)
      --  plus fields for expression

      ----------------------------
      -- 4.3.3  Array Aggregate --
      ----------------------------

      --  ARRAY_AGGREGATE ::=
      --    POSITIONAL_ARRAY_AGGREGATE | NAMED_ARRAY_AGGREGATE

      ---------------------------------------
      -- 4.3.3  Positional Array Aggregate --
      ---------------------------------------

      --  POSITIONAL_ARRAY_AGGREGATE ::=
      --    (EXPRESSION, EXPRESSION {, EXPRESSION})
      --  | (EXPRESSION {, EXPRESSION}, others => EXPRESSION)

      --  See Record_Aggregate (4.3.1) for node structure

      ----------------------------------
      -- 4.3.3  Named Array Aggregate --
      ----------------------------------

      --  NAMED_ARRAY_AGGREGATE ::=
      --  | (ARRAY_COMPONENT_ASSOCIATION {, ARRAY_COMPONENT_ASSOCIATION})

      --  See Record_Aggregate (4.3.1) for node structure

      ----------------------------------------
      -- 4.3.3  Array Component Association --
      ----------------------------------------

      --  ARRAY_COMPONENT_ASSOCIATION ::=
      --    DISCRETE_CHOICE_LIST => EXPRESSION

      --  See Record_Component_Association (4.3.1) for node structure

      --------------------------------------------------
      -- 4.4  Expression/Relation/Term/Factor/Primary --
      --------------------------------------------------

      --  EXPRESSION ::=
      --    RELATION {and RELATION} | RELATION {and then RELATION}
      --  | RELATION {or RELATION}  | RELATION {or else RELATION}
      --  | RELATION {xor RELATION}

      --  RELATION ::=
      --    SIMPLE_EXPRESSION [RELATIONAL_OPERATOR SIMPLE_EXPRESSION]
      --  | SIMPLE_EXPRESSION [not] in RANGE
      --  | SIMPLE_EXPRESSION [not] in SUBTYPE_MARK

      --  SIMPLE_EXPRESSION ::=
      --    [UNARY_ADDING_OPERATOR] TERM {BINARY_ADDING_OPERATOR TERM}

      --  TERM ::= FACTOR {MULTIPLYING_OPERATOR FACTOR}

      --  FACTOR ::= PRIMARY [** PRIMARY] | abs PRIMARY | not PRIMARY

      --  No nodes are generated for any of these constructs. Instead, the
      --  node for the operator appears directly. When we refer to an
      --  expression in this description, we mean any of the possible
      --  consistuent components of an expression (e.g. identifier is
      --  an example of an expression).

      ------------------
      -- 4.4  Primary --
      ------------------

      --  PRIMARY ::=
      --    NUMERIC_LITERAL  | null
      --  | STRING_LITERAL   | AGGREGATE
      --  | NAME             | QUALIFIED_EXPRESSION
      --  | ALLOCATOR        | (EXPRESSION)

      --  Usually there is no explicit node in the tree for primary. Instead
      --  the constituent (e.g. AGGREGATE) appears directly. There are two
      --  exceptions. First, there is an explicit node for a null primary.

      --  N_Null
      --  Sloc points to NULL
      --  plus fields for expression

      --  Second, the case of (EXPRESSION) is handled specially. Ada requires
      --  that the parser keep track of which subexpressions are enclosed
      --  in parentheses, and how many levels of parentheses are used. This
      --  information is required for optimization purposes, and also for
      --  some semantic checks (e.g. (((1))) in a procedure spec does not
      --  conform with ((((1)))) in a procedure body). A single layer of
      --  parentheses, the most common case, is recorded by setting the
      --  Parens flag in the expression node (all nodes that can appear as
      --  expressions contain this flag field). If more than one level of
      --  parentheses is present, then we generate an extra node in the tree:

      --  N_Parenthesized_Expression
      --  Sloc points to left paren
      --  Expression (Node3)
      --  plus fields for expression

      --  Note: the level of parentheses always present in things like
      --  aggregates does not count, only the parentheses in the primary
      --  (EXPRESSION) affect the setting of the Parens flag, or the
      --  creation of N_Parenthesized_Expression nodes.

      --  Note: since N_Parenthesized_Expression itself has a Parens flag,
      --  we only need one of these nodes for every 2 levels of parentheses.

      --------------------
      -- 4.5  Operators --
      --------------------

      --  UNARY_ADDING_OPERATOR        ::=  +   | -

      --  LOGICAL_OPERATOR             ::=  AND | OR  | XOR

      --  RELATIONAL_OPERATOR          ::=  =   | /=  | <   | <= | > | >=

      --  BINARY_ADDING_OPERATOR       ::=  +   | -   | &

      --  MULTIPLYING_OPERATOR         ::=  *   | /   | MOD | REM

      --  HIGHEST_PRECEDENCE_OPERATOR  ::=  **  | ABS | NOT

      --  N_Op_And
      --  Sloc points to AND
      --  Do_Length_Check (Flag4-Sem)
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_And_Then
      --  Sloc points to AND of AND THEN
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Or
      --  Sloc points to OR
      --  Do_Length_Check (Flag4-Sem)
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Or_Else
      --  Sloc points to OR of OR ELSE
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Xor
      --  Sloc points to XOR
      --  Do_Length_Check (Flag4-Sem)
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_In
      --  Sloc points to IN
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Not_In
      --  Sloc points to NOT of NOT IN
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Eq
      --  Sloc points to =
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Ne
      --  Sloc points to /=
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Lt
      --  Sloc points to <
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Le
      --  Sloc points to <=
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Gt
      --  Sloc points to >
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Ge
      --  Sloc points to >=
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Add
      --  Sloc points to + (binary)
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Subtract
      --  Sloc points to - (binary)
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Concat
      --  Sloc points to &
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Multiply
      --  Sloc points to *
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Divide
      --  Sloc points to /
      --  Do_Division_Check (Flag3-Sem)
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Mod
      --  Sloc points to MOD
      --  Do_Division_Check (Flag3-Sem)
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Rem
      --  Sloc points to REM
      --  Do_Division_Check (Flag3-Sem)
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Expon
      --  Sloc points to **
      --  plus fields for binary operator
      --  plus fields for expression

      --  N_Op_Plus
      --  Sloc points to + (unary)
      --  plus fields for unary operator
      --  plus fields for expression

      --  N_Op_Minus
      --  Sloc points to - (unary)
      --  plus fields for unary operator
      --  plus fields for expression

      --  N_Op_Abs
      --  Sloc points to ABS
      --  plus fields for unary operator
      --  plus fields for expression

      --  N_Op_Not
      --  Sloc points to NOT
      --  plus fields for unary operator
      --  plus fields for expression

      --------------------------
      -- 4.6  Type Conversion --
      --------------------------

      --  TYPE_CONVERSION ::=
      --    SUBTYPE_MARK (EXPRESSION) | SUBTYPE_MARK (NAME)

      --  In the (NAME) case, the name is stored as the expression

      --  Note: the parser never generates a type conversion node, since it
      --  looks like an indexed component which is generated by preference.
      --  The semantic pass must correct this misidentification.

      --  N_Type_Conversion
      --  Sloc points to first token of subtype mark
      --  Subtype_Mark (Node4)
      --  Expression (Node3)
      --  Do_Overflow_Check (Flag2-Sem)
      --  Do_Tag_Check (Flag3-Sem)
      --  Do_Length_Check (Flag4-Sem)
      --  Unchecked_Conversion (Flag11-Sem)
      --  plus fields for expression

      --  Note: if a range check is required, then the Do_Range_Check flag
      --  is set in the Expression with the check being done against the
      --  target type range (after the base type conversion, if any).

      -------------------------------
      -- 4.7  Qualified Expression --
      -------------------------------

      --  QUALIFIED_EXPRESSION ::=
      --    SUBTYPE_MARK ' (EXPRESSION) | SUBTYPE_MARK ' AGGREGATE

      --  Note: the parentheses in the (EXPRESSION) case are deemed to enclose
      --  the expression, so the Expression field of this node always points
      --  to a parenthesized expression in this case (i.e. either Parens will
      --  be set, or the referenced node will be an N_Parenthesized_Expression
      --  node if there are additional levels of parentheses.

      --  N_Qualified_Expression
      --  Sloc points to apostrophe
      --  Subtype_Mark (Node4)
      --  Expression (Node3) expression or aggregate
      --  plus fields for expression

      --------------------
      -- 4.8  Allocator --
      --------------------

      --  ALLOCATOR ::=
      --    new SUBTYPE_INDICATION | new QUALIFIED_EXPRESSION

      --  N_Allocator
      --  Sloc points to NEW
      --  Expression (Node3) subtype indication or qualified expression
      --  Do_Storage_Check (Flag2-Sem)
      --  plus fields for expression

      ---------------------------------
      -- 5.1  Sequence Of Statements --
      ---------------------------------

      --  SEQUENCE_OF_STATEMENTS ::= STATEMENT {STATEMENT}

      --  Note: Although the parser will not accept a declaration as a
      --  statement, the semantic analyzer may insert declarations (e.g.
      --  declarations of implicit types needed for execution of other
      --  statements) into a sequence of statements, so the code genmerator
      --  should be prepared to accept a declaration where a statement is
      --  expected. Note also that pragmas can appear as statements.

      --------------------
      -- 5.1  Statement --
      --------------------

      --  STATEMENT ::=
      --    {LABEL} SIMPLE_STATEMENT | {LABEL} COMPOUND_STATEMENT

      --  There is no explicit node in the tree for a statement. Instead, the
      --  individual statement appears directly. Labels are treated  as a
      --  kind of statement, i.e. they are linked into a statement list at
      --  the point they appear, so the labeled statement appears following
      --  the label or labels in the statement list.

      ---------------------------
      -- 5.1  Simple Statement --
      ---------------------------

      --  SIMPLE_STATEMENT ::=
      --    NULL_STATEMENT           | ASSIGNMENT_STATEMENT
      --  | EXIT_STATEMENT           | RETURN_STATEMENT
      --  | GOTO_STATEMENT           | PROCEDURE_CALL_STATEMENT
      --  | ENTRY_CALL_STATEMENT     | REQUEUE_STATEMENT
      --  | DELAY_STATEMENT          | ABORT_STATEMENT
      --  | RAISE_STATEMENT          | CODE_STATEMENT

      -----------------------------
      -- 5.1  Compound Statement --
      -----------------------------

      --  COMPOUND_STATEMENT ::=
      --    IF_STATEMENT         | CASE_STATEMENT
      --  | LOOP_STATEMENT       | BLOCK_STATEMENT
      --  | ACCEPT_STATEMENT     | SELECT_STATEMENT

      ----------------
      -- 5.1  Label --
      ----------------

      --  LABEL ::= <<label_STATEMENT_IDENTIFIER>>

      --  STATEMENT_INDENTIFIER ::= DIRECT_NAME

      --  The IDENTIFIER of a STATEMENT_IDENTIFIER shall be an identifier
      --  (not an OPERATOR_SYMBOL)

      --  Note that the occurrence of a label is not a defining identifier,
      --  but rather a referencing occurrence. The defining occurrence is
      --  in the implicit label declaration which occurs in the innermost
      --  enclosing block.

      --  N_Label
      --  Sloc points to <<
      --  Identifier (Node1) direct name

      --------------------------------
      -- Implicit Label Declaration --
      --------------------------------

      --  An implicit label declaration is created for every occurrence of a
      --  label on a statement or a label on a block or loop. It is chained
      --  in the declarations of the innermost enclosing block as specified
      --  in RM section 5.1 (3).

      --  Note: from the grammar, this might better be called an implicit
      --  statement identifier declaration, but the term we choose seems
      --  friendlier, since at least informally statement identifiers are
      --  called labels in both cases (i.e. when used in labels, and when
      --  used as the identifiers of blocks and loops).

      --  The label field in the implicit label node points to the node
      --  containing the label (N_Label, N_Loop_Statement, N_Block_Statement).

      --  N_Implicit_Label_Declaration
      --  Sloc points to the << of the label
      --  Defining_Identifier (Node1)
      --  Label (Node2) points to the associated node

      -------------------------
      -- 5.1  Null Statement --
      -------------------------

      --  NULL_STATEMENT ::= null;

      --  N_Null_Statement
      --  Sloc points to NULL

      -------------------------------
      -- 5.2  Assignment Statement --
      -------------------------------

      --  ASSIGNMENT_STATEMENT ::=
      --    variable_NAME := EXPRESSION;

      --  N_Assignment_Statement
      --  Sloc points to :=
      --  Name (Node2)
      --  Expression (Node3)
      --  Do_Tag_Check (Flag3-Sem)
      --  Do_Length_Check (Flag4-Sem)
      --  Assignment_OK (Flag5-Sem)
      --  Note: if a range check is required, then the Do_Range_Check flag
      --  is set in the Expression (right hand side), with the check being
      --  done against the type of the Name (left hand side).

      -----------------------
      -- 5.3  If Statement --
      -----------------------

      --  IF_STATEMENT ::=
      --    if CONDITION then
      --      SEQUENCE_OF_STATEMENTS
      --    {elsif CONDITION then
      --      SEQUENCE_OF_STATEMENTS}
      --    [else
      --      SEQUENCE_OF_STATEMENTS]
      --    end if;

      --  N_If_Statement
      --  Sloc points to IF
      --  Condition (Node5)
      --  Then_Statements (List2)
      --  Elsif_Parts (List3) (set to No_List if none present)
      --  Else_Statements (List4) (set to No_List if no else part present)

      --  N_Elsif_Part
      --  Sloc points to ELSIF
      --  Condition (Node5)
      --  Then_Statements (List2)

      --------------------
      -- 5.3  Condition --
      --------------------

      --  CONDITION ::= boolean_EXPRESSION

      -------------------------
      -- 5.4  Case Statement --
      -------------------------

      --  CASE_STATEMENT ::=
      --    case EXPRESSION is
      --      CASE_STATEMENT_ALTERNATIVE
      --      {CASE_STATEMENT_ALTERNATIVE}
      --    end case;

      --  N_Case_Statement
      --  Sloc points to CASE
      --  Expression (Node3)
      --  Alternatives (List4)

      -------------------------------------
      -- 5.4  Case Statement Alternative --
      -------------------------------------

      --  CASE_STATEMENT_ALTERNATIVE ::=
      --    when DISCRETE_CHOICE_LIST =>
      --      SEQUENCE_OF_STATEMENTS

      --  N_Case_Statement_Alternative
      --  Sloc points to WHEN
      --  Discrete_Choices (List4)
      --  Statements (List3)

      -------------------------
      -- 5.5  Loop Statement --
      -------------------------

      --  LOOP_STATEMENT ::=
      --    [loop_STATEMENT_IDENTIFIER :]
      --      [ITERATION_SCHEME] loop
      --        SEQUENCE_OF_STATEMENTS
      --      end loop [loop_IDENTIFIER];

      --  Note: The occurrence of a loop label is not a defining identifier
      --  but rather a referencing occurrence. The defining occurrence is in
      --  the implicit label declaration which occurs in the innermost
      --  enclosing block.

      --  Note: there is always a loop statement identifier present in
      --  the tree, even if none was given in the source. In the case where
      --  no loop identifier is given in the source, the parser creates
      --  a name of the form _Loop_n, where n is a decimal integer (the
      --  two underlines ensure that the loop names created in this manner
      --  do not conflict with any user defined identifiers), and the flag
      --  Has_Created_Identifier is set to True. The only exception to the
      --  rule that all loop statement nodes have identifiers occurs for
      --  loops constructed by the expander, when it is known that no name
      --  is required.

      --  N_Loop_Statement
      --  Sloc points to LOOP
      --  Identifier (Node1) loop identifier (always present, see above)
      --  Iteration_Scheme (Node2) (set to Empty if no iteration scheme)
      --  Statements (List3)
      --  Has_Created_Identifier (Flag1)

      --------------------------
      -- 5.5 Iteration Scheme --
      --------------------------

      --  ITERATION_SCHEME ::=
      --    while CONDITION | for LOOP_PARAMETER_SPECIFICATION

      --  N_Iteration_Scheme
      --  Sloc points to WHILE or FOR
      --  Condition (Node5) (set to Empty if for case)
      --  Loop_Parameter_Specification (Node2) (set to Empty if while case)

      ---------------------------------------
      -- 5.5  Loop parameter specification --
      ---------------------------------------

      --  LOOP_PARAMETER_SPECIFICATION ::=
      --    DEFINING_IDENTIFIER in [reverse] DISCRETE_SUBTYPE_DEFINITION

      --  N_Loop_Parameter_Specification
      --  Sloc points to first identifier
      --  Defining_Identifier (Node1)
      --  Reverse_Present (Flag1)
      --  Discrete_Subtype_Definition (Node4)

      --------------------------
      -- 5.6  Block Statement --
      --------------------------

      --  BLOCK_STATEMENT ::=
      --    [block_STATEMENT_IDENTIFIER:]
      --      [declare
      --        DECLARATIVE_PART]
      --      begin
      --        HANDLED_SEQUENCE_OF_STATEMENTS
      --      end [block_IDENTIFIER];

      --  Note that the occurrence of a block identifier is not a defining
      --  identifier, but rather a referencing occurrence. The defining
      --  occurrence is in the implicit label declaration which occurs in
      --  the innermost enclosing block.

      --  Note: there is always a block statement identifier present in
      --  the tree, even if none was given in the source. In the case where
      --  no block identifier is given in the source, the parser creates
      --  a name of the form _Block_n, where n is a decimal integer (the
      --  two underlines ensure that the block names created in this manner
      --  do not conflict with any user defined identifiers), and the flag
      --  Has_Created_Identifier is set to True. The only exception to the
      --  rule that all loop statement nodes have identifiers occurs for
      --  loops constructed by the expander, when it is known that no name
      --  is required.

      --  N_Block_Statement
      --  Sloc points to DECLARE or BEGIN
      --  Identifier (Node1) block direct name (always present, see above)
      --  Declarations (List3) (set to No_List if no DECLARE part)
      --  Handled_Statement_Sequence (Node4)
      --  Is_Task_Master (Flag5-Sem)
      --  Activation_Chain_Entity (Node2-Sem)
      --  Has_Created_Identifier (Flag1)

      -------------------------
      -- 5.7  Exit Statement --
      -------------------------

      --  EXIT_STATEMENT ::= exit [loop_NAME] [when CONDITION];

      --  N_Exit_Statement
      --  Sloc points to EXIT
      --  Name (Node2) (set to Empty if no loop name present)
      --  Condition (Node5) (set to Empty if no when part present)

      ---------------------------
      -- 5.8  Return Statement --
      ---------------------------

      --  RETURN_STATEMENT is moved to section 6.5

      -------------------------
      -- 5.9  Goto Statement --
      -------------------------

      --  GOTO_STATEMENT ::= goto label_NAME;

      --  N_Goto_Statement
      --  Sloc points to GOTO
      --  Name (Node2)

      ---------------------------------
      -- 6.1  Subprogram Declaration --
      ---------------------------------

      --  SUBPROGRAM_DECLARATION ::= SUBPROGRAM_SPECIFICATION;

      --  N_Subprogram_Declaration
      --  Sloc points to FUNCTION or PROCEDURE
      --  Specification (Node1)
      --  Corresponding_Body (Node5-Sem)
      --  Parent_Spec (Node4-Lib)

      ------------------------------------------
      -- 6.1  Abstract Subprogram Declaration --
      ------------------------------------------

      --  ABSTRACT_SUBPROGRAM_DECLARATION ::=
      --    SUBPROGRAM_SPECIFICATION is abstract;

      --  N_Abstract_Subprogram_Declaration
      --  Sloc points to ABSTRACT
      --  Specification (Node1)

      -----------------------------------
      -- 6.1  Subprogram Specification --
      -----------------------------------

      --  SUBPROGRAM_SPECIFICATION ::=
      --    procedure DEFINING_PROGRAM_UNIT_NAME PARAMETER_PROFILE
      --  | function DEFINING_DESIGNATOR PARAMETER_AND_RESULT_PROFILE

      --  Note: there are no separate nodes for the profiles, instead the
      --  information appears directly in the following nodes.

      --  N_Function_Specification
      --  Sloc points to FUNCTION
      --  Defining_Unit_Name (Node1) (the designator)
      --  Parameter_Specifications (List2) (set to No_List if no formal part)
      --  Subtype_Mark (Node4) for return type
      --  Generic_Parent (Node5-Sem)

      --  N_Procedure_Specification
      --  Sloc points to PROCEDURE
      --  Defining_Unit_Name (Node1)
      --  Parameter_Specifications (List2) (set to No_List if no formal part)
      --  Generic_Parent (Node5-Sem)

      ---------------------
      -- 6.1  Designator --
      ---------------------

      --  DESIGNATOR ::=
      --    [PARENT_UNIT_NAME .] IDENTIFIER | OPERATOR_SYMBOL

      --  Designators that are simply identifiers or operator symbols appear
      --  directly in the tree in this form. The following node is used only
      --  in the case where the designator has a parent unit name component.

      --  N_Designator
      --  Sloc points to period
      --  Name (Node2) holds the parent unit name. Note that this is always
      --   non-Empty, since this node is only used for the case where a
      --   parent library unit package name is present.
      --  Identifier (Node1)

      ------------------------------
      -- 6.1  Defining Designator --
      ------------------------------

      --  DEFINING_DESIGNATOR ::=
      --    DEFINING_PROGRAM_UNIT_NAME | DEFINING_OPERATOR_SYMBOL

      -------------------------------------
      -- 6.1  Defining Program Unit Name --
      -------------------------------------

      --  DEFINING_PROGRAM_UNIT_NAME ::=
      --    [parent_library_unit_package_NAME .] DEFINING_IDENTIFIER

      --  The parent library unit package name is present only in the case
      --  of a child unit name (permissible only for Ada 9X for a library
      --  level unit, i.e. a unit at scope level one). If no such name is
      --  present, the defining program unit name is represented simply as
      --  the defining identifier. In the child unit case, the following
      --  node is used to represent the child unit name.

      --  N_Defining_Program_Unit_Name
      --  Sloc points to period
      --  Name (Node2) holds the parent unit name. Note that this is always
      --   non-Empty, since this node is only used for the case where a
      --   parent library unit package name is present.
      --  Defining_Identifier (Node1)

      --------------------------
      -- 6.1  Operator Symbol --
      --------------------------

      --  OPERATOR_SYMBOL ::= STRING_LITERAL

      --  Note: the fields of the N_Operator_Symbol node are layed out to
      --  match the corresponding fields of an N_Character_Literal node. This
      --  allows easy conversion of the operator symbol node into a character
      --  literal node in the case where a string constant of the form of an
      --  operator symbol is scanned out as such, but turns out semantically
      --  to be a string literal that is not an operator. For details see
      --  Sinfo.Change.Change_Operator_Symbol_To_String_Literal.

      --  N_Operator_Symbol
      --  Sloc points to literal
      --  Chars (Name1) contains the Name_Id for the operator symbol
      --  Strval (Str3) Id of string value. This is used if the operator
      --   symbol turns out to be a normal string after all.
      --  Entity (Node4-Sem)
      --  Etype (Node5-Sem)

      -----------------------------------
      -- 6.1  Defining Operator Symbol --
      -----------------------------------

      --  DEFINING_OPERATOR_SYMBOL ::= OPERATOR_SYMBOL

      --  A defining operator symbol is an entity, which has additional
      --  fields depending on the setting of the Ekind field. These
      --  additional fields are defined (and access subprograms declared)
      --  in package Entity_Info.

      --  Note: N_Defining_Operator_Symbol is an extended node whose fields
      --  are deliberate layed out to match the layout of fields in an ordinary
      --  N_Operator_Symbol node allowing for easy alteration of an operator
      --  symbol node into a defining operator symbol node. For details, see
      --  Sinfo.Change.Change_Operator_Symbol_To_Defining_Operator_Symbol.

      --  N_Defining_Operator_Symbol
      --  Sloc points to literal
      --  Chars (Name1) contains the Name_Id for the operator symbol
      --  Etype (Node5-Sem)

      ----------------------------
      -- 6.1  Parameter Profile --
      ----------------------------

      --  PARAMETER_PROFILE ::= [FORMAL_PART]

      ---------------------------------------
      -- 6.1  Parameter and Result Profile --
      ---------------------------------------

      --  PARAMETER_AND_RESULT_PROFILE ::= [FORMAL_PART] RETURN SUBTYPE_MARK

      --  There is no explicit node in the tree for a parameter and result
      --  profile. Instead the information appears directly in the parent.

      ----------------------
      -- 6.1  Formal part --
      ----------------------

      --  FORMAL_PART ::= (PARAMETER_SPECIFICATION
      --                     {; PARAMETER_SPECIFICATION})

      ----------------------------------
      -- 6.1  Parameter specification --
      ----------------------------------

      --  PARAMETER_SPECIFICATION ::=
      --    DEFINING_IDENTIFIER_LIST : MODE SUBTYPE_MARK
      --      [:= DEFAULT_EXPRESSION]
      --  | DEFINING_IDENTIFIER_LIST : ACCESS_DEFINITION
      --      [:= DEFAULT_EXPRESSION]

      --  Although the syntax allows multiple identifiers in the list, the
      --  semantics is as though successive specifications were given with
      --  identical type definition and expression components. To simplify
      --  semantic processing, the parser represents a multiple declaration
      --  case as a sequence of single Specifications, using the More_Ids and
      --  Prev_Ids flags to preserve the original source form as described
      --  in the section on "Handling of Defining Identifier Lists".

      --  N_Parameter_Specification
      --  Sloc points to first identifier
      --  Defining_Identifier (Node1)
      --  In_Present (Flag1)
      --  Out_Present (Flag2)
      --  Parameter_Type (Node2) subtype mark or access definition
      --  Expression (Node3) (set to Empty if no initialization present)
      --  Do_Accessibility_Check (Flag3-Sem)
      --  More_Ids (Flag5) (set to False if no more identifiers in list)
      --  Prev_Ids (Flag6) (set to False if no previous identifiers in list)

      ---------------
      -- 6.1  Mode --
      ---------------

      --  MODE ::= [in] | in out | out

      --  There is no explicit node in the tree for the Mode. Instead the
      --  In_Present and Out_Present flags are set in the parent node to
      --  record the presence of keywords specifying the mode.

      --------------------------
      -- 6.3  Subprogram Body --
      --------------------------

      --  SUBPROGRAM_BODY ::=
      --    SUBPROGRAM_SPECIFICATION is
      --      DECLARATIVE_PART
      --    begin
      --      HANDLED_SEQUENCE_OF_STATEMENTS
      --    end [DESIGNATOR];

      --  N_Subprogram_Body
      --  Sloc points to FUNCTION or PROCEDURE
      --  Specification (Node1)
      --  Declarations (List3)
      --  Handled_Statement_Sequence (Node4)
      --  Bad_Is_Detected (Flag1) used only by parser
      --  Corresponding_Spec (Node5-Sem)
      --  Acts_As_Spec (Flag4-Sem)
      --  Do_Storage_Check (Flag2-Sem)
      --  Is_Task_Master (Flag5-Sem)
      --  Activation_Chain_Entity (Node2-Sem)
      --  Has_Priority_Pragma (Flag6-Sem)

      -----------------------------------
      -- 6.4  Procedure Call Statement --
      -----------------------------------

      --  PROCEDURE_CALL_STATEMENT ::=
      --    procedure_NAME; | procedure_PREFIX ACTUAL_PARAMETER_PART;

      --  Note: the reason that a procedure call has expression fields is
      --  that it semantically resembles an expression, e.g. overloading is
      --  allowed and a type is concocted for semantic processing purposes.
      --  Certain of these fields, such as Parens are not relevant, but it
      --  is easier to just supply all of them together!

      --  N_Procedure_Call_Statement
      --  Sloc points to first token of name or prefix
      --  Name (Node2) stores name or prefix
      --  Parameter_Associations (List3) (set to No_List if no
      --   actual parameter part)
      --  First_Named_Actual (Node4-Sem)
      --  Controlling_Argument (Node1-Sem) controlling tag (set to Empty if
      --   non-dispatching call)
      --  Do_Elaboration_Check (Flag2-Sem)
      --  Do_Tag_Check (Flag3-Sem)
      --  plus fields for expression

      --  If any IN parameter requires a range check, then the corresponding
      --  argument expression has the Do_Range_Check flag set, and the range
      --  check is done against the formal type. Note that this argument
      --  expression may appear directly in the Parameter_Associations list,
      --  or may be a descendent of an N_Parameter_Association node that
      --  appears in this list.

      ------------------------
      -- 6.4  Function Call --
      ------------------------

      --  FUNCTION_CALL ::=
      --    function_NAME | function_PREFIX ACTUAL_PARAMETER_PART

      --  Note: the parser may generate an indexed component node or simply
      --  a name node instead of a function call node. The semantic pass must
      --  correct this misidentification.

      --  N_Function_Call
      --  Sloc points to first token of name or prefix
      --  Name (Node2) stores name or prefix
      --  Parameter_Associations (List3) (set to No_List if no
      --   actual parameter part)
      --  First_Named_Actual (Node4-Sem)
      --  Controlling_Argument (Node1-Sem) controlling tag (set to Empty if
      --   non-dispatching call)
      --  Do_Elaboration_Check (Flag2)
      --  Do_Tag_Check (Flag3-Sem)
      --  plus fields for expression

      --------------------------------
      -- 6.4  Actual Parameter Part --
      --------------------------------

      --  ACTUAL_PARAMETER_PART ::=
      --    (PARAMETER_ASSOCIATION {,PARAMETER_ASSOCIATION})

      --------------------------------
      -- 6.4  Parameter Association --
      --------------------------------

      --  PARAMETER_ASSOCIATION ::=
      --    [formal_parameter_SELECTOR_NAME =>] ACTUAL_PARAMETER

      --  Note: the N_Parameter_Association node is built only if a formal
      --  parameter selector name is present, otherwise the parameter
      --  association appears in the tree simply as the node for the
      --  actual parameter.

      --  N_Parameter_Association
      --  Sloc points to formal parameter
      --  Selector_Name (Node3) (always non-Empty, since this node is
      --   only used if a formal parameter selector name is present)
      --  Actual_Parameter (Node2)
      --  Next_Named_Actual (Node4-Sem)

      ---------------------------
      -- 6.4  Formal Parameter --
      ---------------------------

      --  FORMAL_PARAMETER ::= parameter_SELECTOR_NAME

      ---------------------------
      -- 6.4  Actual Parameter --
      ---------------------------

      --  ACTUAL_PARAMETER ::= EXPRESSION | variable_NAME

      ---------------------------
      -- 6.5  Return Statement --
      ---------------------------

      --  RETURN_STATEMENT ::= return [EXPRESSION];

      --  N_Return_Statement
      --  Sloc points to RETURN
      --  Expression (Node3) (set to Empty if no expression present)
      --  Do_Tag_Check (Flag3-Sem)

      --  Note: if a range check is required, then an additional type
      --  conversion node is introduced to represent the required check.

      ------------------------------
      -- 7.1  Package Declaration --
      ------------------------------

      --  PACKAGE_DECLARATION ::= PACKAGE_SPECIFICATION;

      --  Note: the activation chain entity for a package spec is used for
      --  all tasks declared in the package spec, or in the package body.

      --  N_Package_Declaration
      --  Sloc points to PACKAGE
      --  Specification (Node1)
      --  Corresponding_Body (Node5-Sem)
      --  Parent_Spec (Node4-Lib)
      --  Activation_Chain_Entity (Node2-Sem)

      --------------------------------
      -- 7.1  Package Specification --
      --------------------------------

      --  PACKAGE_SPECIFICATION ::=
      --    package DEFINING_PROGRAM_UNIT_NAME is
      --      {BASIC_DECLARATIVE_ITEM}
      --    [private
      --      {BASIC_DECLARATIVE_ITEM}]
      --    end [[PARENT_UNIT_NAME .] IDENTIFIER]

      --  N_Package_Specification
      --  Sloc points to PACKAGE
      --  Defining_Unit_Name (Node1)
      --  Visible_Declarations (List2)
      --  Private_Declarations (List4) (set to No_List if no private
      --   part present)
      --  Generic_Parent (Node5-Sem)

      -----------------------
      -- 7.1  Package Body --
      -----------------------

      --  PACKAGE_BODY ::=
      --    package body DEFINING_PROGRAM_UNIT_NAME is
      --      DECLARATIVE_PART
      --    [begin
      --      HANDLED_SEQUENCE_OF_STATEMENTS]
      --    end [[parent_library_unit_package_NAME .] IDENTIFIER];

      --  N_Package_Body
      --  Sloc points to PACKAGE
      --  Defining_Unit_Name (Node1)
      --  Declarations (List3)
      --  Handled_Statement_Sequence (Node4) (set to Empty if not present)
      --  Corresponding_Spec (Node5-Sem)

      -----------------------------------
      -- 7.4  Private Type Declaration --
      -----------------------------------

      --  PRIVATE_TYPE_DECLARATION ::=
      --    type DEFINING_IDENTIFIER [DISCRIMINANT_PART]
      --      is [abstract] [tagged] [limited] private;

      --  Note: TAGGED is not permitted in Ada 83 mode

      --  N_Private_Type_Declaration
      --  Sloc points to TYPE
      --  Defining_Identifier (Node1)
      --  Discriminant_Specifications (List2) (set to No_List if no
      --   discriminant part)
      --  Has_Unknown_Discriminants (Flag3) set if (<>) discriminant
      --  Abstract_Present (Flag4)
      --  Tagged_Present (Flag1)
      --  Limited_Present (Flag2)

      ----------------------------------------
      -- 7.4  Private Extension Declaration --
      ----------------------------------------

      --  PRIVATE_EXTENSION_DECLARATION ::=
      --    type DEFINING_IDENTIFIER [DISCRIMINANT_PART]
      --      is new [abstract] ancestor_SUBTYPE_INDICATION with private;

      --  Note: private extension declarations are not allowed in Ada 83 mode

      --  N_Private_Extension_Declaration
      --  Sloc points to TYPE
      --  Defining_Identifier (Node1)
      --  Discriminant_Specifications (List2) (set to No_List if no
      --   discriminant part)
      --  Has_Unknown_Discriminants (Flag3) set if (<>) discriminant
      --  Abstract_Present (Flag4)
      --  Subtype_Indication (Node4)

      ---------------------
      -- 8.4  Use Clause --
      ---------------------

      --  USE_CLAUSE ::= USE_PACKAGE_CLAUSE | USE_TYPE_CLAUSE

      -----------------------------
      -- 8.4  Use Package Clause --
      -----------------------------

      --  USE_PACKAGE_CLAUSE ::= use package_NAME {, package_NAME};

      --  N_Use_Package_Clause
      --  Sloc points to USE
      --  Names (List2)

      --------------------------
      -- 8.4  Use Type Clause --
      --------------------------

      --  USE_TYPE_CLAUSE ::= use type SUBTYPE_MARK {, SUBTYPE_MARK};

      --  Note: use type clause is not permitted in Ada 83 mode

      --  N_Use_Type_Clause
      --  Sloc points to USE
      --  Subtype_Marks (List2)

      -------------------------------
      -- 8.5  Renaming Declaration --
      -------------------------------

      --  RENAMING_DECLARATION ::=
      --    OBJECT_RENAMING_DECLARATION
      --  | EXCEPTION_RENAMING_DECLARATION
      --  | PACKAGE_RENAMING_DECLARATION
      --  | SUBPROGRAM_RENAMING_DECLARATION
      --  | GENERIC_RENAMING_DECLARATION

      --------------------------------------
      -- 8.5  Object Renaming Declaration --
      --------------------------------------

      --  OBJECT_RENAMING_DECLARATION ::=
      --    DEFINING_IDENTIFIER : SUBTYPE_MARK renames object_NAME;

      --  N_Object_Renaming_Declaration
      --  Sloc points to first identifier
      --  Defining_Identifier (Node1) 
      --  Subtype_Mark (Node4)
      --  Name (Node2)

      -----------------------------------------
      -- 8.5  Exception Renaming Declaration --
      -----------------------------------------

      --  EXCEPTION_RENAMING_DECLARATION ::=
      --    DEFINING_IDENTIFIER : exception renames exception_NAME;

      --  N_Exception_Renaming_Declaration
      --  Sloc points to first identifier 
      --  Defining_Identifier (Node1) 
      --  Name (Node2)

      ---------------------------------------
      -- 8.5  Package Renaming Declaration --
      ---------------------------------------

      --  PACKAGE_RENAMING_DECLARATION ::=
      --    package DEFINING_PROGRAM_UNIT_NAME renames package_NAME;

      --  N_Package_Renaming_Declaration
      --  Sloc points to PACKAGE
      --  Defining_Unit_Name (Node1)
      --  Name (Node2)
      --  Parent_Spec (Node4-Lib)

      ------------------------------------------
      -- 8.5  Subprogram Renaming Declaration --
      ------------------------------------------

      --  SUBPROGRAM_RENAMING_DECLARATION ::=
      --    SUBPROGRAM_SPECIFICATION renames callable_entity_NAME;

      --  N_Subprogram_Renaming_Declaration
      --  Sloc points to RENAMES
      --  Specification (Node1)
      --  Name (Node2)
      --  Parent_Spec (Node4-Lib)
      --  Corresponding_Spec (Node5-Sem)

      -----------------------------------------
      -- 8.5.5  Generic Renaming Declaration --
      -----------------------------------------

      --  GENERIC_RENAMING_DECLARATION ::=
      --    generic package DEFINING_PROGRAM_UNIT_NAME
      --      renames generic_package_NAME
      --  | generic procedure DEFINING_PROGRAM_UNIT_NAME
      --      renames generic_procedure_NAME
      --  | generic function DEFINING_PROGRAM_UNIT_NAME
      --      renames generic_function_NAME

      --  N_Generic_Package_Renaming_Declaration
      --  Sloc points to GENERIC
      --  Defining_Unit_Name (Node1)
      --  Name (Node2)
      --  Parent_Spec (Node4-Lib)

      --  N_Generic_Procedure_Renaming_Declaration
      --  Sloc points to GENERIC
      --  Defining_Unit_Name (Node1)
      --  Name (Node2)
      --  Parent_Spec (Node4-Lib)

      --  N_Generic_Function_Renaming_Declaration
      --  Sloc points to GENERIC
      --  Defining_Unit_Name (Node1)
      --  Name (Node2)
      --  Parent_Spec (Node4-Lib)

      --------------------------------
      -- 9.1  Task Type Declaration --
      --------------------------------

      --  TASK_TYPE_DECLARATION ::=
      --    task type DEFINING_IDENTIFIER [KNOWN_DISCRIMINANT_PART]
      --      [is TASK_DEFINITITION];

      --  N_Task_Type_Declaration
      --  Sloc points to TASK
      --  Defining_Identifier (Node1)
      --  Discriminant_Specifications (List2) (set to No_List if no
      --   discriminant part)
      --  Task_Definition (Node3) (set to Empty if not present)

      ----------------------------------
      -- 9.1  Single Task Declaration --
      ----------------------------------

      --  SINGLE_TASK_DECLARATION ::=
      --    task DEFINING_IDENTIFIER [is TASK_DEFINITION];

      --  N_Single_Task_Declaration
      --  Sloc points to TASK
      --  Defining_Identifier (Node1)
      --  Discriminant_Specifications (List2) (set to No_List if true,
      --   i.e. always set to No_List)
      --  Task_Definition (Node3) (set to Empty if not present)

      --  Note: the reason we add a Discriminant_Specifications field
      --  that is always set to No_List is to alow easier processing of
      --  single task declarations using code for task type declarations.

      --------------------------
      -- 9.1  Task Definition --
      --------------------------

      --  TASK_DEFINITION ::=
      --      {TASK_ITEM}
      --    [private
      --      {TASK_ITEM}
      --    end [task_IDENTIFIER]

      --  Note: as a result of semantic analysis, the list of task items can
      --  include implicit type declarations resulting from entry families.

      --  N_Task_Definition
      --  Sloc points to first token of task definition
      --  Visible_Declarations (List2)
      --  Private_Declarations (List4) (set to No_List if no private part)
      --  Has_Priority_Pragma (Flag6-Sem)
      --  Has_Task_Stack_Size_Pragma (Flag5-Sem)

      --------------------
      -- 9.1  Task Item --
      --------------------

      --  TASK_ITEM ::= ENTRY_DECLARATION | REPRESENTATION_CLAUSE

      --------------------
      -- 9.1  Task Body --
      --------------------

      --  TASK_BODY ::=
      --    task body task_DEFINING_IDENTIFIER is
      --      DECLARATIVE_PART
      --    begin
      --      HANDLED_SEQUENCE_OF_STATEMENTS
      --    end [task_IDENTIFIER];

      --  N_Task_Body
      --  Sloc points to TASK
      --  Defining_Identifier (Node1)
      --  Declarations (List3)
      --  Handled_Statement_Sequence (Node4)
      --  Is_Task_Master (Flag5-Sem)
      --  Activation_Chain_Entity (Node2-Sem)
      --  Corresponding_Spec (Node5-Sem)

      -------------------------------------
      -- 9.4  Protected Type Declaration --
      -------------------------------------

      --  PROTECTED_TYPE_DECLARATION ::=
      --    protected type DEFINING_IDENTIFIER [KNOWN_DISCRIMINANT_PART]
      --      is PROTECTED_DEFINITION;

      --  Note: protected type declarations are not permitted in Ada 83 mode

      --  N_Protected_Type_Declaration
      --  Sloc points to PROTECTED
      --  Defining_Identifier (Node1)
      --  Discriminant_Specifications (List2) (set to No_List if no
      --   discriminant part)
      --  Protected_Definition (Node3)

      ---------------------------------------
      -- 9.4  Single Protected Declaration --
      ---------------------------------------

      --  SINGLE_PROTECTED_DECLARATION ::=
      --    protected DEFINING_IDENTIFIER is PROTECTED_DEFINITION;

      --  Note: single protected declarations are not allowed in Ada 83 mode

      --  N_Single_Protected_Declaration
      --  Sloc points to PROTECTED
      --  Defining_Identifier (Node1)
      --  Discriminant_Specifications (List2) (set to No_List if true,
      --   i.e. always set to No_List)
      --  Protected_Definition (Node3)

      --  Note: the reason we add a Discriminant_Specifications field that
      --  is always set to No_List is to alow easier processing of single
      --  single protected declarations using code for protected type
      --  declarations.

      -------------------------------
      -- 9.4  Protected Definition --
      -------------------------------

      --  PROTECTED_DEFINITION ::=
      --      {PROTECTED_OPERATION_DECLARATION}
      --    [private
      --      {PROTECTED_ELEMENT_DECLARATION}]
      --    end [protected_IDENTIFIER]

      --  N_Protected_Definition
      --  Sloc points to first token of protected definition
      --  Visible_Declarations (List2)
      --  Private_Declarations (List4) (set to No_List if no private part)
      --  Has_Priority_Pragma (Flag6-Sem)

      ------------------------------------------
      -- 9.4  Protected Operation Declaration --
      ------------------------------------------

      --  PROTECTED_OPERATION_DECLARATION ::=
      --    SUBPROGRAM_DECLARATION | ENTRY_DECLARATION

      ----------------------------------------
      -- 9.4  Protected Element Declaration --
      ----------------------------------------

      --  PROTECTED_ELEMENT_DECLARATION ::=
      --    PROTECTED_OPERATION_DECLARATION | COMPONENT_DECLARATION

      -------------------------
      -- 9.4  Protected Body --
      -------------------------

      --  PROTECTED_BODY ::=
      --    protected body DEFINING_IDENTIFIER is
      --      {PROTECTED_OPERATION_ITEM}
      --    end [protected_IDENTIFIER];

      --  Note: protected bodies are not allowed in Ada 83 mode

      --  N_Protected_Body
      --  Sloc points to PROTECTED
      --  Defining_Identifier (Node1)
      --  Declarations (List3) protected operation items (and pragmas)

      -----------------------------------
      -- 9.4  Protected Operation Item --
      -----------------------------------

      --  PROTECTED_OPERATION_ITEM ::=
      --    SUBPROGRAM_DECLARATION | SUBPROGRAM_BODY | ENTRY_BODY

      ------------------------------
      -- 9.5.2  Entry Declaration --
      ------------------------------

      --  ENTRY_DECLARATION ::=
      --    entry DEFINING_IDENTIFIER
      --      [(DISCRETE_SUBTYPE_DEFINITION)] PARAMETER_PROFILE;

      --  N_Entry_Declaration
      --  Sloc points to ENTRY
      --  Defining_Identifier (Node1)
      --  Discrete_Subtype_Definition (Node4) (set to Empty if not present)
      --  Parameter_Specifications (List2) formal part

      -----------------------------
      -- 9.5.2  Accept statement --
      -----------------------------

      --  ACCEPT_STATEMENT ::=
      --    accept entry_DIRECT_NAME
      --      [(ENTRY_INDEX)] PARAMETER_PROFILE [do
      --        HANDLED_SEQUENCE_OF_STATEMENTS
      --    end [entry_IDENTIFIER]];

      --  Note: the syntax does not allow for declarations, but for uniformtiy
      --  with other constructs having a handled sequence of statements, a
      --  Declarations field is present in the node. Normally this will be
      --  empty. The only time it is non-empty is as a result of expansion
      --  activity, on the way to converting the accept to a procedure body.

      --  N_Accept_Statement
      --  Sloc points to ACCEPT
      --  Accept_Name (Node1)
      --  Entry_Index (Node5) (set to Empty if not present)
      --  Declarations (List3) (set to No_List if none, see note above)
      --  Parameter_Specifications (List2) (set to No_List if no formal part)
      --  Handled_Statement_Sequence (Node4)

      ------------------------
      -- 9.5.2  Entry Index --
      ------------------------

      --  ENTRY_INDEX ::= EXPRESSION

      -----------------------
      -- 9.5.2  Entry Body --
      -----------------------

      --  ENTRY_BODY ::=
      --    entry DEFINING_IDENTIFIER ENTRY_BODY_FORMAL_PART ENTRY_BARRIER is
      --      DECLARATIVE_PART
      --    begin
      --      HANDLED_SEQUENCE_OF_STATEMENTS
      --    end [entry_IDENTIFIER];

      --  ENTRY_BARRIER ::= when CONDITION

      --  N_Entry_Body
      --  Sloc points to ENTRY
      --  Defining_Identifier (Node1)
      --  Entry_Body_Formal_Part (Node2)
      --  Declarations (List3)
      --  Handled_Statement_Sequence (Node4)
      --  Condition (Node5) from entry barrier

      -----------------------------------
      -- 9.5.2  Entry Body Formal Part --
      -----------------------------------

      --  ENTRY_BODY_FORMAL_PART ::=
      --    [(ENTRY_INDEX_SPECIFIATION)] [FORMAL_PART]

      --  Note that an entry body formal part node is present even if it is
      --  empty. This reflects the grammar, in which it is the components of
      --  the entry body formal part that are optional, not the entry body
      --  formal part itself.

      --  N_Entry_Body_Formal_Part
      --  Entry_Index_Specification (Node1) (set to Empty if not present)
      --  Parameter_Specifications (List2) (set to No_List if no formal part)

      --------------------------
      -- 9.5.2  Entry Barrier --
      --------------------------

      --  ENTRY_BARRIER ::= when CONDITION

      --------------------------------------
      -- 9.5.2  Entry Index Specification --
      --------------------------------------

      --  ENTRY_INDEX_SPECIFICATION ::=
      --    for DEFINING_IDENTIFIER in DISCRETE_SUBTYPE_DEFINITION

      --  N_Entry_Index_Specification
      --  Sloc points to FOR
      --  Defining_Identifier (Node1)
      --  Discrete_Subtype_Definition (Node4)

      ---------------------------------
      -- 9.5.3  Entry Call Statement --
      ---------------------------------

      --  ENTRY_CALL_STATEMENT ::= entry_NAME [ACTUAL_PARAMETER_PART];

      --  The parser may generate a procedure call for this construct. The
      --  semantic pass must correct this misidentification where needed.

      --  N_Entry_Call_Statement
      --  Sloc points to first token of name
      --  Name (Node2)
      --  Parameter_Associations (List3) (set to No_List if no
      --   actual parameter part)
      --  Do_Elaboration_Check (Flag2-Sem)

      ------------------------------
      -- 9.5.4  Requeue Statement --
      ------------------------------

      --  REQUEUE_STATEMENT ::= requeue entry_NAME [with abort];

      --  Note: requeue statements are not permitted in Ada 83 mode

      --  N_Requeue_Statement
      --  Sloc points to REQUEUE
      --  Name (Node2)
      --  Abort_Present (Flag1)

      --------------------------
      -- 9.6  Delay Statement --
      --------------------------

      --  DELAY_STATEMENT ::=
      --    DELAY_UNTIL_STATEMENT
      --  | DELAY_RELATIVE_STATEMENT

      --------------------------------
      -- 9.6  Delay Until Statement --
      --------------------------------

      --  DELAY_UNTIL_STATEMENT ::= delay until delay_EXPRESSION;

      --  Note: delay until statements are not permitted in Ada 83 mode

      --  N_Delay_Until_Statement
      --  Sloc points to DELAY
      --  Expression (Node3)

      -----------------------------------
      -- 9.6  Delay Relative Statement --
      -----------------------------------

      --  DELAY_RELATIVE_STATEMENT ::= delay delay_EXPRESSION;

      --  N_Delay_Relative_Statement
      --  Sloc points to DELAY
      --  Expression (Node3)

      ---------------------------
      -- 9.7  Select Statement --
      ---------------------------

      --  SELECT_STATEMENT ::=
      --    SELECTIVE_ACCEPT
      --  | CONDITIONAL_ENTRY_CALL
      --  | TIMED_ENTRY_CALL
      --  | ASYNCHRONOUS_SELECT

      -----------------------------
      -- 9.7.1  Selective Accept --
      -----------------------------

      --  SELECTIVE_ACCEPT ::=
      --    select
      --      [GUARD]
      --        SELECT_ALTERNATIVE
      --    {or
      --      [GUARD]
      --        SELECT_ALTERNATIVE}
      --    [else
      --      SEQUENCE_OF_STATEMENTS]
      --    end select;

      --  GUARD ::= when CONDITION =>

      --  Note: the CONDITION that is part of a GUARD is included in the
      --  node for the selective accept alternative for convenience.

      --  N_Selective_Accept
      --  Sloc points to SELECT
      --  Selective_Accept_Alternatives (List1)
      --  Else_Statements (List4) (set to No_List if no else part)

      ------------------
      -- 9.7.1  Guard --
      ------------------

      --  GUARD ::= when CONDITION =>

      --  As noted above, the CONDITION that is part of a GUARD is included
      --  in the node for the selective accept alernative for convenience.

      -------------------------------
      -- 9.7.1  Select Alternative --
      -------------------------------

      --  SELECT_ALTERNATIVE ::=
      --    ACCEPT_ALTERNATIVE | DELAY_ALTERNATIVE | TERMINATE_ALTERNATIVE

      -------------------------------
      -- 9.7.1  Accept Alternative --
      -------------------------------

      --  ACCEPT_ALTERNATIVE ::=
      --    ACCEPT_STATEMENT [SEQUENCE_OF_STATEMENTS]

      --  N_Accept_Alternative
      --  Sloc points to ACCEPT
      --  Accept_Statement (Node2)
      --  Condition (Node5) from the guard (set to Empty if no guard present)
      --  Statements (List3) (set to Empty_List if no statements)

      ------------------------------
      -- 9.7.1  Delay Alternative --
      ------------------------------

      --  DELAY_ALTERNATIVE ::=
      --    DELAY_STATEMENT [SEQUENCE_OF_STATEMENTS]

      --  N_Delay_Alternative
      --  Sloc points to DELAY
      --  Delay_Statement (Node2)
      --  Condition (Node5) from the guard (set to Empty if no guard present)
      --  Statements (List3) (set to Empty_List if no statements)

      ----------------------------------
      -- 9.7.1  Terminate Alternative --
      ----------------------------------

      --  TERMINATE_ALTERNATIVE ::= terminate;

      --  N_Terminate_Alternative
      --  Sloc points to TERMINATE
      --  Condition (Node5) from the guard (set to Empty if no guard present)

      -----------------------------------
      -- 9.7.2  Conditional Entry Call --
      -----------------------------------

      --  CONDITIONAL_ENTRY_CALL ::=
      --    select
      --      ENTRY_CALL_ALTERNATIVE
      --    else
      --      SEQUENCE_OF_STATEMENTS
      --    end select;

      --  N_Conditional_Entry_Call
      --  Sloc points to SELECT
      --  Entry_Call_Alternative (Node1)
      --  Else_Statements (List4)

      -----------------------------------
      -- 9.7.2  Entry Call Alternative --
      -----------------------------------

      --  ENTRY_CALL_ALTERNATIVE ::=
      --    ENTRY_CALL_STATEMENT [SEQUENCE_OF_STATEMENTS]

      --  N_Entry_Call_Alternative
      --  Sloc points to first token of entry call statement
      --  Entry_Call_Statement (Node1)
      --  Statements (List3) (set to Empty_List if no statements)

      -----------------------------
      -- 9.7.3  Timed Entry Call --
      -----------------------------

      --  TIMED_ENTRY_CALL ::=
      --    select
      --      ENTRY_CALL_ALTERNATIVE
      --    or
      --      DELAY_ALTERNATIVE
      --    end select;

      --  N_Timed_Entry_Call
      --  Sloc points to SELECT
      --  Entry_Call_Alternative (Node1)
      --  Delay_Alternative (Node4)

      --------------------------------
      -- 9.7.4  Asynchronous Select --
      --------------------------------

      --  ASYNCHRONOUS_SELECT ::=
      --    select
      --      TRIGGERING_ALTERNATIVE
      --    then abort
      --      ABORTABLE_PART
      --    end select;

      --  Note: asyncrhonous select is not permitted in Ada 83 mode

      --  N_Asynchronous_Select
      --  Sloc points to SELECT
      --  Triggering_Alternative (Node1)
      --  Abortable_Part (Node2)

      -----------------------------------
      -- 9.7.4  Triggering Alternative --
      -----------------------------------

      --  TRIGGERING_ALTERNATIVE ::=
      --    TRIGGERING_STATEMENT [SEQUENCE_OF_STATEMENTS]

      --  N_Triggering_Alternative
      --  Sloc points to first token of triggering statement
      --  Triggering_Statement (Node1)
      --  Statements (List3) (set to Empty_List if no statements)

      ---------------------------------
      -- 9.7.4  Triggering Statement --
      ---------------------------------

      --  TRIGGERING_STATEMENT ::= ENTRY_CALL_STATEMENT | DELAY_STATEMENT

      ---------------------------
      -- 9.7.4  Abortable Part --
      ---------------------------

      --  ABORTABLE_PART ::= SEQUENCE_OF_STATEMENTS

      --  N_Abortable_Part
      --  Sloc points to ABORT
      --  Statements (List3)

      --------------------------
      -- 9.8  Abort Statement --
      --------------------------

      --  ABORT_STATEMENT ::= abort task_NAME {, task_NAME};

      --  N_Abort_Statement
      --  Sloc points to ABORT
      --  Names (List2)

      -------------------------
      -- 10.1.1  Compilation --
      -------------------------

      --  COMPILATION ::= {COMPILATION_UNIT}

      ------------------------------
      -- 10.1.1  Compilation Unit --
      ------------------------------

      --  COMPILATION_UNIT ::=
      --    CONTEXT_CLAUSE [private] LIBRARY_ITEM
      --  | CONTEXT_CLAUSE  SUBUNIT

      --  N_Compilation_Unit
      --  Sloc points to first token of defining unit name
      --  Library_Unit (Node4-Lib) corresponding/parent spec/body
      --  Context_Items (List1) context items and pragmas preceding unit
      --  Private_Present (Flag1) set if library unit has private keyword
      --  Unit (Node2) library item or subunit
      --  Following_Pragmas (List3) pragmas after unit (set to No_List if
      --   no following pragmas)
      --  Preelaborable (Flag2-Sem)
      --  Body_Required (Flag3-Sem) set for spec if body is required
      --  Acts_As_Spec (Flag4-Lib) flag for subprogram body with no spec
      --  Elaborate_Body_Present (Flag7-Sem) set if Elaborate_Body pragma

      --------------------------
      -- 10.1.1  Library Item --
      --------------------------

      --  LIBRARY_ITEM ::=
      --    private LIBRARY_UNIT_DECLARATION | LIBRARY_UNIT_BODY

      --  Note: PRIVATE is not allowed in Ada 83 mode

      --  There is no explicit node in the tree for library item, instead
      --  the declaration or body, and the flag for private if present,
      --  appear in the N_Compilation_Unit clause.

      ----------------------------------------
      -- 10.1.1  Library Unit Declararation --
      ----------------------------------------

      --  LIBRARY_UNIT_DECLARATION ::=
      --    SUBPROGRAM_DECLARATION | PACKAGE_DECLARATION
      --  | GENERIC_DECLARATION    | GENERIC_INSTANTIATION
      --  | LIBRARY_UNIT_RENAMING_DECLARATION

      -------------------------------------------------
      -- 10.1.1  Library Unit Renaming Declararation --
      -------------------------------------------------

      --  LIBRARY_UNIT_RENAMING_DECLARATION ::=
      --    PACKAGE_RENAMING_DECLARATION
      --  | GENERIC_RENAMING_DECLARATION
      --  | SUBPROGRAM_RENAMING_DECLARATION

      -------------------------------
      -- 10.1.1  Library unit body --
      -------------------------------

      --  LIBRARY_UNIT_BODY ::= SUBPROGRAM_BODY | PACKAGE_BODY

      ------------------------------
      -- 10.1.1  Parent Unit Name --
      ------------------------------

      --  PARENT_UNIT_NAME ::= NAME

      ----------------------------
      -- 10.1.2  Context clause --
      ----------------------------

      --  CONTEXT_CLAUSE ::= {CONTEXT_ITEM}

      --  Note: context items can also include pragma Elaborate and pragma
      --  Elaborate_Body, so these items can also appear in this list as
      --  well as other configuration pragmas that might appear. In addition,
      --  any pragmas appearing after the compilation unit, in particular,
      --  pragma Inline's are appended to the end of this list.

      --------------------------
      -- 10.1.2  Context_Item --
      --------------------------

      --  CONTEXT_ITEM ::= WITH_CLAUSE | USE_CLAUSE

      -------------------------
      -- 10.1.2  With clause --
      -------------------------

      --  WITH_CLAUSE ::=
      --    with library_unit_NAME {,library_unit_NAME};

      --  A separate With clause is built for each name, so that we have
      --  a Corresponding_Spec field for each with'ed spec. The flags
      --  First_Name and Last_Name are used to reconstruct the exact
      --  source form. When a list of names appears in one with clause,
      --  the first name in the list has First_Name set, and the last
      --  has Last_Name set. If the with clause has only one name, then
      --  both of the flags First_Name and Last_Name are set in this name.

      --  N_With_Clause
      --  Sloc points to first token of library unit name
      --  Name (Node2)
      --  Library_Unit (Node4-Lib)
      --  Corresponding_Spec (Node5-Sem)
      --  First_Name (Flag5) (set to True if first name or only one name)
      --  Last_Name (Flag6) (set to True if last name or only one name)
      --  Context_Installed (Flag3-Sem)
      --  Elaborate_Present (Flag4-Sem)
      --  Elaborate_All_Present (Flag1-Sem)
      --  Implicit_With (Flag2-Sem)

      ---------------------
      -- 10.2  Body stub --
      ---------------------

      --  BODY_STUB ::=
      --    SUBPROGRAM_BODY_STUB
      --  | PACKAGE_BODY_STUB
      --  | TASK_BODY_STUB
      --  | PROTECTED_BODY_STUB

      ----------------------------------
      -- 10.1.3  Subprogram Body Stub --
      ----------------------------------

      --  SUBPROGRAM_BODY_STUB ::=
      --    SUBPROGRAM_SPECIFICATION is separate;

      --  N_Subprogram_Body_Stub
      --  Sloc points to FUNCTION or PROCEDURE
      --  Specification (Node1)
      --  Library_Unit (Node4-Lib) points to the subunit
      --  Corresponding_Body (Node5-Sem)

      -------------------------------
      -- 10.1.3  Package Body Stub --
      -------------------------------

      --  PACKAGE_BODY_STUB ::=
      --    package body DEFINING_IDENTIFIER is separate;

      --  N_Package_Body_Stub
      --  Sloc points to PACKAGE
      --  Defining_Identifier (Node1)
      --  Library_Unit (Node4-Lib) points to the subunit
      --  Corresponding_Body (Node5-Sem)

      ----------------------------
      -- 10.1.3  Task Body Stub --
      ----------------------------

      --  TASK_BODY_STUB ::=
      --    task body DEFINING_IDENTIFIER is separate;

      --  N_Task_Body_Stub
      --  Sloc points to TASK
      --  Defining_Identifier (Node1)
      --  Library_Unit (Node4-Lib) points to the subunit
      --  Corresponding_Body (Node5-Sem)

      ---------------------------------
      -- 10.1.3  Protected Body Stub --
      ---------------------------------

      --  PROTECTED_BODY_STUB ::=
      --    protected body DEFINING_IDENTIFIER is separate;

      --  Note: protected body stubs are not allowed in Ada 83 mode

      --  N_Protected_Body_Stub
      --  Sloc points to PROTECTED
      --  Defining_Identifier (Node1)
      --  Library_Unit (Node4-Lib) points to the subunit
      --  Corresponding_Body (Node5-Sem)

      ---------------------
      -- 10.1.3  Subunit --
      ---------------------

      --  SUBUNIT ::= separate (PARENT_UNIT_NAME) PROPER_BODY

      --  N_Subunit
      --  Sloc points to SEPARATE
      --  Name (Node2) is the name of the parent unit
      --  Proper_Body (Node1) is the subunit body

      ---------------------------------
      -- 11.1  Exception Declaration --
      ---------------------------------

      --  EXCEPTION_DECLARATION ::= DEFINING_IDENTIFIER_LIST : exception;

      --  For consistency with object declarations etc, the parser converts
      --  the case of multiple identifiers being declared to a series of
      --  declarations in which the expression is copied, using the More_Ids
      --  and Prev_Ids flags to remember the souce form as described in the
      --  section on "Handling of Defining Identifier Lists".

      --  N_Exception_Declaration
      --  Sloc points to EXCEPTION
      --  Defining_Identifier (Node1)
      --  More_Ids (Flag5) (set to False if no more identifiers in list)
      --  Prev_Ids (Flag6) (set to False if no previous identifiers in list)

      ------------------------------------------
      -- 11.2  Handled Sequence Of Statements --
      ------------------------------------------

      --  HANDLED_SEQUENCE_OF_STATEMENTS ::=
      --      SEQUENCE_OF_STATEMENTS
      --    [exception
      --      EXCEPTION_HANDLER
      --      {EXCEPTION_HANDLER}]
      --    [at end
      --      procedure_IDENTIFIER;]

      --  The AT END phrase is a GNAT extension to provide for cleanups. It is
      --  used only internally currently, but is considered to be syntactic.
      --  At the moment, the only cleanup action allowed is a single call to
      --  a parameterless procedure, and the Identifier field of the node is
      --  the procedure to be called. Also there is a current restriction
      --  that exception handles and a cleanup cannot be present in the same
      --  frame, so at least one of Exception_Handlers or the Identifier must
      --  be missing.

      --  N_Handled_Sequence_Of_Statements
      --  Sloc points to first token of first statement
      --  Statements (List3)
      --  Exception_Handlers (List4) (set to No_List if none present)
      --  Identifier (Node1) (set to Empty if no clean up procedure)

      --  Note: the parent always contains a Declarations field which contains
      --  declarations associated with the handled sequence of statements. This
      --  is true even in the case of an accept statement (see description of
      --  the N_Accept_Statement node).

      -----------------------------
      -- 11.2  Exception Handler --
      -----------------------------

      --  EXCEPTION_HANDLER ::=
      --    when [CHOICE_PARAMETER_SPECIFICATION :]
      --      EXCEPTION_CHOICE {| EXCEPTION_CHOICE} =>
      --        SEQUENCE_OF_STATEMENTS

      --  Note: choice parameter specification is not allowed in Ada 83 mode

      --  N_Exception_Handler
      --  Sloc points to WHEN
      --  Choice_Parameter (Node2) (set to Empty if not present)
      --  Exception_Choices (List4)
      --  Statements (List3)

      ------------------------------------------
      -- 11.2  Choice parameter specification --
      ------------------------------------------

      --  CHOICE_PARAMETER_SPECIFICATION ::= DEFINING_IDENTIFIER

      ----------------------------
      -- 11.2  Exception Choice --
      ----------------------------

      --  EXCEPTION_CHOICE ::= exception_NAME | others

      --  Except in the case of OTHERS, no explicit node appears in the tree
      --  for exception choice. Instead the exception name appears directly.
      --  An OTHERS choice is represented by a N_Others_Choice node (see
      --  section 3.8.1.

      ---------------------------
      -- 11.3  Raise Statement --
      ---------------------------

      --  RAISE_STATEMENT ::= raise [exception_NAME];

      --  N_Raise_Statement
      --  Sloc points to RAISE
      --  Name (Node2) (set to Empty if no exception name present)

      -------------------------------
      -- 12.1  Generic Declaration --
      -------------------------------

      --  GENERIC_DECLARATION ::=
      --    GENERIC_SUBPROGRAM_DECLARATION | GENERIC_PACKAGE_DECLARATION

      ------------------------------------------
      -- 12.1  Generic Subprogram Declaration --
      ------------------------------------------

      --  GENERIC_SUBPROGRAM_DECLARATION ::=
      --    GENERIC_FORMAL_PART SUBPROGRAM_SPECIFICATION;

      --  N_Generic_Subprogram_Declaration
      --  Sloc points to GENERIC
      --  Specification (Node1) subprogram specification
      --  Corresponding_Body (Node5-Sem)
      --  Generic_Formal_Declarations (List3) from generic formal part
      --  Parent_Spec (Node4-Lib)

      ---------------------------------------
      -- 12.1  Generic Package Declaration --
      ---------------------------------------

      --  GENERIC_PACKAGE_DECLARATION ::=
      --    GENERIC_FORMAL_PART PACKAGE_SPECIFICATION;

      --  Note: when we do generics right, the Activation_Chain_Entity entry
      --  for this node can be removed (since the expander won't see generic
      --  units any more).

      --  N_Generic_Package_Declaration
      --  Sloc points to GENERIC
      --  Specification (Node1) subprogram specification
      --  Corresponding_Body (Node5-Sem)
      --  Generic_Formal_Declarations (List3) from generic formal part
      --  Parent_Spec (Node4-Lib)
      --  Activation_Chain_Entity (Node2-Sem)

      -------------------------------
      -- 12.1  Generic Formal Part --
      -------------------------------

      --  GENERIC_FORMAL_PART ::=
      --    generic {GENERIC_FORMAL_PARAMETER_DECLARATION | USE_CLAUSE}

      ------------------------------------------------
      -- 12.1  Generic Formal Parameter Declaration --
      ------------------------------------------------

      --  GENERIC_FORMAL_PARAMETER_DECLARATION ::=
      --    FORMAL_OBJECT_DECLARATION
      --  | FORMAL_TYPE_DECLARATION
      --  | FORMAL_SUBPROGRAM_DECLARATION
      --  | FORMAL_PACKAGE_DECLARATION

      ---------------------------------
      -- 12.3  Generic Instantiation --
      ---------------------------------

      --  GENERIC_INSTANTIATION ::=
      --    package DEFINING_PROGRAM_UNIT_NAME is
      --      new generic_package_NAME [GENERIC_ACTUAL_PART];
      --  | procedure DEFINING_PROGRAM_UNIT_NAME is
      --      new generic_procedure_NAME [GENERIC_ACTUAL_PART];
      --  | function DEFINING_DESIGNATOR is
      --      new generic_function_NAME [GENERIC_ACTUAL_PART];

      --  N_Package_Instantiation
      --  Sloc points to PACKAGE
      --  Defining_Unit_Name (Node1)
      --  Name (Node2)
      --  Parent_Spec (Node4-Lib)
      --  Generic_Associations (List3) (set to No_List if no
      --   generic actual part)

      --  N_Procedure_Instantiation
      --  Sloc points to PROCEDURE
      --  Defining_Unit_Name (Node1)
      --  Name (Node2)
      --  Parent_Spec (Node4-Lib)
      --  Generic_Associations (List3) (set to No_List if no
      --   generic actual part)

      --  N_Function_Instantiation
      --  Sloc points to FUNCTION
      --  Defining_Unit_Name (Node1)
      --  Name (Node2)
      --  Generic_Associations (List3) (set to No_List if no
      --   generic actual part)
      --  Parent_Spec (Node4-Lib)

      ------------------------------
      -- 12.3 Generic Actual Part --
      ------------------------------

      --  GENERIC_ACTUAL_PART ::=
      --    (GENERIC_ASSOCIATION {, GENERIC_ASSOCIATION})

      -------------------------------
      -- 12.3  Generic Association --
      -------------------------------

      --  GENERIC_ASSOCIATION ::=
      --    [generic_formal_parameter_SELECTOR_NAME =>]
      --      EXPLICIT_GENERIC_ACTUAL_PARAMETER

      --  Note: unlike the procedure call case, a generic association node
      --  is generated for every association, even if no formal is present.
      --  In this case the parser will leave the Selector_Name field set
      --  to Empty, to be filled in later by the semantic pass.

      --  N_Generic_Association
      --  Sloc points to first token of generic association
      --  Selector_Name (Node3) (set to Empty if no formal
      --   parameter selector name)
      --  Explicit_Generic_Actual_Parameter (Node2)

      ---------------------------------------------
      -- 12.3  Explicit Generic Actual Parameter --
      ---------------------------------------------

      --  EXPLICIT_GENERIC_ACTUAL_PARAMETER ::=
      --    EXPRESSION      | variable_NAME   | subprogram_NAME
      --  | entry_NAME      | SUBTYPE_MARK    | package_instance_NAME

      -------------------------------------
      -- 12.4  Formal Object Declaration --
      -------------------------------------

      --  FORMAL_OBJECT_DECLARATION ::=
      --    DEFINING_IDENTIFIER_LIST :
      --      MODE SUBTYPE_MARK [:= DEFAULT_EXPRESSION];

      --  Although the syntax allows multiple identifiers in the list, the
      --  semantics is as though successive declarations were given with
      --  identical type definition and expression components. To simplify
      --  semantic processing, the parser represents a multiple declaration
      --  case as a sequence of single declarations, using the More_Ids and
      --  Prev_Ids flags to preserve the original source form as described
      --  in the section on "Handling of Defining Identifier Lists".

      --  N_Formal_Object_Declaration
      --  Sloc points to first identifier
      --  Defining_Identifier (Node1) 
      --  In_Present (Flag1)
      --  Out_Present (Flag2)
      --  Subtype_Mark (Node4)
      --  Expression (Node3) (set to Empty if no default expression)
      --  More_Ids (Flag5) (set to False if no more identifiers in list)
      --  Prev_Ids (Flag6) (set to False if no previous identifiers in list)

      -----------------------------------
      -- 12.5  Formal Type Declaration --
      -----------------------------------

      --  FORMAL_TYPE_DECLARATION ::=
      --    type DEFINING_IDENTIFIER [DISCRIMINANT_PART]
      --      is FORMAL_TYPE_DEFINITION;

      --  N_Formal_Type_Declaration
      --  Sloc points to TYPE
      --  Defining_Identifier (Node1)
      --  Formal_Type_Definition (Node3)
      --  Discriminant_Specifications (List2) (set to No_List if no
      --   discriminant part)
      --  Has_Unknown_Discriminants (Flag3) set if (<>) discriminant

      ----------------------------------
      -- 12.5  Formal type definition --
      ----------------------------------

      --  FORMAL_TYPE_DEFINITION ::=
      --    FORMAL_PRIVATE_TYPE_DEFINITION
      --  | FORMAL_DERIVED_TYPE_DEFINITION
      --  | FORMAL_DISCRETE_TYPE_DEFINITION
      --  | FORMAL_SIGNED_INTEGER_TYPE_DEFINITION
      --  | FORMAL_MODULAR_TYPE_DEFINITION
      --  | FORMAL_FLOATING_POINT_DEFINITION
      --  | FORMAL_ORDINARY_FIXED_POINT_DEFINITION
      --  | FORMAL_DECIMAL_FIXED_POINT_DEFINITION
      --  | FORMAL_ARRAY_TYPE_DEFINITION
      --  | FORMAL_ACCESS_TYPE_DEFINITION

      ---------------------------------------------
      -- 12.5.1  Formal Private Type Definition --
      ---------------------------------------------

      --  FORMAL_PRIVATE_TYPE_DEFINITION ::=
      --    [abstract] [tagged] [limited] private

      --  Note: TAGGED is not allowed in Ada 83 mode

      --  N_Formal_Private_Type_Definition
      --  Sloc points to PRIVATE
      --  Abstract_Present (Flag4)
      --  Tagged_Present (Flag1)
      --  Limited_Present (Flag2)

      --------------------------------------------
      -- 12.5.1  Formal Derived Type Definition --
      --------------------------------------------

      --  FORMAL_DERIVED_TYPE_DEFINITION ::= new SUBTYPE_MARK [with private]

      --  Note: WITH PRIVATE is not allowed in Ada 83 mode

      --  N_Formal_Derived_Type_Definition
      --  Sloc points to NEW
      --  Subtype_Mark (Node4)
      --  Private_Present (Flag1)

      ---------------------------------------------
      -- 12.5.2  Formal Discrete Type Definition --
      ---------------------------------------------

      --  FORMAL_DISCRETE_TYPE_DEFINITION ::= (<>)

      --  N_Formal_Discrete_Type_Definition
      --  Sloc points to <>

      ---------------------------------------------------
      -- 12.5.2  Formal Signed Integer Type Definition --
      ---------------------------------------------------

      --  FORMAL_SIGNED_INTEGER_TYPE_DEFINITION ::= range <>

      --  N_Formal_Signed_Integer_Type_Definition
      --  Sloc points to RANGE

      --------------------------------------------
      -- 12.5.2  Formal Modular Type Definition --
      --------------------------------------------

      --  FORMAL_MODULAR_TYPE_DEFINITION ::= mod <>

      --  N_Formal_Modular_Type_Definition
      --  Sloc points to MOD

      ----------------------------------------------
      -- 12.5.2  Formal Floating Point Definition --
      ----------------------------------------------

      --  FORMAL_FLOATING_POINT_DEFINITION ::= digits <>

      --  N_Formal_Floating_Point_Definition
      --  Sloc points to DIGITS

      ----------------------------------------------------
      -- 12.5.2  Formal Ordinary Fixed Point Definition --
      ----------------------------------------------------

      --  FORMAL_ORDINARY_FIXED_POINT_DEFINITION ::= delta <>

      --  N_Formal_Ordinary_Fixed_Point_Definition
      --  Sloc points to DELTA

      ---------------------------------------------------
      -- 12.5.2  Formal Decimal Fixed Point Definition --
      ---------------------------------------------------

      --  FORMAL_DECIMAL_FIXED_POINT_DEFINITION ::= delta <> digits <>

      --  Note: formal decimal fixed point definition not allowed in Ada 83

      --  N_Formal_Decimal_Fixed_Point_Definition
      --  Sloc points to DELTA

      ------------------------------------------
      -- 12.5.3  Formal Array Type Definition --
      ------------------------------------------

      --  FORMAL_ARRAY_TYPE_DEFINITION ::= ARRAY_TYPE_DEFINITION

      -------------------------------------------
      -- 12.5.4  Formal Access Type Definition --
      -------------------------------------------

      --  FORMAL_ACCESS_TYPE_DEFINITION ::= ACCESS_TYPE_DEFINITION

      -----------------------------------------
      -- 12.6  Formal Subprogram Declaration --
      -----------------------------------------

      --  FORMAL_SUBPROGRAM_DECLARATION ::=
      --    with SUBPROGRAM_SPECIFICATION [is SUBPROGRAM_DEFAULT];

      --  N_Formal_Subprogram_Declaration
      --  Sloc points to WITH
      --  Specification (Node1)
      --  Default_Name (Node2) (set to Empty if no subprogram default)
      --  Box_Present (Flag1)

      --  Note: if no subprogram default is present, then Name is set
      --  to Empty, and Box_Present is False.

      ------------------------------
      -- 12.6  Subprogram Default --
      ------------------------------

      --  SUBPROGRAM_DEFAULT ::= DEFAULT_NAME | <>

      --  There is no separate node in the tree for a subprogram default.
      --  Instead the parent (N_Formal_Subprogram_Declaration) node contains
      --  the default name or box indication, as needed.

      ------------------------
      -- 12.6  Default Name --
      ------------------------

      --  DEFAULT_NAME ::= NAME

      --------------------------------------
      -- 12.7  Formal Package Declaration --
      --------------------------------------

      --  FORMAL_PACKAGE_DECLARATION ::=
      --    with package DEFINING_IDENTIFIER
      --      is new generic_package_NAME FORMAL_PACKAGE_ACTUAL_PART;

      --  Note: formal package declarations not allowed in Ada 83 mode

      --  N_Formal_Package_Declaration
      --  Sloc points to WITH
      --  Defining_Identifier (Node1)
      --  Name (Node2)
      --  Generic_Associations (List3) (set to No_List if (<>) case or
      --   empty generic actual part)
      --  Box_Present (Flag1)
      --  Generic_Parent (Node5-Sem)

      --------------------------------------
      -- 12.7  Formal Package Actual Part --
      --------------------------------------

      --  FORMAL_PACKAGE_ACTUAL_PART ::=
      --    (<>) | [GENERIC_ACTUAL_PART]

      --  There is no explicit node in the tree for a formal package
      --  actual part. Instead the information appears in the parent node
      --  (i.e. the formal package declaration node itself).

      ---------------------------------
      -- 13.1  Representation clause --
      ---------------------------------

      --  REPRESENTATION_CLAUSE ::=
      --    ATTRIBUTE_DEFINITION_CLAUSE
      --  | ENUMERATION_REPRESENTATION_CLAUSE
      --  | RECORD_REPRESENTATION_CLAUSE
      --  | AT_CLAUSE

      ---------------------------------------
      -- 13.2  Attribute definition clause --
      ---------------------------------------

      --  ATTRIBUTE_DEFINITION_CLAUSE ::=
      --    for DIRECT_NAME'ATTRIBUTE_DESIGNATOR use EXPRESSION;
      --  | for DIRECT_NAME'ATTRIBUTE_DESIGNATOR use NAME;

      --  In Ada 83, the expression must be a simple expression

      --  N_Attribute_Definition_Clause
      --  Sloc points to FOR
      --  Name (Node2)
      --  Identifier (Node1) the attribute designator
      --  Expression (Node3) the expression or name

      ---------------------------------------------
      -- 13.3  Enumeration representation clause --
      ---------------------------------------------

      --  ENUMERATION_REPRESENTATION_CLAUSE ::=
      --    for first_subtype_DIRECT_NAME use ENUMERATION_AGGREGATE;

      --  N_Enumeration_Representation_Clause
      --  Sloc points to FOR
      --  Identifier (Node1) direct name
      --  Array_Aggregate (Node4)

      ---------------------------------
      -- 13.3  Enumeration aggregate --
      ---------------------------------

      --  ENUMERATION_AGGREGATE ::= ARRAY_AGGREGATE

      ----------------------------------------
      -- 13.4  Record representation clause --
      ----------------------------------------

      --  RECORD_REPRESENTATION_CLAUSE ::=
      --    for first_subtype_DIRECT_NAME use
      --      record [MOD_CLAUSE]
      --        {COMPONENT_CLAUSE}
      --      end record;

      --  N_Record_Representation_Clause
      --  Sloc points to FOR
      --  Identifier (Node1) direct name
      --  Mod_Clause (Node2) (set to Empty if no mod clause present)
      --  Component_Clauses (List3)

      ----------------------
      -- 13.4  Mod clause --
      ----------------------

      --  MOD_CLAUSE ::= at mod static_EXPRESSION;

      --  Note: in Ada 83, the expression must be a simple expression
      --  Note: this feature is obsolescent in Ada 9X

      --  N_Mod_Clause
      --  Sloc points to AT
      --  Expression (Node3)

      ----------------------------
      -- 13.4  Component clause --
      ----------------------------

      --  COMPONENT_CLAUSE ::=
      --    COMPONENT_CLAUSE_COMPONENT_NAME at POSITION
      --      range FIRST_BIT .. LAST_BIT;

      --  COMPONENT_CLAUSE_COMPONENT_NAME ::=
      --    component_DIRECT_NAME
      --  | component_DIRECT_NAME'ATTRIBUTE_DESIGNATOR
      --  | FIRST_SUBTYPE_DIRECT_NAME'ATTRIBUTE_DESIGNATOR

      --  POSITION ::= static_EXPRESSION
      --  FIRST_BIT ::= static_SIMPLE_EXPRESSION
      --  LAST_BIT ::= static_SIMPLE_EXPRESSION

      --  N_Component_Clause
      --  Sloc points to AT
      --  Component_Name (Node1) points to Name or Attribute_Reference
      --  Position (Node2)
      --  First_Bit (Node3)
      --  Last_Bit (Node4)

      ---------------------
      -- 13.5  At clause --
      ---------------------

      --  AT_CLAUSE ::= for DIRECT_NAME use at EXPRESSION;

      --  Note: in Ada 83 the expression must be a simple expression
      --  Note: this feature is obsolescent in Ada 9X

      --  N_At_Clause
      --  Sloc points to FOR
      --  Identifier (Node1)
      --  Expression (Node3)

      --------------------------
      -- 13.8  Code statement --
      --------------------------

      --  CODE_STATEMENT ::= QUALIFIED_EXPRESSION;

      --  N_Code_Statement
      --  Sloc points to first token of subtype mark
      --  Expression (Node3)

   --------------------
   -- Semantic Nodes --
   --------------------

   --  These semantic nodes are used to hold additional semantic information.
   --  They are inserted into the tree as a resut of semantic processing

      ---------------------
      -- Concat Multiple --
      ---------------------

      --  During semantic analysis, this node is created if a sequence of
      --  concatenation nodes construct a single concatenation result. The
      --  creation of this node is used to optimize both compile time
      --  evaluation and run time processing for concatenation, by avoiding
      --  the construction of intermediate results.

      --  N_Concat_Multiple
      --  Sloc points to first of the & operators
      --  Expressions (List3-Sem) points to list of operands
      --  plus fields for expression

      -------------------
      -- Expanded_Name --
      -------------------

      --  The N_Expanded_Name node is used to represent a selected component
      --  name that has been resolved to an expanded name. The semantic phase
      --  replaces N_Selected_Component nodes that represent names by the use
      --  of this node, leaving the N_Selected_Component node used only when
      --  the prefix is a record or protected type.

      --  The fields of the N_Expanded_Name node are layed out identically
      --  to those of the N_Selected_Component node, allowing conversion of
      --  an expanded name node to a selected component node to be done
      --  easily, see Sinfo.Change.Change_Selected_Component_To_Expanded_Name.

      --  N_Expanded_Name
      --  Sloc points to the period
      --  Chars (Name1) copy of Chars field of selector name
      --  Prefix (Node2)
      --  Selector_Name (Node3)
      --  Entity (Node4-Sem)
      --  Redundant_Use (Flag2-Sem) set for redundant use clause
      --  plus fields for expression

      ------------------------
      -- Expression Actions --
      ------------------------

      --  The N_Expression_Actions node is inserted into an expression as a
      --  result of semantic processing that indicates that some actions
      --  (e.g. the creation of some implicit types) are required as part
      --  of the processing for an expression. The list of actions can
      --  include declarations, statements and expressions.

      --  N_Expression_Actions
      --  Sloc is set to some relevant token in the original source
      --  Actions (List1)
      --  Expression (Node3)
      --  plus fields for expression

      -------------------
      -- Freeze Entity --
      -------------------

      --  This node marks the point in a declarative part at which an entity
      --  declared therein becomes frozen. The expander places initialization
      --  procedures for types at those points. Gigi uses the freezing point
      --  to elaborate entities that may depend on previous private types.

      --  N_Freeze_Entity
      --  Entity (Node4-Sem)

      -------------------
      -- Implicit Type --
      -------------------

      --  When types are constructed in situations like:

      --    type x is new integer range 1 .. 10;

      --  which really means:

      --    type anon is new integer;
      --    subtype x is anon range 1 .. 10;

      --  or

      --    var : integer range 1 .. 10;

      --  which really means:

      --    subtype anon is integer range 1 .. 10;
      --    Var : anon;

      --  then an entity is constructed for the implicit type, and it is
      --  treated like any other type. In addition, an implicit type node
      --  is inserted into the declaration list to reflect the presence of
      --  this created type. The only information required is a pointer to
      --  the created entity for the type (which is in the Defining_Identifier
      --  field. The remainder of the tree which would normally be present
      --  for a type declaration is omitted, since all the necessary semantic
      --  information is present in the entity.

      --  Note: the name of the defining identifier of an implicit type
      --  node always starts with the characters "Ityp__", and these are
      --  the only entities whose names start with this prefix.

      --  N_Implicit_Type
      --  Sloc is copied from the node giving rise to the implicit type
      --  Defining_Identifier (Node1) of the implicit type/subtype

   ---------------
   -- Reference --
   ---------------

   --  Actuals in  entry calls are placed into a single parameter block that
   --  is passed by reference to the called task. For parameters passed by
   --  reference, we need to store the "address" using exactly the same
   --  convention as we use for passing normal parameters. We cannot use
   --  the 'Access attribute, since the parameter may not be aliased, and
   --  also we must be able to pass parameters by reference to which the
   --  Aliased attribute does not apply (slices and aggregates). The special
   --  reference node constructs a value of type access to the type of the
   --  expression suitable for this use.

   --  N_Reference
   --  Sloc is copied from the expression (the name of a by-reference object)
   --  Expression (Node3)
   --  plus fields for expression              

   -----------
   -- Empty --
   -----------

   --  N_Empty
   --  Chars (Name1) is set to No_Name
   --  Used as the contents of the Nkind field of the dummy Empty node
   --  and in some other situations to indicate an uninitialized value.

   -----------
   -- Error --
   -----------

   --  N_Error
   --  Chars (Name1) is set to Error_Name
   --  Used as the contents of the Nkind field of the dummy Error node

   --------------------------
   -- Node Type Definition --
   --------------------------

   --  The following is the definition of the Node_Kind type. As previously
   --  discussed, this is separated off to allow rearrangement of the order
   --  to facilitiate definition of subtype ranges. The comments show the
   --  subtype classes which apply to each set of node kinds. The first
   --  entry in the comment characterizes the following list of nodes.

   type Node_Kind is (
      N_Unused_At_Start,

      --  N_Has_Chars
      N_Empty,
      N_Error,

      --  N_Entity, N_Has_Etype, N_Has_Chars
      N_Defining_Identifier,
      N_Defining_Character_Literal,
      N_Defining_Operator_Symbol,

      --  N_Subexpr, N_Has_Etype, N_Has_Chars, N_Entity_Name
      N_Expanded_Name,

      --  N_Direct_Name, N_Subexpr, N_Has_Etype, N_Has_Chars, N_Entity_Name
      N_Identifier,

      --  N_Direct_Name, N_Subexpr, N_Has_Etype, N_Has_Chars
      N_Character_Literal,
      N_Operator_Symbol,

      --  N_Binary_Op, N_Op, N_Subexpr, N_Has_Etype, N_Has_Chars
      N_Op_And,
      N_Op_And_Then,
      N_Op_Or,
      N_Op_Or_Else,
      N_Op_Xor,
      N_Op_In,
      N_Op_Not_In,
      N_Op_Eq,
      N_Op_Ne,
      N_Op_Lt,
      N_Op_Le,
      N_Op_Gt,
      N_Op_Ge,
      N_Op_Add,
      N_Op_Subtract,
      N_Op_Concat,
      N_Op_Multiply,
      N_Op_Divide,
      N_Op_Mod,
      N_Op_Rem,
      N_Op_Expon,

      --  N_Unary_Op, N_Op, N_Subexpr, N_Has_Etype, N_Has_Chars
      N_Op_Plus,
      N_Op_Minus,
      N_Op_Abs,
      N_Op_Not,

      --  N_Subexpr, N_Has_Etype
      N_Integer_Literal,
      N_Real_Literal,
      N_String_Literal,
      N_Explicit_Dereference,
      N_Indexed_Component,
      N_Slice,
      N_Function_Call,
      N_Selected_Component,
      N_Attribute_Reference,
      N_Aggregate,
      N_Extension_Aggregate,
      N_Null,
      N_Parenthesized_Expression,
      N_Reference,
      N_Type_Conversion,
      N_Qualified_Expression,
      N_Allocator,
      N_Concat_Multiple,
      N_Expression_Actions,
      N_Range,

      --  N_Has_Etype, N_Subexpr,
      N_Procedure_Call_Statement,

      --  N_Has_Etype
      N_Subtype_Indication,

      --  N_Access_To_Subprogram_Definition
      N_Access_Function_Definition,
      N_Access_Procedure_Definition,

      --  N_Array_Type_Definition
      N_Unconstrained_Array_Definition,
      N_Constrained_Array_Definition,

      --  N_Body_Stub, N_Later_Decl_Item
      N_Subprogram_Body_Stub,
      N_Package_Body_Stub,
      N_Task_Body_Stub,
      N_Protected_Body_Stub,

      --  N_Generic_Instantiation, N_Later_Decl_Item
      N_Package_Instantiation,
      N_Procedure_Instantiation,
      N_Function_Instantiation,

      --  N_Unit_Body, N_Later_Decl_Item
      N_Subprogram_Body,
      N_Package_Body,

      --  N_Later_Decl_Item
      N_Task_Body,
      N_Subprogram_Declaration,
      N_Package_Declaration,
      N_Task_Type_Declaration,
      N_Single_Task_Declaration,
      N_Implicit_Label_Declaration,
      N_Use_Package_Clause,
      N_Pragma,

      --  N_Generic_Declaration, N_Later_Decl_Item
      N_Generic_Package_Declaration,
      N_Generic_Subprogram_Declaration,

      --  N_Generic_Parameter_Declaration
      N_Formal_Object_Declaration,
      N_Formal_Type_Declaration,
      N_Formal_Subprogram_Declaration,
      N_Formal_Package_Declaration,

      --  N_Renaming_Declaration
      N_Object_Renaming_Declaration,
      N_Exception_Renaming_Declaration,
      N_Package_Renaming_Declaration,
      N_Subprogram_Renaming_Declaration,

      --  N_Generic_Renaming_Declarations, N_Renaming_Declaration
      N_Generic_Package_Renaming_Declaration,
      N_Generic_Procedure_Renaming_Declaration,
      N_Generic_Function_Renaming_Declaration,

      --  N_Representation_Clause
      N_Attribute_Definition_Clause,
      N_Enumeration_Representation_Clause,
      N_Record_Representation_Clause,
      N_Mod_Clause,
      N_Component_Clause,
      N_At_Clause,

      --  N_Subprogram_Specification
      N_Function_Specification,
      N_Procedure_Specification,

      --  N_Statement
      N_Abort_Statement,
      N_Accept_Statement,
      N_Assignment_Statement,
      N_Block_Statement,
      N_Case_Statement,
      N_Code_Statement,
      N_Delay_Relative_Statement,
      N_Delay_Until_Statement,
      N_Entry_Call_Statement,
      N_Exit_Statement,
      N_Goto_Statement,
      N_If_Statement,
      N_Loop_Statement,
      N_Null_Statement,
      N_Raise_Statement,
      N_Requeue_Statement,
      N_Return_Statement,          

      --  Other nodes (not part of any subtype class)
      N_Abortable_Part,
      N_Abstract_Subprogram_Declaration,
      N_Accept_Alternative,
      N_Access_Definition,
      N_Access_To_Object_Definition,
      N_Asynchronous_Select,
      N_Case_Statement_Alternative,
      N_Compilation_Unit,
      N_Component_Association,
      N_Component_Declaration,
      N_Component_List,
      N_Conditional_Entry_Call,
      N_Derived_Type_Definition,
      N_Decimal_Fixed_Point_Definition,
      N_Defining_Program_Unit_Name,
      N_Delay_Alternative,
      N_Delta_Constraint,
      N_Designator,
      N_Digits_Constraint,
      N_Discriminant_Association,
      N_Discriminant_Specification,
      N_Elsif_Part,
      N_Enumeration_Type_Definition,
      N_Entry_Body,
      N_Entry_Body_Formal_Part,
      N_Entry_Call_Alternative,
      N_Entry_Declaration,
      N_Entry_Index_Specification,
      N_Exception_Declaration,
      N_Exception_Handler,
      N_Floating_Point_Definition,
      N_Formal_Decimal_Fixed_Point_Definition,
      N_Formal_Derived_Type_Definition,
      N_Formal_Discrete_Type_Definition,
      N_Formal_Floating_Point_Definition,
      N_Formal_Modular_Type_Definition,
      N_Formal_Ordinary_Fixed_Point_Definition,
      N_Formal_Private_Type_Definition,
      N_Formal_Signed_Integer_Type_Definition,
      N_Freeze_Entity,
      N_Full_Type_Declaration,
      N_Generic_Association,
      N_Handled_Sequence_Of_Statements,
      N_Implicit_Type,
      N_Incomplete_Type_Declaration,
      N_Index_Or_Discriminant_Constraint,
      N_Iteration_Scheme,
      N_Label,
      N_Loop_Parameter_Specification,
      N_Modular_Type_Definition,
      N_Number_Declaration,
      N_Object_Declaration,
      N_Ordinary_Fixed_Point_Definition,
      N_Others_Choice,
      N_Package_Specification,
      N_Parameter_Association,
      N_Parameter_Specification,
      N_Pragma_Argument_Association,
      N_Private_Extension_Declaration,
      N_Private_Type_Declaration,
      N_Protected_Body,
      N_Protected_Definition,
      N_Protected_Type_Declaration,
      N_Range_Constraint,
      N_Real_Range_Specification,
      N_Record_Definition,
      N_Selective_Accept,
      N_Signed_Integer_Type_Definition,
      N_Single_Protected_Declaration,
      N_Subtype_Declaration,
      N_Subunit,
      N_Task_Definition,
      N_Terminate_Alternative,
      N_Timed_Entry_Call,
      N_Triggering_Alternative,
      N_Use_Type_Clause,
      N_Variant,
      N_Variant_Part,
      N_With_Clause,
      N_Unused_At_End);

   ----------------------------
   -- Node Class Definitions --
   ----------------------------

   --  The contents of these classes are keyed to the order of the
   --  literals in the Node_Kind type declaration, as declared above.

   subtype N_Access_To_Subprogram_Definition is Node_Kind range
     N_Access_Function_Definition .. N_Access_Procedure_Definition;

   subtype N_Array_Type_Definition is Node_Kind range
     N_Unconstrained_Array_Definition .. N_Constrained_Array_Definition;

   subtype N_Binary_Op is Node_Kind range
     N_Op_And .. N_Op_Expon;

   subtype N_Body_Stub is Node_Kind range
     N_Subprogram_Body_Stub .. N_Protected_Body_Stub;

   subtype N_Concurrent_Body_Stub is Node_Kind range
     N_Task_Body_Stub .. N_Protected_Body_Stub;

   subtype N_Direct_Name is Node_Kind range
     N_Identifier .. N_Operator_Symbol;

   subtype N_Entity is Node_Kind range
     N_Defining_Identifier .. N_Defining_Operator_Symbol;

   subtype N_Generic_Instantiation is Node_Kind range
     N_Package_Instantiation .. N_Function_Instantiation;

   subtype N_Generic_Parameter_Declaration is Node_Kind range
     N_Formal_Object_Declaration .. N_Formal_Package_Declaration;

   subtype N_Has_Chars is Node_Kind range
     N_Empty .. N_Op_Not;

   subtype N_Has_Etype is Node_Kind range
     N_Defining_Identifier .. N_Subtype_Indication;

   subtype N_Later_Decl_Item is Node_Kind range
     N_Subprogram_Body_Stub .. N_Generic_Subprogram_Declaration;
   --  Note: this is Ada 83 relevant only (see Ada 83 RM 3.9 (2)) and
   --  includes only those items which can appear as later declarative
   --  items. This also includes N_Pragma and N_Implicit_Label_Declaration
   --  which are not specifically in the grammar but may appear as a
   --  valid later declarative items.

   subtype N_Unit_Body is Node_Kind range
     N_Subprogram_Body .. N_Package_Body;

   subtype N_Generic_Declaration is Node_Kind range
     N_Generic_Package_Declaration .. N_Generic_Subprogram_Declaration;

   subtype N_Op is Node_Kind range
     N_Op_And .. N_Op_Not;

   subtype N_Renaming_Declaration is Node_Kind range
     N_Object_Renaming_Declaration .. N_Generic_Function_Renaming_Declaration;

   subtype N_Generic_Renaming_Declaration is Node_Kind range
     N_Generic_Package_Renaming_Declaration ..
     N_Generic_Function_Renaming_Declaration;

   subtype N_Representation_Clause is Node_Kind range
     N_Attribute_Definition_Clause .. N_At_Clause;

   subtype N_Statement is Node_Kind range
     N_Abort_Statement .. N_Return_Statement;
   --  Note that this includes all statement types except for the case
   --  of N_Procedure_Call_Statement, which is considered to be a
   --  subexpression (since it can be overloaded and requires resolution)

   subtype N_Subexpr is Node_Kind range -- things with expr fields
     N_Expanded_Name .. N_Procedure_Call_Statement;

   subtype N_Entity_Name is Node_Kind range
     N_Expanded_Name .. N_Identifier;

   subtype N_Subprogram_Specification is Node_Kind range
     N_Function_Specification .. N_Procedure_Specification;

   subtype N_Unary_Op is Node_Kind range
     N_Op_Plus .. N_Op_Not;

   ---------------------------
   -- Node Access Functions --
   ---------------------------

   --  The following functions return the contents of the indicated field of
   --  the node referenced by the argument, which is a Node_Id. They provide
   --  logical access to fields in the node which could be accessed using
   --  the Tree.Generic_Access package, but the idea is always to use these
   --  higher level routines which preserve strong typing. In debug mode,
   --  these routines check that they are being applied to an appropriate
   --  node, as well as checking that the node is in range.

   function Abort_Present
     (N : Node_Id) return Boolean;    -- Flag1

   function Abortable_Part
     (N : Node_Id) return Node_Id;    -- Node2

   function Abstract_Present
     (N : Node_Id) return Boolean;    -- Flag4

   function Accept_Name
     (N : Node_Id) return Node_Id;    -- Node1

   function Accept_Statement
     (N : Node_Id) return Node_Id;    -- Node2

   function Actions
     (N : Node_Id) return List_Id;    -- List1

   function Activation_Chain_Entity
     (N : Node_Id) return Node_Id;    -- Node2

   function Acts_As_Spec
     (N : Node_Id) return Boolean;    -- Flag4

   function Actual_Parameter
     (N : Node_Id) return Node_Id;    -- Node2

   function Aliased_Present
     (N : Node_Id) return Boolean;    -- Flag1

   function All_Present
     (N : Node_Id) return Boolean;    -- Flag1

   function Alternatives
     (N : Node_Id) return List_Id;    -- List4

   function Analyzed
     (N : Node_Id) return Boolean;    -- Flag19

   function Array_Aggregate
     (N : Node_Id) return Node_Id;    -- Node4

   function Assignment_OK
     (N : Node_Id) return Boolean;    -- Flag5

   function Bad_Is_Detected
     (N : Node_Id) return Boolean;    -- Flag1

   function Body_Required
     (N : Node_Id) return Boolean;    -- Flag3

   function Box_Present
     (N : Node_Id) return Boolean;    -- Flag1

   function Char_Literal_Value
     (N : Node_Id) return Char_Code;  -- Char_Code2

   function Chars
     (N : Node_Id) return Name_Id;    -- Name1

   function Choice_Parameter
     (N : Node_Id) return Node_Id;    -- Node2

   function Choices
     (N : Node_Id) return List_Id;    -- List1

   function Component_Associations
     (N : Node_Id) return List_Id;    -- List4

   function Component_Clauses
     (N : Node_Id) return List_Id;    -- List3

   function Component_Declarations
     (N : Node_Id) return List_Id;    -- List3

   function Component_List
     (N : Node_Id) return Node_Id;    -- Node1

   function Component_Name
     (N : Node_Id) return Node_Id;    -- Node1

   function Condition
     (N : Node_Id) return Node_Id;    -- Node5

   function Constant_Present
     (N : Node_Id) return Boolean;    -- Flag2

   function Constraint
     (N : Node_Id) return Node_Id;    -- Node3

   function Constraints
     (N : Node_Id) return List_Id;    -- List1

   function Context_Installed
     (N : Node_Id) return Boolean;    -- Flag3

   function Context_Items
     (N : Node_Id) return List_Id;    -- List1

   function Controlling_Argument
     (N : Node_Id) return Node_Id;    -- Node1

   function Corresponding_Body
     (N : Node_Id) return Node_Id;    -- Node5

   function Corresponding_Spec
     (N : Node_Id) return Node_Id;    -- Node5

   function Debug_Statement
     (N : Node_Id) return Node_Id;    -- Node3

   function Decimal
     (N : Node_Id) return Boolean;    -- Flag2

   function Declarations
     (N : Node_Id) return List_Id;    -- List3

   function Default_Name
     (N : Node_Id) return Node_Id;    -- Node2

   function Defining_Identifier
     (N : Node_Id) return Node_Id;    -- Node1

   function Defining_Unit_Name
     (N : Node_Id) return Node_Id;    -- Node1

   function Delay_Alternative
     (N : Node_Id) return Node_Id;    -- Node4

   function Delay_Statement
     (N : Node_Id) return Node_Id;    -- Node2

   function Delta_Expression
     (N : Node_Id) return Node_Id;    -- Node3

   function Denominator
     (N : Node_Id) return Uint;       -- Uint4

   function Digits_Expression
     (N : Node_Id) return Node_Id;    -- Node2

   function Discrete_Choices
     (N : Node_Id) return List_Id;    -- List4

   function Discrete_Range
     (N : Node_Id) return Node_Id;    -- Node4

   function Discrete_Subtype_Definition
     (N : Node_Id) return Node_Id;    -- Node4

   function Discrete_Subtype_Definitions
     (N : Node_Id) return List_Id;    -- List2

   function Discriminant_Specifications
     (N : Node_Id) return List_Id;    -- List2

   function Discriminant_Type
     (N : Node_Id) return Node_Id;    -- Node2

   function Do_Access_Check
     (N : Node_Id) return Boolean;    -- Flag2

   function Do_Accessibility_Check
     (N : Node_Id) return Boolean;    -- Flag3

   function Do_Discriminant_Check
     (N : Node_Id) return Boolean;    -- Flag3

   function Do_Division_Check
     (N : Node_Id) return Boolean;    -- Flag3

   function Do_Elaboration_Check
     (N : Node_Id) return Boolean;    -- Flag2

   function Do_Length_Check
     (N : Node_Id) return Boolean;    -- Flag4

   function Do_Overflow_Check
     (N : Node_Id) return Boolean;    -- Flag2

   function Do_Range_Check
     (N : Node_Id) return Boolean;    -- Flag9

   function Do_Storage_Check
     (N : Node_Id) return Boolean;    -- Flag2

   function Do_Tag_Check
     (N : Node_Id) return Boolean;    -- Flag3

   function Elaborate_Present
     (N : Node_Id) return Boolean;    -- Flag4

   function Elaborate_All_Present
     (N : Node_Id) return Boolean;    -- Flag1

   function Elaborate_Body_Present
     (N : Node_Id) return Boolean;    -- Flag7

   function Else_Statements
     (N : Node_Id) return List_Id;    -- List4

   function Elsif_Parts
     (N : Node_Id) return List_Id;    -- List3

   function Enclosing_Variant
     (N : Node_Id) return Node_Id;    -- Node2

   function Entity
     (N : Node_Id) return Node_Id;    -- Node4

   function Entry_Body_Formal_Part
     (N : Node_Id) return Node_Id;    -- Node2

   function Entry_Call_Alternative
     (N : Node_Id) return Node_Id;    -- Node1

   function Entry_Call_Statement
     (N : Node_Id) return Node_Id;    -- Node1

   function Entry_Index
     (N : Node_Id) return Node_Id;    -- Node5

   function Entry_Index_Specification
     (N : Node_Id) return Node_Id;    -- Node1

   function Error_Posted
     (N : Node_Id) return Boolean;    -- Flag20

   function Etype
     (N : Node_Id) return Node_Id;    -- Node5

   function Evaluate_Once
     (N : Node_Id) return Boolean;    -- Flag10

   function Exception_Choices
     (N : Node_Id) return List_Id;    -- List4

   function Exception_Handlers
     (N : Node_Id) return List_Id;    -- List4

   function Explicit_Generic_Actual_Parameter
     (N : Node_Id) return Node_Id;    -- Node2

   function Expression
     (N : Node_Id) return Node_Id;    -- Node3

   function Expressions
     (N : Node_Id) return List_Id;    -- List3

   function First_Bit
     (N : Node_Id) return Node_Id;    -- Node3

   function First_Name
     (N : Node_Id) return Boolean;    -- Flag5

   function First_Named_Actual
     (N : Node_Id) return Node_Id;    -- Node4

   function Following_Pragmas
     (N : Node_Id) return List_Id;    -- List3

   function Formal_Type_Definition
     (N : Node_Id) return Node_Id;    -- Node3

   function Generic_Associations
     (N : Node_Id) return List_Id;    -- List3

   function Generic_Formal_Declarations
     (N : Node_Id) return List_Id;    -- List3

   function Generic_Parent
     (N : Node_Id) return Node_Id;    -- Node5

   function Handled_Statement_Sequence
     (N : Node_Id) return Node_Id;    -- Node4

   function Has_Created_Identifier
     (N : Node_Id) return Boolean;    -- Flag1

   function Has_No_Side_Effects
     (N : Node_Id) return Boolean;    -- Flag8

   function Has_Priority_Pragma
     (N : Node_Id) return Boolean;    -- Flag6

   function Has_Task_Stack_Size_Pragma
     (N : Node_Id) return Boolean;    -- Flag5

   function Has_Unknown_Discriminants
     (N : Node_Id) return Boolean;    -- Flag3

   function High_Bound
     (N : Node_Id) return Node_Id;    -- Node2

   function Identifier
     (N : Node_Id) return Node_Id;    -- Node1

   function Implicit_Types
     (N : Node_Id) return List_Id;    -- List2

   function Implicit_With
     (N : Node_Id) return Boolean;    -- Flag2

   function In_Present
     (N : Node_Id) return Boolean;    -- Flag1

   function Intval
     (N : Node_Id) return Uint;       -- Uint3

   function Is_Evaluated
     (N : Node_Id) return Boolean;    -- Flag7

   function Is_Overloaded
     (N : Node_Id) return Boolean;    -- Flag5

   function Is_Task_Master
     (N : Node_Id) return Boolean;    -- Flag5

   function Is_Static
     (N : Node_Id) return Boolean;    -- Flag6

   function Iteration_Scheme
     (N : Node_Id) return Node_Id;    -- Node2

   function Left_Opnd
     (N : Node_Id) return Node_Id;    -- Node2

   function Label
     (N : Node_Id) return Node_Id;    -- Node2

   function Last_Bit
     (N : Node_Id) return Node_Id;    -- Node4

   function Last_Name
     (N : Node_Id) return Boolean;    -- Flag6

   function Library_Unit
     (N : Node_Id) return Node_Id;    -- Node4

   function Literals
     (N : Node_Id) return List_Id;    -- List1

   function Limited_Present
     (N : Node_Id) return Boolean;    -- Flag2

   function Loop_Parameter_Specification
     (N : Node_Id) return Node_Id;    -- Node2

   function Low_Bound
     (N : Node_Id) return Node_Id;    -- Node1

   function Mod_Clause
     (N : Node_Id) return Node_Id;    -- Node2

   function More_Ids
     (N : Node_Id) return Boolean;    -- Flag5

   function Name
     (N : Node_Id) return Node_Id;    -- Node2

   function Names
     (N : Node_Id) return List_Id;    -- List2

   function Next_Named_Actual
     (N : Node_Id) return Node_Id;    -- Node4

   function Null_Present
     (N : Node_Id) return Boolean;    -- Flag3

   function Null_Record_Present
     (N : Node_Id) return Boolean;    -- Flag2

   function Numerator
     (N : Node_Id) return Uint;       -- Uint3

   function Object_Definition
     (N : Node_Id) return Node_Id;    -- Node2

   function Others_Discrete_Choices
     (N : Node_Id) return List_Id;    -- List1

   function Out_Present
     (N : Node_Id) return Boolean;    -- Flag2

   function Parameter_Associations
     (N : Node_Id) return List_Id;    -- List3

   function Parameter_Specifications
     (N : Node_Id) return List_Id;    -- List2

   function Parameter_Type
     (N : Node_Id) return Node_Id;    -- Node2

   function Parens
     (N : Node_Id) return Boolean;    -- Flag1

   function Parent_Spec
     (N : Node_Id) return Node_Id;    -- Node4

   function Position
     (N : Node_Id) return Node_Id;    -- Node2

   function Pragma_Argument_Associations
     (N : Node_Id) return List_Id;    -- List2

   function Preelaborable
     (N : Node_Id) return Boolean;    -- Flag2

   function Prefix
     (N : Node_Id) return Node_Id;    -- Node2

   function Prev_Ids
     (N : Node_Id) return Boolean;    -- Flag6

   function Private_Declarations
     (N : Node_Id) return List_Id;    -- List4

   function Private_Present
     (N : Node_Id) return Boolean;    -- Flag1

   function Proper_Body
     (N : Node_Id) return Node_Id;    -- Node1

   function Protected_Definition
     (N : Node_Id) return Node_Id;    -- Node3

   function Protected_Present
     (N : Node_Id) return Boolean;    -- Flag1

   function Range_Constraint
     (N : Node_Id) return Node_Id;    -- Node4

   function Range_Expression
     (N : Node_Id) return Node_Id;    -- Node4

   function Real_Range_Specification
     (N : Node_Id) return Node_Id;    -- Node4

   function Record_Extension_Part
     (N : Node_Id) return Node_Id;    -- Node3

   function Redundant_Use
     (N : Node_Id) return Boolean;    -- Flag2

   function Reverse_Present
     (N : Node_Id) return Boolean;    -- Flag1

   function Right_Opnd
     (N : Node_Id) return Node_Id;    -- Node3

   function Selective_Accept_Alternatives
     (N : Node_Id) return List_Id;    -- List1

   function Selector_Name
     (N : Node_Id) return Node_Id;    -- Node3

   function Selector_Names
     (N : Node_Id) return List_Id;    -- List1

   function Specification
     (N : Node_Id) return Node_Id;    -- Node1

   function Statements
     (N : Node_Id) return List_Id;    -- List3

   function Strval
     (N : Node_Id) return String_Id;  -- Str3

   function Subtype_Indication
     (N : Node_Id) return Node_Id;    -- Node4

   function Subtype_Mark
     (N : Node_Id) return Node_Id;    -- Node4

   function Subtype_Marks
     (N : Node_Id) return List_Id;    -- List2

   function Tagged_Present
     (N : Node_Id) return Boolean;    -- Flag1

   function Task_Definition
     (N : Node_Id) return Node_Id;    -- Node3

   function Then_Statements
     (N : Node_Id) return List_Id;    -- List2

   function Triggering_Alternative
     (N : Node_Id) return Node_Id;    -- Node1

   function Triggering_Statement
     (N : Node_Id) return Node_Id;    -- Node1

   function Type_Definition
     (N : Node_Id) return Node_Id;    -- Node3

   function Unchecked_Conversion
     (N : Node_Id) return Boolean;    -- Flag11

   function Unit
     (N : Node_Id) return Node_Id;    -- Node2

   function Variant_Part
     (N : Node_Id) return Node_Id;    -- Node4

   function Variants
     (N : Node_Id) return List_Id;    -- List1

   function Visible_Declarations
     (N : Node_Id) return List_Id;    -- List2

   --  End functions (note used by xsinfo utility program to end processing)

   ----------------------------
   -- Node Update Procedures --
   ----------------------------

   --  These are the corresponding node update routines, which again provide
   --  a high level logical access with type checking. In addition to setting
   --  the indicated field of the node N to the given Val, in the case of
   --  tree pointers (List1-4), the parent pointer of the Val node is set to
   --  point back to node N. This automates the setting of the parent pointer.

   procedure Set_Abort_Present
     (N : Node_Id; Val : Boolean);    -- Flag1

   procedure Set_Abortable_Part
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Abstract_Present
     (N : Node_Id; Val : Boolean);    -- Flag4

   procedure Set_Accept_Name
     (N : Node_Id; Val : Node_Id);    -- Node1

   procedure Set_Accept_Statement
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Actions
     (N : Node_Id; Val : List_Id);    -- List1

   procedure Set_Activation_Chain_Entity
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Acts_As_Spec
     (N : Node_Id; Val : Boolean);    -- Flag4

   procedure Set_Actual_Parameter
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Aliased_Present
     (N : Node_Id; Val : Boolean);    -- Flag1

   procedure Set_All_Present
     (N : Node_Id; Val : Boolean);    -- Flag1

   procedure Set_Alternatives
     (N : Node_Id; Val : List_Id);    -- List4

   procedure Set_Analyzed
     (N : Node_Id; Val : Boolean);    -- Flag19

   procedure Set_Array_Aggregate
     (N : Node_Id; Val : Node_Id);    -- Node4

   procedure Set_Assignment_OK
     (N : Node_Id; Val : Boolean);    -- Flag5

   procedure Set_Bad_Is_Detected
     (N : Node_Id; Val : Boolean);    -- Flag1

   procedure Set_Body_Required
     (N : Node_Id; Val : Boolean);    -- Flag3

   procedure Set_Box_Present
     (N : Node_Id; Val : Boolean);    -- Flag1

   procedure Set_Char_Literal_Value
     (N : Node_Id; Val : Char_Code);  -- Char_Code2

   procedure Set_Chars
     (N : Node_Id; Val : Name_Id);    -- Name1

   procedure Set_Choice_Parameter
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Choices
     (N : Node_Id; Val : List_Id);    -- List1

   procedure Set_Component_Associations
     (N : Node_Id; Val : List_Id);    -- List4

   procedure Set_Component_Clauses
     (N : Node_Id; Val : List_Id);    -- List3

   procedure Set_Component_Declarations
     (N : Node_Id; Val : List_Id);    -- List3

   procedure Set_Component_List
     (N : Node_Id; Val : Node_Id);    -- Node1

   procedure Set_Component_Name
     (N : Node_Id; Val : Node_Id);    -- Node1

   procedure Set_Condition
     (N : Node_Id; Val : Node_Id);    -- Node5

   procedure Set_Constant_Present
     (N : Node_Id; Val : Boolean);    -- Flag2

   procedure Set_Constraint
     (N : Node_Id; Val : Node_Id);    -- Node3

   procedure Set_Constraints
     (N : Node_Id; Val : List_Id);    -- List1

   procedure Set_Context_Installed
     (N : Node_Id; Val : Boolean);    -- Flag3

   procedure Set_Context_Items
     (N : Node_Id; Val : List_Id);    -- List1

   procedure Set_Controlling_Argument
     (N : Node_Id; Val : Node_Id);    -- Node1

   procedure Set_Corresponding_Body
     (N : Node_Id; Val : Node_Id);    -- Node5

   procedure Set_Corresponding_Spec
     (N : Node_Id; Val : Node_Id);    -- Node5

   procedure Set_Debug_Statement
     (N : Node_Id; Val : Node_Id);    -- Node3

   procedure Set_Decimal
     (N : Node_Id; Val : Boolean);    -- Flag2

   procedure Set_Declarations
     (N : Node_Id; Val : List_Id);    -- List3

   procedure Set_Default_Name
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Defining_Identifier
     (N : Node_Id; Val : Node_Id);    -- Node1

   procedure Set_Defining_Unit_Name
     (N : Node_Id; Val : Node_Id);    -- Node1

   procedure Set_Delay_Alternative
     (N : Node_Id; Val : Node_Id);    -- Node4

   procedure Set_Delay_Statement
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Delta_Expression
     (N : Node_Id; Val : Node_Id);    -- Node3

   procedure Set_Denominator
     (N : Node_Id; Val : Uint);       -- Uint4

   procedure Set_Digits_Expression
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Discrete_Choices
     (N : Node_Id; Val : List_Id);    -- List4

   procedure Set_Discrete_Range
     (N : Node_Id; Val : Node_Id);    -- Node4

   procedure Set_Discrete_Subtype_Definition
     (N : Node_Id; Val : Node_Id);    -- Node4

   procedure Set_Discrete_Subtype_Definitions
     (N : Node_Id; Val : List_Id);    -- List2

   procedure Set_Discriminant_Specifications
     (N : Node_Id; Val : List_Id);    -- List2

   procedure Set_Discriminant_Type
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Do_Access_Check
     (N : Node_Id; Val : Boolean);    -- Flag2

   procedure Set_Do_Accessibility_Check
     (N : Node_Id; Val : Boolean);    -- Flag3

   procedure Set_Do_Discriminant_Check
     (N : Node_Id; Val : Boolean);    -- Flag3

   procedure Set_Do_Division_Check
     (N : Node_Id; Val : Boolean);    -- Flag3

   procedure Set_Do_Elaboration_Check
     (N : Node_Id; Val : Boolean);    -- Flag2

   procedure Set_Do_Length_Check
     (N : Node_Id; Val : Boolean);    -- Flag4

   procedure Set_Do_Overflow_Check
     (N : Node_Id; Val : Boolean);    -- Flag2

   procedure Set_Do_Range_Check
     (N : Node_Id; Val : Boolean);    -- Flag9

   procedure Set_Do_Storage_Check
     (N : Node_Id; Val : Boolean);    -- Flag2

   procedure Set_Do_Tag_Check
     (N : Node_Id; Val : Boolean);    -- Flag3

   procedure Set_Elaborate_Present
     (N : Node_Id; Val : Boolean);    -- Flag4

   procedure Set_Elaborate_All_Present
     (N : Node_Id; Val : Boolean);    -- Flag1

   procedure Set_Elaborate_Body_Present
     (N : Node_Id; Val : Boolean);    -- Flag7

   procedure Set_Else_Statements
     (N : Node_Id; Val : List_Id);    -- List4

   procedure Set_Elsif_Parts
     (N : Node_Id; Val : List_Id);    -- List3

   procedure Set_Enclosing_Variant
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Entity
     (N : Node_Id; Val : Node_Id);    -- Node4

   procedure Set_Entry_Body_Formal_Part
     (N : Node_Id; Val : Node_Id);     -- Node2

   procedure Set_Entry_Call_Alternative
     (N : Node_Id; Val : Node_Id);    -- Node1

   procedure Set_Entry_Call_Statement
     (N : Node_Id; Val : Node_Id);    -- Node1

   procedure Set_Entry_Index
     (N : Node_Id; Val : Node_Id);    -- Node5

   procedure Set_Entry_Index_Specification
     (N : Node_Id; Val : Node_Id);    -- Node1

   procedure Set_Error_Posted
     (N : Node_Id; Val : Boolean);    -- Flag20

   procedure Set_Etype
     (N : Node_Id; Val : Node_Id);    -- Node5

   procedure Set_Evaluate_Once
     (N : Node_Id; Val : Boolean);    -- Flag10

   procedure Set_Exception_Choices
     (N : Node_Id; Val : List_Id);    -- List4

   procedure Set_Exception_Handlers
     (N : Node_Id; Val : List_Id);    -- List4

   procedure Set_Explicit_Generic_Actual_Parameter
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Expression
     (N : Node_Id; Val : Node_Id);    -- Node3

   procedure Set_Expressions
     (N : Node_Id; Val : List_Id);    -- List3

   procedure Set_First_Bit
     (N : Node_Id; Val : Node_Id);    -- Node3

   procedure Set_First_Name
     (N : Node_Id; Val : Boolean);    -- Flag5

   procedure Set_First_Named_Actual
     (N : Node_Id; Val : Node_Id);    -- Node4

   procedure Set_Following_Pragmas
     (N : Node_Id; Val : List_Id);    -- List3

   procedure Set_Formal_Type_Definition
     (N : Node_Id; Val : Node_Id);    -- Node3

   procedure Set_Generic_Associations
     (N : Node_Id; Val : List_Id);    -- List3

   procedure Set_Generic_Formal_Declarations
     (N : Node_Id; Val : List_Id);    -- List3

   procedure Set_Generic_Parent
     (N : Node_Id; Val : Node_Id);    -- Node5

   procedure Set_Handled_Statement_Sequence
     (N : Node_Id; Val : Node_Id);    -- Node4

   procedure Set_Has_Created_Identifier
     (N : Node_Id; Val : Boolean);    -- Flag1

   procedure Set_Has_No_Side_Effects
     (N : Node_Id; Val : Boolean);    -- Flag8

   procedure Set_Has_Priority_Pragma
     (N : Node_Id; Val : Boolean);    -- Flag6

   procedure Set_Has_Task_Stack_Size_Pragma
     (N : Node_Id; Val : Boolean);    -- Flag5

   procedure Set_Has_Unknown_Discriminants
     (N : Node_Id; Val : Boolean);    -- Flag3

   procedure Set_High_Bound
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Identifier
     (N : Node_Id; Val : Node_Id);    -- Node1

   procedure Set_Implicit_Types
     (N : Node_Id; Val : List_Id);    -- List2

   procedure Set_Implicit_With
     (N : Node_Id; Val : Boolean);    -- Flag2

   procedure Set_In_Present
     (N : Node_Id; Val : Boolean);    -- Flag1

   procedure Set_Intval
     (N : Node_Id; Val : Uint);       -- Uint3

   procedure Set_Is_Evaluated
     (N : Node_Id; Val : Boolean);    -- Flag7

   procedure Set_Is_Overloaded
     (N : Node_Id; Val : Boolean);    -- Flag5

   procedure Set_Is_Task_Master
     (N : Node_Id; Val : Boolean);    -- Flag5

   procedure Set_Is_Static
     (N : Node_Id; Val : Boolean);    -- Flag6

   procedure Set_Iteration_Scheme
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Label
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Last_Bit
     (N : Node_Id; Val : Node_Id);    -- Node4

   procedure Set_Last_Name
     (N : Node_Id; Val : Boolean);    -- Flag6

   procedure Set_Library_Unit
     (N : Node_Id; Val : Node_Id);    -- Node4

   procedure Set_Left_Opnd
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Literals
     (N : Node_Id; Val : List_Id);    -- List1

   procedure Set_Limited_Present
     (N : Node_Id; Val : Boolean);    -- Flag2

   procedure Set_Loop_Parameter_Specification
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Low_Bound
     (N : Node_Id; Val : Node_Id);    -- Node1

   procedure Set_Mod_Clause
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_More_Ids
     (N : Node_Id; Val : Boolean);    -- Flag5

   procedure Set_Name
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Names
     (N : Node_Id; Val : List_Id);    -- List2

   procedure Set_Next_Named_Actual
     (N : Node_Id; Val : Node_Id);    -- Node4

   procedure Set_Null_Present
     (N : Node_Id; Val : Boolean);    -- Flag3

   procedure Set_Null_Record_Present
     (N : Node_Id; Val : Boolean);    -- Flag2

   procedure Set_Numerator
     (N : Node_Id; Val : Uint);       -- Uint3

   procedure Set_Object_Definition
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Others_Discrete_Choices
     (N : Node_Id; Val : List_Id);    -- List1

   procedure Set_Out_Present
     (N : Node_Id; Val : Boolean);    -- Flag2

   procedure Set_Parameter_Associations
     (N : Node_Id; Val : List_Id);    -- List3

   procedure Set_Parameter_Specifications
     (N : Node_Id; Val : List_Id);    -- List2

   procedure Set_Parameter_Type
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Parens
     (N : Node_Id; Val : Boolean);    -- Flag1

   procedure Set_Parent_Spec
     (N : Node_Id; Val : Node_Id);    -- Node4

   procedure Set_Position
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Pragma_Argument_Associations
     (N : Node_Id; Val : List_Id);    -- List2

   procedure Set_Preelaborable
     (N : Node_Id; Val : Boolean);    -- Flag2

   procedure Set_Prefix
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Prev_Ids
     (N : Node_Id; Val : Boolean);    -- Flag6

   procedure Set_Private_Declarations
     (N : Node_Id; Val : List_Id);    -- List4

   procedure Set_Private_Present
     (N : Node_Id; Val : Boolean);    -- Flag1

   procedure Set_Proper_Body
     (N : Node_Id; Val : Node_Id);    -- Node1

   procedure Set_Protected_Definition
     (N : Node_Id; Val : Node_Id);    -- Node3

   procedure Set_Protected_Present
     (N : Node_Id; Val : Boolean);    -- Flag1

   procedure Set_Range_Constraint
     (N : Node_Id; Val : Node_Id);    -- Node4

   procedure Set_Range_Expression
     (N : Node_Id; Val : Node_Id);    -- Node4

   procedure Set_Real_Range_Specification
     (N : Node_Id; Val : Node_Id);    -- Node4

   procedure Set_Record_Extension_Part
     (N : Node_Id; Val : Node_Id);    -- Node3

   procedure Set_Redundant_Use
     (N : Node_Id; Val : Boolean);    -- Flag2

   procedure Set_Reverse_Present
     (N : Node_Id; Val : Boolean);    -- Flag1

   procedure Set_Right_Opnd
     (N : Node_Id; Val : Node_Id);    -- Node3

   procedure Set_Selective_Accept_Alternatives
     (N : Node_Id; Val : List_Id);    -- List1

   procedure Set_Selector_Name
     (N : Node_Id; Val : Node_Id);    -- Node3

   procedure Set_Selector_Names
     (N : Node_Id; Val : List_Id);    -- List1

   procedure Set_Specification
     (N : Node_Id; Val : Node_Id);    -- Node1

   procedure Set_Statements
     (N : Node_Id; Val : List_Id);    -- List3

   procedure Set_Strval
     (N : Node_Id; Val : String_Id);  -- Str3

   procedure Set_Subtype_Indication
     (N : Node_Id; Val : Node_Id);    -- Node4

   procedure Set_Subtype_Mark
     (N : Node_Id; Val : Node_Id);    -- Node4

   procedure Set_Subtype_Marks
     (N : Node_Id; Val : List_Id);    -- List2

   procedure Set_Tagged_Present
     (N : Node_Id; Val : Boolean);    -- Flag1

   procedure Set_Task_Definition
     (N : Node_Id; Val : Node_Id);    -- Node3

   procedure Set_Then_Statements
     (N : Node_Id; Val : List_Id);    -- List2

   procedure Set_Triggering_Alternative
     (N : Node_Id; Val : Node_Id);    -- Node1

   procedure Set_Triggering_Statement
     (N : Node_Id; Val : Node_Id);    -- Node1

   procedure Set_Type_Definition
     (N : Node_Id; Val : Node_Id);    -- Node3

   procedure Set_Unchecked_Conversion
     (N : Node_Id; Val : Boolean);    -- Flag11

   procedure Set_Unit
     (N : Node_Id; Val : Node_Id);    -- Node2

   procedure Set_Variant_Part
     (N : Node_Id; Val : Node_Id);    -- Node4

   procedure Set_Variants
     (N : Node_Id; Val : List_Id);    -- List1

   procedure Set_Visible_Declarations
     (N : Node_Id; Val : List_Id);    -- List2

   --------------------
   -- Inline Pragmas --
   --------------------

   pragma Inline (Abort_Present);
   pragma Inline (Abortable_Part);
   pragma Inline (Abstract_Present);
   pragma Inline (Accept_Name);
   pragma Inline (Accept_Statement);
   pragma Inline (Actions);
   pragma Inline (Activation_Chain_Entity);
   pragma Inline (Acts_As_Spec);
   pragma Inline (Actual_Parameter);
   pragma Inline (Aliased_Present);
   pragma Inline (All_Present);
   pragma Inline (Alternatives);
   pragma Inline (Analyzed);
   pragma Inline (Array_Aggregate);
   pragma Inline (Assignment_OK);
   pragma Inline (Bad_Is_Detected);
   pragma Inline (Body_Required);
   pragma Inline (Box_Present);
   pragma Inline (Char_Literal_Value);
   pragma Inline (Chars);
   pragma Inline (Choice_Parameter);
   pragma Inline (Choices);
   pragma Inline (Component_Associations);
   pragma Inline (Component_Clauses);
   pragma Inline (Component_Declarations);
   pragma Inline (Component_List);
   pragma Inline (Component_Name);
   pragma Inline (Condition);
   pragma Inline (Constant_Present);
   pragma Inline (Constraint);
   pragma Inline (Constraints);
   pragma Inline (Context_Installed);
   pragma Inline (Context_Items);
   pragma Inline (Controlling_Argument);
   pragma Inline (Corresponding_Body);
   pragma Inline (Corresponding_Spec);
   pragma Inline (Debug_Statement);
   pragma Inline (Declarations);
   pragma Inline (Decimal);
   pragma Inline (Default_Name);
   pragma Inline (Defining_Identifier);
   pragma Inline (Defining_Unit_Name);
   pragma Inline (Delay_Alternative);
   pragma Inline (Delay_Statement);
   pragma Inline (Delta_Expression);
   pragma Inline (Denominator);
   pragma Inline (Digits_Expression);
   pragma Inline (Discrete_Choices);
   pragma Inline (Discrete_Range);
   pragma Inline (Discrete_Subtype_Definition);
   pragma Inline (Discrete_Subtype_Definitions);
   pragma Inline (Discriminant_Specifications);
   pragma Inline (Discriminant_Type);
   pragma Inline (Do_Access_Check);
   pragma Inline (Do_Accessibility_Check);
   pragma Inline (Do_Discriminant_Check);
   pragma Inline (Do_Elaboration_Check);
   pragma Inline (Do_Length_Check);
   pragma Inline (Do_Division_Check);
   pragma Inline (Do_Overflow_Check);
   pragma Inline (Do_Range_Check);
   pragma Inline (Do_Storage_Check);
   pragma Inline (Do_Tag_Check);
   pragma Inline (Elaborate_Present);
   pragma Inline (Elaborate_All_Present);
   pragma Inline (Elaborate_Body_Present);
   pragma Inline (Else_Statements);
   pragma Inline (Elsif_Parts);
   pragma Inline (Enclosing_Variant);
   pragma Inline (Entity);
   pragma Inline (Entry_Body_Formal_Part);
   pragma Inline (Entry_Call_Alternative);
   pragma Inline (Entry_Call_Statement);
   pragma Inline (Entry_Index);
   pragma Inline (Entry_Index_Specification);
   pragma Inline (Error_Posted);
   pragma Inline (Etype);
   pragma Inline (Evaluate_Once);
   pragma Inline (Exception_Choices);
   pragma Inline (Exception_Handlers);
   pragma Inline (Explicit_Generic_Actual_Parameter);
   pragma Inline (Expression);
   pragma Inline (Expressions);
   pragma Inline (First_Bit);
   pragma Inline (First_Name);
   pragma Inline (First_Named_Actual);
   pragma Inline (Following_Pragmas);
   pragma Inline (Formal_Type_Definition);
   pragma Inline (Generic_Associations);
   pragma Inline (Generic_Formal_Declarations);
   pragma Inline (Generic_Parent);
   pragma Inline (Handled_Statement_Sequence);
   pragma Inline (Has_Created_Identifier);
   pragma Inline (Has_No_Side_Effects);
   pragma Inline (Has_Priority_Pragma);
   pragma Inline (Has_Task_Stack_Size_Pragma);
   pragma Inline (Has_Unknown_Discriminants);
   pragma Inline (High_Bound);
   pragma Inline (Identifier);
   pragma Inline (Implicit_Types);
   pragma Inline (Implicit_With);
   pragma Inline (In_Present);
   pragma Inline (Intval);
   pragma Inline (Is_Evaluated);
   pragma Inline (Is_Overloaded);
   pragma Inline (Is_Static);
   pragma Inline (Is_Task_Master);
   pragma Inline (Iteration_Scheme);
   pragma Inline (Label);
   pragma Inline (Last_Bit);
   pragma Inline (Last_Name);
   pragma Inline (Library_Unit);
   pragma Inline (Left_Opnd);
   pragma Inline (Limited_Present);
   pragma Inline (Literals);
   pragma Inline (Loop_Parameter_Specification);
   pragma Inline (Low_Bound);
   pragma Inline (Mod_Clause);
   pragma Inline (More_Ids);
   pragma Inline (Name);
   pragma Inline (Names);
   pragma Inline (Next_Named_Actual);
   pragma Inline (Null_Present);
   pragma Inline (Null_Record_Present);
   pragma Inline (Numerator);
   pragma Inline (Object_Definition);
   pragma Inline (Others_Discrete_Choices);
   pragma Inline (Out_Present);
   pragma Inline (Parameter_Associations);
   pragma Inline (Parameter_Specifications);
   pragma Inline (Parameter_Type);
   pragma Inline (Parens);
   pragma Inline (Parent_Spec);
   pragma Inline (Position);
   pragma Inline (Pragma_Argument_Associations);
   pragma Inline (Preelaborable);
   pragma Inline (Prefix);
   pragma Inline (Prev_Ids);
   pragma Inline (Private_Declarations);
   pragma Inline (Private_Present);
   pragma Inline (Proper_Body);
   pragma Inline (Protected_Definition);
   pragma Inline (Protected_Present);
   pragma Inline (Range_Constraint);
   pragma Inline (Range_Expression);
   pragma Inline (Real_Range_Specification);
   pragma Inline (Record_Extension_Part);
   pragma Inline (Redundant_Use);
   pragma Inline (Reverse_Present);
   pragma Inline (Right_Opnd);
   pragma Inline (Selective_Accept_Alternatives);
   pragma Inline (Selector_Name);
   pragma Inline (Selector_Names);
   pragma Inline (Specification);
   pragma Inline (Statements);
   pragma Inline (Strval);
   pragma Inline (Subtype_Indication);
   pragma Inline (Subtype_Mark);
   pragma Inline (Subtype_Marks);
   pragma Inline (Tagged_Present);
   pragma Inline (Task_Definition);
   pragma Inline (Then_Statements);
   pragma Inline (Triggering_Alternative);
   pragma Inline (Triggering_Statement);
   pragma Inline (Type_Definition);
   pragma Inline (Unchecked_Conversion);
   pragma Inline (Unit);
   pragma Inline (Variant_Part);
   pragma Inline (Variants);
   pragma Inline (Visible_Declarations);

   pragma Inline (Set_Analyzed);
   pragma Inline (Set_Error_Posted);

end Sinfo;
