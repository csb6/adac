/****************************************************************************/
/*									    */
/*			   GNAT COMPILER COMPONENTS			    */
/*                                                                          */
/*                   ADA CHAPTER 3: DECLARATIONS AND TYPES                  */
/*                            - GNU SPECIFIC -                              */
/*                                                                          */
/*                              Specification                               */
/*									    */
/*			      $Revision: 1.39 $ 			    */
/*									    */
/*	       Copyright (c) 1992,1993, NYU, All Rights Reserved	    */
/*									    */
/* GNAT is free software;  you can  redistribute it  and/or modify it under */
/* terms  of the GNU  General  Public  License	as  published  by the  Free */
/* Software  Foundation;  either version 2,  or (at your option)  any later */
/* version.  GNAT is distributed  in the hope  that it will be useful,	but */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANT- */
/* ABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public */
/* License  for  more details.	You should have received  a copy of the GNU */
/* General Public License along with GNAT;  see file COPYING. If not, write */
/* to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */
/*									    */
/****************************************************************************/

/* Standard data type sizes.  Most of these are not used.  */

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef SHORT_TYPE_SIZE
#define SHORT_TYPE_SIZE (BITS_PER_UNIT * MIN ((UNITS_PER_WORD + 1) / 2, 2))
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_LONG_TYPE_SIZE
#define LONG_LONG_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef FLOAT_TYPE_SIZE
#define FLOAT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

/* The choice of SIZE_TYPE here is very problematic.  We need a signed
   type whose bit width is Pmode.  Assume "long" is such a type here.  */
#undef SIZE_TYPE
#define SIZE_TYPE "long int"

/* Flags added to GCC type nodes.  */

/* Nonzero if this type is a record corresponding to a fat pointer.  */
#define TYPE_FAT_POINTER_P(NODE) TYPE_LANG_FLAG_0 (NODE)

/* Nonzero if this is a modular type with a modulus that is not equal to
   two to the power of its mode's size.  */
#define TYPE_MODULAR_P(NODE) TYPE_LANG_FLAG_1 (NODE)

/* Nonzero if this is a FUNCTION_TYPE that denotes a function returning
   an unconstrained array or record.  */
#define TYPE_RETURNS_UNCONSTRAINED_P(NODE) TYPE_LANG_FLAG_2 (NODE)

/* This field is only defined for FUNCTION_TYPE nodes. If the Ada
   subprogram contains no parameters passed by copy in/copy out then this
   field is 0. Otherwise it points to a list of nodes used to specify the
   return values of the out (or in out) parameters that qualify to be passed
   by copy in copy out.  It is a CONSTRUCTOR.  For a full description of the
   cico parameter passing mechanism refer to the routine gnat_to_gnu_entity. */
#define TYPE_CI_CO_LIST(NODE)   (tree) TYPE_LANG_SPECIFIC (NODE)

/* For an INTEGER_TYPE with TYPE_MODULAR_P, this is the value of the
   modulus. */
#define TYPE_MODULUS(NODE)	(tree) TYPE_LANG_SPECIFIC (NODE)

/* For an INTEGER_TYPE that is the TYPE_DOMAIN of some ARRAY_TYPE, points to
   the type corresponding to the Ada index type.  */
#define TYPE_INDEX_TYPE(NODE)	(tree) TYPE_LANG_SPECIFIC (NODE)

/* For a RECORD_TYPE that is a fat pointer, point to the type for the
   unconstrained object.  */
#define TYPE_UNCONSTRAINED_ARRAY(NODE)  (tree) TYPE_LANG_SPECIFIC (NODE)

/* For a RECORD_TYPE that is not a fat pointer, point to the original subtype
   for the _parent field, if there was one.  */
#define TYPE_PARENT_SUBTYPE(NODE)  (tree) TYPE_LANG_SPECIFIC (NODE)

/* Nonzero in a FIELD_DECL that represents a discriminant.  */
#define DECL_DISCRIMINANT_P(NODE) DECL_LANG_FLAG_0 (NODE)

/* Nonzero if this decl is always used by reference; i.e., an INDIRECT_REF
   is needed to access the object.  */
#define DECL_BY_REF_P(NODE) DECL_LANG_FLAG_1 (NODE)

/* Nonzero in a FIELD_DECL that is a parent field.  */
#define DECL_PARENT_P(NODE) DECL_LANG_FLAG_2 (NODE)

/* Variables expected by the GCC back-end.  */

/* A node which has tree code ERROR_MARK, and whose type is itself.  */
extern tree error_mark_node;

/* Various standard data types and nodes.  */

extern tree integer_type_node;
extern tree unsigned_type_node;
extern tree char_type_node;
extern tree void_type_node;
extern tree void_type_decl_node;
extern tree integer_zero_node;
extern tree integer_one_node;
extern tree null_pointer_node;

/* The FUNCTION_DECL node for the function currently being compiled, or 0
   if between functions.  */
extern tree current_function_decl;

/* Variables created for the sole tree translator sake. Their names and
   types can be changed as desired.  */

/* type declaration node  <==> typedef void *T */
extern tree ptr_void_type_node;

/* function type declaration -- void T() */
extern tree void_ftype;

/* type declaration node  <==> typedef void *T() */
extern tree ptr_void_ftype;

/* A function declaration node for a run-time function for allocating memory.
   Ada allocators cause calls to this function to be generated.   */
extern tree malloc_decl;

/* Types and decls used by our temporary exception mechanism.  See
   init_decl_processing for details.  */
extern tree jmpbuf_type;
extern tree jmpbuf_ptr_type;
extern tree jmpbuf_decl;
extern tree excptr_decl;
extern tree longjmp_decl;
extern tree setjmp_decl;
extern tree raise_constraint_decl;
extern tree unchecked_union_node;

/* Routines expected by the gcc back-end. They must have exactly the same
   prototype and names as below.  */

/* Returns non-zero if we are currently in the global binding level       */
extern int global_bindings_p	PROTO((void));

/* Returns the list of declarations in the current level. Note that this list
   is in reverse order (it has to be so for back-end compatibility).  */
extern tree getdecls			PROTO((void));

/* Nonzero if the current level needs to have a BLOCK made.  */
extern int kept_level_p 		PROTO((void));

/* Enter a new binding level. The input parameter is ignored, but has to be
   specified for back-end compatibility.  */
extern void pushlevel			PROTO((int));

/* Exit a binding level.
   Pop the level off, and restore the state of the identifier-decl mappings
   that were in effect when this level was entered.

   If KEEP is nonzero, this level had explicit declarations, so
   and create a "block" (a BLOCK node) for the level
   to record its declarations and subblocks for symbol table output.

   If FUNCTIONBODY is nonzero, this level is the body of a function,
   so create a block as if KEEP were set and also clear out all
   label names.

   If REVERSE is nonzero, reverse the order of decls before putting
   them into the BLOCK.  */
extern tree poplevel		PROTO((int,int, int));

/* Insert BLOCK at the end of the list of subblocks of the
   current binding level.  This is used when a BIND_EXPR is expanded,
   to handle the BLOCK node inside the BIND_EXPR.  */
extern void insert_block		PROTO((tree));

/* Set the BLOCK node for the innermost scope
   (the one we are currently in).  */
extern void set_block			PROTO((tree));

/* Records a ..._DECL node DECL as belonging to the current lexical scope.
   Returns the ..._DECL node. */
extern tree pushdecl			PROTO((tree));

/* Create the predefined scalar types such as `integer_type_node' needed 
   in the gcc back-end and initialize the global binding level.  */
extern void init_decl_processing	PROTO((void));

/* Return an integer type with the number of bits of precision given by  
   PRECISION.  UNSIGNEDP is nonzero if the type is unsigned; otherwise
   it is a signed type.  */
extern tree type_for_size		PROTO((unsigned, int));

/* Return a data type that has machine mode MODE.  UNSIGNEDP selects
   an unsigned type; otherwise a signed type is returned.  */
extern tree type_for_mode		PROTO((enum machine_mode, int));

/* Return the unsigned version of a TYPE_NODE, a scalar type.  */
extern tree unsigned_type		PROTO((tree));

/* Return the signed version of a TYPE_NODE, a scalar type.  */
extern tree signed_type			PROTO((tree));

/* Return a type the same as TYPE except unsigned or signed according to
   UNSIGNEDP.  */
extern tree signed_or_unsigned_type	PROTO((int, tree));

/* This routine is called in tree.c to print an error message for invalid use
   of an incomplete type.  */
extern void incomplete_type_error	PROTO((tree, tree));

/* This function is called indirectly from toplev.c to handle incomplete 
   declarations, i.e. VAR_DECL nodes whose DECL_SIZE is zero.  To be precise,
   compile_file in toplev.c makes an indirect call through the function pointer
   incomplete_decl_finalize_hook which is initialized to this routine in
   init_decl_processing.  */
extern void finish_incomplete_decl	PROTO((tree));

/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.  */
extern tree convert			PROTO((tree, tree));

/* Routines created solely for the tree translator's sake. Their prototypes
   can be changed as desired.  */

/* GNAT_DEFINING_IDENTIFIER is a GNAT tree node for a defining identifier.
   GNU_DECL is the GCC tree which is to be associated with
   GNAT_DEFINING_IDENTIFIER. Such gnu tree node is always an ..._DECL node.
   If NO_CHECK is nonzero, the latter check is suppressed. 
   If GNU_DECL is zero, a previous association is to be reset.  */
extern void save_gnu_tree		PROTO((gnat_tree, tree, int));

/* GNAT_DEFINING_IDENTIFIER is a GNAT tree node for a defining identifier.
   Return the ..._DECL node that was associated with it.  If there is no tree
   node associated with GNAT_DEFINING_IDENTIFIER, abort.  */
extern tree get_gnu_tree		PROTO((gnat_tree));

/* Return nonzero if a GCC tree has been associated with
   GNAT_DEFINING_IDENTIFIER.  */
extern int present_gnu_tree	PROTO((gnat_tree));

/* Initialize tables for above routines.  */
extern void init_gnat_to_gnu		PROTO((void));

/* Given a record type (RECORD_TYPE) and a chain of FIELD_DECL
   nodes (FIELDLIST), finish constructing the record or union type.  */
extern void finish_record_type		PROTO((tree, tree));

/* Returns a FUNCTION_TYPE node. RETURN_TYPE is the type returned by the
   subprogram. If it is void_type_node, then we are dealing with a procedure,
   otherwise we are dealing with a function. PARAM_DECL_LIST is a list of
   PARM_DECL nodes that are the subprogram arguments.  CICO_LIST is the
   copy-in/copy-out list to be stored into TYPE_CI_CO_LIST.  */
extern tree create_subprog_type		PROTO((tree, tree, tree));

/* Returns a TYPE_DECL node. TYPE_NAME gives the name of the type (a character
   string) and TYPE is a ..._TYPE node giving its data type.  */
extern tree create_type_decl		PROTO((char *, tree));

/* Returns a GCC VAR_DECL node. VAR_NAME gives the name of the variable (a
   character string). ASM_NAME is its assembler name (if provided).  TYPE is
   its data type (a GCC ..._TYPE node).  VAR_INIT is the GCC tree for an
   optional initial expression; NULL_TREE if none.

   CONST_FLAG is nonzero if this variable is constant.

   PUBLIC_FLAG is nonzero if this definition is to be made visible outside of
   the current compilation unit. This flag should be set when processing the
   variable definitions in a package specification.  EXTERN_FLAG is nonzero 
   when processing an external variable declaration (as opposed to a
   definition: no storage is to be allocated for the variable here).
   STATIC_FLAG is only relevant when not at top level.  In that case
   it indicates whether to always allocate storage to the variable.
   VOLATILE_FLAG is true if the variable is to be treated as volatile.  */
extern tree create_var_decl	PROTO((char *, char *, tree, tree,
				       int, int, int, int, int));

/* Obtain any pending elaborations and clear the old list.  */
extern tree get_pending_elaborations PROTO((void));

/* Add some pending elaborations to the current list.  */
extern void add_pending_elaborations PROTO ((tree, tree));


/* Returns a FIELD_DECL node. FIELD_NAME the field name, FIELD_TYPE is its
   type, and RECORD_TYPE is the type of the parent.  PACKED is nonzero if
   this field is in a record type with a "pragma pack".  */
extern tree create_field_decl	PROTO((char *, tree, tree, int));

/* Returns a PARM_DECL node. PARAM_NAME is the name of the parameter,
   PARAM_TYPE is its type.  */
extern tree create_param_decl	PROTO((char *, tree));

/* Returns a FUNCTION_DECL node.  SUBPROG_NAME is the name of the subprogram,
   ASM_NAME is its assembler name, SUBPROG_TYPE is its type (a FUNCTION_TYPE
   node), PARAM_DECL_LIST is the list of the subprogram arguments (a list of
   PARM_DECL nodes chained through the TREE_CHAIN field).

   INLINE_FLAG, PUBLIC_FLAG, EXTERN_FLAG, and PURE_FLAG are used to set the
   appropriate fields in the FUNCTION_DECL.  */
extern tree create_subprog_decl	PROTO((char *, char *, tree, tree,
				       int, int, int, int));

/* Returns a LABEL_DECL node for LABEL_NAME.  */
extern tree create_label_decl	PROTO((char *));

/* Set up the framework for generating code for SUBPROG_DECL, a subprogram
   body. This routine needs to be invoked before processing the declarations
   appearing in the subprogram.  */
extern void begin_subprog_body	PROTO((tree));

/* Finish the definition of the current subprogram and compile it all the way
   to assembler language output.  */
extern void end_subprog_body	PROTO((void));

/* Convert a pointer to a constrained array into a pointer to an unconstrained
   array.  This involves making a template.  If ALLOCATE is nonzero, the
   template must be created using an allocator instead of in the
   current context's memory.  */
extern tree convert_to_unconstrained PROTO((tree, tree, int));

/* EXP is an expression for the size of an object.  If this size contains
   discriminant references, replace them with the maximum (if MAX_P) or
   minimum (if ! MAX_P) possible value of the discriminant.  */
extern tree max_size		PROTO((tree, int));
