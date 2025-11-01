/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                   ADA CHAPTER 3: DECLARATIONS AND TYPES                  */
/*                            - GNU SPECIFIC -                              */
/*                                                                          */
/*                                 Body                                     */
/*                                                                          */
/*                            $Revision: 1.61 $                             */
/*                                                                          */
/*             Copyright (c) 1992,1993, NYU, All Rights Reserved            */
/*                                                                          */
/* GNAT is free software;  you can  redistribute it  and/or modify it under */
/* terms  of the GNU  General  Public  License  as  published  by the  Free */
/* Software  Foundation;  either version 2,  or (at your option)  any later */
/* version.  GNAT is distributed  in the hope  that it will be useful,  but */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANT- */
/* ABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public */
/* License  for  more details.  You should have received  a copy of the GNU */
/* General Public License along with GNAT;  see file COPYING. If not, write */
/* to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */
/*                                                                          */
/****************************************************************************/

#include "config.h"
#include "tree.h"
#include "flags.h"
#include "a-ada.h"
#include "a-types.h"
#include "a-atree.h"
#include "a-sinfo.h"
#include "a-einfo.h"
#include "a-namet.h"
#include "a-string.h"
#include "a-uint.h"
#include "a-trans.h"
#include "a-gtran3.h"
#include "a-trans3.h"
#include "a-trans4.h"
#include "a-misc.h"
#include "a-rtree.h"
#include "convert.h"

#undef  NULL
#define NULL 0


/* Global Variables for the various types we create.  */ 

tree error_mark_node;
tree integer_type_node;
tree unsigned_type_node;
tree char_type_node;
tree void_type_node;
tree void_type_decl_node;
tree ptr_void_type_node;
tree void_ftype;
tree ptr_void_ftype;

tree malloc_decl;
tree jmpbuf_type;
tree jmpbuf_ptr_type;
tree jmpbuf_decl;
tree excptr_decl;
tree longjmp_decl;
tree setjmp_decl;
tree raise_constraint_decl;
tree unchecked_union_node;

tree integer_zero_node;
tree integer_one_node;
tree null_pointer_node;

tree current_function_decl = NULL;

static int contains_placeholder_except_p PROTO((tree, tree));

/* Routines to Associate and Retrieve GCC Nodes with Gnat Nodes: */

/* Associates a GNAT tree node to a GCC tree node. It is used in
   `save_gnu_tree', `get_gnu_tree' and `present_gnu_tree'. See documentation
   of `save_gnu_tree' for more info.  */
static tree *associate_gnat_to_gnu;

/* Initialize the association of GNAT nodes to GCC trees.  */

void
init_gnat_to_gnu ()
{
  gnat_tree gnat_node;

  associate_gnat_to_gnu   = (tree *) xmalloc (max_gnat_nodes * sizeof (tree));

  for (gnat_node = 0; gnat_node < max_gnat_nodes; gnat_node++)
    associate_gnat_to_gnu [gnat_node]   = NULL_TREE;

  associate_gnat_to_gnu   -= First_Node_Id;
}

/* GNAT_DEFINING_IDENTIFIER is a GNAT tree node for a defining identifier.
   GNU_DECL is the GCC tree which is to be associated with
   GNAT_DEFINING_IDENTIFIER. Such gnu tree node is always an ..._DECL node.
   If NO_CHECK is nonzero, the latter check is suppressed.

   If GNU_DECL is zero, a previous association is to be reset.  */

void
save_gnu_tree (gnat_defining_identifier, gnu_decl, no_check)
     gnat_tree gnat_defining_identifier;
     tree gnu_decl;
     int no_check;
{
  if (gnu_decl
      && (associate_gnat_to_gnu [gnat_defining_identifier]
	  || (! no_check && TREE_CODE_CLASS (TREE_CODE (gnu_decl)) != 'd')))
    abort ();

  associate_gnat_to_gnu [gnat_defining_identifier] = gnu_decl;
}

/* GNAT_DEFINING_IDENTIFIER is a GNAT tree node for a defining identifier.
   Return the ..._DECL node that was associated with it.  If there is no tree
   node associated with GNAT_DEFINING_IDENTIFIER, abort.  */

tree
get_gnu_tree (gnat_defining_identifier)
     gnat_tree gnat_defining_identifier;
{
  if (! associate_gnat_to_gnu [gnat_defining_identifier])
    abort ();

  return associate_gnat_to_gnu [gnat_defining_identifier];
}

/* Return nonzero if a GCC tree has been associated with
   GNAT_DEFINING_IDENTIFIER.  */

int
present_gnu_tree (gnat_defining_identifier)
     gnat_tree gnat_defining_identifier;
{
  return (associate_gnat_to_gnu [gnat_defining_identifier] != NULL_TREE);
}

/* For each binding contour we allocate a binding_level structure which records
   the entities defined or declared in that contour. Contours include:

	the global one
	one for each subprogram definition
	one for each compound statement (declare block)

   Binding contours are used to create GCC tree BLOCK nodes.  */

struct binding_level
{
  /* A chain of ..._DECL nodes for all variables, constants, functions,
     parameters and type declarations.  These ..._DECL nodes are chained
     through the TREE_CHAIN field. Note that these ..._DECL nodes are stored
     in the reverse of the order supplied to be compatible with the
     back-end.  */
  tree names;
  /* For each level (except the global one), a chain of BLOCK nodes for all
     the levels that were entered and exited one level down from this one.  */
  tree blocks;
  /* The back end may need, for its own internal processing, to create a BLOCK
     node. This field is set aside for this purpose. If this field is non-null
     when the level is popped, i.e. when poplevel is invoked, we will use such
     block instead of creating a new one from the 'names' field, that is the
     ..._DECL nodes accumulated so far.  Typically the routine 'pushlevel'
     will be called before setting this field, so that if the front-end had
     inserted ..._DECL nodes in the current block they will not be lost.   */
  tree block_created_by_back_end;
  /* The binding level containing this one (the enclosing binding level). */
  struct binding_level *level_chain;
};

/* The binding level currently in effect.  */
static struct binding_level *current_binding_level = NULL;

/* A chain of binding_level structures awaiting reuse.  */
static struct binding_level *free_binding_level = NULL;

/* The outermost binding level. This binding level is created when the
   compiler is started and it will exist through the entire compilation.  */
static struct binding_level *global_binding_level;

/* Binding level structures are initialized by copying this one.  */
static struct binding_level clear_binding_level = {NULL, NULL, NULL, NULL};

/* Return non-zero if we are currently in the global binding level.  */

int
global_bindings_p ()
{
  return current_binding_level == global_binding_level ? -1 : 0;
}

/* Return the list of declarations in the current level. Note that this list
   is in reverse order (it has to be so for back-end compatibility).  */

tree
getdecls ()
{
  return current_binding_level->names;
}

/* Nonzero if the current level needs to have a BLOCK made.  */

int
kept_level_p ()
{
  return (current_binding_level->names != 0);
}

/* Enter a new binding level. The input parameter is ignored, but has to be
   specified for back-end compatibility.  */

void
pushlevel (ignore)
     int ignore;
{
  struct binding_level *newlevel = NULL;

  /* Reuse a struct for this binding level, if there is one.  */
  if (free_binding_level)
    {
      newlevel = free_binding_level;
      free_binding_level = free_binding_level->level_chain;
    }
  else
    newlevel =
      (struct binding_level *) xmalloc (sizeof (struct binding_level));

  *newlevel = clear_binding_level;

  /* Add this level to the front of the chain (stack) of levels that are
     active.  */
  newlevel->level_chain = current_binding_level;
  current_binding_level = newlevel;
}

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

tree
poplevel (keep, reverse, functionbody)
     int keep;
     int reverse;
     int functionbody;
{
  /* Points to a GCC BLOCK tree node. This is the BLOCK node construted for the
     binding level that we are about to exit and which is returned by this
     routine.  */
  tree block_node = NULL_TREE;
  tree decl_chain;
  tree decl_node;
  tree subblock_chain = current_binding_level->blocks;
  tree subblock_node;
  tree block_created_by_back_end;

  /* Reverse the list of XXXX_DECL nodes if desired.  Note that the ..._DECL
     nodes chained through the `names' field of current_binding_level are in
     reverse order except for PARM_DECL node, which are explicitely stored in
     the right order.  */
  decl_chain = (reverse) ? nreverse (current_binding_level->names)
                         : current_binding_level->names;

  /* Output any nested inline functions within this block which must be
     compiled because their address is needed. */
  for (decl_node = decl_chain; decl_node; decl_node = TREE_CHAIN (decl_node))
    if ((TREE_CODE (decl_node) == FUNCTION_DECL)
	&& ! TREE_ASM_WRITTEN (decl_node)
	&& (DECL_INITIAL (decl_node) != 0)
	&& TREE_ADDRESSABLE (decl_node))
      {
	push_function_context ();
	output_inline_function (decl_node);
	pop_function_context ();
      }

  block_created_by_back_end = current_binding_level->block_created_by_back_end;
  if (block_created_by_back_end != 0)
    {
      block_node = block_created_by_back_end;

      /* Check if we are about to discard some information that was gathered
	 by the front-end. Nameley check if the back-end created a new block 
	 without calling pushlevel first. To understand why things are lost
	 just look at the next case (i.e. no block created by back-end.  */
      if ((keep || functionbody) && (decl_chain || subblock_chain))
	abort ();
    }

  /* If there were any declarations in the current binding level, or if this
     binding level is a function body, or if there are any nested blocks then
     create a BLOCK node to record them for the life of this function.  */
  else if (keep || functionbody)
    block_node = build_block (keep ? decl_chain : 0, 0, subblock_chain, 0, 0);

  /* Record the BLOCK node just built as the subblock its enclosing scope.  */
  for (subblock_node = subblock_chain; subblock_node;
       subblock_node = TREE_CHAIN (subblock_node))
    BLOCK_SUPERCONTEXT (subblock_node) = block_node;

  /* Clear out the meanings of the local variables of this level.  */

  for (subblock_node = decl_chain; subblock_node;
       subblock_node = TREE_CHAIN (subblock_node))
    if (DECL_NAME (subblock_node) != 0)
      /* If the identifier was used or addressed via a local extern decl,  
	 don't forget that fact.   */
      if (DECL_EXTERNAL (subblock_node))
	{
	  if (TREE_USED (subblock_node))
	    TREE_USED (DECL_NAME (subblock_node)) = 1;
	  if (TREE_ADDRESSABLE (subblock_node))
	    TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (subblock_node)) = 1;
	}

  {
    /* Pop the current level, and free the structure for reuse.  */
    struct binding_level *level = current_binding_level;
    current_binding_level = current_binding_level->level_chain;
    level->level_chain = free_binding_level;
    free_binding_level = level;
  }

  if (functionbody)
    {
      /* This is the top level block of a function. The ..._DECL chain stored
	 in BLOCK_VARS are the function's parameters (PARM_DECL nodes). Don't
	 leave them in the BLOCK because they are found in the FUNCTION_DECL
	 instead.  */
      DECL_INITIAL (current_function_decl) = block_node;
      BLOCK_VARS (block_node) = 0;
    }
  else if (block_node)
    {
      if (block_created_by_back_end == NULL)
	current_binding_level->blocks
	  = chainon (current_binding_level->blocks, block_node);
    }

  /* If we did not make a block for the level just exited, any blocks made for
     inner levels (since they cannot be recorded as subblocks in that level)
     must be carried forward so they will later become subblocks of something
     else.  */
  else if (subblock_chain)
    current_binding_level->blocks
      = chainon (current_binding_level->blocks, subblock_chain);
  if (block_node)
    TREE_USED (block_node) = 1;

  return block_node;
}

/* Insert BLOCK at the end of the list of subblocks of the
   current binding level.  This is used when a BIND_EXPR is expanded,
   to handle the BLOCK node inside the BIND_EXPR.  */

void
insert_block (block)
     tree block;
{
  TREE_USED (block) = 1;
  current_binding_level->blocks
    = chainon (current_binding_level->blocks, block);
}

/* Set the BLOCK node for the innermost scope
   (the one we are currently in).  */

void
set_block (block)
     tree block;
{
  current_binding_level->block_created_by_back_end = block;
}

/* Records a ..._DECL node DECL as belonging to the current lexical scope.
   Returns the ..._DECL node. */

tree
pushdecl (decl)
     tree decl;
{
  /* External objects aren't nested, other objects may be.  */
  if (DECL_EXTERNAL (decl))
    DECL_CONTEXT (decl) = 0;
  else
    DECL_CONTEXT (decl) = current_function_decl;

  /* Put the declaration on the list.  The list of declarations is in reverse
     order. The list will be reversed later if necessary.  This needs to be
     this way for compatibility with the back-end.

     Don't put TYPE_DECLs for UNCONSTRAINED_ARRAY_TYPE into the list.  They
     will cause trouble with the debugger and aren't needed anyway.  */
  if (TREE_CODE (decl) != TYPE_DECL
      || TREE_CODE (TREE_TYPE (decl)) != UNCONSTRAINED_ARRAY_TYPE)
    {
      TREE_CHAIN (decl) = current_binding_level->names;
      current_binding_level->names = decl;
    }

  /* For the declartion of a type, set its name if it either is not already
     set or is an implicit type name.  We'd rather have the type named with a
     real name and all the pointer types to the same object have the same
     POINTER_TYPE node.  Code in this function in c-decl.c makes a copy
     of the type node here, but that may cause us trouble with incomplete
     types, so let's not try it (at least for now).  */

  if (TREE_CODE (decl) == TYPE_DECL
      && (TYPE_NAME (TREE_TYPE (decl)) == 0
	  || 0 == strncmp (IDENTIFIER_POINTER (TYPE_NAME (TREE_TYPE (decl))),
			   "ityp__", 6)))
    TYPE_NAME (TREE_TYPE (decl)) = DECL_NAME (decl);

  return decl;
}

/* Create the predefined scalar types such as `integer_type_node' needed 
   in the gcc back-end and initialize the global binding level.  */

void
init_decl_processing ()
{
  tree endlink;

  /* The structure `tree_identifier' is the GCC tree data structure that holds
     IDENTIFIER_NODE nodes. We need to call `set_identifier_size' to tell GCC
     that we have not added any language specific fields to IDENTIFIER_NODE
     nodes.  */
  set_identifier_size (sizeof (struct tree_identifier));

  lineno = 0;

  /* incomplete_decl_finalize_hook is defined in toplev.c. It needs to be set
     by each front end to the appropriate routine that handles incomplete 
     VAR_DECL nodes. This routine will be invoked by compile_file when a  
     VAR_DECL node of DECL_SIZE zero is encountered.  */
  incomplete_decl_finalize_hook = finish_incomplete_decl;

  /* Make the binding_level structure for global names.  */
  current_function_decl = 0;
  current_binding_level = 0;
  free_binding_level = 0;
  pushlevel (0);
  global_binding_level = current_binding_level;

  /* In Ada, we use a signed type for SIZETYPE.  Use the signed type
     corresponding to the size of Pmode.  */
  sizetype = type_for_size (GET_MODE_BITSIZE (Pmode), 0);
  pushdecl (build_decl (TYPE_DECL, get_identifier (SIZE_TYPE), sizetype));

  integer_type_node = type_for_size (INT_TYPE_SIZE, 0) ;
  pushdecl (build_decl (TYPE_DECL, get_identifier ("int"), integer_type_node));
  unsigned_type_node = type_for_size (INT_TYPE_SIZE, 1);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned int"),
			unsigned_type_node));
  char_type_node = type_for_size (CHAR_TYPE_SIZE, 1);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned char"),
			char_type_node));

  error_mark_node = make_node (ERROR_MARK);
  TREE_TYPE (error_mark_node) = error_mark_node;

  integer_zero_node = build_int_2 (0, 0);
  integer_one_node = build_int_2 (1, 0);

  size_zero_node = build_int_2 (0, 0);
  TREE_TYPE (size_zero_node) = sizetype;
  size_one_node = build_int_2 (1, 0);
  TREE_TYPE (size_one_node) = sizetype;

  void_type_node = make_node (VOID_TYPE);
  layout_type (void_type_node);
  TYPE_ALIGN (void_type_node) = BITS_PER_UNIT;
  void_type_decl_node
    = pushdecl (build_decl (TYPE_DECL, get_identifier ("void"),
			    void_type_node));

  ptr_void_type_node = build_pointer_type (void_type_node);

  null_pointer_node = build_int_2 (0, 0);
  TREE_TYPE (null_pointer_node) = ptr_void_type_node;
  layout_type (TREE_TYPE (null_pointer_node));

  void_ftype = build_function_type (void_type_node, NULL_TREE);
  ptr_void_ftype = build_pointer_type (void_ftype);

  /* Now declare runtime functions. */
  endlink = tree_cons (NULL_TREE, void_type_node, NULL_TREE);

  /* malloc is a function declaration tree for a function to allocate
     memory.  */
  malloc_decl = create_subprog_decl ("malloc", NULL_PTR,
				     build_function_type (ptr_void_type_node,
							  tree_cons (NULL_TREE,
								     sizetype,
								     endlink)),
				     NULL_TREE, 0, 1, 1, 0);

  /* Make the types and global variables used for exception processing.  
     We assume here that a jmp_buf is no more than 100 size_t entries.  */
  jmpbuf_type
    = build_array_type (sizetype, build_index_type (build_int_2 (100, 0)));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("jmpbuf_t"), jmpbuf_type));
  jmpbuf_ptr_type = build_pointer_type (jmpbuf_type);

  /* __gnat_jmpbuf is a pointer to the current jmpbuf.  */
  jmpbuf_decl = create_var_decl ("__gnat_jmpbuf", NULL_PTR, jmpbuf_ptr_type,
				 NULL_TREE, 0, 1, 0, 0, 0);

  /* An exception is a single byte.  __gnat_exception points to the current
     exception.  */
  excptr_decl = create_var_decl ("__gnat_exception", NULL_PTR,
				 build_pointer_type (char_type_node),
				 NULL_TREE, 0, 1, 0, 0, 0);

  /* longjmp is a function that returns void.  Its first operand is a
     pointer to a jmpbuf and its second operand is an integer.  */
  longjmp_decl
    = create_subprog_decl
      /* longjmp is called by catch_except, unless for some exceptions
      ("longjmp", NULL_PTR,   */
      ("catch_except", NULL_PTR,
       build_function_type (void_type_node,
			    tree_cons (NULL_TREE,  jmpbuf_ptr_type,
				       tree_cons (NULL_TREE, integer_type_node,
						  endlink))),
       NULL_TREE, 0, 1, 1, 0);
  build_pointer_type (longjmp_decl);

  /* Indicate that longjmp never returns.  */
  TREE_THIS_VOLATILE (longjmp_decl) = 1;
  TREE_SIDE_EFFECTS (longjmp_decl) = 1;

  /* setjmp returns an integer and has one operand, which is a pointer to
     a jmpbuf.  */
  setjmp_decl
    = create_subprog_decl
      ("setjmp", NULL_PTR,
       build_function_type (integer_type_node,
			    tree_cons (NULL_TREE,  jmpbuf_ptr_type, endlink)),
       NULL_TREE, 0, 1, 1, 0);

  /* __gnat_raise_constraint takes no operands and never returns.  */
  raise_constraint_decl
    = create_subprog_decl
      ("__gnat_raise_constraint", NULL_PTR,
       build_function_type (void_type_node, endlink),
       NULL_TREE, 0, 1, 1, 0);

  TREE_THIS_VOLATILE (raise_constraint_decl) = 1;
  TREE_SIDE_EFFECTS (raise_constraint_decl) = 1;

  /* Make a UNION_TYPE that will be copied to form a union to be used to
     do an unchecked conversion.  */
  unchecked_union_node = make_node (UNION_TYPE);
}

/* This routine is called in tree.c to print an error message for invalid use
   of an incomplete type.  */

void
incomplete_type_error (dont_care_1, dont_care_2)
     tree dont_care_1, dont_care_2;
{
  abort ();
}

/* This function is called indirectly from toplev.c to handle incomplete 
   declarations, i.e. VAR_DECL nodes whose DECL_SIZE is zero.  To be precise,
   compile_file in toplev.c makes an indirect call through the function pointer
   incomplete_decl_finalize_hook which is initialized to this routine in
   init_decl_processing.  */

void
finish_incomplete_decl (dont_care)
     tree dont_care;
{
  abort ();
}

/* Given a record type (RECORD_TYPE) and a chain of FIELD_DECL
   nodes (FIELDLIST), finish constructing the record or union type.  */

void
finish_record_type (record_type, fieldlist)
     tree record_type;
     tree fieldlist;
{
  TYPE_FIELDS (record_type) = fieldlist;
  TYPE_STUB_DECL (record_type)
    = pushdecl (build_decl (TYPE_DECL, NULL_TREE, record_type));
  layout_type (record_type);
  rest_of_type_compilation (record_type, global_bindings_p ());
}

/* Return a FUNCTION_TYPE node. RETURN_TYPE is the type returned by the
   subprogram. If it is void_type_node, then we are dealing with a procedure,
   otherwise we are dealing with a function. PARAM_DECL_LIST is a list of
   PARM_DECL nodes that are the subprogram arguments.  CICO_LIST is the
   copy-in/copy-out list to be stored into TYPE_CICO_LIST.  */

tree
create_subprog_type (return_type, param_decl_list, cico_list)
     tree return_type;
     tree param_decl_list;
     tree cico_list;
{
  /* A chain of TREE_LIST nodes whose TREE_VALUEs are the data type nodes of
     the subprogram formal parameters. This list is generated by traversing th
     input list of PARM_DECL nodes.  */
  tree param_type_list = NULL;
  tree param_decl;
  tree type;

  for (param_decl = param_decl_list; param_decl;
       param_decl = TREE_CHAIN (param_decl))
    param_type_list = tree_cons (NULL_TREE, TREE_TYPE (param_decl),
				 param_type_list);

  /* The list of the function parameter types has to be terminated by the void
     type to signal to the back-end that we are not dealing with a variable
     parameter subprogram, but that the subprogram has a fixed number of
     parameters.  */
  param_type_list = tree_cons (NULL_TREE, void_type_node, param_type_list);

  /* The list of argument types has been created in reverse
     so nreverse it.   */
  param_type_list = nreverse (param_type_list);

  type = build_function_type (return_type, param_type_list);

  /* TYPE may have been shared since GCC hashes types.  If it has a CICO_LIST
     or the new type should, make a copy of TYPE.  */
  if (TYPE_CI_CO_LIST (type) != 0 || cico_list != 0)
    {
      push_obstacks (TYPE_OBSTACK (type), TYPE_OBSTACK (type));
      type = copy_node (type);
      pop_obstacks ();
    }

  TYPE_CI_CO_LIST (type) = cico_list;
  return type;
}

/* Return a TYPE_DECL node. TYPE_NAME gives the name of the type (a character
   string) and TYPE is a ..._TYPE node giving its data type.  */

tree
create_type_decl (type_name, type)
     char *type_name;
     tree type;
{
  tree id_node   = type_name ? get_identifier (type_name): NULL_TREE;
  tree type_decl = build_decl (TYPE_DECL, id_node, type);

  /* Add this decl to the current binding level.  */
  type_decl = pushdecl (type_decl);

  /* Pass type declaration information to the debugger unless this is an
     UNCONSTRAINED_ARRAY_TYPE, which the debugger does not support.  */
  if (TREE_CODE (type) != UNCONSTRAINED_ARRAY_TYPE)
    rest_of_decl_compilation (type_decl, NULL, global_bindings_p (), 0);

  return type_decl;
}

/* This listhead is used to record any global objects that need elaboration.
   TREE_PURPOSE is the variable to be elaborated and TREE_VALUE is the
   initial value to assign.  */

static tree pending_elaborations;

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

tree
create_var_decl (var_name, asm_name, type, var_init, const_flag, public_flag,
		 extern_flag, static_flag, volatile_flag)
     char *var_name;
     char *asm_name;
     tree type;
     tree var_init;
     int const_flag;
     int public_flag;
     int extern_flag;
     int static_flag;
     int volatile_flag;
{
  tree id_node  = get_identifier (var_name);
  tree var_decl
    = build_decl (const_flag && var_init && TREE_CONSTANT (var_init)
		  ? CONST_DECL : VAR_DECL, id_node, type);

  /* If this is external, throw away any initializations unless this is a
     CONST_DECL (meaning we have a constant); they will be done elsewhere.  If
     we are defining a global here, leave a constant initialization and save
     any variable elaborations for the elaboration routine.  */

  if (extern_flag && TREE_CODE (var_decl) != CONST_DECL)
    var_init = 0;

  if (global_bindings_p () && var_init != 0 && ! TREE_CONSTANT (var_init))
    {
      add_pending_elaborations (var_decl, var_init);
      var_init = 0;
    }

  DECL_INITIAL  (var_decl) = var_init;
  TREE_READONLY (var_decl) = const_flag;
  DECL_EXTERNAL (var_decl) = extern_flag;
  TREE_PUBLIC   (var_decl) = public_flag || extern_flag;
  TREE_STATIC   (var_decl)
    = (global_bindings_p () ? !extern_flag : static_flag || volatile_flag);
  TREE_CONSTANT (var_decl) = TREE_CODE (var_decl) == CONST_DECL;
  TREE_THIS_VOLATILE (var_decl) = TREE_SIDE_EFFECTS (var_decl) = volatile_flag;

  /* At the global binding level we need to allocate static storage for the
     variable if and only if its not external. If we are not at the top level
     we always allocate automatic storage. */
  if (asm_name)
    DECL_ASSEMBLER_NAME (var_decl) = get_identifier (asm_name);

  /* If the type is a RECORD_TYPE and its size depends on a discriminant,
     the size to be used for the object is the maximum possible size.  */
  if (TREE_CODE (type) == RECORD_TYPE && ! TREE_CONSTANT (TYPE_SIZE (type))
      && contains_placeholder_p (TYPE_SIZE (type)))
    DECL_SIZE (var_decl) = max_size (TYPE_SIZE (type), 1);

  /* Add this decl to the current binding level and generate any
     needed code and RTL. */
  var_decl = pushdecl (var_decl);
  expand_decl (var_decl);
  expand_decl_init (var_decl);
  if (TREE_CODE (var_decl) != CONST_DECL)
    rest_of_decl_compilation (var_decl, 0, global_bindings_p (), 0);

  return var_decl;
}

/* Returns a FIELD_DECL node. FIELD_NAME the field name, FIELD_TYPE is its
   type, and RECORD_TYPE is the type of the parent.  PACKED is nonzero if
   this field is in a record type with a "pragma pack".  */

tree
create_field_decl (field_name, field_type, record_type, packed)
     char *field_name;
     tree field_type;
     tree record_type;
     int packed;
{
  tree field_id   = field_name ? get_identifier (field_name): NULL_TREE;
  tree field_decl = build_decl (FIELD_DECL, field_id, field_type);

  DECL_CONTEXT (field_decl) = record_type;

  /* If this is to be packed and it is an integral type make a bit field whose
     width is the precision of the type.  */
  if (packed && INTEGRAL_TYPE_P (field_type)
      && TREE_CODE (TYPE_SIZE (field_type)) == INTEGER_CST)
    {
      DECL_BIT_FIELD (field_decl) = 1;
      DECL_PACKED (field_decl) = 1;
      DECL_FIELD_SIZE (field_decl) = TYPE_PRECISION (field_type);
    }
  else
    DECL_ALIGN (field_decl)
      = MAX (DECL_ALIGN (field_decl),
	     packed ? BITS_PER_UNIT : TYPE_ALIGN (TREE_TYPE (field_decl)));

  /* If the type is a RECORD_TYPE and its size depends on a discriminant,
     the size to be used for the object is the maximum possible size.  */
  if (TREE_CODE (field_type) == RECORD_TYPE
      && ! TREE_CONSTANT (TYPE_SIZE (field_type))
      && contains_placeholder_except_p (TYPE_SIZE (field_type), record_type))
    DECL_SIZE (field_decl) = max_size (TYPE_SIZE (field_type), 1);

  return field_decl;
}

/* Returns a PARM_DECL node. PARAM_NAME is the name of the parameter,
   PARAM_TYPE is its type.  */

tree
create_param_decl (param_name, param_type)
     char *param_name;
     tree param_type;
{
  tree param_id   = get_identifier (param_name);
  tree param_decl = build_decl (PARM_DECL, param_id, param_type);

  DECL_ARG_TYPE (param_decl) = param_type;
  return param_decl;
}

/* Add some pending elaborations on the list .  */

void 
add_pending_elaborations (var_decl, var_init)
     tree var_decl;
     tree var_init;
{
  pending_elaborations = tree_cons (var_decl, var_init, pending_elaborations);
}

/* Obtain any pending elaborations and clear the old list.  */

tree
get_pending_elaborations ()
{
  /* Each thing added to the list went on the end; we want it on the
     beginning.  */
  tree result = nreverse (pending_elaborations);

  pending_elaborations = 0;
  return result;
}

/* Returns a LABEL_DECL node for LABEL_NAME.  */

tree
create_label_decl (label_name)
     char *label_name;
{
  tree label_id   = get_identifier (label_name);
  tree label_decl = build_decl (LABEL_DECL, label_id, void_type_node);

  DECL_CONTEXT (label_decl)     = current_function_decl;
  DECL_MODE (label_decl)        = VOIDmode;
  DECL_SOURCE_LINE (label_decl) = lineno;
  DECL_SOURCE_FILE (label_decl) = input_filename;

  return label_decl;
}

/* Returns a FUNCTION_DECL node.  SUBPROG_NAME is the name of the subprogram,
   ASM_NAME is its assembler name, SUBPROG_TYPE is its type (a FUNCTION_TYPE
   node), PARAM_DECL_LIST is the list of the subprogram arguments (a list of
   PARM_DECL nodes chained through the TREE_CHAIN field).

   INLINE_FLAG, PUBLIC_FLAG, EXTERN_FLAG, and PURE_FLAG are used to set the
   appropriate fields in the FUNCTION_DECL.  */

tree
create_subprog_decl (subprog_name, asm_name, subprog_type, param_decl_list,
		     inline_flag, public_flag, extern_flag, pure_flag)
     char *subprog_name;
     char *asm_name;
     tree subprog_type;
     tree param_decl_list;
     int inline_flag;
     int public_flag;
     int extern_flag;
     int pure_flag;
{
  tree subprog_id   = get_identifier (subprog_name);
  tree return_type  = TREE_TYPE (subprog_type);
  tree subprog_decl = build_decl (FUNCTION_DECL, subprog_id, subprog_type);

  DECL_EXTERNAL (subprog_decl) = extern_flag;
  TREE_PUBLIC (subprog_decl)   = public_flag;
  DECL_INLINE (subprog_decl)   = inline_flag;
  TREE_READONLY (subprog_decl) = pure_flag;
  DECL_ARGUMENTS (subprog_decl) = param_decl_list;
  DECL_RESULT (subprog_decl)    = build_decl (RESULT_DECL, 0, return_type);

  if (asm_name)
    DECL_ASSEMBLER_NAME (subprog_decl) = get_identifier (asm_name);

  /* Add this decl to the current binding level.  */
  subprog_decl = pushdecl (subprog_decl);

  /* Output the assembler code and/or RTL for the declaration.  */
  rest_of_decl_compilation (subprog_decl, 0, global_bindings_p (), 0);

  return subprog_decl;
}

/* Count how deep we are into nested functions.  This is because
   we shouldn't call the backend function context routines unless we
   are in a nested function.  */

static int function_nesting_depth;

/* Set up the framework for generating code for SUBPROG_DECL, a subprogram
   body. This routine needs to be invoked before processing the declarations
   appearing in the subprogram.  */

void
begin_subprog_body (subprog_decl)
     tree subprog_decl;
{
  tree param_decl_list;
  tree param_decl;
  tree next_param;

  if (function_nesting_depth++ != 0)
    push_function_context ();

  announce_function (subprog_decl);

  /* Make this field nonzero so further routines know that this is not
     tentative. error_mark_node is replaced below (in poplevel) with the
     adequate BLOCK.  */
  DECL_INITIAL (subprog_decl)  = error_mark_node;

  /* This is a definition, not a reference. So clear DECL_EXTERNAL. */
  DECL_EXTERNAL (subprog_decl) = 0;

  /* This function exists in static storage. This does not mean `static' in
     the C sense!  */
  TREE_STATIC (subprog_decl)   = 1;

  /* Enter a new binding level.  */
  temporary_allocation ();
  current_function_decl = subprog_decl;
  pushlevel (0);

  make_function_rtl (subprog_decl);

  /* Push all the PARM_DECL nodes onto the current scope (i.e. the scope of the
     subprogram body) so that they can be recognized as local variables in the
     subprogram. 

     The list of PARM_DECL nodes is stored in the right order in
     DECL_ARGUMENTS.  Since ..._DECL nodes get stored in the reverse order in
     which they are transmitted to `pushdecl' we need to reverse the list of
     PARM_DECLs if we want it to be stored in the right order. The reason why
     we want to make sure the PARM_DECLs are stored in the correct order is
     that this list will be retrieved in a few lines with a call to `getdecl'
     to store it back into the DECL_ARGUMENTS field.  */
    param_decl_list = nreverse (DECL_ARGUMENTS (subprog_decl));

    for (param_decl = param_decl_list; param_decl; param_decl = next_param)
      {
	next_param = TREE_CHAIN (param_decl);
	TREE_CHAIN (param_decl) = NULL;
	pushdecl (param_decl);
      }

  /* Store back the PARM_DECL nodes. They appear in the right order. */
  DECL_ARGUMENTS (subprog_decl) = getdecls ();

  init_function_start   (subprog_decl, input_filename, lineno);
  expand_function_start (subprog_decl, 0);
}


/* Finish the definition of the current subprogram and compile it all the way
   to assembler language output.  */

void
end_subprog_body (void)
{
  poplevel (1, 0, 1);

  /* Mark the RESULT_DECL as being in this subprogram. */
  DECL_CONTEXT (DECL_RESULT (current_function_decl)) = current_function_decl;

  expand_function_end (input_filename, lineno, 0);
  rest_of_compilation (current_function_decl);

  /* If we are not at the bottom of the function nesting stack, pop up to
     the containing function.  Otherwise show we aren't in any function
     and switch back to permanent allocation.  */
  if (--function_nesting_depth != 0)
    pop_function_context ();
  else
    {
      current_function_decl = 0;
      permanent_allocation ();
    }
}

/* Return 1 if EXP contains a PLACEHOLDER_EXPR for any type other than T;
   i.e., if it represents a size or offset that depends on a field within a
   record other than the RECORD_TYPE denoted by T.  If T is zero,
   return 1 for any PLACEHOLDER_EXPR.

   Note that we only allow such expressions within simple arithmetic
   or a COND_EXPR.  */

static int
contains_placeholder_except_p (exp, t)
     tree exp;
     tree t;
{
  register enum tree_code code = TREE_CODE (exp);
  tree inner;

  /* If we have a WITH_RECORD_EXPR, it "cancels" any PLACEHOLDER_EXPR
     in it since it is supplying a value for it.  */
  if (code == WITH_RECORD_EXPR)
    return 0;

  switch (TREE_CODE_CLASS (code))
    {
    case 'r':
      for (inner = TREE_OPERAND (exp, 0);
	   TREE_CODE_CLASS (TREE_CODE (inner)) == 'r';
	   inner = TREE_OPERAND (inner, 0))
	;
      return (TREE_CODE (inner) == PLACEHOLDER_EXPR
	      && TREE_TYPE (inner) != t);

    case '1':
    case '2':  case '<':
    case 'e':
      switch (tree_code_length[(int) code])
	{
	case 1:
	  return contains_placeholder_except_p (TREE_OPERAND (exp, 0), t);
	case 2:
	  return (code != RTL_EXPR
		  && code != CONSTRUCTOR
		  && ! (code == SAVE_EXPR && SAVE_EXPR_RTL (exp) != 0)
		  && code != WITH_RECORD_EXPR
		  && (contains_placeholder_except_p (TREE_OPERAND (exp, 0), t)
		      || contains_placeholder_except_p (TREE_OPERAND (exp, 1),
							t)));
	case 3:
	  return
	    (code == COND_EXPR
	     && (contains_placeholder_except_p (TREE_OPERAND (exp, 0), t)
		 || contains_placeholder_except_p (TREE_OPERAND (exp, 1), t)
		 || contains_placeholder_except_p (TREE_OPERAND (exp, 2), t)));
	}
    }

  return 0;
}

#ifndef MAX_BITS_PER_WORD
#define MAX_BITS_PER_WORD  BITS_PER_WORD
#endif

/* This variable keeps a table for types for each precision so that we only 
   allocate each of them once. Signed and unsigned types are kept separate.

   Note that these types are only used when fold-const requests something
   special.  Perhaps we should NOT share these types; we'll see how it
   goes later.  */
static tree signed_and_unsigned_types[MAX_BITS_PER_WORD + 1][2];

/* Return an integer type with the number of bits of precision given by  
   PRECISION.  UNSIGNEDP is nonzero if the type is unsigned; otherwise
   it is a signed type.  */

tree
type_for_size (precision, unsignedp)
     unsigned precision;
     int unsignedp;
{
  tree t;
  int moment;

  if (precision <= MAX_BITS_PER_WORD
      && signed_and_unsigned_types[precision][unsignedp] != 0)
    return signed_and_unsigned_types[precision][unsignedp];

  /* Since we will keep these types around, they must be permanent.  */
  moment = suspend_momentary ();
  push_obstacks_nochange ();
  end_temporary_allocation ();

 if (unsignedp)
    t = signed_and_unsigned_types[precision][1]
      = make_unsigned_type (precision);
  else
    t = signed_and_unsigned_types[precision][0]
      = make_signed_type (precision);

  pop_obstacks ();
  resume_momentary (moment);

  return t;
}

/* Return a data type that has machine mode MODE.  UNSIGNEDP selects
   an unsigned type; otherwise a signed type is returned.  */

tree
type_for_mode (mode, unsignedp)
     enum machine_mode mode;
     int unsignedp;
{
  return type_for_size (GET_MODE_BITSIZE (mode), unsignedp);
}

/* Return the unsigned version of a TYPE_NODE, a scalar type.  */

tree
unsigned_type (type_node)
     tree type_node;
{
  return type_for_size (TYPE_PRECISION (type_node), 1);
}

/* Return the signed version of a TYPE_NODE, a scalar type.  */

tree
signed_type (type_node)
     tree type_node;
{
  return type_for_size (TYPE_PRECISION (type_node), 0);
}

/* Return a type the same as TYPE except unsigned or signed according to
   UNSIGNEDP.  */

tree
signed_or_unsigned_type (unsignedp, type)
     int unsignedp;
     tree type;
{
  if (! INTEGRAL_TYPE_P (type) || TREE_UNSIGNED (type) == unsignedp)
    return type;
  else
    return type_for_size (TYPE_PRECISION (type), unsignedp);
}

/* EXP is an expression for the size of an object.  If this size contains
   discriminant references, replace them with the maximum (if MAX_P) or
   minimum (if ! MAX_P) possible value of the discriminant.  */

tree
max_size (exp, max_p)
     tree exp;
     int max_p;
{
  enum tree_code code = TREE_CODE (exp);
  tree type = TREE_TYPE (exp);

  switch (TREE_CODE_CLASS (code))
    {
    case 'd':
    case 'c':
      return exp;

    case 'r':
      /* If this contains a PLACEHOLDER_EXPR, it is the thing we want to
	 modify.  Otherwise, we abort since it is something we can't
	 handle.  */
      if (! contains_placeholder_p (exp))
	abort ();

      type = TREE_TYPE (TREE_OPERAND (exp, 1));
      return
	max_size (max_p ? TYPE_MAX_VALUE (type) : TYPE_MIN_VALUE (type), 1);

    case '1':
    case '2':
    case '<':
    case 'e':
      switch (tree_code_length[(int) code])
	{
	case 1:
	  return
	    fold (build1 (code, type,
			  max_size (TREE_OPERAND (exp, 0),
				    code == NEGATE_EXPR ? ! max_p : max_p)));

	case 2:
	  if (code == RTL_EXPR)
	    abort ();

	  return
	    fold (build (code, type,
			 max_size (TREE_OPERAND (exp, 0), max_p),
			 max_size (TREE_OPERAND (exp, 1),
				   code == MINUS_EXPR ? ! max_p : max_p)));

	case 3:
	  if (code == SAVE_EXPR)
	    return exp;
	  else if (code == COND_EXPR)
	    return fold (build (MAX_EXPR, type,
				max_size (TREE_OPERAND (exp, 1), max_p),
				max_size (TREE_OPERAND (exp, 2), max_p)));
	}
    }

  abort ();
}

/* Convert a pointer to a constrained array into a pointer to an unconstrained
   array.  This involves making a template.  If ALLOCATE is nonzero, the
   template must be created using an allocator instead of in the
   current context's memory.  */

tree
convert_to_unconstrained (type, expr, allocate)
     tree type;
     tree expr;
     int allocate;
{
  tree template_elts = NULL_TREE;
  tree template_type = TREE_TYPE (TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (type))));
  tree template_cons, template_addr;
  tree field;
  tree array_type;

  /* If EXPR is a constant of zero, we make a fat pointer that has a null
     pointer to the template and array.  */
  if (integer_zerop (expr))
    return
      build_constructor
	(type,
	 tree_cons (TYPE_FIELDS (type),
		    convert (TREE_TYPE (TYPE_FIELDS (type)), expr),
		    tree_cons (TREE_CHAIN (TYPE_FIELDS (type)),
			       convert (build_pointer_type (template_type),
					expr),
			       NULL_TREE)));

  /* First make a the list for a CONSTRUCTOR for the template.   Do down the
     field list of the template instead of the type chain because this
     array might be an Ada array of arrays and we can't tell where the
     nested arrays stop being the underlying object.  */
  for (array_type = TREE_TYPE (TREE_TYPE (expr)),
       field = TYPE_FIELDS (template_type);
       field;
       array_type = TREE_TYPE (array_type),
       field = TREE_CHAIN (TREE_CHAIN (field)))
    {
      tree min = TYPE_MIN_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (array_type)));
      tree max = TYPE_MAX_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (array_type)));

      /* If either MIN or MAX involve a PLACEHOLDER_EXPR, we must
	 surround them with a WITH_RECORD_EXPR giving EXPR as the
	 OBJECT.  */
      if (! TREE_CONSTANT (min) && contains_placeholder_p (min))
	min = build (WITH_RECORD_EXPR, TREE_TYPE (min), min, expr);
      if (! TREE_CONSTANT (max) && contains_placeholder_p (max))
	max = build (WITH_RECORD_EXPR, TREE_TYPE (max), max, expr);

      template_elts = tree_cons (field, min,
				 tree_cons (TREE_CHAIN (field), max,
					    template_elts));
    }

  template_cons = build_constructor (template_type, template_elts);

  /* If the template needs to be allocated the heap, build a constructor
     for it.  Otherwise, just take the address of the constructor.  */
  if (allocate)
    template_addr = build_allocator (template_type, template_cons,
				     0, build_pointer_type (template_type));
  else
    template_addr = build_unary_op (ADDR_EXPR, NULL_TREE, template_cons);

  /* The result is a CONSTRUCTOR for the fat pointer.  */
  return
    build_constructor (type,
		       tree_cons (TYPE_FIELDS (type), expr,
				  tree_cons (TREE_CHAIN (TYPE_FIELDS (type)),
					     template_addr, NULL_TREE)));
}

/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.  */

tree
convert (type, expr)
     tree type, expr;
{
  enum tree_code code = TREE_CODE (type);

  /* If EXPR is already the right type, we are done.  */
  if (type == TREE_TYPE (expr))
    return expr;

  /* There are some special cases of expressions that we process
     specially.  */
  switch (TREE_CODE (expr))
    {
    case ERROR_MARK:
      return expr;

    case TRANSFORM_EXPR:
      /* Just set its type here.  We will do the actual conversion in
	 gnat_expand_expr.  */
      TREE_TYPE (expr) = type;
      return expr;

    case STRING_CST:
      /* If we are converting a STRING_CST to another constrained array type,
	 just make a new one in the proper type.  */
      if (TREE_CODE (type) != ARRAY_TYPE)
	break;

      expr = copy_node (expr);
      TREE_TYPE (expr) = type;
      return expr;

    case UNCONSTRAINED_ARRAY_REF:
      /* Convert this to the type of the inner array by getting the address of
	 the array from the template.  */
      expr = build_unary_op (INDIRECT_REF, NULL_TREE,
			     build_component_ref (TREE_OPERAND (expr, 0),
						  get_identifier ("p_array"),
						  NULL_TREE));
      break;
    }

  /* Check for converting a pointer to a constrained array into a pointer to
     a constrained array.  */
  if (TYPE_FAT_POINTER_P (type)
      && (integer_zerop (expr)
	  || (TREE_CODE (TREE_TYPE (expr)) == POINTER_TYPE
	      && TREE_CODE (TREE_TYPE (TREE_TYPE (expr))) == ARRAY_TYPE)))
    return convert_to_unconstrained (type, expr, 0);

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (expr)))
    return fold (build1 (NOP_EXPR, type, expr));

  switch (code)
    {
    case VOID_TYPE:
      return build1 (CONVERT_EXPR, type, expr);

    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
      return fold (convert_to_integer (type, expr));

    case POINTER_TYPE:
      return fold (convert_to_pointer (type, expr));

    case REAL_TYPE:
      return fold (convert_to_real (type, expr));

    case ARRAY_TYPE:
    case RECORD_TYPE:
      /* In these cases, assume the front-end has validated the conversion.
	 If the conversion is valid, it will bit a bit-wise conversion, so
	 it can be viewed as an unchecked conversion.  */
      return build1 (UNCHECKED_CONVERT_EXPR, type, expr);

    case UNCONSTRAINED_ARRAY_TYPE:
      /* In this case EXPR must be a constrained array.  In that case,
	 take its address, convert it to a fat pointer, and then
	 dereference it.  */
      if (TREE_CODE (TREE_TYPE (expr)) != ARRAY_TYPE)
	abort ();

      return
	build_unary_op
	  (INDIRECT_REF, NULL_TREE,
	   convert_to_unconstrained (TREE_TYPE (type),
				     build_unary_op (ADDR_EXPR,
						     NULL_TREE, expr),
				     0));
    default:
      abort ();
    }
}
