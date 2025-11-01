/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                               MISCELLANEOUS                              */
/*                                                                          */
/*                                  Body                                    */
/*                                                                          */
/*                             $Revision: 1.40 $                            */
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
#include <stdio.h>
#include <string.h>
#include "tree.h"
#include "rtl.h"
#include "expr.h"
#include "a-ada.h"
#include "a-types.h"
#include "a-atree.h"
#include "a-sinfo.h"
#include "a-einfo.h"
#include "a-namet.h"
#include "a-string.h"
#include "a-uint.h"
#include "a-trans.h"
#include "a-trans3.h"
#include "a-trans4.h"
#include "a-misc.h"
#include "a-rtree.h"
#include "flags.h"

extern char *xmalloc ();
extern char *main_input_filename;

/* Tables describing GCC tree codes used only by GNAT.  

   Table indexed by tree code giving a string containing a character
   classifying the tree code.  Possibilities are
   t, d, s, c, r, <, 1 and 2.  See cp-tree.def for details.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

char *gnat_tree_code_type[] = {
  "x",
#include "a-tree.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

int gnat_tree_code_length[] = {
  0,
#include "a-tree.def"
};
#undef DEFTREECODE

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

char *gnat_tree_code_name[] = {
  "@@dummy",
#include "a-tree.def"
};
#undef DEFTREECODE

/* some functions to interface with the front end and pass parameters and
   switches */

static int front_argc = 1;
static char *front_argv[100];

/* predefined exceptions */ 
char constraint_error; 
char numeric_error; 
char program_error; 
char storage_error; 
char tasking_error; 

/* Variables for exception processing.  */

char * __gnat_exception;
size_t *__gnat_jmpbuf;

int
arg_count (void)
{
  return front_argc;
}

int
len_arg (arg_num)
     int arg_num;
{
  if (arg_num < front_argc - 1)
    return strlen (front_argv[arg_num]);
  else
    return strlen (input_filename);
}

int
fill_arg (a, i)
     char * a;
     int i;
{
  if (i < front_argc - 1)
    strncpy (a, front_argv[i], strlen(front_argv[i]));
  else
    strncpy (a, input_filename, strlen (input_filename));

}
/* This function is called by the exception mechanism. It calls abort if there 
   is no handler for the exception so that the debugger can get control with a
   stackframe that is intact */

void 
catch_except (ptr, i)
     void *ptr;
     int i;

{
  extern char debug__get_debug_flag_k (); 

  /*  extern Boolean errout__errors_detected;
      extern errout__finalize_error_output ();
      */

  /*  if an exception handler is active and the -dk debug switch is not set, 
      then just branch to the exception handler */

  if (ptr &&  !debug__get_debug_flag_k ())
    longjmp (ptr, i);

  /*  if no exception handler, or if -dk switch set, and this is a predefined 
      exception, then give appropriate message and abort */

  else 
    {
      if (__gnat_exception == &constraint_error) 
	printf ("\nraised Constraint_Error\n"); 
      else if (__gnat_exception == &numeric_error) 
	printf ("\nraised Numeric_Error\n"); 
      else if (__gnat_exception == &program_error) 
	printf ("\nraised Program_Error\n"); 
      else if (__gnat_exception == &storage_error)
	printf ("\nraised Storage_Error\n");
      else if (__gnat_exception == &tasking_error)
	printf ("\nraised Tasking_Error\n");

  /*  if not predefined exception, then go handle it if there is an exception 
      handler active, else print message and abort */

      else if (!ptr)
	printf ("\nraised unhandled exception\n");
      else
	longjmp (ptr, i);

  /*  here is the abort call for the case of a standard exception with the -dk
      debug flag set, or for any exception if no exception handler is active */

      abort ();
    }
}

/* Raise constraint error.  */

void
__gnat_raise_constraint ()
{
  __gnat_exception = &constraint_error;

  catch_except (__gnat_jmpbuf, 1);
}

/* Root node of the tree read in.  Used only by yyparse.  */
gnat_tree gnat_root;

/* Global Variables Expected by gcc: */

char *language_string = "GNU Ada";
int current_function_returns_null;
int flag_traditional;		/* Used by dwarfout.c.  */

/* Routines Expected by gcc:  */

/* For most front-ends, this is the parser for the language.  For us, we
   process the GNAT tree.  */

int
yyparse ()
{
  /* Make up what Gigi uses as a jmpbuf.  */
  size_t jmpbuf[100];

  /* Set up to catch unhandled exceptions.  */
  if (setjmp (jmpbuf))
    abort ();

  __gnat_jmpbuf = jmpbuf;

  /* let's call the front-end elaboration procedures */
  ada__bind ();
  /* call the front end */
  _ada_gnat1drv ();

  return 0;
}

/* Decode all the language specific options that cannot be decoded by GCC. The
   option decoding phase of GCC calls this routine on the flags that it cannot
   decode. This routine returns 1 if it is successful, otherwise it
   returns 0. */

int
lang_decode_option (p)
     char *p;
{
  if (strncasecmp (p, "-gnat", 5))
    /* we assume for the moment that all front end options are passed
       with "gnat" prefix */
    return 0;
  else
    {
      front_argv[front_argc] =  (char *) malloc (strlen (p) - 4);
      front_argv[front_argc][0] = '-';
      strcpy (front_argv[front_argc] + 1, p + 5);
      front_argc ++;
      return 1;
    }
}

/* Perform all the initialization steps that are language-specific.  */

void
lang_init ()
{
  extern char **save_argv;
  /* init variables to simulate command line args for gnat */
  front_argv [0] = save_argv[0]; /* take the exec name from gcc (toplev) */
  front_argc ++;  /* the last argument is the filename */   

  main_input_filename = input_filename;

}

/* Perform all the finalization steps that are language-specific.  */

void
lang_finish ()
{}

/* Return a short string identifying this language to the debugger.  */

char *
lang_identify ()
{
  return "ada";
}

/* If DECL has a cleanup, build and return that cleanup here.
   This is a callback called by expand_expr.  */

tree
maybe_build_cleanup (decl)
     tree decl;
{
  /* There are no cleanups in C.  */
  return NULL_TREE;
}

/* Print any language-specific compilation statistics.  */

void
print_lang_statistics ()
{}

/* Hooks for print-tree.c:  */

void
print_lang_decl (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{}

void
print_lang_type (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
  /* Print the two fields we added to the tree.  */
  if (TREE_CODE (node) == INTEGER_TYPE && TYPE_INDEX_TYPE (node))
    print_node (file, "index type", TYPE_INDEX_TYPE (node), indent + 4);
  else if (TREE_CODE (node) == FUNCTION_TYPE && TYPE_CI_CO_LIST (node))
    print_node (file, "ci_co_list", TYPE_CI_CO_LIST (node), indent + 4);
}


void
print_lang_identifier (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{}

/* Expands GNAT-specific GCC tree nodes.  The only ones we support here are
   TRANSFORM_EXPR and UNCHECKED_CONVERT_EXPR.  */

static rtx
gnat_expand_expr (exp, target, tmode, modifier)
     tree exp;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
{
  tree type = TREE_TYPE (exp);

  /* Update EXP to be the new expression to expand.  */

  switch (TREE_CODE (exp))
    {
    case TRANSFORM_EXPR:
      /* If we will ignore our result, just generate code.  Otherwise,
	 expand it.  */
      if (target == const0_rtx || TREE_CODE (type) == VOID_TYPE)
	{
	  gnat_to_code (TREE_COMPLEXITY (exp));
	  return target;
	}

      exp = gnat_to_gnu (TREE_COMPLEXITY (exp));

      /* If convert was called on this TRANSFORM_EXPR, it will now have a type,
	 so we must do the conversion now.  */
      if (type != error_mark_node)
	exp = convert (type, exp);
      break;

    case UNCHECKED_CONVERT_EXPR:
      /* If the input and output are both the same mode (usually BLKmode),
	 just return the expanded input since we want just the bits.  */
      if (TYPE_MODE (type) == TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))))
	exp = TREE_OPERAND (exp, 0);

      /* If either mode is BLKmode, memory will be involved, so do this
	 via pointer punning.  */
      else if (TYPE_MODE (type) == BLKmode
	       || TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))) == BLKmode)
	exp
	  = build_unary_op (INDIRECT_REF, NULL_TREE,
			    convert (build_pointer_type (type),
				     build_unary_op (ADDR_EXPR, NULL_TREE,
						     TREE_OPERAND (exp, 0))));

      /* Otherwise make a union of the two types, convert to the union, and
	 extract the other value.  */
      else
	{
	  /* Note that copy_node puts objects in current_obstack and we
	     take advantage of that here since we want these objects to
	     all be in the momentary obstack.  */
	  tree in_type = TREE_TYPE (TREE_OPERAND (exp, 0));
	  tree union_type = copy_node (unchecked_union_node);
	  tree in_field = create_field_decl ("in", in_type, union_type, 0);
	  tree out_field = create_field_decl ("out", type, union_type, 0);

	  finish_record_type (union_type,
			      chainon (chainon (NULL_TREE, in_field),
				       out_field));

	  exp = build (COMPONENT_REF, type,
		       build1 (CONVERT_EXPR, union_type,
			       TREE_OPERAND (exp, 0)),
		       out_field);
	}
      break;

    default:
      abort ();
    }

  return expand_expr (exp, target, tmode, modifier);
}

/* Make a TRANSFORM_EXPR to later expand GNAT_NODE into an object
   of GNU_TYPE.  */

tree
make_transform_expr (gnat_node, gnu_type)
     gnat_tree gnat_node;
     tree gnu_type;
{
  tree gnu_result = build (TRANSFORM_EXPR, gnu_type);

  TREE_SIDE_EFFECTS (gnu_result) = 1;
  TREE_COMPLEXITY (gnu_result) = gnat_node;
  return gnu_result;
}

/* Performs whatever initialization steps needed by the language-dependent
   lexical analyzer.

   Define the additional tree codes here.  This isn't the best place to put
   it, but it's where g++ does it.  */

void
init_lex ()
{
  lang_expand_expr = gnat_expand_expr;

  tree_code_type
    = (char **) realloc (tree_code_type,
			 sizeof (char *) * LAST_GNAT_TREE_CODE);
  tree_code_length
    = (int *) realloc (tree_code_length,
		       sizeof (int) * LAST_GNAT_TREE_CODE);
  tree_code_name
    = (char **) realloc (tree_code_name,
			 sizeof (char *) * LAST_GNAT_TREE_CODE);

  bcopy ((char *) gnat_tree_code_type,
	 (char *) (tree_code_type + (int) LAST_AND_UNUSED_TREE_CODE),
	 ((LAST_GNAT_TREE_CODE - (int) LAST_AND_UNUSED_TREE_CODE)
	  * sizeof (char *)));

  bcopy ((char *)gnat_tree_code_length,
	 (char *) (tree_code_length + (int) LAST_AND_UNUSED_TREE_CODE),
	 ((LAST_GNAT_TREE_CODE - (int) LAST_AND_UNUSED_TREE_CODE)
	  * sizeof (int)));

  bcopy ((char *) gnat_tree_code_name,
	 (char *) (tree_code_name + (int) LAST_AND_UNUSED_TREE_CODE),
	 ((LAST_GNAT_TREE_CODE - (int) LAST_AND_UNUSED_TREE_CODE)
	  * sizeof (char *)));
}

/* Sets some debug flags for the parsed. It does nothing here.  */

void
set_yydebug (value)
     int value;
{}



/* Utility Routines needed by the Tree Translator: */

/* GNU_TYPE is the type of a subprogram parameter.  Determine from the type if
   it should be passed by reference.  */

int
pass_by_ref (gnu_type)
     tree gnu_type;
{
  /* Otherwise, we pass only BLKmode and unconstrained objects by
     reference.  */
  return (TREE_CODE (gnu_type) == UNCONSTRAINED_ARRAY_TYPE
	  || TYPE_MODE (gnu_type) == BLKmode);
}
