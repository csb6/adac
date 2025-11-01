/****************************************************************************/
/*									    */
/*			   GNAT COMPILER COMPONENTS			    */
/*                                                                          */
/*                              MISCELLANEOUS                               */
/*                                                                          */
/*                              Specification                               */
/*									    */
/*			      $Revision: 1.13 $ 			    */
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

/* Variables expected by the gcc back-end for reference or merely linking
   purposes:  */

/* A string identifing the compiler.  */
extern char *language_string;

/* The gcc back-end sets this in jump.c.  */
extern int current_function_returns_null;

/* Ada language-specific GC tree codes.  */
#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) SYM,
enum gnat_tree_code {
  __DUMMY = LAST_AND_UNUSED_TREE_CODE,
#include "a-tree.def"
  LAST_GNAT_TREE_CODE
};
#undef DEFTREECODE

/* Routines expected by the gcc back-end for call-back or merely linking
   purposes. They must have exactly the same prototype and name as expected
   by GCC.  */

/* Decode all the language specific options that cannot be decoded by GCC. The
   option decoding phase of GCC calls this routine on the flags that it cannot
   decode. This routine returns 1 if it is successful, otherwise it
   returns 0. */
extern int lang_decode_option	PROTO((char *));

/* Perform all the initialization steps that are language-specific.  */
extern void lang_init		PROTO((void));

/* Perform all the finalization steps that are language-specific.  */
extern void lang_finish		PROTO((void));

/* Print any language-specific compilation statistics.  */
extern void print_lang_statistics	PROTO((void));

/* Return a short string identifying this language to the debugger.  */
extern char *lang_identify	PROTO((void));

#ifdef BUFSIZ
/* Hooks for `print_node'.  */
extern void print_lang_decl	PROTO((FILE *, tree, int));
extern void print_lang_type	PROTO((FILE *, tree, int));
extern void print_lang_identifier PROTO((FILE *, tree, int));
#endif

/* Performs whatever initialization steps needed by the language-dependent
   lexical analyzer.  */
extern void init_lex		PROTO((void));

/* Sets some debug flags for the parser. It does nothing here.  */
extern void set_yydebug		PROTO((int));

/* Utility routines created for the tree translator's sake. Their prototypes
   can be changed as desired.  */

/* Make a TRANSFORM_EXPR to later expand GNAT_NODE into an object
   of GNU_TYPE.  */
extern tree make_transform_expr PROTO((gnat_tree, tree));

/* GNU_TYPE is the type of a subprogram parameter.  Determine from the type if
   it should be passed by reference.  */
extern int pass_by_ref		PROTO((tree));


/* elaboration routines for the front end */
extern   void elab_all_gnat          PROTO((void));
