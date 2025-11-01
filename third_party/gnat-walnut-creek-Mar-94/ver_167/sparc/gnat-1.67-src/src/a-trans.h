/****************************************************************************/
/*									    */
/*			   GNAT COMPILER COMPONENTS			    */
/*									    */
/*		              TREE TRANSFORMER  			    */
/*									    */
/*				Specification				    */
/*									    */
/*			      $Revision: 1.8 $ 			    */
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

/* Type used to identify entries in the gnat tree. A node of type `gnat_tree'
   is the root of a gnat tree or subtree. */
typedef Tree_Id gnat_tree;

#define BAD_GNAT_TREE ((gnat_tree) -1)

/* Offset from first sloc in file.  Set in parallel with LINENO
   and CURRENT_FILE_NAME.  */
extern int sloc_offset;

/* For most front-ends, this is the parser for the language.  For us, we
   process the GNAT tree.  */
extern int yyparse		PROTO((void));

/* This function is the driver of the GNAT to GCC tree transformation process.
   GNAT_NODE is the root of some gnat tree.  It generates code for that
   part of the tree.  */
extern void gnat_to_code	PROTO((gnat_tree));

/* GNAT_NODE is the root of some GNAT tree.  Return the root of the
   GCC tree corresponding to that GNAT tree.  Normally, no code is generated;
   we just return an equivalent tree which is used elsewhere to generate
   code.  */
extern tree gnat_to_gnu		PROTO((gnat_tree));

/* Determine the input_filename and the lineno from the source location
   (Sloc) of GNAT_NODE node.  Set the global variable input_filename and
   lineno.  If WRITE_NOTE_P is true, emit a line number note. */
extern void set_lineno		PROTO((gnat_tree, int));

/* Post an error message.  MSG is the error message, properly annotated.
   NODE is the node at which to post the error and the node to use for the
   "&" substitution.  */
extern void post_error		PROTO((char *, gnat_tree));

/* Initialize the table that maps GNAT codes to GCC codes for simple
   binary and unary operations.  */
extern void init_code_table	PROTO((void));
