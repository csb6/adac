/****************************************************************************/
/*									    */
/*			   GNAT COMPILER COMPONENTS			    */
/*                                                                          */
/*                   ADA CHAPTER 3: DECLARATIONS AND TYPES                  */
/*                            - GNAT SPECIFIC -                             */
/*                                                                          */
/*                              Specification                               */
/*									    */
/*			      $Revision: 1.15 $                             */
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

/* Given GNAT_ENTITY, a GNAT defining identifier node, which denotes some Ada
   entity, this routine returns the equivalent GCC tree for that entity
   (an ..._DECL node) and associates the ..._DECL node with the input GNAT
   defining identifier.

   If GNAT_ENTITY is a variable or a constant declaration, GNU_EXPR gives its
   initial value (in GCC tree form). This is optional for variables.

   DEFINITION is nonzero if this call is intended for a definition.  This is
   used for separate compilation where it necessary to know whether an
   external declaration or a definition should be created if the GCC equivalent
   was not created previously.  The value of 1 is normally used for a non-zero
   DEFINITION, but a value of 2 is used in special circumstances, defined in
   the code.  */
extern tree gnat_to_gnu_entity	PROTO((gnat_tree, tree, int));

/* Given GNAT_ENTITY, an entity in the incoming GNAT tree, return a
   GCC type corresponding to that entity.  GNAT_ENTITY is assumed to
   refer to an Ada type.  */
extern tree gnat_to_gnu_type	PROTO((gnat_tree));

/* Given GNAT_ENTITY, elaborate all expressions that are required to
   be elaborated at the point of its definition, but do nothing else.  */
extern void elaborate_entity	PROTO((gnat_tree));

/* Called when we need to protect a variable object using a save_expr.  */
extern tree maybe_variable	PROTO((tree));

/* Given a GNU tree and a GNAT list of choices, generate an expression to test
   the value passed against the list of choices.  */
extern tree choices_to_gnu	PROTO((tree, gnat_tree));

/* Create the call to the record initialization routine.  */
extern tree create_rec_init_call PROTO((tree, gnat_tree));

/* Given a type T, a FIELD_DECL F, and a replacement value R,
   return a new type with all size expressions that contain F
   updated by replacing F with R.  This is identical to GCC's
   substitute_in_type except that it knows about TYPE_INDEX_TYPE.  */
extern tree gnat_substitute_in_type PROTO((tree, tree, tree));

/* Return a name for GNAT_ENTITY concatenated with two underscores and
   STRING.  */

extern char *create_concat_name PROTO((gnat_tree, char *));
