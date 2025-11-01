/****************************************************************************/
/*									    */
/*			   GNAT COMPILER COMPONENTS			    */
/*                                                                          */
/*                   ADA CHAPTER 4: NAMES AND EXPRESSIONS                   */
/*                            - GNU SPECIFIC -                              */
/*                                                                          */
/*                              Specification                               */
/*									    */
/*			      $Revision: 1.16 $ 			    */
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

/* Prepare expr to be an argument of a TRUTH_NOT_EXPR or other logical
   operation.

   This preparation consists of taking the ordinary
   representation of an expression expr and producing a valid tree
   boolean expression describing whether expr is nonzero.  We could
   simply always do build_binary_op (NE_EXPR, expr, integer_zero_node, 1),
   but we optimize comparisons, &&, ||, and !.

   The resulting type should always be the same as the input type.
   This function is simpler than the corresponding C version since
   the only possible operands will be things of Boolean type.  */
extern tree truthvalue_conversion      PROTO((tree));

/* Make a binary operation of kind OP_CODE.  RESULT_TYPE is the type
   desired for the result.  Usually the operation is to be performed
   in that type.  For MODIFY_EXPR and ARRAY_REF, RESULT_TYPE may be 0
   in which case the type to be used will be derived from the operands.  */
extern tree build_binary_op	PROTO((enum tree_code, tree, tree, tree));

/* Similar, but make unary operation.   */
extern tree build_unary_op	PROTO((enum tree_code, tree, tree));

/* Return a CONSTRUCTOR of TYPE whose list is LIST.  */
extern tree build_constructor	PROTO((tree, tree));

/* Return a COMPONENT_REF to access a field that is given by COMPONENT,
   an IDENTIFIER_NODE giving the name of the field, FIELD, a FIELD_DECL,
   for the field, or both.  */
extern tree build_component_ref	PROTO((tree, tree, tree));
/* Same as build_component_Ref but looks in __Parent field for inherited 
   fields */
extern tree build_tagged_component_ref	PROTO((tree, tree, tree));

/* Build a GCC tree to correspond to allocating an object of TYPE whose
   initial value if INIT, if INIT is nonzero.  Convert the expression to
   RESULT_TYPE, which must be some type of pointer.  Return the tree. 
   GNAT_TYPE is the type of the underlying object in case we need to 
   call a record initialization procedure.  */
extern tree build_allocator	PROTO((tree, tree, gnat_tree, tree));

/* Indicate that we need to make the address of EXPR_NODE and it therefore
   should not be allocated in a register. Return 1 if successful.  */
extern int mark_addressable	PROTO((tree));
