/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                   ADA CHAPTER 4: NAMES AND EXPRESSIONS                   */
/*                            - GNU SPECIFIC -                              */
/*                                                                          */
/*                                  Body                                    */
/*                                                                          */
/*                            $Revision: 1.43 $                             */
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
#include "a-trans3.h"
#include "a-trans4.h"
#include "a-misc.h"

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

tree
truthvalue_conversion (expr)
     tree expr;
{
  register enum tree_code code;
  tree type = TREE_TYPE (expr);

  switch (TREE_CODE (expr))
    {
    case EQ_EXPR:  case NE_EXPR: case LE_EXPR: case GE_EXPR:
    case LT_EXPR:  case GT_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case ERROR_MARK:
      return expr;

    case COND_EXPR:
      /* Distribute the conversion into the arms of a COND_EXPR.  */
      return fold (build (COND_EXPR, type, TREE_OPERAND (expr, 0),
			  truthvalue_conversion (TREE_OPERAND (expr, 1)),
			  truthvalue_conversion (TREE_OPERAND (expr, 2))));
    }

  return build_binary_op (NE_EXPR, type, expr,
			  convert (type, integer_zero_node));
}

/* We have a comparison or assignment operation on two array types, T1 and T2.
   Return the type that both operands should be converted to, if any.
   Otherwise return zero.  */

static tree
find_common_array_type (t1, t2)
     tree t1, t2;
{
  /* If either type is non-BLKmode, use it.  Note that we know that we will
     not have any alignment problems since if we did the non-BLKmode
     type could not have been used.  */
  if (TYPE_MODE (t1) != BLKmode)
    return t1;
  else if (TYPE_MODE (t2) != BLKmode)
    return t2;

  /* Otherwise, return the type that has a constant size.  */
  if (TREE_CONSTANT (TYPE_SIZE (t1)))
    return t1;
  else if (TREE_CONSTANT (TYPE_SIZE (t2)))
    return t2;

  /* In this case, both types have variable size.  It's probably
     best to leave the "type mismatch" because changing it could
     case a bad self-referential reference.  */
  return 0;
}

/* Make a binary operation of kind OP_CODE.  RESULT_TYPE is the type
   desired for the result.  Usually the operation is to be performed
   in that type.  For MODIFY_EXPR and ARRAY_REF, RESULT_TYPE may be 0
   in which case the type to be used will be derived from the operands.

   This function is very much unlike the ones for C and C++ since we
   have already done any type conversion and matching required.  All we
   have to do here is validate the work done by SEM and handle subtypes.  */

tree
build_binary_op (op_code, result_type, left_operand, right_operand)
     enum tree_code op_code;
     tree result_type;
     tree left_operand;
     tree right_operand;
{
  tree left_type  = TREE_TYPE (left_operand);
  tree right_type = TREE_TYPE (right_operand);
  tree left_base_type = left_type, right_base_type = right_type;
  tree operation_type = result_type;
  tree result;
  int has_side_effects = 0;
  int suppress_modulus = 0;

  /* Get the base types of both operands for integer and real types.  */
  if (TREE_CODE (left_base_type) == INTEGER_TYPE
      && TREE_TYPE (left_base_type) != 0)
    left_base_type = TREE_TYPE (left_base_type);
  if (TREE_CODE (right_base_type) == INTEGER_TYPE
      && TREE_TYPE (right_base_type) != 0)
    right_base_type = TREE_TYPE (right_base_type);

  switch (op_code)
    {
    case MODIFY_EXPR:
      if (operation_type == 0)
	operation_type = left_type;

      /* If we are copying one array to another, find the best type to use.  */
      if (TREE_CODE (left_type) == ARRAY_TYPE
	  && TREE_CODE (right_type) == ARRAY_TYPE)
	{
	  tree best_type = find_common_array_type (left_type, right_type);

	  if (best_type && left_type != best_type)
	    left_operand = convert (best_type, left_operand);
	  if (best_type && right_type != best_type)
	    right_operand = convert (best_type, right_operand);

	  if (best_type)
	    operation_type = best_type;
	}
      else if (TREE_TYPE (right_operand) != operation_type)
	right_operand = convert (operation_type, right_operand);

      has_side_effects = 1;
      suppress_modulus = 1;
      break;

    case ARRAY_REF:
      if (operation_type == 0)
	operation_type = TREE_TYPE (left_type);

      if (right_type != TYPE_DOMAIN (left_type))
	right_operand = convert (TYPE_DOMAIN (left_type), right_operand);

      suppress_modulus = 1;
      break;

    case GE_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case LT_EXPR:
      if (TREE_CODE (left_type) == POINTER_TYPE)
	abort ();

      /* ... fall through ... */

    case EQ_EXPR:
    case NE_EXPR:
      /* Unless the objects are arrays or records, the base types must be
	 the same; convert both operands to the base types.  If we have
	 arrays, use the best type.  */
      if (left_base_type != right_base_type)
	{
	  if (TREE_CODE (left_base_type) == ARRAY_TYPE
	      && TREE_CODE (right_base_type) == ARRAY_TYPE)
	    {
	      tree best_type = find_common_array_type (left_type, right_type);

	      if (best_type && left_type != best_type)
		left_operand = convert (best_type, left_operand);
	      if (best_type && right_type != best_type)
		right_operand = convert (best_type, right_operand);
	    }
	  else if (TREE_CODE (left_base_type) == RECORD_TYPE
		   && TREE_CODE (right_base_type) == RECORD_TYPE)
	    {
	      /* The only way these are permitted to be the same is if both
		 types have the same name.  In that case, one of them must
		 not be self-referential.  Use that one as the best type.
		 Even better is if one is of fixed size.  */
	      tree best_type = 0;

	      if (TYPE_NAME (left_base_type) == 0
		  || TYPE_NAME (left_base_type) != TYPE_NAME (right_base_type))
		abort ();

	      if (TREE_CONSTANT (TYPE_SIZE (left_base_type)))
		best_type = left_base_type;
	      else if (TREE_CONSTANT (TYPE_SIZE (right_base_type)))
		best_type = right_base_type;
	      else if (! contains_placeholder_p (TYPE_SIZE (left_base_type)))
		best_type = left_base_type;
	      else if (! contains_placeholder_p (TYPE_SIZE (right_base_type)))
		best_type = right_base_type;
	      else
		abort ();

	      left_operand = convert (best_type, left_operand);
	      right_operand = convert (best_type, right_operand);
	    }
	  else
	    abort ();
	}

      /* If we are comparing a fat pointer against zero, we need to 
	 just compare the template pointer.  */
      else if (TYPE_FAT_POINTER_P (left_base_type)
	       && TREE_CODE (right_operand) == CONSTRUCTOR
	       && integer_zerop (TREE_VALUE (TREE_OPERAND (right_operand, 1))))
	{
	  right_operand = build_component_ref (left_operand,
					       NULL_TREE,
					       TYPE_FIELDS (left_base_type));
	  left_operand = convert (TREE_TYPE (right_operand),
				  integer_zero_node);
	}
      else
	{
	  left_operand = convert (left_base_type, left_operand);
	  right_operand = convert (right_base_type, right_operand);
	}

      suppress_modulus = 1;
      break;

    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      /* In these, the result type and the left operand type should be the
	 same.  Do the operation in that type and convert the right
	 operand (which is an integer) to the left type.  */
      if (left_type != result_type)
	abort ();

      right_operand = convert (left_type, right_operand);
      has_side_effects = 1;
      break;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
      left_operand = truthvalue_conversion (left_operand);
      right_operand = truthvalue_conversion (right_operand);
      /* ... fall through ... */
    default:
      /* The result type should be the same as the base types of the
	 both operands (and they should be the same).  Convert
	 everything to the result type.  */

      if (result_type != left_base_type || left_base_type != right_base_type)
	abort ();

      left_operand = convert (result_type, left_operand);
      right_operand = convert (result_type, right_operand);
    }

  result = build (op_code, operation_type, left_operand, right_operand);
  TREE_SIDE_EFFECTS (result) = has_side_effects;

  if (result_type != 0 && result_type != operation_type)
    result = convert (result_type, result);

  result = fold (result);
  TREE_CONSTANT (result)
    = TREE_CONSTANT (left_operand) & TREE_CONSTANT (right_operand);

  /* If we are working with modular types, perform the MOD operation.  */
  if (! suppress_modulus && TYPE_MODULAR_P (result_type))
    result = fold (build (TRUNC_MOD_EXPR, result_type, result,
			  convert (result_type, TYPE_MODULUS (result_type))));

  return result;
}

/* Similar, but for unary operations.  */

tree
build_unary_op (op_code, result_type, operand)
     enum tree_code op_code;
     tree result_type;
     tree operand;
{
  tree type = TREE_TYPE (operand);
  tree base_type;
  tree result;
  int suppress_modulus = 0;
  int side_effects = 0;

  /* Get the base types of the operand for integer and real types.  */
  for (base_type = type; 
       (INTEGRAL_TYPE_P (base_type) || FLOAT_TYPE_P (base_type))
       && TREE_TYPE (base_type);
       base_type = TREE_TYPE (base_type))
    ;

  switch (op_code)
    {
    case TRUTH_NOT_EXPR:
      if (result_type != base_type)
	abort ();

      result = invert_truthvalue (truthvalue_conversion (operand));
      break;

    case ADDR_EXPR:
      if (TREE_CODE (operand) == INDIRECT_REF
	  || TREE_CODE (operand) == UNCONSTRAINED_ARRAY_REF)
	result = TREE_OPERAND (operand, 0);
      else
	result = fold (build1 (op_code, build_pointer_type (type), operand));

      TREE_CONSTANT (result) = staticp (operand) || TREE_CONSTANT (operand);
      suppress_modulus = 1;
      break;

    case INDIRECT_REF:
      /* If we want to refer to an entire unconstrained array,
	 make up an expression to do so.  This will never survive to
	 the backend.  */
      if (TYPE_FAT_POINTER_P (type))
	result = build1 (UNCONSTRAINED_ARRAY_REF,
			 TYPE_UNCONSTRAINED_ARRAY (type), operand);
      else
	result = fold (build1 (op_code, TREE_TYPE (type), operand));

      suppress_modulus = 1;
      side_effects = flag_volatile;
      break;

    default:
      if (result_type != base_type)
	abort ();

      result = fold (build1 (op_code, result_type, convert (result_type,
							    operand)));
    }

  if (side_effects)
    TREE_SIDE_EFFECTS (result) = 1;

  if (result_type != 0 && TREE_TYPE (result) != result_type)
    result = convert (result_type, result);

  /* If we are working with modular types, perform the MOD operation.  */
  if (! suppress_modulus && TYPE_MODULAR_P (result_type))
    result = fold (build (TRUNC_MOD_EXPR, result_type, result,
			  convert (result_type, TYPE_MODULUS (result_type))));

  return result;
}

/* Return a CONSTRUCTOR of TYPE whose list is LIST.  */

tree
build_constructor (type, list)
     tree type;
     tree list;
{
  tree elmt;
  int allconstant = 1;
  tree result;

  for (elmt = list; elmt; elmt = TREE_CHAIN (elmt))
    if (! TREE_CONSTANT (TREE_VALUE (elmt)))
      allconstant = 0;

  result = build (CONSTRUCTOR, type, NULL_TREE, list);
  TREE_CONSTANT (result) = allconstant;
  TREE_STATIC (result) = allconstant;

  return result;
}

/* Return a COMPONENT_REF to access a field that is given by COMPONENT,
   an IDENTIFIER_NODE giving the name of the field, FIELD, a FIELD_DECL,
   for the field, or both.

   We also handle the fact that we might have been passed a pointer to the
   actual record and know how to look for fields in variant parts.  */

tree
build_component_ref (record_variable, component, field)
     tree record_variable;
     tree component;
     tree field;
{
  tree record_type = TREE_TYPE (record_variable);
  tree ref;

  /* Handle added pointer for pass-by-reference values.  */
  if (TREE_CODE (record_type) == POINTER_TYPE)
    {
      record_variable
	= build_unary_op (INDIRECT_REF, NULL_TREE, record_variable);
      record_type = TREE_TYPE (record_variable);
    }

  if ((TREE_CODE (record_type) != RECORD_TYPE
       && TREE_CODE (record_type) != UNION_TYPE
       && TREE_CODE (record_type) != QUAL_UNION_TYPE)
      || TYPE_SIZE (record_type) == 0)
    abort ();

  if (field == 0 || DECL_CONTEXT (field) != record_type)
    /* Check if there is a field with name COMPONENT in the record.  */
    {
      if (component == 0)
	abort ();

      /* ??? Explore later if we can use the TYPE_LANG_SPECIFIC optimization
	 that appears in C version of this function.  */

      for (field = TYPE_FIELDS (record_type); field;
	   field = TREE_CHAIN (field))
	{
	  if (DECL_NAME (field) == component)
	    break;
	  else if (DECL_NAME (field) == 0)
	     {
	      tree field_ref
		= build_component_ref (record_variable, NULL_TREE, field);

	      ref = build_component_ref (field_ref, component, NULL_TREE);

	      if (ref != 0)
		return ref;
	    }
	}
    }

  if (!field)
    return 0;

  ref = build (COMPONENT_REF, TREE_TYPE (field), record_variable, field);

  if (TREE_READONLY (record_variable) || TREE_READONLY (field))
    TREE_READONLY (ref) = 1;
  if (TREE_THIS_VOLATILE (record_variable) || TREE_THIS_VOLATILE (field))
    TREE_THIS_VOLATILE (ref) = 1;

  return ref;
}


/* a variation of the precedant function which looks in the field __parent
   if the field is not defined at this level */

tree
build_tagged_component_ref (record_variable, component, field)
     tree record_variable;
     tree component;
     tree field;
{
  tree local_field;
  tree parent_ref;
  static tree parent_comp = 0;
  tree comp_ref;

  if (!parent_comp) 
    parent_comp = get_identifier (Get_Name_String (Name_uParent)); 

  /* We first see if the field is present at this level.  */
  comp_ref = build_component_ref (record_variable, component, field);
  if (comp_ref)
    return comp_ref;

  /* If it is not present we look recursively in the parent.  */

  parent_ref = build_component_ref (record_variable, parent_comp, NULL_TREE);
  if (parent_ref)
    return build_tagged_component_ref (parent_ref, component, field);
  else
    /* The field was not found in the hierarchy. Should not happen. */
    abort ();
}


/* Build a GCC tree to correspond to allocating an object of TYPE whose
   initial value is INIT, if INIT is nonzero.  Convert the expression to
   RESULT_TYPE, which must be some type of pointer.  Return the tree. 
   GNAT_TYPE is the type of the underlying object in case we need to 
   call a record initialization procedure.  */

tree
build_allocator (type, init, gnat_type, result_type)
     tree type;
     tree init;
     gnat_tree gnat_type;
     tree result_type;
{
  tree size = TYPE_SIZE (type);
  tree ptr_type;
  tree result;

  /* If TYPE is an unconstrained array, it must be the case that INIT
     is nonzero and an UNCONSTRAINED_ARRAY_REF.  Set SIZE to be the sum of
     the sizes of the object, it's template, and the fat pointer.  Allocate
     the whole thing and fill in the parts.  */
  if (TREE_CODE (type) == UNCONSTRAINED_ARRAY_TYPE)
    {
      tree fat_ptr_type = TYPE_POINTER_TO (type);
      tree array_type = TREE_TYPE (TREE_TYPE (TYPE_FIELDS (fat_ptr_type)));
      tree template_type
	= TREE_TYPE (TREE_TYPE (TREE_CHAIN (TYPE_FIELDS (fat_ptr_type))));
      tree fat_ptr_size = TYPE_SIZE (fat_ptr_type);
      tree template_size = TYPE_SIZE (template_type);
      tree storage;
      tree old_template_ptr, old_array_ptr, new_template_ptr, new_array_ptr;

      if (init == 0 || TREE_CODE (init) != UNCONSTRAINED_ARRAY_REF)
	abort ();

      size = size_binop (PLUS_EXPR, 
			 build (WITH_RECORD_EXPR, sizetype,
				TYPE_SIZE (array_type),
				TREE_OPERAND (init, 0)),
			 size_binop (PLUS_EXPR, fat_ptr_size, template_size));

      storage = build (CALL_EXPR, build_pointer_type (char_type_node),
		       build_unary_op (ADDR_EXPR, NULL_TREE, malloc_decl),
		       build_tree_list (NULL_TREE,
					size_binop (CEIL_DIV_EXPR,
						    size,
						    size_int (BITS_PER_UNIT))),
		       NULL_TREE);
      TREE_SIDE_EFFECTS (storage) = 1;
      storage = save_expr (storage);

      /* Skip the fat pointer to get the template, then skip both to get
	 the memory for the array.  */
      new_template_ptr
	= convert (build_pointer_type (template_type),
		   build (PLUS_EXPR, TREE_TYPE (storage),
			  storage,
			  convert (TREE_TYPE (storage),
				   size_in_bytes (fat_ptr_type))));

      new_array_ptr
	= convert
	  (build_pointer_type (array_type),
	   build (PLUS_EXPR, TREE_TYPE (storage),
		  storage,
		  convert (TREE_TYPE (storage),
			   size_binop (PLUS_EXPR,
				       size_in_bytes (fat_ptr_type),
				       size_in_bytes (template_type)))));

      old_template_ptr = build_component_ref (TREE_OPERAND (init, 0),
					      get_identifier ("p_template"),
					      NULL_TREE);

      old_array_ptr = build_component_ref (TREE_OPERAND (init, 0),
					   get_identifier ("p_array"),
					   NULL_TREE);

      result = build_unary_op (INDIRECT_REF, fat_ptr_type,
			       convert (build_pointer_type (fat_ptr_type),
					storage));

      result = build (COMPOUND_EXPR, fat_ptr_type,
		      build_binary_op (MODIFY_EXPR, template_type,
				       build_unary_op (INDIRECT_REF, NULL_TREE,
						       new_template_ptr),
				       build_unary_op (INDIRECT_REF, NULL_TREE,
						       old_template_ptr)),
		      result);

      result = build (COMPOUND_EXPR, fat_ptr_type,
		      build_binary_op (MODIFY_EXPR, array_type,
				       build_unary_op (INDIRECT_REF, NULL_TREE,
						       new_array_ptr),
				       build_unary_op (INDIRECT_REF, NULL_TREE,
						       old_array_ptr)),
		      result);

      return
	build_binary_op
	  (MODIFY_EXPR, fat_ptr_type,
	   result,
	   build_constructor
	   (fat_ptr_type,
	    tree_cons (TYPE_FIELDS (fat_ptr_type), new_array_ptr,
		       tree_cons (TREE_CHAIN (TYPE_FIELDS (fat_ptr_type)),
				  new_template_ptr, NULL_TREE))));
    }

  /* If TYPE is an discriminated record, allocate an object that is
     the maximum size of the record.  Note that the record must have had
     default discriminant or this would not be valid Ada.  */
  if (TREE_CODE (size) != INTEGER_CST && contains_placeholder_p (size))
    size = max_size (size, 1);

  /* Call the allocator and return an object whose type is a pointer to
     TYPE.  Note that we pass the size in bytes and convert to sizetype.  */

  result
    = build (CALL_EXPR, build_pointer_type (type),
	     build_unary_op (ADDR_EXPR, NULL_TREE, malloc_decl),
	     chainon (NULL_TREE,
		      build_tree_list (NULL_TREE,
				       size_binop (CEIL_DIV_EXPR,
						   convert (sizetype, size),
						   size_int (BITS_PER_UNIT)))),
	     NULL_TREE);
  TREE_SIDE_EFFECTS (result) = 1;

  /* If we have an initial value, put the new address into a SAVE_EXPR, assign
     the value, and return the address.  Do this with a COMPOUND_EXPR.  */

  if (init)
    {
      result = save_expr (result);
      result = build (COMPOUND_EXPR, TREE_TYPE (result),
		      build_binary_op (MODIFY_EXPR, type,
				       build_unary_op (INDIRECT_REF, type,
						       result),
				       init),
		      result);
    }

  /* If the result is not of the proper type, convert it.  But check for
     converting a pointer to a constrained type into a pointer to
     an unconstrained type.  In that case, we have to allocate the template
     from memory as well.  */
  if (TREE_TYPE (result) != result_type)
    {
      if (TYPE_FAT_POINTER_P (result_type)
	  & TREE_CODE (TREE_TYPE (TREE_TYPE (result))) == ARRAY_TYPE)
	result = convert_to_unconstrained (result_type, result, 1);
      else
	result = convert (result_type, result);
    }

  return result;
}

/* Indicate that we need to make the address of EXPR_NODE and it therefore
   should not be allocated in a register. Return 1 if successful.  */

int
mark_addressable (tree expr_node)
{
  while (1)
    switch (TREE_CODE (expr_node))
      {
      case ADDR_EXPR:
      case COMPONENT_REF:
      case ARRAY_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
	expr_node = TREE_OPERAND (expr_node, 0);
	break;

      case CONSTRUCTOR:
	TREE_ADDRESSABLE (expr_node) = 1;
	return 1;

      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	put_var_into_stack (expr_node);
	TREE_ADDRESSABLE (expr_node) = 1;
	return 1;

      case FUNCTION_DECL:
	TREE_ADDRESSABLE (expr_node) = 1;
	return 1;

      default:
	return 1;
    }
}
