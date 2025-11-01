/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                   ADA CHAPTER 3: DECLARATIONS AND TYPES                  */
/*                            - GNAT SPECIFIC -                             */
/*                                                                          */
/*                                  Body                                    */
/*                                                                          */
/*                            $Revision: 1.127 $                             */
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
#include "obstack.h"
#include "flags.h"
#include "convert.h"

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
#include "a-gtran3.h"
#include "a-misc.h"
#include "a-rtree.h"

static tree maybe_placeholder		PROTO((tree));
static tree elaborate_expression	PROTO((gnat_tree, gnat_tree, char *,
					       int));
static void components_to_record	PROTO((tree, gnat_tree, tree, int));
static tree create_enum_initializer	PROTO((gnat_tree, tree));
static char *get_operator_name		PROTO((char *));
static char *qualified_overloaded_name  PROTO((gnat_tree));
static char *qualified_name		PROTO((gnat_tree));
static char *get_external_name		PROTO((gnat_tree));

/* Given GNAT_ENTITY, an entity in the incoming GNAT tree, return a
   GCC type corresponding to that entity.  GNAT_ENTITY is assumed to
   refer to an Ada type.  */

tree
gnat_to_gnu_type (gnat_entity)
     gnat_tree gnat_entity;
{
  tree gnu_decl;

  /* Convert the ada entity type into a GCC TYPE_DECL node.  */
  gnu_decl = gnat_to_gnu_entity (gnat_entity, NULL_TREE, 0);
  if (TREE_CODE (gnu_decl) != TYPE_DECL)
    abort ();

  return TREE_TYPE (gnu_decl);
}

/* These two variables are used to defer recursively expanding incomplete
   types while we are processing a record.  */

static int defer_incomplete_level = 0;
static struct incomplete
{
  struct incomplete *next;
  tree access_type;
  gnat_tree full_type;
} *defer_incomplete_list = 0;

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

tree
gnat_to_gnu_entity (gnat_entity, gnu_expr, definition)
     gnat_tree gnat_entity;
     tree gnu_expr;
     int definition;
{
  char *entity_name;
  tree gnu_type;
  /* Contains the gnu XXXX_DECL tree node which is equivalent to the input
     GNAT tree. This node will be associated with the GNAT node by calling
     the save_gnu_tree routine at the end of the `switch' statement.  */
  tree gnu_decl = 0;
  /* Nonzero if we have already saved gnu_decl as a gnat association.  */
  int saved = 0;
  /* Nonzero if we were already in permanent allocation.  */
  int was_permanent = ! allocation_temporary_p ();
  /* Nonzero if we were in momentary allocation.  */
  int was_momentary;
  Entity_Kind kind = Ekind (gnat_entity);

  /* If this is entity 0, something went badly wrong.  */
  if (gnat_entity == 0)
    abort ();

  /* If we've already processed this entity, return what we got last time.
     If we are defining the node, we should not have already processed it.
     In that case, we will abort below when we try to save a new GCC tree for
     this object.

     We make an exception here for subprograms since we may have processed
     both the spec and body, depending on the circumstances.  This is a
     bit of a kludge, but we are only using the kludge to disable an error
     check, so it's not too bad.

     We also need to handle the case of getting VOID_TYPE when a
     Full_Declaration exists.  */

  if ((! definition || kind == E_Function || kind == E_Procedure)
      && present_gnu_tree (gnat_entity))
    {
      gnu_decl = get_gnu_tree (gnat_entity);

      if (TREE_CODE (gnu_decl) == TYPE_DECL
	  && TREE_CODE (TREE_TYPE (gnu_decl)) == VOID_TYPE
	  && IN (Ekind (gnat_entity), Incomplete_Kind)
	  && Present (Full_Declaration (gnat_entity)))
	{
	  gnu_decl = get_gnu_tree (Full_Declaration (gnat_entity));
	  save_gnu_tree (gnat_entity, NULL_TREE, 0);
	  save_gnu_tree (gnat_entity, gnu_decl, 0);
	}

      return gnu_decl;
    }

  /* Get the name of the entity and set up the line number and filename of
     the original definition for use in any decl we make.  */
  entity_name = Get_Name_String (Chars (gnat_entity));
  set_lineno (gnat_entity, 0);

  /* If we are not defining this node, it is external and must be
     permanently allocated.  If we are not already in permanent
     allocation, go there now.  */
  if (! definition && ! was_permanent)
    {
      push_obstacks_nochange ();
      end_temporary_allocation ();
    }

  /* Make sure objects we allocate aren't in the momentary obstack.  */
  was_momentary = suspend_momentary ();

  switch (kind)
    {
    case E_Constant:
      /* If this is a use of a deferred constant, get its full
	 declaration.  */
      if (! definition && Present (Full_Declaration (gnat_entity)))
	return gnat_to_gnu_entity (Full_Declaration (gnat_entity),
				   gnu_expr, definition);

      /* If we have an external constant that we are not defining,
	 get the expression that is was defined to represent.  We
	 may throw that expression away later if it is not a 
	 constant.  */
      if (! definition && Present (Expression (Parent (gnat_entity))))
	gnu_expr = gnat_to_gnu (Expression (Parent (gnat_entity)));

      /* Ignore deferred constant definitions; they are processed fully in the
	 front-end.  For deferred constant references, get the full
         definition.  */
      if (definition && gnu_expr == 0)
	return error_mark_node;
      else if (! definition && IN (Ekind (gnat_entity), Incomplete_Kind)
	       && Present (Full_Declaration (gnat_entity)))
        return gnat_to_gnu_entity (Full_Declaration (gnat_entity),
                                   NULL_TREE, 0);

      goto object;

    case E_Component:
    case E_Discriminant:
      /* if the variable is an inherited record component (in the case of
	 extended record types) just return the inherited entity, which
	 must be a FIELD_DECL.  */
      if (Present (Original_Record_Component (gnat_entity))
	  && Original_Record_Component (gnat_entity) != gnat_entity)
	{
	  gnu_decl
	    = gnat_to_gnu_entity (Original_Record_Component (gnat_entity),
				  gnu_expr, definition);

	  if (TREE_CODE (gnu_decl) != FIELD_DECL)
	    abort ();

	  break;
	}
      goto object;

    case E_Loop_Parameter:
    case E_Out_Parameter:
    case E_Exception:
    case E_Variable:

      /* Simple variables, loop variables, OUT parameters, and exceptions.  */
    object:
      {
	tree gnu_type = gnat_to_gnu_type (Etype (gnat_entity));
	int used_by_ref = 0;
	int const_flag = kind == E_Constant;
	tree gnu_size = NULL_TREE;

	/* If we get here, it means we have not yet done anything with this
	   entity.  If we are not defining it here, it must be external,
	   otherwise we should have defined it already.
	   Also, reject objects whose types are unconstrained arrays
	   or VOID.  */

	if ((! definition && ! Is_Public (gnat_entity))
	    || TREE_CODE (gnu_type) == UNCONSTRAINED_ARRAY_TYPE
	    || TREE_CODE (gnu_type) == VOID_TYPE)
	  abort ();

	/* If we are defining the object, see if it has a Size value and
	   validate it if so.  */
	if (definition && Has_Size_Clause (gnat_entity))
	  {
	    gnu_size = UI_To_gnu (Esize (gnat_entity), sizetype);

	    /* A size is not permitted for variable-sized objects.  */
	    if (TREE_CODE (TYPE_SIZE (gnu_type)) != INTEGER_CST)
	      {
		post_error ("SIZE not permitted for variabled-sized &",
			    gnat_entity);
		gnu_size = 0;
	      }

	    /* The specified size must be at least as large as that of the
	       type for non-integral types and the precision for integers.  */
	    else if ((INTEGRAL_TYPE_P (gnu_type)
		      && (TREE_INT_CST_HIGH (gnu_size) != 0
			  || (TREE_INT_CST_LOW (gnu_size)
			      < TYPE_PRECISION (gnu_type))))
		     || INT_CST_LT (gnu_size, TYPE_SIZE (gnu_type)))
	      {
		post_error ("& cannot fit in the specified size", gnat_entity);
		gnu_size = 0;
	      }
	  }

	/* If we are defining the object and it has an Address clause we must
	   get the address expression from the saved GCC tree for the
	   object.  */
	if (definition && Has_Address_Clause (gnat_entity))
	  {
	    tree gnu_address = get_gnu_tree (gnat_entity);

	    save_gnu_tree (gnat_entity, NULL_TREE, 0);

	    if (gnu_size)
	      {
		post_error ("both SIZE and ADDRESS specified for &",
			    gnat_entity);
		gnu_size = 0;
	      }

	    gnu_type = build_pointer_type (gnu_type);
	    gnu_address = convert (gnu_type, gnu_address);
	    used_by_ref = 1;
	    const_flag = ! Is_Public (gnat_entity);

	    /* If we don't have an initializing expression for the underlying
	       variable, the initializing expression for the pointer is the
	       specified address.  Otherwise, we have to make a COMPOUND_EXPR
	       to assign both the address and the initial value.  */
	    if (gnu_expr == 0)
	      gnu_expr = gnu_address;
	    else
	      gnu_expr
		= build (COMPOUND_EXPR, gnu_type,
			 build_binary_op
			 (MODIFY_EXPR, NULL_TREE,
			  build_unary_op (INDIRECT_REF, NULL_TREE,
					  gnu_address),
			  gnu_expr),
			 gnu_address);
	  }

	/* See if this is a renaming.  If it is, see what we are renaming.
	   If what we are renaming is a decl, just return that decl for
	   us as well.  Otherwise, make this into a constant pointer to
	   the object we are to rename.  An initializer is invalid here.  */
	if (Present (Renamed_Object (gnat_entity)))
	  {
	    if (gnu_expr)
	      abort ();

	    gnu_decl = gnat_to_gnu (Renamed_Object (gnat_entity));

	    if (TREE_CODE_CLASS (TREE_CODE (gnu_decl)) == 'd')
	      break;

	    const_flag = 1;
	    gnu_expr = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_decl);
	    gnu_type = build_pointer_type (gnu_type);
	    used_by_ref = 1;
	  }

	/* If we are at top level and this object is of variable size,
	   make the actual type a hidden pointer to the real type and
	   make the initializer be a memory allocation and initialization.  */

	if (global_bindings_p ()
	    && TREE_CODE (TYPE_SIZE (gnu_type)) != INTEGER_CST)
	  {
	    gnu_type = build_pointer_type (gnu_type);
	    gnu_expr = build_allocator (TREE_TYPE (gnu_type), gnu_expr,
					Etype (gnat_entity), gnu_type);
	    used_by_ref = 1;
	  }

	/* If this is a pointer and it does not have an initializing
	   expression, initialize it to NULL.  */
	if ((TREE_CODE (gnu_type) == POINTER_TYPE
	     || TYPE_FAT_POINTER_P (gnu_type))
	    && gnu_expr == 0)
	  gnu_expr = integer_zero_node;

	if (gnu_expr)
	  gnu_expr = convert (gnu_type, gnu_expr);

	gnu_decl = create_var_decl (entity_name,
				    global_bindings_p () || ! definition
				    ? get_external_name (gnat_entity)
				    : NULL_PTR,
				    gnu_type, gnu_expr,
				    const_flag,
				    Is_Public (gnat_entity), !definition,
				    kind == E_Exception,
				    Is_Volatile (gnat_entity));

	DECL_BY_REF_P (gnu_decl) = used_by_ref;
	if (gnu_size)
	  DECL_SIZE (gnu_decl) = gnu_size;

      }
      break;

    case E_Named_Integer:
    case E_Named_Real:
      /* These should not be present in any part of the tree we look at.  */
      abort ();

    case E_Void:
      /* Return a TYPE_DECL for "void" that we previously made.  */
      gnu_decl = void_type_decl_node;
      break;

    case E_Character_Type:
      /* Strictly speaking, character types are enumeral types, not
	 integer types.  However, for the predefined character
	 type, we do not list all the literals.  So if the literals
	 are not specified, make this an unsigned type.  Otherwise,
	 handle as a normal enumeration type.  */
      if (No (Literals (Type_Definition (Parent (gnat_entity)))))
	{
	  gnu_type = make_unsigned_type (UI_To_Int (Esize (gnat_entity)));
	  break;
	}
      /* ... fall through ... */
    case E_Boolean_Type:
    case E_Enumeration_Type:
      {
	/* Here we have a list of enumeral constants in First_Literal.
	   We make a CONST_DECL for each and build into GNU_LITERAL_LIST
	   the list to be places into TYPE_FIELDS.  Each node in the list
	   is a TREE_LIST node whose TREE_VALUE is the literal name
	   and whose TREE_PURPOSE is the value of the literal.

	   Esize contains the number of bits needed to represent the enumeral
	   type, Type_Low_Bound also points to the first literal and 
	   Type_High_Bound points to the last literal.  */

	gnat_tree gnat_literal;
	tree gnu_literal_list = NULL_TREE;

	/* Make a signed type if the representation of the first literal
	   is negative; otherwise make an unsigned type.  */
	if (tree_int_cst_lt (UI_To_gnu
			     (Enumeration_Rep (First_Literal (gnat_entity)),
			      integer_type_node),
			     integer_zero_node))
	  gnu_type = make_signed_type (UI_To_Int (Esize (gnat_entity)));
	else
	  gnu_type = make_unsigned_type (UI_To_Int (Esize (gnat_entity)));

	TREE_SET_CODE (gnu_type, ENUMERAL_TYPE);

	for (gnat_literal = First_Literal (gnat_entity);
	     Present (gnat_literal);
	     gnat_literal = Next_Literal (gnat_literal))
	  {
	    tree gnu_value = UI_To_gnu (Enumeration_Rep (gnat_literal),
					gnu_type);
	    char *name = Get_Name_String (Chars (gnat_literal));
	    tree gnu_literal
	      = create_var_decl (name, 0, gnu_type, gnu_value, 1, 0, 0, 0, 0);

	    save_gnu_tree (gnat_literal, gnu_literal, 0);

	    /* ??? If the literals name is a character literal, don't
	       add it to the fields since it will confuse debuggers.  */
	    if (name[0] != '\'')
	      gnu_literal_list = tree_cons (DECL_NAME (gnu_literal),
					    DECL_INITIAL (gnu_literal),
					    gnu_literal_list);
	  }

	TYPE_FIELDS (gnu_type) = nreverse (gnu_literal_list);

	/* We have to be very careful here that we don't get an infinite
	   recursion when we get the bounds of this type, since those bounds
	   are objects of this type.  So set up a temporary definition now
	   and update the precise type later.   */
	gnu_decl = create_type_decl (entity_name, gnu_type);
	save_gnu_tree (gnat_entity, gnu_decl, 0);
	saved = 1;

	TYPE_MIN_VALUE (gnu_type) = gnat_to_gnu (Type_Low_Bound (gnat_entity));
	TYPE_MAX_VALUE (gnu_type)
	  = gnat_to_gnu (Type_High_Bound (gnat_entity));

	/* If we have an enumeration table and we are defining this
	   type, declare the enumeration table.  */
	if (definition && Present (Lit_Name_Table (gnat_entity)))
	  gnat_to_gnu_entity
	    (Lit_Name_Table (gnat_entity),
	     create_enum_initializer
	     (gnat_entity,
	      gnat_to_gnu_type (Etype (Lit_Name_Table (gnat_entity)))),
	     1);
      }
      break;

    case E_Integer_Type:
      /* For integer types, just make a signed type the appropriate number
	 of bits.  */
      if (Esize (gnat_entity) == 0)
	abort ();

      gnu_type = make_signed_type (UI_To_Int (Esize (gnat_entity)));
      break;

    case E_Modular_Type:
      /* For modular types, make the unsigned type of the proper number of
	 bits and then set up the modulus, if required.  */
      {
	int esize;
	enum machine_mode mode;
	tree gnu_modulus;

	if (Esize (gnat_entity) == 0)
	  abort ();

	/* Find the smallest mode at least ESIZE bits wide and make a class
	   using that mode.  */

	esize = UI_To_Int (Esize (gnat_entity));
	for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT);
	     GET_MODE_BITSIZE (mode) < esize;
	     mode = GET_MODE_WIDER_MODE (mode))
	  ;

	gnu_type = make_unsigned_type (GET_MODE_BITSIZE (mode));

	/* Get the modulus in this type.  If it overflows, assume it is because
	   it is equal to 2**Esize.  Note that there is no overflow checking
	   done on unsigned type, so we detect the overflow by looking for
	   a modulus of zero, which is otherwise invalid.  */
	gnu_modulus = UI_To_gnu (Modulus (gnat_entity), gnu_type);
	if (! integer_zerop (gnu_modulus))
	  {
	    TYPE_MODULAR_P (gnu_type) = 1;
	    TYPE_MODULUS (gnu_type) = gnu_modulus;
	  }
      }
      break;

    case E_Integer_Subtype:
    case E_Enumeration_Subtype:
    case E_Modular_Subtype:
      /* For integral subtypes, we make a new INTEGER_TYPE.  Note
	 that we do not want to call build_range_type since we would
	 like each subtype node to be distinct.  This will be important
	 when memory aliasing is implemented.

	 The TREE_TYPE field of the INTEGER_TYPE we make points to the
	 parent type; this fact is used by the arithmetic conversion
	 functions.  */

      gnu_type = make_node (INTEGER_TYPE);
      TREE_TYPE (gnu_type) = gnat_to_gnu_type (Etype (gnat_entity));
      TYPE_PRECISION (gnu_type) = UI_To_Int (Esize (gnat_entity));

      TYPE_MIN_VALUE (gnu_type)
	= elaborate_expression (Type_Low_Bound (gnat_entity),
				gnat_entity, "L", definition);

      TYPE_MAX_VALUE (gnu_type)
	= elaborate_expression (Type_High_Bound (gnat_entity),
				gnat_entity, "U", definition);

      /* This shold be an unsigned type if the lower bound is constant
	 and non-negative; a signed type otherwise.  */
      TREE_UNSIGNED (gnu_type)
	= (TREE_CODE (TYPE_MIN_VALUE (gnu_type)) == INTEGER_CST
	   && TREE_INT_CST_HIGH (TYPE_MIN_VALUE (gnu_type)) >= 0);

      layout_type (gnu_type);
      break;

    case E_Fixed_Type:
    case E_Float_Type:
      /* We handle both floating types and fixed types as floating-point
	 types.  */
      if (Esize (gnat_entity) == 0)
	abort ();

      gnu_type = make_node (REAL_TYPE);
      TYPE_PRECISION (gnu_type) = UI_To_Int (Esize (gnat_entity));
      layout_type (gnu_type);
      break;

    case E_Decimal_Type:
      abort ();

    case E_Fixed_Subtype:
    case E_Float_Subtype:
    case E_Decimal_Subtype:
      /* For each of these subtypes, just use the base type for now.  */
      gnu_type = gnat_to_gnu_type (Etype (gnat_entity));
      break;

    case E_Exception_Type:
      /* This is just a character.  */
      gnu_type = char_type_node;
      break;

      /* Array and String Types and Subtypes

	 Unconstrained array types are represented by E_Array_Type and
	 constrained array types are represented by E_Array_Subtype.  There
	 are no actual objects of an unconstrained array type; all we have
	 are pointers to that type.

	 The following fields are defined on array types and subtypes:

		Component_Type     Component type of the array.
		Number_Dimensions  Number of dimensions (an int).
		First_Index	   Type of first index.  */

    case E_String_Type:
    case E_Array_Type:
      {
	tree gnu_template_fields = NULL_TREE;
	tree gnu_template_type = make_node (RECORD_TYPE);
	tree gnu_ptr_template = build_pointer_type (gnu_template_type);
	tree gnu_fat_type = make_node (RECORD_TYPE);
	int ndim = Number_Dimensions (gnat_entity);
	tree *gnu_index_types = (tree *) alloca (ndim * sizeof (tree *));
	int index;
	gnat_tree gnat_ind_subtype;
	tree gnu_template_reference;
	tree tem;

	/* Build the fat pointer type.  Use a "void *" object instead of
	   a pointer to the array type since we don't have the array type
	   yet (it will reference the fat pointer via the bounds).  */
	tem = chainon (chainon (NULL_TREE,
				create_field_decl ("p_array",
						   ptr_void_type_node,
						   gnu_fat_type, 0)),
		       create_field_decl ("p_template",
					  gnu_ptr_template,
					  gnu_fat_type, 0));

	finish_record_type (gnu_fat_type, tem);

	/* Build a reference to the template from a PLACEHOLDER_EXPR that
	   is the fat pointer.  This will be used to access the individual
	   fields once we build them.  */
	gnu_template_reference
	  = build_unary_op (INDIRECT_REF, gnu_template_type,
			    build (COMPONENT_REF, gnu_ptr_template,
				   build (PLACEHOLDER_EXPR, gnu_fat_type),
				   TREE_CHAIN (TYPE_FIELDS (gnu_fat_type))));

	/* Now create the GCC type for each index and add the fields for
	   that index to the template.  */
	for (index = 0, gnat_ind_subtype = First_Index (gnat_entity);
	     index < ndim;
	     index++, gnat_ind_subtype = Next_Index (gnat_ind_subtype))
	  {
	    char field_name[10];
	    tree gnu_ind_subtype = gnat_to_gnu_type (Etype (gnat_ind_subtype));
	    tree gnu_min_field, gnu_max_field, gnu_min, gnu_max;

	    /* Make the FIELD_DECLs for the minimum and maximum of this
	       type and then make extractions of that field from the
	       template.  */
	    sprintf (field_name, "LB%d", index);
	    gnu_min_field = create_field_decl (field_name, gnu_ind_subtype,
					       gnu_template_type, 0);
	    field_name[0] = 'U';
	    gnu_max_field = create_field_decl (field_name, gnu_ind_subtype,
					       gnu_template_type, 0);

	    gnu_template_fields
	      = chainon (chainon (gnu_template_fields, gnu_min_field),
			 gnu_max_field);

	    /* We can't use build_component_ref here since the template
	       type isn't complete yet.  */
	    gnu_min = build (COMPONENT_REF, gnu_ind_subtype,
			     gnu_template_reference, gnu_min_field);
	    gnu_max = build (COMPONENT_REF, gnu_ind_subtype,
			     gnu_template_reference, gnu_max_field);

	    /* Make a range type with the new ranges, but using
	       the Ada subtype.  Then we convert to sizetype and handle the
	       superflat case.  */
	    gnu_ind_subtype = build_range_type (gnu_ind_subtype,
						gnu_min, gnu_max);

	    gnu_min = convert (sizetype, gnu_min);
	    gnu_max = size_binop (MAX_EXPR, convert (sizetype, gnu_max),
				  size_binop (MINUS_EXPR, gnu_min,
					      size_int (1)));

	    gnu_index_types[index] = build_index_2_type (gnu_min, gnu_max);
	    TYPE_INDEX_TYPE (gnu_index_types[index]) = gnu_ind_subtype;
	  }

	/* Install all the fields into the template.  */
	finish_record_type (gnu_template_type, gnu_template_fields);

	/* Now make the array of arrays and update the pointer to the array
	   in the fat pointer.  Note that it is the first field.  */

	tem = gnat_to_gnu_type (Component_Type (gnat_entity));

	/* If the component type is a RECORD_TYPE that has a self-referential
	   size, make a new RECORD_TYPE whose size is the maximum.  */
	if (TREE_CODE (tem) == RECORD_TYPE
	    && TREE_CODE (TYPE_SIZE (tem)) != INTEGER_CST
	    && contains_placeholder_p (TYPE_SIZE (tem)))
	  {
	    int moment = suspend_momentary ();

	    push_obstacks (TYPE_OBSTACK (tem), TYPE_OBSTACK (tem));
	    tem = copy_node (tem);
	    TYPE_SIZE (tem) = max_size (TYPE_SIZE (tem), 1);
	    pop_obstacks ();
	    resume_momentary (moment);
	  }

	for (index = ndim - 1; index >= 0; index--)
	  tem = build_array_type (tem, gnu_index_types[index]);

	TREE_TYPE (TYPE_FIELDS (gnu_fat_type)) = build_pointer_type (tem);
	TYPE_FAT_POINTER_P (gnu_fat_type) = 1;

	/* The result type is an UNCONSTRAINED_ARRAY_TYPE that indicates the
	   corresponding fat pointer.  */
	gnu_type = make_node (UNCONSTRAINED_ARRAY_TYPE);
	TREE_TYPE (gnu_type) = TYPE_POINTER_TO (gnu_type) = gnu_fat_type;
	TYPE_UNCONSTRAINED_ARRAY (gnu_fat_type) = gnu_type;
      }
      break;

    case E_String_Subtype:
    case E_Array_Subtype:
      /* This is the actual data type for array variables.  Multidimensional
	 arrays are implemented in the gnu tree as arrays of arrays.  Note
	 that for the moment arrays which have sparse enumeration subtypes as
	 index components create sparse arrays, which is obviously space
	 inefficient but so much easier to code for now. 

	 Also note that the subtype never refers to the unconstrained
	 array type, which is somewhat at variance with Ada semantics. 

	 First check to see if this is simply a renaming of the array
	 type.  If so, the result is the array type.  */
      if (! Is_Constrained (gnat_entity))
	gnu_type = gnat_to_gnu_type (Etype (gnat_entity));
      else
	{
	  int index;
	  int array_dim = Number_Dimensions (gnat_entity);
	  gnat_tree gnat_ind_subtype;
	  tree *gnu_index_type = (tree *) alloca (array_dim * sizeof (tree *));

	  /* First create the gnu types for each index.  */

	  for (index = 0, gnat_ind_subtype = First_Index (gnat_entity);
	       index < array_dim;
	       index++, gnat_ind_subtype = Next_Index (gnat_ind_subtype))
	    {
	      tree gnu_index_subtype
		= gnat_to_gnu_type (Etype (gnat_ind_subtype));
	      tree gnu_min
		= convert (sizetype, TYPE_MIN_VALUE (gnu_index_subtype));
	      tree gnu_max
		= convert (sizetype, TYPE_MAX_VALUE (gnu_index_subtype));

	      gnu_index_type[index]
		= build_index_2_type (gnu_min,
				      size_binop (MAX_EXPR, gnu_max,
						  size_binop (MINUS_EXPR,
							      gnu_min,
							      size_int (1))));

	      /* If TYPE_INDEX_TYPE is already set, it must be the proper
		 type.  This could only happen if this node was reused via
		 the type hashing mechanism in tree.c.  Don't set the index
		 type again in case we are now in a different obstack from that
		 of the actual type.  We don't worry about the slight memory
		 leak here.  */

	      if (TYPE_INDEX_TYPE (gnu_index_type[index]) == 0)
		TYPE_INDEX_TYPE (gnu_index_type[index]) = gnu_index_subtype;
	    }

	  /* Then flatten: create the array of arrays.  */

	  gnu_type = gnat_to_gnu_type (Component_Type (gnat_entity));

	  /* If the component type is a RECORD_TYPE that has a self-referential
	     size, make a new RECORD_TYPE whose size is the maximum.  */
	  if (TREE_CODE (gnu_type) == RECORD_TYPE
	      && TREE_CODE (TYPE_SIZE (gnu_type)) != INTEGER_CST
	      && contains_placeholder_p (TYPE_SIZE (gnu_type)))
	    {
	      int moment = suspend_momentary ();

	      push_obstacks (TYPE_OBSTACK (gnu_type), TYPE_OBSTACK (gnu_type));
	      gnu_type = copy_node (gnu_type);
	      TYPE_SIZE (gnu_type) = max_size (TYPE_SIZE (gnu_type), 1);
	      pop_obstacks ();
	      resume_momentary (moment);
	    }

	  for (index = array_dim - 1; index >= 0; index --)
	    gnu_type = build_array_type (gnu_type, gnu_index_type[index]);
	}
      break;

    case E_Slice_Subtype:
      /* This is an abbreviated way of making an array subtype.  See
	 a-einfo.h for details.  */
      {
	tree gnu_comp_type = gnat_to_gnu_type (Component_Type (gnat_entity));
	tree gnu_min
	  = maybe_variable
	    (gnat_to_gnu (Low_Bound (Slice_Range (gnat_entity))));
	tree gnu_max
	  = maybe_variable
	    (gnat_to_gnu (High_Bound (Slice_Range (gnat_entity))));
	tree gnu_range_type 
	  = build_range_type (TREE_TYPE (gnu_min), gnu_min, gnu_max);
	tree gnu_index_min = convert (sizetype, gnu_min);
	tree gnu_index_max = size_binop (MAX_EXPR, convert (sizetype, gnu_max),
					 size_binop (MINUS_EXPR, gnu_min,
						     size_int (1)));
	tree gnu_index_type
	  = build_index_2_type (gnu_index_min, gnu_index_max);

	/* If TYPE_INDEX_TYPE is already set, it must be the proper type.  This
	   could only happen if this node was reused via the type hashing
	   mechanism in tree.c.  Don't set the index type again in case we
	   are now in a different obstack from that of the actual type.  We
	   don't worry about the slight memory leak here.  */

	if (TYPE_INDEX_TYPE (gnu_index_type) == 0)
	  TYPE_INDEX_TYPE (gnu_index_type) = gnu_range_type;

	/* If the component type is a RECORD_TYPE that has a self-referential
	   size, make a new RECORD_TYPE whose size is the maximum.  */
	if (TREE_CODE (gnu_comp_type) == RECORD_TYPE
	    && TREE_CODE (TYPE_SIZE (gnu_comp_type)) != INTEGER_CST
	    && contains_placeholder_p (TYPE_SIZE (gnu_comp_type)))
	  {
	    int moment = suspend_momentary ();

	    push_obstacks (TYPE_OBSTACK (gnu_comp_type),
			   TYPE_OBSTACK (gnu_comp_type));
	    gnu_type = copy_node (gnu_comp_type);
	    TYPE_SIZE (gnu_comp_type)
	      = max_size (TYPE_SIZE (gnu_comp_type), 1);
	    pop_obstacks ();
	    resume_momentary (moment);
	  }

	gnu_type = build_array_type (gnu_comp_type, gnu_index_type);
      }
      break;

    case E_String_Literal_Subtype:
      /* Create the type for a string literal. */
      {
	tree gnu_string_type = gnat_to_gnu_type (Etype (gnat_entity));
	tree gnu_string_array_type
	  = TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_TYPE (gnu_string_type))));
	tree gnu_string_index_type
	  = TREE_TYPE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_string_array_type)));
	tree gnu_upper_bound = UI_To_gnu (String_Literal_Length (gnat_entity),
					  gnu_string_index_type);
	tree gnu_range_type
	  = build_range_type (gnu_string_index_type,
			      convert (gnu_string_index_type,
				       integer_one_node),
			      gnu_upper_bound);
	tree gnu_index_type
	  = build_index_2_type (convert (sizetype,
					 TYPE_MIN_VALUE (gnu_range_type)),
				convert (sizetype,
					 TYPE_MAX_VALUE (gnu_range_type)));

	if (TYPE_INDEX_TYPE (gnu_index_type) == 0)
	  TYPE_INDEX_TYPE (gnu_index_type) =  gnu_range_type;

	gnu_type
	  = build_array_type (gnat_to_gnu_type (Component_Type (gnat_entity)),
			      gnu_index_type);
      }
      break;

    case E_Enum_Table_Type:
      /* Create the type for an enumeration literal table.  */
      {
	tree gnu_high_bound = gnat_to_gnu (Table_High_Bound (gnat_entity));
	tree gnu_range_type
	  = build_range_type (TREE_TYPE (gnu_high_bound),
			      convert (TREE_TYPE (gnu_high_bound),
				       integer_zero_node),
			      gnu_high_bound);
	tree gnu_index_type
	  = build_index_2_type (convert (sizetype,
					 TYPE_MIN_VALUE (gnu_range_type)),
				convert (sizetype,
					 TYPE_MAX_VALUE (gnu_range_type)));

	if (TYPE_INDEX_TYPE (gnu_index_type) == 0)
	  TYPE_INDEX_TYPE (gnu_index_type) = gnu_range_type;

	gnu_type
	  = build_array_type (gnat_to_gnu_type (Component_Type (gnat_entity)),
			      gnu_index_type);
      }
      break;

    /* Record Types and Subtypes

       The following fields are defined on record types:

		Has_Discriminants	True if the record has discriminants
		First_Discriminant	Points to head of list of discriminants
		First_Entity		Points to head of list of fields
		Is_Tagged_Type		True if the record is tagged

       Implementation of Ada records and discriminated records:

       A record type definition is transformed into the equivalent of a C
       struct definition.  The fields that are the discriminants which are
       found in the Full_Type_Declaration node and the elements of the
       Component_List found in the Record_Type_Definition node.  The
       Component_List can be a recursive structure since each Variant of
       the Variant_Part of the Component_List has a Component_List.

       Processing of a record type definition comprises starting the list of
       field declarations here from the discriminants and the calling the
       function components_to_record to add the rest of the fields from the
       component list and return the gnu type node. The function
       components_to_record will call itself recursively as it traverses
       the tree.  */

    case E_Record_Type:
      {
	gnat_tree gnat_impl_type;
        gnat_tree full_definition = Parent (gnat_entity);
        gnat_tree record_definition = Type_Definition (full_definition);
	gnat_tree gnat_field;
        tree gnu_field;
	char *field_id;
	tree gnu_field_type;
	tree gnu_field_list = NULL_TREE;
	int packed = Is_Packed (gnat_entity);

	/* If this is a record extension, go a level further to find the 
	   record definition */
	if (Nkind (record_definition) == N_Derived_Type_Definition)
	  record_definition = Record_Extension_Part (record_definition); 

	/* Make a node for the record.  If we are not defining the record,
	   suppress expanding incomplete types and save the node as the type
	   for GNAT_ENTITY..  */
	gnu_type = make_node (RECORD_TYPE);
	if (! definition)
	  {
	    defer_incomplete_level++;
	    gnu_decl = create_type_decl (entity_name, gnu_type);
	    save_gnu_tree (gnat_entity, gnu_decl, 0);
	    saved = 1;
	  }

	/* Add the fields for the discriminants into the record.  */
        if (Has_Discriminants (gnat_entity))
          {
	    for (gnat_field = First_Discriminant (gnat_entity);
		 Present (gnat_field);
		 gnat_field = Next_Discriminant (gnat_field))
	      {
		field_id = Get_Name_String (Chars (gnat_field));
		gnu_field_type = gnat_to_gnu_type (Etype (gnat_field));
		gnu_field = create_field_decl (field_id, gnu_field_type,
					       gnu_type, packed);
		DECL_DISCRIMINANT_P (gnu_field) = 1;

		/* Associate the FIELD_DECL node just created with the
		   corresponding gnat defining identifier.  */
		save_gnu_tree (gnat_field, gnu_field, 0);

		gnu_field_list = chainon (gnu_field, gnu_field_list);
	      }
	  }

	/* Process any implicit type definitions used within this record.
	   They may depend on the discriminants, and the following components
	   may depend on them. */
	if (definition && Present (Implicit_Types (record_definition)))
	  for (gnat_impl_type = First (Implicit_Types (record_definition));
	       gnat_impl_type; gnat_impl_type = Next (gnat_impl_type))
	    gnat_to_code (gnat_impl_type);

	/* Add the listed fields into the record and finish up.  */
	components_to_record (gnu_type, Component_List (record_definition),
			      gnu_field_list, packed);

	/* If it is a tagged record force the type to BLKmode to insure
	   that these objects will always be placed in memory.

	   ??? This is probably wrong and we need to understand
	   precisely how we are using these types.  */
        if (Is_Tagged_Type (gnat_entity))
	  TYPE_MODE (gnu_type) = BLKmode;

	/* If we are not defining this record, re-enable processing of
	   incomplete types.  If there were no other disables and we have
	   some to process, do so.  */
	if (! definition && --defer_incomplete_level == 0
	    && defer_incomplete_list != 0)
	  {
	    struct incomplete *p = defer_incomplete_list;

	    defer_incomplete_list = 0;
	    for (; p; p = p->next)
	      {
		TREE_TYPE (p->access_type) = gnat_to_gnu_type (p->full_type);
		TYPE_POINTER_TO (TREE_TYPE (p->access_type)) = p->access_type;
	      }
	  }
      }
      break;

    case E_Record_Subtype:
      /* Create the gnu subtype from the gnu type by calling
	 substitute_in_type for each discriminant expresion.  This function
	 returns a new tree from the type tree by substituting the discriminant
	 expression for the subtype for the occurences of the discriminant in
	 the base type definition.  */
      {
	gnat_tree gnat_discriminant_expr;
	gnat_tree gnat_field;

	gnu_type = TREE_TYPE (
	     gnat_to_gnu_entity (Base_Type (gnat_entity), NULL_TREE, 0));
	if (Present (Discriminant_Constraint (gnat_entity)))
	  for (gnat_field = First_Discriminant (Base_Type (gnat_entity)),
	       gnat_discriminant_expr
	       = First_Elmt (Discriminant_Constraint (gnat_entity));
	       Present (gnat_field);
	       gnat_field = Next_Discriminant (gnat_field),
	       gnat_discriminant_expr = Next_Elmt (gnat_discriminant_expr))
	    gnu_type
	      = gnat_substitute_in_type
		(gnu_type, get_gnu_tree (gnat_field),
		 elaborate_expression (Id_Of (gnat_discriminant_expr),
				       gnat_entity,
				       Get_Name_String (Chars (gnat_field)),
				       definition));
      }
      break;

    case E_Access_Type:
    case E_Anonymous_Access_Type:
    case E_Access_Subprogram_Type:
    case E_Access_Subtype:
    case E_Allocator_Type:
    case E_General_Access_Type:
      /* Get the type of the thing we are to point to and build
	 a pointer to it. */

      gnu_type
	= build_pointer_type
	  (gnat_to_gnu_type (Directly_Designated_Type (gnat_entity)));

      /* If this is a reference (not a definition) to an incomplete
	 type, save our current definition, evaluate the actual type,
	 and replace the tentative type we made with the actual one. 
	 If we are to defer actually looking up the actual type, make an
	 entry in the deferred list.  */

      if (! definition
	  && (IN (Ekind (Directly_Designated_Type (gnat_entity)),
		  Incomplete_Kind)))
	{
	  /* It is possible that the above call to gnat_to_gnu_type
	     resolved our type.  If so, just return it.  */
	  if (present_gnu_tree (gnat_entity))
	    return get_gnu_tree (gnat_entity);

	  /* Otherwise, make our type and do the processing
	     described above.  */
	  gnu_decl = create_type_decl (entity_name, gnu_type);
	  save_gnu_tree (gnat_entity, gnu_decl, 0);
	  saved = 1;

	  if (defer_incomplete_level == 0)
	    {
	      TREE_TYPE (gnu_type)
		= gnat_to_gnu_type (Full_Declaration
				    (Directly_Designated_Type (gnat_entity)));
	      TYPE_POINTER_TO (TREE_TYPE (gnu_type)) = gnu_type;
	    }
	  else
	    {
	      struct incomplete *p
		= (struct incomplete *) oballoc (sizeof (struct incomplete));

	      p->access_type = gnu_type;
	      p->full_type
		= Full_Declaration (Directly_Designated_Type (gnat_entity));
	      p->next = defer_incomplete_list;
	      defer_incomplete_list = p;
	    }
	}
      break;

    /* Subprogram Entities

       The following access functions are defined for subprograms (functions 
       or procedures):

		First_Formal	The first formal parameter.
		Is_Imported     Indicates that the subprogram has appeared in
				an INTERFACE or IMPORT pragma. For now we  
				assume that the external language is C.
		Is_Inlined      True if the subprogram is to be inlined.

       In addition for function subprograms we have:

		Etype       	Return type of the function.

       Each parameter is first checked by calling pass_by_ref on its type to
       determine if it is passed by reference.  For parameters which are copied
       in, if they are Ada IN OUT or OUT parameters, their return value becomes
       part of a record which becomes the return type of the function (C
       function - note that this applies only to Ada procedures so there is no
       Ada return type). Additional code to store back the parameters will be
       generated on the caller side.  This transformation is done here, not in
       the front-end.

       The intended result of the transformation can be seen from the
       equivalent source rewritings that follow:

                                                   struct temp {int a,b};
       procedure P (A,B: IN OUT ...) is            temp P (int A,B) {
        ..                                            ..
       end P;                                        return {A,B};
                                                   }
                              procedure call

                                              {
                                                  temp t;
       P(X,Y);                                    t = P(X,Y);
                                                  X = t.a , Y = t.b;
                                              }

       For subprogram types we need to perform mainly the same conversions to
       GCC form that are needed for procedures and function declarations.  The
       only difference is that at the end, we make a type declaration instead
       of a function declaration.  */

    case E_Subprogram_Type:
    case E_Function:
    case E_Procedure:
      {
	/* The first GCC parameter declaration (a PARM_DECL node).  The
	   PARM_DECL nodes are chained through the TREE_CHAIN field, so this
	   actually is the head of this parameter list.  */
	tree gnu_param_list = NULL_TREE;
	/* The type returned by a function. If the subprogram is a procedure
	   this type should be void_type_node.  */
	tree gnu_return_type = void_type_node;
        /* List of fields in return type of procedure with copy in copy out
	   parameters.  */
        tree gnu_field_list = NULL_TREE;
	/* Non-null for subprograms containing  parameters passed by copy in
	   copy out (Ada IN OUT or OUT parameters not passed by reference),
	   in which case it is the list of nodes used to specify the values of
	   the in out/out parameters that are returned as a record upon
	   procedure return.  The TREE_PURPOSE of an element of this list is
	   a field of the record and the TREE_VALUE is the PARM_DECL
	   corresponding to that field.  This list will be saved in the
	   TYPE_CI_CO_LIST field of the FUNCTION_TYPE node we create.  */
	tree gnu_return_list = NULL_TREE;
	gnat_tree gnat_param;
	int inline_flag = Is_Inlined (gnat_entity);
	int public_flag = Is_Public (gnat_entity);
	int extern_flag
	  = ((Is_Public (gnat_entity) && !definition)
	     || Is_Imported (gnat_entity));
	int pure_flag = Is_Pure (gnat_entity);
	int returns_unconstrained = 0;
	char *ext_name = NULL;
	int copy_in_copy_out_flag;
	int has_copy_in_out = 0;

	/* If the subprogram has an alias, it is probably inherited, so
	   we can use the original one */
	if (Present (Alias (gnat_entity)))
	  {
	    gnu_decl = gnat_to_gnu_entity (Alias (gnat_entity),
					   gnu_expr, definition);
	    break;
	  }

	if (kind == E_Function || kind == E_Subprogram_Type)
	  gnu_return_type = gnat_to_gnu_type (Etype (gnat_entity));

	/* If we are supposed to return an unconstrained array,
	   actually return a fat pointer and make a note of that.  Return
	   a pointer to an unconstrained record of variable size.  */
	if (TREE_CODE (gnu_return_type) == UNCONSTRAINED_ARRAY_TYPE)
	  {
	    gnu_return_type = TREE_TYPE (gnu_return_type);
	    returns_unconstrained = 1;
	  }
	else if (TREE_CODE (TYPE_SIZE (gnu_return_type)) != INTEGER_CST
		 && contains_placeholder_p (TYPE_SIZE (gnu_return_type)))
	  {
	    gnu_return_type = build_pointer_type (gnu_return_type);
	    returns_unconstrained = 1;
	  }

	/* Look at all our parameters and get the type of
	   each.  While doing this, build a copy-out structure if
	   we need one.  */

	for (gnat_param = First_Formal (gnat_entity);
	     Present (gnat_param);
	     gnat_param = Next_Formal (gnat_param))
	  {
	    char *param_name = Get_Name_String (Chars (gnat_param));
	    tree gnu_param_type = gnat_to_gnu_type (Etype (gnat_param));
	    tree gnu_param, gnu_field;
	    int by_ref_p = 0;

            if (pass_by_ref (gnu_param_type))
	      {
		/* All parameters are passed by value by GCC. So to pass a
		   parameter by reference we need to pass a pointer to it.  */
		gnu_param_type = build_pointer_type (gnu_param_type);
		copy_in_copy_out_flag = 0;
		by_ref_p = 1;
	      }
            else
	      copy_in_copy_out_flag = (Ekind (gnat_param) != E_In_Parameter);

	    /* If this is an OUT parameter that isn't passed by reference
	       and isn't a pointer, we don't make a PARM_DECL for it.
	       Instead, it will be a VAR_DECL created when we process the
	       procedure.  */
	    if (Ekind (gnat_param) == E_Out_Parameter && ! by_ref_p
		&& TREE_CODE (gnu_param_type) != POINTER_TYPE)
	      gnu_param = 0;
	    else
	      {
		gnu_param = create_param_decl (param_name, gnu_param_type);
		DECL_BY_REF_P (gnu_param) = by_ref_p;
		save_gnu_tree (gnat_param, gnu_param, 0);
		gnu_param_list = chainon (gnu_param, gnu_param_list);
	      }

            if (copy_in_copy_out_flag)
	      {
		if (! has_copy_in_out)
		  {
		    if (TREE_CODE (gnu_return_type) != VOID_TYPE)
		      abort ();

		    gnu_return_type = make_node (RECORD_TYPE);
		    has_copy_in_out = 1;
		  }

		gnu_field = create_field_decl (param_name, gnu_param_type,
					       gnu_return_type, 0);
		gnu_field_list = chainon (gnu_field, gnu_field_list);
		gnu_return_list = tree_cons (gnu_field, gnu_param,
					     gnu_return_list);
	      }
	  }

	if (gnu_field_list != 0)
	  finish_record_type (gnu_return_type, nreverse (gnu_field_list));

	/* If we have a CICO list but it has only one entry, we convert
	   this function into a function that simply returns that one
	   object.  */
	if (list_length (gnu_return_list) == 1)
	  gnu_return_type = TREE_TYPE (TREE_PURPOSE (gnu_return_list));

	/* Both lists ware built in reverse.  */
	gnu_param_list = nreverse (gnu_param_list);
	gnu_return_list = nreverse (gnu_return_list);

	gnu_type = create_subprog_type (gnu_return_type, gnu_param_list,
					gnu_return_list);
	TYPE_RETURNS_UNCONSTRAINED_P (gnu_type) = returns_unconstrained;

	/* Top-level or external functions need to have an assembler name.
	   This is passed to create_subprog_decl through the ext_name argument.
	   For Pragma Interface subprograms with no Pragma Interface_Name,
	   the simple name already in entity_name is correct, and this is
	   what is gotten when ext_name is NULL.  If Interface_Name is
	   specified, then the name is extracted from the N_String_Literal
	   node containing the string specified in the Pragma.  If there is
	   no Pragma Interface, then the Ada fully qualified name (modified
	   for overloading) is created by get_external_name. */

	if (Present (Interface_Name (gnat_entity)))
	  {
	    String_Id gnat_string = Strval (Interface_Name (gnat_entity));
	    int length = String_Length (gnat_string);
	    int i;

	    ext_name = (char *) alloca (length + 1);
	    for (i = 0; i < length; i++)
	      ext_name[i] = Get_String_Char (gnat_string, i + 1);
	    ext_name[i] = 0;
	  }
	else if (Is_Imported (gnat_entity))
	  ;
	else if (global_bindings_p () || ! definition)
	  ext_name = get_external_name (gnat_entity);

	/* If our name is that of an operator, and we don't have an external
	   name, make an external name to be the converted name of the
	   operator.  Note that when we have an external name,
	   get_external_name has already done this conversion.  */

	if (entity_name[0] == '"' && ext_name == 0)
	  ext_name = get_operator_name (entity_name);

        if (kind == E_Subprogram_Type)
          gnu_decl = create_type_decl (entity_name, gnu_type);
        else
	  gnu_decl = create_subprog_decl (entity_name, ext_name, gnu_type,
				  	  gnu_param_list, inline_flag,
					  public_flag, extern_flag, pure_flag);
      }
      break;

    case E_Incomplete_Type:
    case E_Private_Type:
    case E_Limited_Private_Type:
    case E_Limited_Type:
      /* If we are not defining this type, go directly to the full
	 declaration.  */
      if (! definition)
	return
	  gnat_to_gnu_entity (Full_Declaration (gnat_entity), NULL_TREE, 0);

      /* For incomplete types, make a dummy VOID_TYPE entry which will be
	 replaced later.  */
      gnu_type = make_node (VOID_TYPE);

      /* Save this type as the full declaration's type so we can do any needed
	 updates when we see it.  */
      if (Present (Full_Declaration (gnat_entity)))
	{
	  gnu_decl = create_type_decl (entity_name, gnu_type);
	  save_gnu_tree (Full_Declaration (gnat_entity), gnu_decl, 0);
	}
      break;

    case E_Class_Type:
      /* We consider a Class type as the Root type of the Class.  This is a 
	 simple way to implement view-conversion. */
      gnu_decl = gnat_to_gnu_entity (Etype (gnat_entity), gnu_expr, 0);
      break;

    case E_Class_Subtype:
      /* a Class Subtype is a Class type with a particular size (it is used 
	 to allocate class-wide object as a copy of another object). The 
         front-end provides a record equivalent type for it in field 
         Equivalent_Type */

      gnu_decl = gnat_to_gnu_entity 
	(Equivalent_Type (gnat_entity), gnu_expr, 0);
      break;

    case E_Task_Type:
    case E_Task_Subtype:
      gnu_type = gnat_to_gnu_type (Task_Value_Type (gnat_entity));
      break;

    case E_Protected_Type:
      abort ();

    case E_Label:
      gnu_decl = create_label_decl (entity_name);
      break;

    case E_Block:
    case E_Loop:
      /* Nothing at all to do here, so just return an ERROR_MARK and claim
	 we've already saved it, so we don't try to.  */
      gnu_decl = error_mark_node;
      saved = 1;
      break;

    default:
      abort ();
    }

  if (gnu_decl == 0 && IN (kind, Type_Kind))
    gnu_decl = create_type_decl (entity_name, gnu_type);

  /* If we haven't already, associate the ..._DECL node that we just made with
     the input GNAT entity node. */
  if (! saved)
    save_gnu_tree (gnat_entity, gnu_decl, 0);

  /* Restore our previous allocation, if not previously permanent and we
     changed it.  */
  if (! definition && ! was_permanent)
    pop_obstacks ();
  resume_momentary (was_momentary);

  return gnu_decl;
}

/* Given GNAT_ENTITY, elaborate all expressions that are required to
   be elaborated at the point of its definition, but do nothing else.  */

void
elaborate_entity (gnat_entity)
     gnat_tree gnat_entity;
{
  switch (Ekind (gnat_entity))
    {
    case E_Integer_Subtype:
    case E_Modular_Subtype:
    case E_Enumeration_Subtype:
      /* We have to elaborate the expressions for the low and high bounds
	 unless the expression is a discriminant.  */
      if (Nkind (Type_Low_Bound (gnat_entity)) != N_Identifier
	  || Ekind (Entity (Type_Low_Bound (gnat_entity))) != E_Discriminant)
	elaborate_expression (Type_Low_Bound (gnat_entity), gnat_entity,
			      "L", 1);

      if (Nkind (Type_High_Bound (gnat_entity)) != N_Identifier
	  || Ekind (Entity (Type_High_Bound (gnat_entity))) != E_Discriminant)
	elaborate_expression (Type_High_Bound (gnat_entity), gnat_entity,
			      "U", 1);
      break;

    case E_Record_Type:
      {
        gnat_tree full_definition = Parent (gnat_entity);
	gnat_tree record_definition = Type_Definition (full_definition);
	gnat_tree gnat_impl_type;

	/* If this is a record extension, go a level further to find the 
	   record definition */
	if (Nkind (record_definition) == N_Derived_Type_Definition)
	  record_definition = Record_Extension_Part (record_definition); 

	if (Present (Implicit_Types (record_definition)))
	  for (gnat_impl_type = First (Implicit_Types (record_definition));
	       gnat_impl_type; gnat_impl_type = Next (gnat_impl_type))
	    elaborate_entity (gnat_impl_type);
      }
      break;

    case E_Record_Subtype:
      if (Present (Discriminant_Constraint (gnat_entity)))
	{
	  gnat_tree gnat_discriminant_expr;
	  gnat_tree gnat_field;

	  for (gnat_field = First_Discriminant (Base_Type (gnat_entity)),
	       gnat_discriminant_expr
	       = First_Elmt (Discriminant_Constraint (gnat_entity));
	       Present (gnat_field);
	       gnat_field = Next_Discriminant (gnat_field),
	       gnat_discriminant_expr = Next_Elmt (gnat_discriminant_expr))
	    elaborate_expression (Id_Of (gnat_discriminant_expr),
				  gnat_entity, Get_Name_String (gnat_field),
				  1);
	}
      break;

    }
}

/* EXP may be a FIELD_DECL.  If so, make the appropriate COMPONENT_REF
   involving a PLACEHOLDER_EXPR.

   This function must be called whenever we have something that is allowed to
   be a discriminant.  */

static tree
maybe_placeholder (exp)
     tree exp;
{
  if (TREE_CODE (exp) == FIELD_DECL)
    return build (COMPONENT_REF, TREE_TYPE (exp),
		  build (PLACEHOLDER_EXPR, DECL_CONTEXT (exp)),
		  exp);

  return exp;
}

/* Called when we need to protect a variable object using a save_expr.  */

tree
maybe_variable (operand)
     tree operand;
{
  if (TREE_CODE (operand) == INTEGER_CST)
    return operand;
  else if (TREE_CODE (operand) == UNCONSTRAINED_ARRAY_REF)
    return build1 (UNCONSTRAINED_ARRAY_REF, TREE_TYPE (operand),
		   variable_size (TREE_OPERAND (operand, 0)));
  else
    return variable_size (operand);
}

/* Given a GNAT tree GNAT_EXPR, for an expression which is a value within a
   type definition (either a bound or a discriminant value) for GNAT_ENTITY,
   return the GCC tree to use for that expression.  NAME is the qualification
   to use if an external name is appropriate and DEFINITION is nonzero
   if this is a definition of GNAT_ENTITY.  */

static tree
elaborate_expression (gnat_expr, gnat_entity, name, definition)
     gnat_tree gnat_expr;
     gnat_tree gnat_entity;
     char *name;
     int definition;
{
  tree gnu_expr;

  /* If we already elaborated this expression (e.g., it was involved
     in the definition of a private type), use the old value.  */
  if (present_gnu_tree (gnat_expr))
    return get_gnu_tree (gnat_expr);

  /* Otherwise, convert this tree to its GCC equivalant, handling any
     references to a discriminant.  */
  gnu_expr = maybe_placeholder (gnat_to_gnu (gnat_expr));

  /* If this entity is defined at top level and a bound or discriminant
     value isn't a constant or a reference to a discriminant, replace the
     bound by a variable that will be initialized to contain the bound when
     the package containing the definition is elaborated.  Note that we rely
     here on the fact that an expression cannot contain both the discriminant
     and some other variable.  */

  if ((Is_Public (gnat_entity) || global_bindings_p ())
      && ! TREE_CONSTANT (gnu_expr) && ! contains_placeholder_p (gnu_expr))
    gnu_expr = create_var_decl (create_concat_name (gnat_entity, name),
				NULL_PTR, TREE_TYPE (gnu_expr), gnu_expr,
				0, Is_Public (gnat_entity),
				! definition, 0, 0);
  else
    gnu_expr = maybe_variable (gnu_expr);

  /* Save the expression in case we try to elaborate this entity again. 
     Since this is not a DECL, don't check it.  */
  save_gnu_tree (gnat_expr, gnu_expr, 1);

  return gnu_expr;
}

/* Given a GNU tree and a GNAT list of choices, generate an expression to test
   the value passed against the list of choices.  */

tree
choices_to_gnu (operand, choices)
     tree operand;
     gnat_tree choices;
{
  gnat_tree choice;
  tree result = integer_zero_node;
  tree this_test, low, high;

  for (choice = First (choices); Present (choice); choice = Next (choice))
    {
      switch (Nkind (choice))
	{
	case N_Range:
	  low = gnat_to_gnu (Low_Bound (choice));
	  high = gnat_to_gnu (High_Bound (choice));

	  /* There's no good type to use here, so we might as well use
	     integer_type_node.  */
	  this_test
	    = build_binary_op (TRUTH_ANDIF_EXPR, integer_type_node,
			       build_binary_op (GE_EXPR, integer_type_node,
						operand, low),
			       build_binary_op (LE_EXPR, integer_type_node,
						operand, high));
	  break;

	case N_Identifier:
	  /* This represents either a subtype range or an enumeration
	     literal.  Ekind says which.  If an enumeration literal,
	     fall through to the next case.  */
	  if (Ekind (Entity (choice)) != E_Enumeration_Literal)
	    {
	      tree type = gnat_to_gnu_type (Entity (choice));

	      low = TYPE_MIN_VALUE (type);
	      high = TYPE_MAX_VALUE (type);

	      this_test
		= build_binary_op (TRUTH_ANDIF_EXPR, integer_type_node,
				   build_binary_op (GE_EXPR, integer_type_node,
						    operand, low),
				   build_binary_op (LE_EXPR, integer_type_node,
						    operand, high));
	      break;
	    }
	  /* ... fall through ... */
	case N_Character_Literal:
	case N_Integer_Literal:
	  this_test = build_binary_op (EQ_EXPR, integer_type_node, operand,
				       gnat_to_gnu (choice));
	  break;

	case N_Others_Choice:
	  this_test = integer_one_node;
	  break;

	default:
	  abort ();
	}

      result = build_binary_op (TRUTH_ORIF_EXPR, integer_type_node,
				result, this_test);
    }

  return result;
}

/* Return a GCC tree for a record type given a GNAT Component_List and a chain
   of GCC trees for fields that are in the record and have already been
   processed.  When called from gnat_to_gnu_entity during the processing of a
   record type definition, the GCC nodes for the discriminants will be on
   the chain.  The other calls to this function are recursive calls from
   itself for the Component_List of a variant and the chain is empty. 

   PACKED is nonzero if this field is for a record with "pragma pack". 

   The processing of the component list fills in the chain with all of the
   fields of the record and then the record type is created.  */

static void
components_to_record (record_type, component_list, gnu_field_list, packed)
     tree record_type;
     gnat_tree component_list;
     tree gnu_field_list;
{
  gnat_tree component_decl;
  gnat_tree gnat_field;
  gnat_tree variant_part;

  /* For each variable within each component declaration create a GCC field
     and add it to the list.  */

  if (Present (Component_Declarations (component_list)))
    for (component_decl = First (Component_Declarations (component_list));
	 Present (component_decl);
	 component_decl = Next (component_decl))
      {
	gnat_tree gnat_field = Defining_Identifier (component_decl);
	char *field_id = Get_Name_String (Chars (gnat_field));
	tree gnu_field_type = gnat_to_gnu_type (Etype (gnat_field));
	tree gnu_field = create_field_decl (field_id, gnu_field_type,
						record_type, packed);

	/* If this is the _Parent field, we have two things to do.  First, we
	   put the first before any discriminants, instead of after them as is
	   the case for all other fields.  Second, we check for the case where
	   the field is a self-referential type.  If it is, it will be
	   referencing discriminants that appear later in the record and hence
	   depend on its size.  In that case, go back to the base type of the
	   field and replace all discriminants with a reference to the parent
	   within RECORD_TYPE.  */
	if (Chars (gnat_field) == Name_uParent)
	  { 
	    DECL_PARENT_P (gnu_field) = 1;
	    gnu_field_list = chainon (gnu_field_list, gnu_field);

	    if (! TREE_CONSTANT (TYPE_SIZE (gnu_field_type))
		&& contains_placeholder_p (TYPE_SIZE (gnu_field_type)))
	      {
		gnat_tree gnat_base_type = Base_Type (Etype (gnat_field));
		tree gnu_new_type = gnat_to_gnu_type (gnat_base_type);
		tree gnu_this_parent
		  = build (COMPONENT_REF, NULL_TREE,
			   build (PLACEHOLDER_EXPR, record_type),
			   gnu_field);
		gnat_tree gnat_discrim;

		for (gnat_discrim = First_Discriminant (gnat_base_type);
		     Present (gnat_discrim);
		     gnat_discrim = Next_Discriminant (gnat_discrim))
		  {
		    tree gnu_discrim = get_gnu_tree (gnat_discrim);

		    gnu_new_type
		      = substitute_in_type
			(gnu_new_type, gnu_discrim,
			 build (COMPONENT_REF, TREE_TYPE (gnu_discrim),
				gnu_this_parent, gnu_discrim));
		  }

		/* Save the old type of the parent for when we make a subtype
		   of this record type.   Then set the new typoe for the
		   field.  */
		TYPE_PARENT_SUBTYPE (record_type) = TREE_TYPE (gnu_field);
		TREE_TYPE (gnu_field) = TREE_TYPE (gnu_this_parent)
		  = gnu_new_type;
	      }
	  }
	else
	  gnu_field_list = chainon (gnu_field, gnu_field_list);

	save_gnu_tree (gnat_field, gnu_field, 0);
      }

  /* At the end of the component list there may be a variant part. We create a
     QUAL_UNION_TYPE for it since the variants are mutually exclusive and
     should go in the same memory.  To do this we need to treat each  variant
     as a record whose elements are created from the component list for the
     variant.  So here we create the records from the lists for the variants
     and put them all into the QUAL_UNION_TYPE.  */

  variant_part = Variant_Part (component_list);
  if (Present (variant_part))
    {
      tree gnu_discriminant
	= maybe_placeholder (gnat_to_gnu (Name (variant_part)));
      gnat_tree variant;
      tree gnu_field;
      tree gnu_union_type = make_node (QUAL_UNION_TYPE);
      tree gnu_union_field;
      tree gnu_variant_list = NULL_TREE;

      for (variant = First (Variants (variant_part)); Present (variant);
	   variant = Next (variant))
	{
	  tree gnu_variant_type = make_node (RECORD_TYPE);

	  components_to_record (gnu_variant_type,
				Component_List (variant), NULL_TREE, packed);
	  gnu_field = create_field_decl (NULL, gnu_variant_type,
					 gnu_union_type, packed);
	  /* The last choice should always be "Others".  */
	  DECL_QUALIFIER (gnu_field)
	    = (Present (Next (variant))
	       ? choices_to_gnu (gnu_discriminant, Discrete_Choices (variant))
	       : integer_one_node);
	  gnu_variant_list = chainon (gnu_field, gnu_variant_list);
	}

      finish_record_type (gnu_union_type, nreverse (gnu_variant_list));
      gnu_union_field
	= create_field_decl (NULL, gnu_union_type, record_type, packed);
      gnu_field_list = chainon (gnu_union_field, gnu_field_list);
    }

  finish_record_type (record_type, nreverse (gnu_field_list));
}

/* Create a CONSTRUCTOR for the enumeration literal table of 
   GNAT_ENUM_TYPE.  The GCC type of the literal table is GNU_TABLE_TYPE.  */

static tree
create_enum_initializer (gnat_enum_type, gnu_table_type)
     gnat_tree gnat_enum_type;
     tree gnu_table_type;
{
  tree gnu_a_string_type = TREE_TYPE (gnu_table_type);
  tree gnu_char_type
    = TREE_TYPE (TREE_TYPE (TREE_TYPE (TYPE_FIELDS (gnu_a_string_type))));
  tree gnu_char_domain_type
    = TYPE_DOMAIN (TREE_TYPE (TREE_TYPE (TYPE_FIELDS (gnu_a_string_type))));
  tree gnu_size_1 = size_int (1);
  tree gnu_list = NULL_TREE;
  gnat_tree gnat_literal;

  /* Make a STRING_CST for each literal and add it to the CONSTRUCTOR.  */
  for (gnat_literal = First_Literal (gnat_enum_type);
       Present (gnat_literal);
       gnat_literal = Next_Literal (gnat_literal))
    {
      char *name = Get_Name_String (Chars (gnat_literal));
      int length = strlen (name);
      tree gnu_lit_range = build_range_type (gnu_char_domain_type,
					     convert (gnu_char_domain_type,
						      integer_one_node),
					     convert (gnu_char_domain_type,
						      build_int_2 (length,
								   0)));
      tree gnu_lit_index
	= build_index_2_type (convert (sizetype,
				       TYPE_MIN_VALUE (gnu_lit_range)),
			      convert (sizetype,
				       TYPE_MAX_VALUE (gnu_lit_range)));
      tree gnu_lit_type = build_array_type (gnu_char_type, gnu_lit_index);
      tree gnu_literal = build_string (length, name);

      if (TYPE_INDEX_TYPE (gnu_lit_index) == 0)
	TYPE_INDEX_TYPE (gnu_lit_index) = gnu_lit_range;

      TREE_TYPE (gnu_literal) = gnu_lit_type;

      gnu_list = tree_cons (NULL_TREE,
			    convert (gnu_a_string_type,
				     build_unary_op (ADDR_EXPR, NULL_TREE,
						     gnu_literal)),
			    gnu_list);
    }

  return build_constructor (gnu_table_type, nreverse (gnu_list));
}

/* Given a type T, a FIELD_DECL F, and a replacement value R,
   return a new type with all size expressions that contain F
   updated by replacing F with R.  This is identical to GCC's
   substitute_in_type except that it knows about TYPE_INDEX_TYPE.  */

tree
gnat_substitute_in_type (t, f, r)
     tree t, f, r;
{
  switch (TREE_CODE (t))
    {
    case POINTER_TYPE:
    case VOID_TYPE:
      return t;

    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case CHAR_TYPE:
      if ((TREE_CODE (TYPE_MIN_VALUE (t)) != INTEGER_CST
	   && contains_placeholder_p (TYPE_MIN_VALUE (t)))
	  || (TREE_CODE (TYPE_MAX_VALUE (t)) != INTEGER_CST
	      && contains_placeholder_p (TYPE_MAX_VALUE (t))))
	{	
	  tree new;

	  new
	    = build_range_type (t,
				substitute_in_expr (TYPE_MIN_VALUE (t), f, r),
				substitute_in_expr (TYPE_MAX_VALUE (t), f, r));

	  if (TYPE_INDEX_TYPE (t))
	    TYPE_INDEX_TYPE (new)
	      = gnat_substitute_in_type (TYPE_INDEX_TYPE (t), f, r);
	  return new;
	}

      return t;

    case REAL_TYPE:
      if ((TREE_CODE (TYPE_MIN_VALUE (t)) != INTEGER_CST
	   && contains_placeholder_p (TYPE_MIN_VALUE (t)))
	  || (TREE_CODE (TYPE_MAX_VALUE (t)) != INTEGER_CST
	      && contains_placeholder_p (TYPE_MAX_VALUE (t))))
	{
	  t = build_type_copy (t);
	  TYPE_MIN_VALUE (t) = substitute_in_expr (TYPE_MIN_VALUE (t), f, r);
	  TYPE_MAX_VALUE (t) = substitute_in_expr (TYPE_MAX_VALUE (t), f, r);
	}
      return t;

    case COMPLEX_TYPE:
      return build_complex_type (gnat_substitute_in_type (TREE_TYPE (t),
							  f, r));

    case OFFSET_TYPE:
    case METHOD_TYPE:
    case REFERENCE_TYPE:
    case FILE_TYPE:
    case SET_TYPE:
    case STRING_TYPE:
    case FUNCTION_TYPE:
    case LANG_TYPE:
      /* Don't know how to do these yet.  */
      abort ();

    case ARRAY_TYPE:
      t = build_array_type (gnat_substitute_in_type (TREE_TYPE (t), f, r),
			    gnat_substitute_in_type (TYPE_DOMAIN (t), f, r));
      TYPE_SIZE (t) = 0;
      layout_type (t);
      return t;

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	tree new = copy_node (t);
	tree field;
	tree last_field = 0;
	tree parent_subtype = 0;

	/* If we have a parent subtype, substitute into that.  */
	if (TYPE_PARENT_SUBTYPE (t))
	  parent_subtype = gnat_substitute_in_type (TYPE_PARENT_SUBTYPE (t),
						    f, r);

	/* Start out with no fields, make new fields, and chain them
	   in.  */

	TYPE_FIELDS (new) = 0;
	TYPE_SIZE (new) = 0;

	for (field = TYPE_FIELDS (t); field;
	     field = TREE_CHAIN (field))
	  {
	    tree new_field = copy_node (field);

	    /* If this is a PARENT field and the parent subtype now
	       has a non-self-referential length, use it as the type
	       of this field.  Then show we no longer need to
	       worry about a parent subtype.  */
	    if (DECL_PARENT_P (field) && parent_subtype != 0
		&& (TREE_CONSTANT (TYPE_SIZE (parent_subtype))
		    || ! contains_placeholder_p (TYPE_SIZE (parent_subtype))))
	      {
		TREE_TYPE (new_field) = parent_subtype;
		parent_subtype = 0;
	      }
	    else
	      TREE_TYPE (new_field)
		= gnat_substitute_in_type (TREE_TYPE (new_field), f, r);

	    /* If this is an anonymous field and the type of this field is
	       a UNION_TYPE or RECORD_TYPE with no elements, ignore it.  If
	       the type just has one element, treat that as the field. 
	       But don't do this if we are processing a QUAL_UNION_TYPE.  */
	    if (TREE_CODE (t) != QUAL_UNION_TYPE && DECL_NAME (new_field) == 0
		&& (TREE_CODE (TREE_TYPE (new_field)) == UNION_TYPE
		    || TREE_CODE (TREE_TYPE (new_field)) == RECORD_TYPE))
	      {
		if (TYPE_FIELDS (TREE_TYPE (new_field)) == 0)
		  continue;

		if (TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (new_field))) == 0)
		  new_field = TYPE_FIELDS (TREE_TYPE (new_field));
	      }

	    DECL_CONTEXT (new_field) = new;
	    DECL_SIZE (new_field) = 0;

	    if (TREE_CODE (t) == QUAL_UNION_TYPE)
	      {
		/* Do the substitution inside the qualifier and if we find
		   that this field will not be present, omit it.  */
		DECL_QUALIFIER (new_field)
		  = substitute_in_expr (DECL_QUALIFIER (field), f, r);
		if (integer_zerop (DECL_QUALIFIER (new_field)))
		  continue;
	      }

	    if (last_field == 0)
	      TYPE_FIELDS (new) = new_field;
	    else
	      TREE_CHAIN (last_field) = new_field;

	    last_field = new_field;

	    /* If this is a qualified type and this field will always be
	       present, we are done.  */
	    if (TREE_CODE (t) == QUAL_UNION_TYPE
		&& integer_onep (DECL_QUALIFIER (new_field)))
	      break;
	  }

	/* If this used to be a qualified union type, but we now know what
	   field will be present, make this a normal union.  */
	if (TREE_CODE (new) == QUAL_UNION_TYPE
	    && (TYPE_FIELDS (new) == 0
		|| integer_onep (DECL_QUALIFIER (TYPE_FIELDS (new)))))
	  TREE_SET_CODE (new, UNION_TYPE);

	TYPE_PARENT_SUBTYPE (new) = parent_subtype;
	layout_type (new);
	return new;
      }
    }
}

/* The external name of an entity is the:

	fully qualified name for true external objects (qualified_name) 
	qualified name & '__' & number for overloaded subprograms
        (the complete signature function is here but not used for now).  */

static struct obstack ext_name_obstack;
static char *ext_name_firstobj;

/* Return a string representing the external name to be used for
   GNAT_ENTITY.  */

static char *
get_external_name (gnat_entity)
     gnat_tree gnat_entity;
{
  return (Has_Homonym (gnat_entity)
	  ? qualified_overloaded_name (gnat_entity)
	  : qualified_name (gnat_entity));
}

void
ob_init ()
{
  if (!ext_name_firstobj)
    {
      gcc_obstack_init (&ext_name_obstack);
      ext_name_firstobj = obstack_alloc (&ext_name_obstack, 1);
    }
  else
    obstack_free (&ext_name_obstack, ext_name_firstobj);
}

/* Compute the assembler name to use for NAME, which might start with
   a quote.  We use an symbolic form of the operator concatenated with
   whatever follows the trailing quote.  */

static char *
get_operator_name (name)
     char *name;
{
  char *opname;
  char *full_name;

  if (*name != '"')
    return name;

  switch (*++name)
    {
    case '+':
      opname = "_pls";
      break;
    case '-':
      opname = "_mns";
      break;
    case '*':
      if (name[1] == '*')
	opname = "_pow";
      else
	opname = "_mul";
      break;
    case '/':
      if (name[1] == '=')
	opname = "_ne";
      else
	opname = "_div";
      break;
    case '&':
      opname = "_cat";
      break;
    case '=':
      opname = "_eq";
      break;
    case '<':
      if (name[1] == '=')
	opname = "_le";
      else
	opname = "_lt";
      break;
    case '>':
      if (name[1] == '=')
	opname = "_ge";
      else
	opname = "_gt";
      break;
    case 'a':
      if (name[1] == 'b')
	opname = "_abs";
      else
	opname = "_and";
      break;
    case 'm':
      opname = "_mod";
      break;
    case 'n':
      opname = "_not";
      break;
    case 'o':
      opname = "_or";
      break;
    case 'r':
      opname = "_rem";
      break;
    case 'x':
      opname = "_xor";
      break;
    default:
      abort ();
    }

  /* Skip to trailing '"'.  */
  while (*name++ != '"')
    ;

  /* If there's nothing else, we are done.  Otherwise concatenate OPNAME
     to what's left of NAME.  */
  if (*name == 0)
    return opname;

  full_name = (char *) oballoc (strlen (opname) + strlen (name) + 1);
  strcpy (full_name, opname);
  strcat (full_name, name);

  return full_name;
}

void
compute_qualified_name (gnat_tree gnat_entity)
{
  char *name;

  /* If the entity is a child package, its name is not a Defining_Identifier,
     but a Defining_Program_Unit_Name, which does not have a chars field.
     Its simple name is the final identifier, which is the name to use. */ 

  if (Nkind (gnat_entity) == N_Defining_Program_Unit_Name)
    gnat_entity = Defining_Identifier (gnat_entity);

  /* The scope of Standard is Standard itself. */
  if (Scope (Scope (gnat_entity)) != Scope (gnat_entity))
    {
      compute_qualified_name (Scope (gnat_entity));
      obstack_grow (&ext_name_obstack, "__", 2);
    }

  /* Now get the name of the entity, converting names of operators into
     a form valid for the assembler.  */

  name = Get_Name_String (Chars (gnat_entity));
  if (name[0] == '"')
    name = get_operator_name (name);

  obstack_grow (&ext_name_obstack, name, strlen (name));

}   

char *
qualified_name (gnat_entity) 
     gnat_tree gnat_entity;
{
  ob_init ();

  /* if this is a a main subprogram, we prepend a prefix to avoid clashes
     with external C names as main or C library names */
  if (Scope (Scope (gnat_entity)) == Scope (gnat_entity)
      && Is_Subprogram (gnat_entity))
    obstack_grow (&ext_name_obstack, "_ada_", 5);

  compute_qualified_name (gnat_entity);
  obstack_1grow (&ext_name_obstack, 0);

  return (char *) obstack_base (&ext_name_obstack);
}

char *
qualified_overloaded_name (gnat_entity)
     gnat_tree gnat_entity;
{
  gnat_tree e; 
  int number;
  char buf[10];

  for (e = Homonym (gnat_entity),
       number = 1;
       (Present (e)); 
       e = Homonym (e)) 
    if (Scope (e) == Scope (gnat_entity)) number ++;

  sprintf (buf, "%d", number);

  ob_init ();
  compute_qualified_name (gnat_entity);

  if (number != 1)  
   {
     obstack_grow (&ext_name_obstack, "__", 2);
     obstack_grow (&ext_name_obstack, buf, strlen (buf));
   }

  obstack_1grow (&ext_name_obstack, 0);
  return (char *) obstack_base (&ext_name_obstack);
}


char *
qualified_signature (gnat_entity)
     gnat_tree gnat_entity;
{
  gnat_tree gnat_param;

  ob_init ();
  compute_qualified_name (gnat_entity);

  for (gnat_param = First_Formal (gnat_entity);
       Present (gnat_param);
       gnat_param = Next_Formal (gnat_param))
    {
      obstack_grow (&ext_name_obstack, "__", 2);
      compute_qualified_name (Etype (gnat_param));
    }

  obstack_1grow (&ext_name_obstack, 0);
  return (char *) obstack_base (&ext_name_obstack);
}

/* Return a name for GNAT_ENTITY concatenated with "._" and
   STRING.  */

char *
create_concat_name (gnat_entity, string)
     gnat_tree gnat_entity;
     char *string;
{
  ob_init ();
  compute_qualified_name (gnat_entity);
  obstack_grow (&ext_name_obstack, "___", 3);
  obstack_grow (&ext_name_obstack, string, strlen (string));
  obstack_1grow (&ext_name_obstack, 0);

  return (char *) obstack_base (&ext_name_obstack);
}
