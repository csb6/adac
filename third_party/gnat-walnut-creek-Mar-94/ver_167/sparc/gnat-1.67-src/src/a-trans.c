/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*									    */
/*		              TREE TRANSFORMER  			    */
/*									    */
/*				    Body				    */
/*                                                                          */
/*                            $Revision: 1.128 $                            */
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
#include "a-gtran3.h"
#include "a-trans4.h"
#include "a-misc.h"
#include "a-rtree.h"

int max_gnat_nodes;
int number_names;
struct Node *Nodes_Ptr;
struct Name_Entry *Names_Ptr;
struct String_Entry * Strings_Ptr;
Char_Code * String_Chars_Ptr;
struct List_Header *List_Headers_Ptr;
char * Name_Chars_Ptr;
struct Uint_Entry *Uints_Ptr;
Int *Udigits_Ptr;
Int Number_Units;
struct Needed_File_Info *File_Info_Ptr;
int **Lines_Ptrs;

/* Offset from first sloc in file.  Set in parallel with LINENO
   and CURRENT_FILE_NAME.  */
int sloc_offset;

/* Map GNAT tree codes to GCC tree codes for simple expressions.  */

static enum tree_code gnu_codes[Number_Node_Kinds];

/* Structure used to record sequences of statements that need to be
   executed in the next elaboration procedure to be generated.  */
struct elab_list
{
  struct elab_list *next;
  gnat_tree block;
};

/* Head and tail of statement list.  */
static struct elab_list *first_to_elab, *last_to_elab;

/* Head of GNAT tree.  */
extern gnat_tree gnat_root;

static tree tree_transform PROTO((gnat_tree));
static tree emit_check PROTO((tree, tree));
static tree emit_access_check PROTO((tree));
static tree emit_discriminant_check PROTO((tree, gnat_tree));
static tree emit_range_check PROTO((tree, gnat_tree));
static tree emit_index_check PROTO((tree, tree, tree, tree));
static tree assoc_to_constructor PROTO((gnat_tree, tree, tree));
static tree pos_to_constructor PROTO((gnat_tree, tree, gnat_tree));
static tree maybe_implicit_deref PROTO((tree));
static tree make_save_expr PROTO((tree));
static tree maybe_unconstrained_array PROTO((tree));
static void build_package_elab	PROTO((gnat_tree, int, tree, gnat_tree));
static void build_subprogram_elab PROTO((gnat_tree));

/* This is the main program of the back-end.  It sets up all the table
   structures and then generates code.  */

void
gigi (gnat_root,
      max_gnat_node,
      number_name,
      nodes_ptr,
      names_ptr,
      strings_ptr,
      string_chars_ptr,
      list_headers_ptr,
      name_chars_ptr,
      uints_ptr,
      udigits_ptr,
      number_units,
      file_info_ptr,
      lines_ptrs)
     gnat_tree gnat_root;
     int max_gnat_node;
     int number_name;
     struct Node *nodes_ptr;
     struct Name_Entry *names_ptr;
     struct String_Entry *strings_ptr;
     Char_Code *string_chars_ptr;
     struct List_Header *list_headers_ptr;
     char *name_chars_ptr;
     struct Uint_Entry *uints_ptr;
     Int *udigits_ptr;
     Int number_units;
     struct Needed_File_Info *file_info_ptr;
     int **lines_ptrs;
{
  max_gnat_nodes = max_gnat_node;
  number_names =   number_name;
  Nodes_Ptr = nodes_ptr - First_Node_Id;
  Names_Ptr = names_ptr - First_Name_Id;
  Strings_Ptr = strings_ptr - First_String_Id;
  String_Chars_Ptr = string_chars_ptr;
  List_Headers_Ptr = list_headers_ptr - First_List_Id;
  Name_Chars_Ptr = name_chars_ptr;
  Uints_Ptr = uints_ptr - Uint_First_Entry;
  Udigits_Ptr = udigits_ptr;
  Number_Units = number_units;
  File_Info_Ptr = file_info_ptr;
  Lines_Ptrs = lines_ptrs;

  if (Nkind (gnat_root) != N_Compilation_Unit)
    abort ();

  set_lineno (gnat_root, 0);

  /* Initialize ourselves.  */
  init_gnat_to_gnu ();
  init_code_table ();

  gnat_to_code (gnat_root);
}


/* This function is the driver of the GNAT to GCC tree transformation process.
   GNAT_NODE is the root of some gnat tree.  It generates code for that
   part of the tree.  */

void
gnat_to_code (gnat_node)
     gnat_tree gnat_node;
{
  tree gnu_root = tree_transform (gnat_node);

  /* This should just generate code, not return a value.  If it returns
     a value, something is wrong.  */
  if (gnu_root != error_mark_node)
    abort ();
}

/* GNAT_NODE is the root of some GNAT tree.  Return the root of the
   GCC tree corresponding to that GNAT tree.  Normally, no code is generated;
   we just return an equivalent tree which is used elsewhere to generate
   code.  */

tree
gnat_to_gnu (gnat_node)
     gnat_tree gnat_node;
{
  tree gnu_root = tree_transform (gnat_node);

  /* If we got no code as a result, something is wrong.  */
  if (gnu_root == error_mark_node)
    abort ();

  return gnu_root;
}

/* This function is the driver of the GNAT to GCC tree transformation process.
   It is the entry point of the tree transformer.  GNAT_NODE is the root of
   some GNAT tree.  Return the root of the corresponding GCC tree or
   error_mark_node to signal that there is no GCC tree to return.

   The latter is the case if only code generation actions have to be performed
   like in the case of if statements, loops, etc.  This routine is wrapped
   in the above two routines for most purposes.  */

static tree
tree_transform (gnat_node)
     gnat_tree gnat_node;
{
  /* If this node has a type, get the GCC equivalent.  But don't do this
     for N_Expression_Actions because it might be defining the type in
     one of its Actions.  */
  tree gnu_result_type
    = ((Nkind (gnat_node) != N_Expression_Actions
	&& IN (Nkind (gnat_node), N_Has_Etype))
       ? gnat_to_gnu_type (Etype (gnat_node)) : void_type_node);
  tree gnu_result = error_mark_node; /* Default to no value. */
  tree gnu_expr;
  enum tree_code gnu_code;
  gnat_tree gnat_temp;

  /* Set input_file_name and lineno from the Sloc in the GNAT tree. */
  set_lineno (gnat_node, 0);

  /* If this is a subexpression we are to evaluate once, see if we already have
     and return the old value if so.  */
  if (IN (Nkind (gnat_node), N_Subexpr) && Evaluate_Once (gnat_node)
      && present_gnu_tree (gnat_node))
    return get_gnu_tree (gnat_node);

  /* If this is a Statement and we are at top level, we add the statement
     as an elaboration for a null tree.  That will cause it to be placed
     in the elaboration procedure.  */
  if (global_bindings_p ()
      && (IN (Nkind (gnat_node), N_Statement)
	  || Nkind (gnat_node) == N_Procedure_Call_Statement))
    {
      add_pending_elaborations (NULL_TREE,
				make_transform_expr (gnat_node,
						     error_mark_node));
      return error_mark_node;
    }

  switch (Nkind (gnat_node))
    {
    /********************************/
    /* Chapter 2: Lexical Elements: */
    /********************************/

    case N_Identifier:
    case N_Expanded_Name:
      /* If the Etype of this node does not equal the Etype of the
	 Entity, something is wrong with the entity map, probably
	 in generic instantiation.  However, this does not apply to
	 types.  Since we sometime have strange Ekind's, just do
	 this test for objects.  Also, if the Etype of the Entity
	 is private, the Etype of the N_Identifier is allowed to be the
	 full type.  */

      gnat_temp = Entity (gnat_node);

      if (Etype (gnat_node) != Etype (gnat_temp)
	  && ! (IN (Ekind (Etype (gnat_temp)), Private_Kind)
		&& (Etype (gnat_node)
		    == Full_Declaration (Etype (gnat_temp))))
	  && (Ekind (gnat_temp) == E_Variable
	      || Ekind (gnat_temp) == E_Component
	      || Ekind (gnat_temp) == E_Constant
	      || Ekind (gnat_temp) == E_Loop_Parameter
	      || IN (Ekind (gnat_temp), Formal_Kind)))
	{
	  post_error ("internal type mismatch~!", gnat_node);
	  abort ();
	}

      gnu_result = gnat_to_gnu_entity (Entity (gnat_node), NULL_TREE, 0);

      /* The GNAT tree has the type of a function as the type of its result. */
      if (TREE_CODE (TREE_TYPE (gnu_result)) == FUNCTION_TYPE)
	gnu_result_type = TREE_TYPE (gnu_result);

      /* Some objects (such as parameters passed by reference, globals of
	 variable size, and renamed objects) actually represent the address
	 of the object.  In that case, we must do the dereference unless
	 we have a fat pointer.  Call fold here since GNU_RESULT may be
	 a CONST_DECL.  */
      if (TREE_CODE_CLASS (TREE_CODE (gnu_result)) == 'd'
	  && DECL_BY_REF_P (gnu_result))
	gnu_result = build_unary_op (INDIRECT_REF, NULL_TREE,
				     fold (gnu_result));

      /* We always want to return the underlying INTEGER_CST for an
	 enumeration literal to avoid the need to call fold in lots
	 of places.  */
      if (TREE_CODE (gnu_result) == CONST_DECL)
	gnu_result = DECL_INITIAL (gnu_result);

      break;

    case N_Integer_Literal:
      gnu_result = UI_To_gnu (Intval (gnat_node), gnu_result_type);
      break;

    case N_Character_Literal:
      /* If a Entity is present, it means that this was one of the
	 literals in a user-defined character type.  In that case,
	 just return the value in the CONST_DECL.  Otherwise, use the
	 character code.  In that case, the base type should be an
	 INTEGER_TYPE, but we won't bother checking for that.  */
      if (Present (Entity (gnat_node)))
	gnu_result = DECL_INITIAL (get_gnu_tree (Entity (gnat_node)));
      else
	gnu_result = build_int_2 (Char_Literal_Value (gnat_node), 0);
      break;

    case N_Real_Literal:
      {
	/* We want to avoid doing explicit host machine arithmetic.  The
	   best way to do this is when EXPON_EXPR is implemented, but the
	   kludge that follows is the best way for now. */

	tree gnu_numerator = UI_To_gnu (Numerator (gnat_node),
					gnu_result_type);
	tree gnu_denominator;

	if (Decimal (gnat_node))
	  {
	    int exp_value = UI_To_Int (Denominator (gnat_node));
	    static int pow10s[] = {1, 10, 100, 1000, 10000, 100000, 1000000,
				   10000000, 100000000, 1000000000};

	    if (exp_value < sizeof pow10s / sizeof pow10s[0])
	      gnu_denominator = convert (gnu_result_type,
					 build_int_2 (pow10s[exp_value], 0));
	    else
	      {
		char *exponent = (char *) alloca (40);
		REAL_VALUE_TYPE value;

		sprintf (exponent, "1.0e%d", exp_value);
		value = REAL_VALUE_ATOF (exponent,
					 TYPE_MODE (gnu_result_type));
		gnu_denominator = build_real (gnu_result_type, value);
	      }
	  }
	else
	  gnu_denominator = UI_To_gnu (Denominator (gnat_node),
				       gnu_result_type);

	gnu_result = build_binary_op (RDIV_EXPR, gnu_result_type,
				      gnu_numerator, gnu_denominator);
        break;
      }

    case N_String_Literal:
      {
	/* We assume here that all strings are of type standard.string.
	   "Wierd" types of string have been converted to an aggregate
	   by the expander and we won't worry about wide characters
	   for now.  */
	String_Id gnat_string = Strval (gnat_node);
	int length = String_Length (gnat_string);
	char *string = (char *) alloca (length + 1);
	int i;

	/* Build the string with the characters in the literal and
	   end it with a null.  Note that Ada strings are 1-origin.  */
	for (i = 0; i < length; i++)
	  string[i] = Get_String_Char (gnat_string, i + 1);

	string[i] = 0;
	gnu_result = build_string (length, string);

	/* Strings in GCC don't normally have types, but we want
	   this to not be converted to the array type.  */
	TREE_TYPE (gnu_result) = gnu_result_type;
      }
      break;

    case N_Pragma:
      break;

    /**************************************/
    /* Chapter 3: Declarations and Types: */
    /**************************************/

    case N_Implicit_Type:
    case N_Full_Type_Declaration:
    case N_Subtype_Declaration:
    case N_Incomplete_Type_Declaration:
    case N_Private_Type_Declaration:
      {
	gnat_tree gnat_entity = Defining_Identifier (gnat_node);
	tree gnu_old
	  = present_gnu_tree (gnat_entity) ? get_gnu_tree (gnat_entity) : 0;
	tree gnu_new;

	/* If we are to delay elaboration of this type, just do any
	   elaborations needed for expressions within the declaration and
	   make a VOID_TYPE entry for this node and its Full_Declaration (if
	   any) in case something points to it.  Don't do this if it
	   has already been done (the only way that can happen is if
	   the private completion is also delayed).  */
	if (Is_Delayed (gnat_entity))
	  {
	    elaborate_entity (gnat_entity);

	    if (gnu_old == 0)
	      {
		char *entity_name = Get_Name_String (Chars (gnat_entity));
		tree gnu_type = make_node (VOID_TYPE);
		tree gnu_decl = create_type_decl (entity_name, gnu_type);

		save_gnu_tree (gnat_entity, gnu_decl, 0);
		if (IN (Ekind (gnat_entity), Incomplete_Kind)
		    && Present (Full_Declaration (gnat_entity)))
		  save_gnu_tree (Full_Declaration (gnat_entity), gnu_decl, 0);
	      }

	    break;
	  }

	/* If we saved away a VOID_TYPE for this node it means that this
	   made the type that corresponds to the full type of an incomplete
	   type.  Clear that type for now and then update the type in the
	   pointers.  */
	if (gnu_old != 0)
	  {
	    if (TREE_CODE (gnu_old) != TYPE_DECL
		|| TREE_CODE (TREE_TYPE (gnu_old)) != VOID_TYPE)
	      abort ();

	    save_gnu_tree (gnat_entity, NULL_TREE, 0);
	  }

	/* Now fully elaborate the type.  */
	gnu_new = gnat_to_gnu_entity (gnat_entity, NULL_TREE, 1);
	if (TREE_CODE (gnu_new) != TYPE_DECL)
	  abort ();

	/* If we have an old type and we've made pointers to this type, 
	   update those pointers.  */
	if (gnu_old != 0 && TYPE_POINTER_TO (TREE_TYPE (gnu_old)) != 0)
	  {
	    TREE_TYPE (TYPE_POINTER_TO (TREE_TYPE (gnu_old)))
	      = TREE_TYPE (gnu_new);
	    TYPE_POINTER_TO (TREE_TYPE (gnu_new))
	      = TYPE_POINTER_TO (TREE_TYPE (gnu_old));
	  }
      }
      break;

    case N_Number_Declaration:
      /* These will get folded into the tree for us, so we can ignore
	 them.  */
      break;

    case N_Object_Declaration:
    case N_Exception_Declaration:
    case N_Object_Renaming_Declaration:
      gnat_temp = Defining_Identifier (gnat_node);
      if (Present (Expression (gnat_node)))
	{
	  gnu_expr = gnat_to_gnu (Expression (gnat_node));
	  if (Do_Range_Check (Expression (gnat_node)))
	    gnu_expr = emit_range_check (gnu_expr, Etype (gnat_temp));

	  /* If this object has its elaboration delayed, we must force
	     evaluation of GNU_EXPR right now and save it for when
	     the object is frozen.  */
	  if (Is_Delayed (gnat_temp))
	    {
	      if ((Is_Public (gnat_temp) || global_bindings_p ())
		  && ! TREE_CONSTANT (gnu_expr))
		gnu_expr
		  = create_var_decl (create_concat_name (gnat_temp, "init"),
				     NULL_PTR, TREE_TYPE (gnu_expr), gnu_expr,
				     0, Is_Public (gnat_temp), 0, 0, 0);
	      else
		gnu_expr = maybe_variable (gnu_expr);

	      save_gnu_tree (gnat_node, gnu_expr, 1);
	    }
	}
      else
	gnu_expr = NULL_TREE;

      /* Note that we must call gnat_to_gnu each time in case we are
	 initializing multiple variables, since it must have the same
	 semantics as if they had separate initializations.  */
      if (! Is_Delayed (gnat_temp))
	gnat_to_gnu_entity (gnat_temp, gnu_expr, 1);
      break;

    case N_Implicit_Label_Declaration:
      gnat_to_gnu_entity (Defining_Identifier (gnat_node), NULL_TREE, 1);
      break;

    case N_Subprogram_Renaming_Declaration:
    case N_Package_Renaming_Declaration:
    case N_Exception_Renaming_Declaration:
      /* These are fully handled in the front end.  */
      break;

    /*************************************/
    /* Chapter 4: Names and Expressions: */
    /*************************************/

    case N_Explicit_Dereference:
      gnu_result = gnat_to_gnu (Prefix (gnat_node));

      /* Emit access check if necessary */
      if (Do_Access_Check (gnat_node))
	gnu_result = emit_access_check (gnu_result);

      gnu_result = build_unary_op (INDIRECT_REF, gnu_result_type, gnu_result);
      break;

    case N_Indexed_Component:
      {
	tree gnu_array_object = gnat_to_gnu (Prefix (gnat_node));
	tree gnu_type;

	/* Emit access check if necessary */
	if (Do_Access_Check (gnat_node))
	  gnu_array_object = emit_access_check (gnu_array_object);

	gnu_array_object = maybe_implicit_deref (gnu_array_object);
	gnu_array_object = maybe_unconstrained_array (gnu_array_object);
	gnu_result = gnu_array_object;

	for (gnat_temp = First (Expressions (gnat_node)),
	     gnu_type = TREE_TYPE (gnu_array_object);
	     Present (gnat_temp);
	     gnat_temp = Next (gnat_temp),
	     gnu_type = TREE_TYPE (gnu_type))
	  {
	    if (TREE_CODE (gnu_type) != ARRAY_TYPE)
	      abort ();

	    gnu_expr = gnat_to_gnu (gnat_temp);

	    if (Do_Range_Check (gnat_temp))
	      gnu_expr
		= emit_index_check
		  (gnu_array_object, gnu_expr, 
		   TYPE_MIN_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type))),
		   TYPE_MAX_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type))));

	    gnu_result = build_binary_op (ARRAY_REF, NULL_TREE,
					  gnu_result, gnu_expr);
	  }
      }
      break;

    case N_Slice:
      gnu_result = gnat_to_gnu (Prefix (gnat_node));

      /* Emit access check if necessary */
      if (Do_Access_Check (gnat_node))
	gnu_result = emit_access_check (gnu_result);

      /* Reference the desired first component of Prefix, where the first
	 index is the low bound of our type.  Then takes its address and
	 convert it to a pointer to our type.  Finally, indirectly 
	 reference that.  Note that most of these operations are just
	 performed to get the types correct and will go away.  */
      gnu_result = maybe_implicit_deref (gnu_result);
      gnu_result = maybe_unconstrained_array (gnu_result);
      gnu_result
	= build_binary_op (ARRAY_REF, NULL_TREE, gnu_result, 
			   TYPE_MIN_VALUE (TYPE_DOMAIN (gnu_result_type)));
      gnu_result = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_result);
      gnu_result = convert (build_pointer_type (gnu_result_type), gnu_result);
      gnu_result = build_unary_op (INDIRECT_REF, NULL_TREE, gnu_result);
      break;

    case N_Selected_Component:
      {
	tree gnu_prefix = gnat_to_gnu (Prefix (gnat_node));
	gnat_tree gnat_field = Selector_Name (gnat_node);
        char *gnat_field_name = Get_Name_String (Chars (gnat_field));
	tree gnu_field = gnat_to_gnu (gnat_field);
        gnat_tree gnat_pref_type = Etype (Prefix (gnat_node));

	/* If there are discriminants, the prefix might be evaluated more
	   than once, which is a problem if it has side-effects.  */
	if (Has_Discriminants (Prefix (gnat_node))
	    && TREE_SIDE_EFFECTS (gnu_prefix))
	  gnu_prefix = make_save_expr (gnu_prefix);

	/* emit access and discriminant check if necessary */
	if (Do_Access_Check (gnat_node))
	  gnu_prefix = emit_access_check (gnu_prefix);
	if (Do_Discriminant_Check (gnat_node))
	  gnu_prefix = emit_discriminant_check (gnu_prefix,
						gnat_node);

	if (Is_Tagged_Type (gnat_pref_type) 
	    || (Is_Access_Type (gnat_pref_type)
		&& Is_Tagged_Type (Designated_Type (gnat_pref_type))))
	    gnu_result = 
	    build_tagged_component_ref (gnu_prefix,
					get_identifier (gnat_field_name),
					gnu_field);
	else
	  gnu_result = 
	    build_component_ref (gnu_prefix,
				 get_identifier (gnat_field_name),
				 gnu_field);
	if (gnu_result == 0)
	  abort ();
      }
      break;

    case N_Attribute_Reference:
      {
        /* The attribute designator (like an enumeration value). */
        int attribute = Get_Attribute_Id (Chars (Identifier (gnat_node)));
        tree gnu_prefix = gnat_to_gnu (Prefix (gnat_node));
	tree gnu_type = TREE_TYPE (gnu_prefix);

	/* Emit access check if necessary */
	if (Do_Access_Check (gnat_node))
	  gnu_prefix = emit_access_check (gnu_prefix);

        switch (attribute)
          {
	  case Attr_Pos:
	  case Attr_Val:
	    /* These are just conversions until we support representation
	       clauses for enumerations, at which time we have to
	       figure out what to really do.  */
	    gnu_result = gnat_to_gnu (Expression (gnat_node));
	    break;

	  case Attr_Pred:
	  case Attr_Succ:
	    /* These just add or subject the constant 1 until representations
	       are supported.  */
	    gnu_result = build_binary_op (attribute == Attr_Pred
					  ? MINUS_EXPR : PLUS_EXPR,
					  gnu_result_type,
					  gnat_to_gnu (Expression (gnat_node)),
					  convert (gnu_result_type,
						   integer_one_node));
	    break;

	  case Attr_Address:
	    /* If we are taking 'Address of an unconstrained object,
	       this is the pointer to the underlying array.  */
	    if (TREE_CODE (gnu_prefix) == UNCONSTRAINED_ARRAY_REF)
	      {
		tree gnu_fat_pointer = TREE_OPERAND (gnu_prefix, 0);
		tree gnu_fat_type = TREE_TYPE (gnu_fat_pointer);

		gnu_result
		  = build_component_ref (gnu_fat_pointer, NULL_TREE,
					 TYPE_FIELDS (gnu_fat_type));
		break;
	      }
	    /* ... fall through ... */
          case Attr_Access:
	    mark_addressable (gnu_prefix);
            gnu_result = build_unary_op (ADDR_EXPR, gnu_result_type,
					 gnu_prefix);
	    break;

	  case Attr_Size:
	    if (TREE_CODE_CLASS (TREE_CODE (gnu_prefix)) == 'd'
		&& TREE_CODE (gnu_prefix) != TYPE_DECL)
	      gnu_result = DECL_SIZE (gnu_prefix);
	    /* If this is an itegral type, return the precision, else
	       the size.  */
	    else if (INTEGRAL_TYPE_P (gnu_type))
	      gnu_result = build_int_2 (TYPE_PRECISION (gnu_type), 0);
	    else
	      gnu_result = max_size (TYPE_SIZE (gnu_type), 1);
	    break;

	  case Attr_First:
	  case Attr_Last:
	    if (INTEGRAL_TYPE_P (gnu_type))
	      {
		if (attribute == Attr_First)
		  gnu_result = TYPE_MIN_VALUE (gnu_type);
		else
		  gnu_result = TYPE_MAX_VALUE (gnu_type);
		break;
	      }
	    /* ... fall through ... */
	  case Attr_Length:
	    {
	      int Dimension = (Present (Expression (gnat_node))
			       ? UI_To_Int (Intval (Expression (gnat_node)))
			       : 1);

	      /* Make sure any implicit dereference gets done.  */
	      gnu_prefix = maybe_implicit_deref (gnu_prefix);
	      gnu_prefix = maybe_unconstrained_array (gnu_prefix);
	      gnu_type = TREE_TYPE (gnu_prefix);

	      for (; Dimension > 1; Dimension--)
		gnu_type = TREE_TYPE (gnu_type);

	      if (TREE_CODE (gnu_type) != ARRAY_TYPE)
		abort ();

	      if (attribute == Attr_First)
		gnu_result
		  = TYPE_MIN_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type)));
	      else if (attribute == Attr_Last)
		gnu_result
		  = TYPE_MAX_VALUE (TYPE_INDEX_TYPE (TYPE_DOMAIN (gnu_type)));
	      else if (attribute == Attr_Length)
		gnu_result
		  = size_binop (EASY_DIV_EXPR, TYPE_SIZE (gnu_type),
				TYPE_SIZE (TREE_TYPE (gnu_type)));

	      /* If this has a PLACEHOLDER_EXPR, qualify it by the object
		 we are handling.  Note that these attributes could not
		 have been used on an unconstrained array type.  */
	      if (TREE_CODE (gnu_result) != INTEGER_CST
		  && contains_placeholder_p (gnu_result))
		gnu_result = build (WITH_RECORD_EXPR, TREE_TYPE (gnu_result),
				    gnu_result, gnu_prefix);
	      break;
	    }

	  case Attr_Position:
	  case Attr_First_Bit:
	  case Attr_Last_Bit:
	    {
	      int bitsize, bitpos;
	      tree offset;
	      enum machine_mode mode;
	      int unsignedp, volatilep;

	      if (TREE_CODE (gnu_prefix) != COMPONENT_REF)
		abort ();

	      get_inner_reference (gnu_prefix, &bitsize, &bitpos,
				   &offset, &mode, &unsignedp, &volatilep);

	      if (offset)
		offset = size_binop (PLUS_EXPR, offset, size_int (bitpos));
	      else
		offset = size_int (bitpos);

	      if (attribute == Attr_Position)
		gnu_result = size_binop (TRUNC_DIV_EXPR, offset,
					 size_int (BITS_PER_UNIT));
	      else if (attribute == Attr_First_Bit)
		gnu_result = offset;
	      else if (attribute == Attr_Last_Bit)
		gnu_result = size_binop (PLUS_EXPR, offset,
					 size_int (bitsize - 1));
	    }
	    break;

          default:
	    post_error ("unsupported attribute~!", gnat_node);
	    abort ();
          }
      }
      break;

    case N_Aggregate:
      if (TREE_CODE (gnu_result_type) == RECORD_TYPE)
	gnu_result
	  = assoc_to_constructor (First (Component_Associations (gnat_node)),
				  gnu_result_type, NULL_TREE);
      else if (TREE_CODE (gnu_result_type) == ARRAY_TYPE)
	gnu_result = pos_to_constructor (First (Expressions (gnat_node)),
					 gnu_result_type,
					 Component_Type (Etype (gnat_node)));
      else
	abort ();
      break;

    case N_Null:
      gnu_result = null_pointer_node;
      break;

    case N_Type_Conversion:
      /* Get the operand expression.  */
      gnu_result = gnat_to_gnu (Expression (gnat_node));

      /* If a range check is needed, emit code to generate it. */
      if (Do_Range_Check (Expression (gnat_node)))
	gnu_result = 
	  emit_range_check (gnu_result,
			    Entity (Subtype_Mark (gnat_node)));

      /* If this is an unchecked conversion, emit the apropriate operation.
	 Otherwise, do the explicit conversion.  Don't assume that the
	 code on our exit will do it since there are many exceptions
	 to doing conversion there, none of which apply here.  */
      if (Unchecked_Conversion (gnat_node))
	gnu_result
	  = build1 (UNCHECKED_CONVERT_EXPR, gnu_result_type, gnu_result);
      else
	gnu_result = convert (gnu_result_type, gnu_result);
      break;

    case N_Parenthesized_Expression:
    case N_Qualified_Expression:
      /* Just get the operand expression.  The conversion at the end
	 of this function will do the actual operation.  */
      gnu_result = gnat_to_gnu (Expression (gnat_node));
      break;

    case N_Op_In:
    case N_Op_Not_In:
      {
	tree gnu_object = gnat_to_gnu (Left_Opnd (gnat_node));
	gnat_tree gnat_range = Right_Opnd (gnat_node);
	tree gnu_low;
	tree gnu_high;

	/* GNAT_RANGE is either an N_Range node or an identifier
	   denoting a subtype.  */
	if (Nkind (gnat_range) == N_Range)
	  {
	    gnu_low = gnat_to_gnu (Low_Bound (gnat_range));
	    gnu_high = gnat_to_gnu (High_Bound (gnat_range));
	  }
	else if (Nkind (gnat_range) == N_Identifier)
	  {
	    tree gnu_range_type = gnat_to_gnu_type (Entity (gnat_range));

	    gnu_low = TYPE_MIN_VALUE (gnu_range_type);
	    gnu_high = TYPE_MAX_VALUE (gnu_range_type);
	  }
	else
	  abort ();

	/* If LOW and HIGH are identical, perform an equality test.
	   Otherwise, ensure that GNU_OBJECT is only evaluated once
	   and perform a full range test.  */
	if (operand_equal_p (gnu_low, gnu_high, 0))
	  gnu_result = build_binary_op (EQ_EXPR, gnu_result_type,
					gnu_object, gnu_low);
	else
	  {
	    gnu_object = make_save_expr (gnu_object);
	    gnu_result
	      = build_binary_op (TRUTH_ANDIF_EXPR, gnu_result_type,
				 build_binary_op (GE_EXPR, gnu_result_type,
						  gnu_object, gnu_low),
				 build_binary_op (LE_EXPR, gnu_result_type,
						  gnu_object, gnu_high));
	  }

	if (Nkind (gnat_node) == N_Op_Not_In)
	  gnu_result = invert_truthvalue (gnu_result);
      }
      break;

    case N_Op_Divide:
      if (TREE_CODE (gnu_result_type) == REAL_TYPE)
	gnu_result = build_binary_op (RDIV_EXPR, gnu_result_type,
				      gnat_to_gnu (Left_Opnd (gnat_node)),
				      gnat_to_gnu (Right_Opnd (gnat_node)));

      else if (TREE_CODE (gnu_result_type) == INTEGER_TYPE)
	gnu_result = build_binary_op (TRUNC_DIV_EXPR, gnu_result_type,
				      gnat_to_gnu (Left_Opnd (gnat_node)),
				      gnat_to_gnu (Right_Opnd (gnat_node)));

      else
	abort ();
      break;

    case N_Op_Or:    case N_Op_And:      case N_Op_Xor:
      /* These can either be operations on booleans or on modular types.  
	 Fall through for boolean types since that's the way GNU_CODES is
	 set up.  */
      if (TREE_CODE (gnu_result_type) == INTEGER_TYPE
	  && TREE_UNSIGNED (gnu_result_type))
	{
	  enum tree_code code
	    = (Nkind (gnat_node) == N_Op_Or ? BIT_IOR_EXPR
	       : Nkind (gnat_node) == N_Op_And ? BIT_AND_EXPR
	       : BIT_XOR_EXPR);

	  gnu_result = build_binary_op (code, gnu_result_type,
					gnat_to_gnu (Left_Opnd (gnat_node)),
					gnat_to_gnu (Right_Opnd (gnat_node)));
	  break;
	}

      /* ... fall through ... */

    case N_Op_Eq:    case N_Op_Ne:	 case N_Op_Lt:
    case N_Op_Le:    case N_Op_Gt:       case N_Op_Ge:
    case N_Op_Add:   case N_Op_Subtract: case N_Op_Multiply:
    case N_Op_Mod:   case N_Op_Rem:	 case N_Op_Expon:
    case N_Op_And_Then:  case N_Op_Or_Else:
      {
	enum tree_code code = gnu_codes[Nkind (gnat_node)];
	tree gnu_lhs = gnat_to_gnu (Left_Opnd (gnat_node));
	tree gnu_rhs = gnat_to_gnu (Right_Opnd (gnat_node));

	/* If this is a comparison operator, convert any references to
	   an unconstrained array value into a reference to the
	   actual array.  */
	if (TREE_CODE_CLASS (code) == '<')
	  {
	    gnu_lhs = maybe_unconstrained_array (gnu_lhs);
	    gnu_rhs = maybe_unconstrained_array (gnu_rhs);
	  }

	gnu_result = build_binary_op (code, gnu_result_type, gnu_lhs, gnu_rhs);
      }
      break;

    case N_Op_Plus:
      gnu_result = gnat_to_gnu (Right_Opnd (gnat_node));
      break;

    case N_Op_Not:
      /* This case can apply to a boolean or a modular type.
	 Fall through for a boolean operand since GNU_CODES is set
	 up to handle this.  */
      if (TREE_CODE (gnu_result_type) == INTEGER_TYPE
	  && TREE_UNSIGNED (gnu_result_type))
	{
	  gnu_result = build_unary_op (BIT_NOT_EXPR, gnu_result_type,
				       gnat_to_gnu (Right_Opnd (gnat_node)));
	  break;
	}

      /* ... fall through ... */

    case N_Op_Minus:  case N_Op_Abs:
      gnu_result = build_unary_op (gnu_codes[Nkind (gnat_node)],
				   gnu_result_type,
				   gnat_to_gnu (Right_Opnd (gnat_node)));
      break;

    case N_Allocator:
      {
	tree gnu_init = 0;
	gnat_tree gnat_type = 0;
	tree gnu_type;

	/* The Expression operand can either be an N_Identifier, which
	   must represent a type, or an N_Qualified_Expression, which
	   contains both the object type and an initial value for the
	   object.  */
	if (Nkind (Expression (gnat_node)) == N_Identifier)
	  {
	    gnat_type = Entity (Expression (gnat_node));
	    gnu_type = gnat_to_gnu_type(gnat_type);
	  }
	else if (Nkind (Expression (gnat_node)) == N_Qualified_Expression)
	  {
	    /* If might seem like we want the type to be obtaind from
	       the Subtype_Mark, but, in fact, the type of the Expression
	       is correct.  */
	    gnu_init = gnat_to_gnu (Expression (Expression (gnat_node)));
	    gnu_type = TREE_TYPE (gnu_init);
	  }
	else
	  abort ();

	return build_allocator (gnu_type, gnu_init, gnat_type,
				gnu_result_type);
      }
      break;

    case N_Expression_Actions:
      /* Start an expression statement, preform any actions specified in
	 the node, expand our expression, and then end the statement. 
	 If we are not within any subprogram, we cannot actually generate
	 RTL, so make a TRANSFORM_EXPR if there is anything unusual in
	 the actions.  Note that we don't know our own type in that
	 case, but convert knows what to do.

	 ??? There is a potential problem here if our type is declared
	 inside this node and is used in the containing expression and
	 this occurs at the global level and in a node that has
	 nontrivial actions.  I don't think that actually occurs, so we
	 might be lucky.  */

      if (global_bindings_p ())
	{
	  for (gnat_temp = First (Actions (gnat_node)); gnat_temp;
	       gnat_temp = Next (gnat_temp))
	    switch (Nkind (gnat_temp))
	      {
	      case N_Implicit_Type:
	      case N_Full_Type_Declaration:
	      case N_Subtype_Declaration:
	      case N_Number_Declaration:
	      case N_Object_Declaration:
	      case N_Exception_Declaration:
		break;
	      default:
		return make_transform_expr (gnat_node, error_mark_node);
	      }
	}

      gnu_result = expand_start_stmt_expr ();
      for (gnat_temp = First (Actions (gnat_node)); gnat_temp;
	   gnat_temp = Next (gnat_temp))
	gnat_to_code (gnat_temp);

      /* Now that we've defined any types in Actions, we get safely get
	 our type.  */
      gnu_result_type = gnat_to_gnu_type (Etype (gnat_node));
      expand_expr_stmt (gnat_to_gnu (Expression (gnat_node)));
      gnu_result = expand_end_stmt_expr (gnu_result);
      break;

    /***************************/
    /* Chapter 5: Statements:  */
    /***************************/

    case N_Label:
      expand_label (gnat_to_gnu (Identifier (gnat_node)));
      break;

    case N_Null_Statement:
      break;

    case N_Assignment_Statement:
      {
	/* Get the LHS and RHS of the statement and convert any 
	   reference to an unconstrained array into a reference to
	   the underlying array.  */
	tree gnu_rhs
	  = maybe_unconstrained_array (gnat_to_gnu (Expression (gnat_node)));
	tree gnu_lhs
	  = maybe_unconstrained_array (gnat_to_gnu (Name (gnat_node)));

	/* If range check is needed, emit code to generate it */
	if (Do_Range_Check (Expression (gnat_node)))
	  gnu_rhs = emit_range_check (gnu_rhs, Etype (Name (gnat_node)));

	set_lineno (gnat_node, 1);
	expand_expr_stmt (build_binary_op (MODIFY_EXPR, NULL_TREE,
					   gnu_lhs, gnu_rhs));
      }
      break;

    case N_If_Statement:
      /* Start an IF statement giving the condition.  */
      gnu_expr = gnat_to_gnu (Condition (gnat_node));
      set_lineno (gnat_node, 1);
      expand_start_cond (gnu_expr, 0);

      push_momentary ();

      /* Generate code for the statements to be executed if the condition
	 is true.  */

      for (gnat_temp = First (Then_Statements (gnat_node));
	   Present (gnat_temp);
	   gnat_temp = Next (gnat_temp))
	{
	  gnat_to_code (gnat_temp);
	  clear_momentary();
	}

      /* Generate each of the "else if" parts.  */
      if (Present (Elsif_Parts (gnat_node)))
	{
	  for (gnat_temp = First (Elsif_Parts (gnat_node));
	       Present (gnat_temp);
	       gnat_temp = Next (gnat_temp))
	    {
	      gnat_tree gnat_statement;

	      /* Set up the line numbers for each condition we test.  */
	      set_lineno (gnat_node, 1);
	      expand_start_elseif (gnat_to_gnu (Condition (gnat_temp)));

	      for (gnat_statement = First (Then_Statements (gnat_temp));
		   Present (gnat_statement);
		   gnat_statement = Next (gnat_statement))
		{
		  gnat_to_code (gnat_statement);
		  clear_momentary ();
		}
	    }
	}

      /* Finally, handle any statements in the "else" part.  */
      if (Present (Else_Statements (gnat_node)))
	{
	  expand_start_else ();

	  for (gnat_temp = First (Else_Statements (gnat_node));
	       Present (gnat_temp);
	       gnat_temp = Next (gnat_temp))
	    {
	      gnat_to_code (gnat_temp);
	      clear_momentary ();
	    }
	}

      pop_momentary ();
      expand_end_cond ();
      break;

    case N_Case_Statement:
      {
	gnat_tree gnat_when;
	gnat_tree gnat_choice;
	tree gnu_label;
	gnat_tree gnat_statement;

	gnu_expr = gnat_to_gnu (Expression (gnat_node));
	set_lineno (gnat_node, 1);
	expand_start_case (1, gnu_expr, TREE_TYPE (gnu_expr), "case");

	push_momentary();

	for (gnat_when = First (Alternatives (gnat_node));
	     Present (gnat_when);
	     gnat_when = Next (gnat_when))
	  {
	    /* First compile all the different case choices for the  current
	       WHEN alternative.  */

	    for (gnat_choice = First (Discrete_Choices (gnat_when));
		 Present (gnat_choice); gnat_choice = Next (gnat_choice))
              {
 	        gnu_label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

		switch (Nkind (gnat_choice))
		  {
		  case N_Range:
		    pushcase_range
		      (gnat_to_gnu (Low_Bound (gnat_choice)),
		       gnat_to_gnu (High_Bound (gnat_choice)),
		       convert, gnu_label, NULL_PTR);
		    break;

		  case N_Identifier:
                  case N_Expanded_Name:
		    /* This represents either a subtype range or a static value
		       of some kind; Ekind says which.  If a static value,
		       fall through to the next case.  */
		    if (IN (Ekind (Entity (gnat_choice)), Type_Kind))
		      {
			tree type
			  = gnat_to_gnu_type (Entity (gnat_choice));

			pushcase_range (fold (TYPE_MIN_VALUE (type)),
					fold (TYPE_MAX_VALUE (type)),
					convert, gnu_label, NULL_PTR);
			break;
		      }
		    /* ... fall through ... */
		  case N_Character_Literal:
		  case N_Integer_Literal:
		    pushcase (gnat_to_gnu (gnat_choice), convert,
			      gnu_label, NULL_PTR);
		    break;

		  case N_Others_Choice:
		    pushcase (NULL_TREE, convert, gnu_label, NULL_PTR);
		    break;

		  default:
		    abort ();
		  }
	      }

	    /* After compiling the choices attached to the WHEN compile the
	       body of statements that have to be executed, should the
	       "WHEN ... =>" be taken.  */
	    for (gnat_statement = First (Statements (gnat_when));
		 Present (gnat_statement);
		 gnat_statement = Next (gnat_statement))
	      {
		gnat_to_code (gnat_statement);
		clear_momentary ();
	      }

	    /* Communicate to GCC that we are done with the current WHEN,
	       i.e. insert a "break" statement.  */
	    set_lineno (gnat_node, 1);
	    expand_exit_something ();
	  }

	pop_momentary();
	set_lineno (gnat_node, 1);
	expand_end_case (gnu_expr);
      }
      break;

    case N_Loop_Statement:
      {
	/* The loop variable in GCC form, if any. */
	tree gnu_loop_var = NULL_TREE;
	/* PREINCREMENT_EXPR or PREDECREMENT_EXPR.  */
	enum tree_code gnu_update;
	/* Used if this is a named loop for so EXIT can work.  */
	struct nesting *loop_id;
	/* Condition to continue loop tested at top of loop.  */
	tree gnu_top_condition = integer_one_node;
	/* Similar, but tested at bottom of loop.  */
	tree gnu_bottom_condition = integer_one_node;
	gnat_tree gnat_statement;
	gnat_tree gnat_iter_scheme = Iteration_Scheme (gnat_node);
	int enclosing_if_p = 0;

	/* Set the condition that under which the loop should continue. 
	   For "LOOP .... END LOOP;" the condition is always true.  */
	if (No (gnat_iter_scheme))
	  ;
	/* The case "WHILE condition LOOP ..... END LOOP;" */
	else if (Present (Condition (gnat_iter_scheme)))
	  gnu_top_condition = gnat_to_gnu (Condition (gnat_iter_scheme));
        else
	  {
	    /* We have an iteration scheme.  */
	    gnat_tree gnat_loop_spec
	      = Loop_Parameter_Specification (gnat_iter_scheme);
	    gnat_tree gnat_loop_var = Defining_Identifier (gnat_loop_spec);
	    gnat_tree gnat_type = Etype (gnat_loop_var);
	    tree gnu_type = gnat_to_gnu_type (gnat_type);
	    tree gnu_low = TYPE_MIN_VALUE (gnu_type);
	    tree gnu_high = TYPE_MAX_VALUE (gnu_type);
	    int reversep = Reverse_Present (gnat_loop_spec);
	    tree gnu_first = reversep ? gnu_high : gnu_low;
	    tree gnu_last = reversep ? gnu_low : gnu_high;
	    enum tree_code end_code = reversep ? GE_EXPR : LE_EXPR;
	    tree gnu_base_type
	      = TREE_TYPE (gnu_type) ? TREE_TYPE (gnu_type) : gnu_type;
	    tree gnu_limit
	      = (reversep ? TYPE_MIN_VALUE (gnu_base_type)
		 : TYPE_MAX_VALUE (gnu_base_type));

	    /* We know the loop variable will not overflow if GNU_LAST is
	       a constant and is not equal to GNU_LIMIT.  If it might
	       overflow, we have to move the limit test to the end of
	       the loop.  In that case, we have to test for an
	       empty loop outside the loop.  */
	    if (TREE_CODE (gnu_last) != INTEGER_CST
		|| TREE_CODE (gnu_limit) != INTEGER_CST
		|| tree_int_cst_equal (gnu_last, gnu_limit))
	      {
		gnu_expr = build_binary_op (LE_EXPR, integer_type_node,
					    gnu_low, gnu_high);
		set_lineno (gnat_node, 1);
		expand_start_cond (gnu_expr, 0);
		enclosing_if_p = 1;
	      }


	    /* Open a new nesting level that will surround the loop to declare
	       the loop index variable.  */
	    pushlevel (0);
	    expand_start_bindings (0);

	    /* Declare the loop index and set it to its initial value.  */
	    gnu_loop_var = gnat_to_gnu_entity (gnat_loop_var, gnu_first, 1);

	    /* Set either the top or bottom exit condition as
	       appropriate depending on whether we know an overflow
	       cannot occur or not. */
	    if (enclosing_if_p)
	      gnu_bottom_condition
		= build_binary_op (NE_EXPR, integer_type_node,
				   gnu_loop_var, gnu_last);
	    else
	      gnu_top_condition
		= build_binary_op (end_code, integer_type_node,
				   gnu_loop_var, gnu_last);

	    gnu_update = reversep ? PREDECREMENT_EXPR : PREINCREMENT_EXPR;
	  }

	set_lineno (gnat_node, 1);
	if (gnu_loop_var)
	  loop_id = expand_start_loop_continue_elsewhere (1);
	else
	  loop_id = expand_start_loop (1);

	/* If the loop was named, have the name point to this loop.  In this
	   case, the association is not a ..._DECL node; in fact, it isn't
	   a GCC tree node at all.  Since this name is referenced inside
	   the loop, do it before we process the statements of the loop.  */
        if (Present (Identifier (gnat_node)))
	  save_gnu_tree (Entity (Identifier (gnat_node)),
			 (tree) loop_id, 1);

	set_lineno (gnat_node, 1);
	expand_exit_loop_if_false (NULL_PTR, gnu_top_condition);
	push_momentary ();

	for (gnat_statement = First (Statements (gnat_node));
	     Present (gnat_statement);
	     gnat_statement = Next (gnat_statement))
	  {
	    gnat_to_code (gnat_statement);
	    clear_momentary ();
	  }

	pop_momentary ();
	set_lineno (gnat_node, 1);
	expand_exit_loop_if_false (NULL_PTR, gnu_bottom_condition);

	if (gnu_loop_var)
	  {
	    expand_loop_continue_here ();
	    gnu_expr = build_binary_op (gnu_update, TREE_TYPE (gnu_loop_var),
					gnu_loop_var,
					convert (TREE_TYPE (gnu_loop_var),
						 integer_one_node));
	    set_lineno (gnat_node, 1);
	    expand_expr_stmt (gnu_expr);
	  }

	set_lineno (gnat_node, 1);
	expand_end_loop ();

	if (gnu_loop_var)
	  {
	    /* Close the nesting level that sourround the loop that was used to
	       declare the loop index variable.   */
	    set_lineno (gnat_node, 1);
	    expand_end_bindings (getdecls (), 1, 0);
	    poplevel (1, 1, 0);
	  }

	if (enclosing_if_p)
	  {
	    set_lineno (gnat_node, 1);
	    expand_end_cond ();
	  }
      }
      break;

    case N_Block_Statement:
      pushlevel (0);
      expand_start_bindings (0);

      /* Process the declarations inside the block statement.   */
      if (Present (Declarations (gnat_node)))
	for (gnat_temp = First (Declarations (gnat_node));
	     Present (gnat_temp); gnat_temp = Next (gnat_temp))
	  gnat_to_code (gnat_temp);

      gnat_to_code (Handled_Statement_Sequence (gnat_node));

      set_lineno (gnat_node, 1);
      expand_end_bindings (getdecls (), 1, 0);
      poplevel (kept_level_p (), 1, 0);
      break;

    case N_Exit_Statement:
      {
	/* Which loop to exit, NULL if the current loop.   */
	struct nesting *loop_id = NULL_PTR;
	/* The GCC version of the optional GNAT condition node attached to the
	   exit statement. Exit the loop if this is false.  */
	tree gnu_cond = integer_zero_node;

	if (Present (Name (gnat_node)))
	  loop_id
	    = (struct nesting *) get_gnu_tree (Entity (Name (gnat_node)));

	if (Present (Condition (gnat_node)))
	  gnu_cond
	    = invert_truthvalue
	      (truthvalue_conversion (gnat_to_gnu (Condition (gnat_node))));

	set_lineno (gnat_node, 1);
	expand_exit_loop_if_false (loop_id, gnu_cond);
      }
      break;

    case N_Return_Statement:
      {
	/* The gnu function type of the subprogram currently processed.  */
	tree gnu_subprog_type = TREE_TYPE (current_function_decl);
	/* The return value from the subprogram.  */
	tree gnu_ret_val = 0;

	/* If we are dealing with a "return;" from an Ada procedure with
	   parameters passed by copy in copy out, we need to return a record
	   containing the final values of these parameters.  If the list
	   contains only one entry, return just that entry.

	   For a full description of the copy in copy out parameter mechanism,
	   see the part of the gnat_to_gnu_entity routine dealing with the
	   translation of subprograms. */
	if (TYPE_CI_CO_LIST (gnu_subprog_type) != NULL_TREE)
	  {
	    if (list_length (TYPE_CI_CO_LIST (gnu_subprog_type)) == 1)
	      gnu_ret_val = TREE_VALUE (TYPE_CI_CO_LIST (gnu_subprog_type));
	    else
	      gnu_ret_val
		= build_constructor (TREE_TYPE (gnu_subprog_type),
				     TYPE_CI_CO_LIST (gnu_subprog_type));
	  }

	/* If the Ada subprogram is a function, we just need to return the
	   expression.   If the subprogram returns an unconstrained
	   array, we have to allocate a new version of the result and
	   return it.  */

	else if (Present (Expression (gnat_node)))
	  {
	    gnu_ret_val  = gnat_to_gnu (Expression (gnat_node));

	    if (TYPE_RETURNS_UNCONSTRAINED_P (gnu_subprog_type))
	      gnu_ret_val = build_allocator (TREE_TYPE (gnu_ret_val),
					     gnu_ret_val,
					     Etype (Expression (gnat_node)),
					     TREE_TYPE (gnu_subprog_type));
	  }

	set_lineno (gnat_node, 1);
	if (gnu_ret_val)
	  expand_return (build_binary_op (MODIFY_EXPR, NULL_TREE,
					  DECL_RESULT (current_function_decl),
					  gnu_ret_val));
	else
	  expand_null_return ();

      }
      break;

    case N_Goto_Statement:
      {
	tree gnu_label;

	gnu_label = gnat_to_gnu (Name (gnat_node));
	TREE_USED (gnu_label) = 1;
	set_lineno (gnat_node, 1);
	expand_goto (gnu_label);
	break;
      }

    case N_Raise_Statement:
      {
	/* If an exception is specifed, set __gnat_exception to the address
	   of the exception.  Then do a longjmp through __gnat_jmpbuf.   */
	tree gnu_call
	  = build (CALL_EXPR, void_type_node,
		   build_unary_op (ADDR_EXPR, NULL_TREE, longjmp_decl),
		   chainon (chainon (NULL_TREE,
				     build_tree_list (NULL_TREE, jmpbuf_decl)),
			    build_tree_list (NULL_TREE, integer_one_node)),
		   NULL_TREE);
	TREE_SIDE_EFFECTS (gnu_call) = 1;

	if (Present (Name (gnat_node)))
	  {
	    gnu_expr
	      = build_binary_op
		(MODIFY_EXPR, NULL_TREE, excptr_decl,
		 build_unary_op (ADDR_EXPR, NULL_TREE,
				 gnat_to_gnu (Name (gnat_node))));
	    set_lineno (gnat_node, 1);
	    expand_expr_stmt (gnu_expr);
	  }

	set_lineno (gnat_node, 1);
	expand_expr_stmt (gnu_call);
      }
      break;

    /****************************/
    /* Chapter 6: Subprograms:  */
    /****************************/

    case N_Subprogram_Declaration:
      gnat_to_code (Specification (gnat_node));
      break;

    case N_Abstract_Subprogram_Declaration:
      break;

    case N_Function_Specification:
    case N_Procedure_Specification:
      /* Consider this a "definition" even though we won't actually be
	 making code for the subprogram here.  This is because if we
	 see the spec and are actually generating code, we know the body
	 must be in this same file.  */
      if (! Is_Delayed (Defining_Unit_Name (gnat_node)))
	gnat_to_gnu_entity (Defining_Unit_Name (gnat_node), NULL_TREE, 1);
      break;

    case N_Defining_Program_Unit_Name:
      /* For a child unit identifier go up a level to get the
         specificaton. */
      gnat_to_code (Parent (gnat_node));
      break;

    case N_Subprogram_Body:
      {
	/* Definining identifier of a parameter to the subprogram.  */
        gnat_tree gnat_param;
        /* The declared entity currently being processed in the declarative
	   part of the subprogram body.  */
        gnat_tree gnat_entity;
	/* The defining identifier for the subprogram body. Note that if a
	   specification has appeared before for this body, then the identifier
	   occurring in that specification will also be a defining identifier
	   and all the calls to this subprogram will point to that
	   specification.  */
	gnat_tree gnat_subprog_id
	  = (Present (Corresponding_Spec (gnat_node))
	     ? Corresponding_Spec (gnat_node)
	     : Defining_Unit_Name (Specification (gnat_node)));
	/* The FUNCTION_DECL node corresponding to the subprogram spec.   */
	tree gnu_subprog_decl;
	/* The FUNCTION_TYPE node corresponding to the subprogram spec.  */
	tree gnu_subprog_type;
	tree gnu_cico_list;

	/* If this is a generic object, ignore it.  */
	if (Ekind (gnat_subprog_id) == E_Generic_Procedure
	    || Ekind (gnat_subprog_id) == E_Generic_Function)
	  break;

	gnu_subprog_decl = gnat_to_gnu_entity (gnat_subprog_id, NULL_TREE, 1);
	gnu_subprog_type = TREE_TYPE (gnu_subprog_decl);

	begin_subprog_body (gnu_subprog_decl);
	set_lineno (gnat_node, 1);

	pushlevel (0);
  	expand_start_bindings (0);

	/* See if there are any parameters for which we don't yet have
	   GCC entities.  These must be for OUT parameters for which we
	   will be making VAR_DECL nodes here.  Fill them in to
	   TYPE_CI_CO_LIST, which must contain the empty entry as well.
	   We can match up the entries because TYPE_CI_CO_LIST is in the
	   order of the parameters.

	   If we make any new nodes here, make sure that they are in
	   the object that the function declaration's type is in because we
	   will be using them in the context of the caller.  */

	push_obstacks (TYPE_OBSTACK (gnu_subprog_type),
		       TYPE_OBSTACK (gnu_subprog_type));

	gnu_cico_list = TYPE_CI_CO_LIST (gnu_subprog_type);
	for (gnat_param = First_Formal (gnat_subprog_id);
	     Present (gnat_param);
	     gnat_param = Next_Formal (gnat_param))
	  if (! present_gnu_tree (gnat_param))
	    {
	      /* Skip any entries that have been already filled in; they
		 must correspond to IN OUT parameters.  */
	    for (; gnu_cico_list != 0 && TREE_VALUE (gnu_cico_list) != 0;
		 gnu_cico_list = TREE_CHAIN (gnu_cico_list))
	      ;

	    TREE_VALUE (gnu_cico_list)
	      = gnat_to_gnu_entity (gnat_param, NULL_TREE, 1);
	  }

	pop_obstacks ();

	/* Process the declarations inside the subprogram body.  */
	for (gnat_entity = First (Declarations (gnat_node));
	     Present (gnat_entity); gnat_entity = Next (gnat_entity))
	  gnat_to_code (gnat_entity);

	/* Generate the code of the subprogram itself.  A return statement
	   will be present and any OUT parameters will be handled there.  */
	gnat_to_code (Handled_Statement_Sequence (gnat_node));

	set_lineno (gnat_node, 1);
	expand_end_bindings (getdecls (), 1, 0);
	poplevel (kept_level_p (), 1, 0);
	end_subprog_body ();

	/* Throw away any VAR_DECLs we made for OUT parameters; they must
	   not be seen when we call this function and will be in 
	   unallocated memory anyway.  Also throw away DECL_RTL in
	   any PARM_DECLs unless this function was saved for inline, in
	   which case the DECL_RTLs are in preserved memory.  */
	for (gnat_param = First_Formal (gnat_subprog_id);
	     Present (gnat_param);
	     gnat_param = Next_Formal (gnat_param))
	  {
	    tree gnu_param = get_gnu_tree (gnat_param);

	    if (TREE_CODE (gnu_param) == VAR_DECL)
	      save_gnu_tree (gnat_param, NULL_TREE, 0);
	    else if (TREE_CODE (gnu_param) == PARM_DECL
		     && DECL_SAVED_INSNS (gnu_subprog_decl) == 0)
	      DECL_RTL (gnu_param) = DECL_INCOMING_RTL (gnu_param) = 0;
	  }

	/* Similarly, discard DECL_RTL of the return value.  */
	if (DECL_SAVED_INSNS (gnu_subprog_decl) == 0)
	  DECL_RTL (DECL_RESULT (gnu_subprog_decl))
	    = DECL_INCOMING_RTL (DECL_RESULT (gnu_subprog_decl)) = 0;
      }
      break;

    case N_Function_Call:
    case N_Procedure_Call_Statement:
      {
	/* The GCC node corresponding to the GNAT subprogram name.  This can
	   either be a FUNCTION_DECL node if we are dealing with a standard
	   subprogram call, or an indirect reference expression (an
	   INDIRECT_REF node) pointing to a subprogram.  */
	tree gnu_subprog_node = gnat_to_gnu (Name (gnat_node));
	/* The FUNCTION_TYPE node giving the GCC type of the subprogram.  */
	tree gnu_subprog_type = TREE_TYPE (gnu_subprog_node);
	tree gnu_subprog_addr
	  = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_subprog_node);
	gnat_tree gnat_formal;
	gnat_tree gnat_actual;
	tree gnu_actual_list = NULL_TREE;
	tree gnu_subprog_call;

	if (TREE_CODE (gnu_subprog_type) != FUNCTION_TYPE)
	  abort ();

	/* The only way we can be making a call via an access type is
	   if Name is an explicit dereference.  In that case, get the
	   list of formal args from the type the access type is pointing
	   to.  Otherwise, get the formals from entity being called.  */
	if (Nkind (Name (gnat_node)) == N_Explicit_Dereference)
	  gnat_formal = First_Formal (Etype (Name (gnat_node)));
	else
	  gnat_formal = First_Formal (Entity (Name (gnat_node)));

	/* Create the list of the actual parameters as GCC expects it, namely
	   a chain of TREE_LIST nodes in which the TREE_VALUE field of each
	   node is a parameter-expression and the TREE_PURPOSE field is
	   null.  Skip OUT parameters that are not passed by reference.  */

        for (gnat_actual = First_Actual (gnat_node);
             Present (gnat_actual);
             gnat_formal = Next_Formal (gnat_formal),
             gnat_actual = Next_Actual (gnat_actual))
	  {
	    tree gnu_actual = gnat_to_gnu (gnat_actual);
	    tree gnu_formal_type = gnat_to_gnu_type (Etype (gnat_formal));

	    if (Do_Range_Check (gnat_actual))
	      gnu_actual = emit_range_check (gnu_actual, Etype (gnat_formal));

	    /* If we have not saved a GCC object for the formal, it means
	       it is an OUT parameter not passed by reference.  Otherwise,
	       look at the PARM_DECL to see if it is passed by reference. */
	    if (present_gnu_tree (gnat_formal)
		&& DECL_BY_REF_P (get_gnu_tree (gnat_formal)))
	      {
		/* The symmetry of the paths to the type of an entity is
		   broken here since arguments don't know that they will
		   be passed by ref. */
		gnu_formal_type = TREE_TYPE (get_gnu_tree (gnat_formal));
		gnu_actual = build_unary_op (ADDR_EXPR, NULL_TREE, gnu_actual);
	      }
	    else if (! present_gnu_tree (gnat_formal)
		     || TREE_CODE (get_gnu_tree (gnat_formal)) != PARM_DECL)
	      continue;

	    gnu_actual_list
	      = chainon (gnu_actual_list,
			 build_tree_list (NULL_TREE,
					  convert (gnu_formal_type,
						   gnu_actual)));
	  }

	gnu_subprog_call = build (CALL_EXPR, TREE_TYPE (gnu_subprog_type),
				  gnu_subprog_addr, gnu_actual_list,
				  NULL_TREE);
	TREE_SIDE_EFFECTS (gnu_subprog_call) = 1;

	/* If it is a function call, the result is the call expression.  */
	if (Nkind (gnat_node) == N_Function_Call)
	  {
	    gnu_result = gnu_subprog_call;

	    /* If the function returns an unconstrained array, we have to
	       de-dereference the pointer.  */
	    if (TYPE_RETURNS_UNCONSTRAINED_P (gnu_subprog_type))
	      gnu_result = build_unary_op (INDIRECT_REF, NULL_TREE,
					   gnu_result);
	    break;
	  }

	/* If this is the case where the GNAT tree contains a procedure call
	   but the Ada procedure has copy in copy out parameters, the special
	   parameter passing mechanism must be used.  */
	else if (TYPE_CI_CO_LIST (gnu_subprog_type) != NULL_TREE)
	  {
	    /* List of FIELD_DECLs associated with the PARM_DECLs of the copy 
	       in copy out parameters.  */
	    tree scalar_return_list = TYPE_CI_CO_LIST (gnu_subprog_type);
	    int length = list_length (scalar_return_list);

	    if (length > 1)
	      gnu_subprog_call = save_expr (gnu_subprog_call);

	    if (Nkind (Name (gnat_node)) == N_Explicit_Dereference)
	      gnat_formal = First_Formal (Etype (Name (gnat_node)));
	    else
	      gnat_formal = First_Formal (Entity (Name (gnat_node)));

	    for (gnat_actual = First_Actual (gnat_node);
		 Present (gnat_actual);
		 gnat_formal = Next_Formal (gnat_formal),
		 gnat_actual = Next_Actual (gnat_actual))
	      /* If we are dealing with a copy in copy out parameter, we must
		 retrieve its value from the record returned in the function
		 call.  */
	      if (!pass_by_ref (gnat_to_gnu_type (Etype (gnat_formal)))
		  && Ekind (gnat_formal) != E_In_Parameter)
		{
		  /* Get the value to assign to this OUT or IN OUT 
		     parameter.  It is either the result of the function if
		     there is only a single such parameter or the appropriate
		     field from the record returned.  */
		  tree gnu_result 
		    = length == 1 ? gnu_subprog_call
		      : build_component_ref
			(gnu_subprog_call, NULL_TREE,
			 TREE_PURPOSE (scalar_return_list));
		  /* If the actual is a conversion, get the inner expression,
		     which will be the real destination, and convert the
		     result to the type of the actual parameter.  */
		  gnat_tree gnat_name
		    = Nkind (gnat_actual) == N_Type_Conversion
		      ? Expression (gnat_actual) : gnat_actual;
		  tree gnu_actual = gnat_to_gnu (gnat_name);

		  gnu_result = convert (TREE_TYPE (gnu_actual), gnu_result);

		  set_lineno (gnat_node, 1);
		  expand_expr_stmt (build_binary_op (MODIFY_EXPR, NULL_TREE,
						     gnu_actual, gnu_result));
		  scalar_return_list = TREE_CHAIN (scalar_return_list);
		}

	    break;
	  }

	set_lineno (gnat_node, 1);
	expand_expr_stmt (gnu_subprog_call);
      }
      break;

    /*************************/
    /* Chapter 7: Packages:  */
    /*************************/

    case N_Package_Declaration:
      /* The only time we actually see this node is if it is not the top-level
	 unit.  So just expand the specification.  */
      gnat_to_code (Specification (gnat_node));
      break;

    case N_Package_Specification:
      /* Process every declaration in the specification, both visible and
	 private.  */

      if (Present (Visible_Declarations (gnat_node)))
	for (gnat_temp = First (Visible_Declarations (gnat_node));
	     Present (gnat_temp); gnat_temp = Next (gnat_temp))
	  gnat_to_code (gnat_temp);

      if (Present (Private_Declarations (gnat_node)))
	for (gnat_temp = First (Private_Declarations (gnat_node));
	     Present (gnat_temp); gnat_temp = Next (gnat_temp))
	  gnat_to_code (gnat_temp);

      break;

    case N_Package_Body:
      /* If this is the body of a generic package - do nothing */
      if (Ekind (Corresponding_Spec (gnat_node)) == E_Generic_Package)
	break;

      /* The only time we get here is if we are not processing this
	 package body at the top level.  So just process all declarations and
	 statements.  Package declarations have the same persistence as those
	 in the containing object, so don't push a binding level here.

	 ??? This means that declarations and statements will be intermixed,
	 which might be trouble when we deal with cleanups, but worry about
	 it then.  */
      if (Present (Declarations (gnat_node)))
        for (gnat_temp = First (Declarations (gnat_node));
             Present (gnat_temp); gnat_temp = Next (gnat_temp))
	  gnat_to_code (gnat_temp);

      /* If we are at the top level, record that we must generate code for
	 any statements that actually belong in the body.  Otherwise,e
	 generate code now.  */
      if (Present (Handled_Statement_Sequence (gnat_node)))
	{
	  if (global_bindings_p ())
	    {
	      struct elab_list *p
		= (struct elab_list *) malloc (sizeof (struct elab_list));

	      p->next = 0;
	      p->block = Handled_Statement_Sequence (gnat_node);
	      if (first_to_elab)
		last_to_elab->next = p;
	      else
		first_to_elab = p;

	      last_to_elab = p;
	    }
	  else
	    gnat_to_code (Handled_Statement_Sequence (gnat_node));
	}
      break;

    /*********************************/
    /* Chapter 8: Visibility Rules:  */
    /*********************************/

    case N_Use_Package_Clause:
    case N_Use_Type_Clause:
      /* Nothing to do here - but these may appear in list of declarations */
      break;

    /***********************/
    /* Chapter 9: Tasks:   */
    /***********************/

    case N_Task_Type_Declaration:
      break;

    case N_Single_Task_Declaration:
      gnat_to_gnu_entity (Defining_Identifier (gnat_node), NULL_TREE, 1);
      break;

    /***********************************************************/
    /* Chapter 10: Program Structure and Compilation Issues:   */
    /***********************************************************/

    case N_Compilation_Unit:
      /* If we have a package body, process and make an elaboration
	 routine for the spec and then do the same for the body.  */
      if (Nkind (Unit (gnat_node)) == N_Package_Body)
	{
	  first_to_elab = 0;
	  gnat_to_code (Parent (Corresponding_Spec (Unit (gnat_node))));
	  build_package_elab (Corresponding_Spec (Unit (gnat_node)), 0,
			      get_pending_elaborations (), 0);

	  for (gnat_temp = First (Declarations (Unit (gnat_node)));
	       Present (gnat_temp);
	       gnat_temp = Next (gnat_temp))
	    gnat_to_code (gnat_temp);

	  build_package_elab (Defining_Unit_Name (Unit (gnat_node)), 1,
			      get_pending_elaborations (),
			      Handled_Statement_Sequence (Unit (gnat_node)));
	}

      /* If we have a package spec, handle the declarations and
	 elaboration function for it.  */
      else if (Nkind (Unit (gnat_node)) == N_Package_Declaration)
	{
	  gnat_tree gnat_spec = Specification (Unit (gnat_node));

	  first_to_elab = 0;
	  gnat_to_code (gnat_spec);

	  build_package_elab (Defining_Unit_Name (gnat_spec), 0,
			      get_pending_elaborations (), 0);
	}

      /* For library level subprogram bodies create (currently
	 empty) elaboration functions for spec and body.  */
      else if (Nkind (Unit (gnat_node)) == N_Subprogram_Body)
	{
	  build_subprogram_elab (Defining_Unit_Name
				 ( Specification (Unit (gnat_node))));
	  gnat_to_code (Unit (gnat_node));
	}


      /* Otherwise, process whatever unit we are called with.  */
      else
	  gnat_to_code (Unit (gnat_node));
      break;

    case N_Subprogram_Body_Stub:
    case N_Package_Body_Stub:
      /* Simply process whatever unit is being inserted.  */
      gnat_to_code (Library_Unit (gnat_node));
      break;

    case N_Subunit:
      gnat_to_code (Proper_Body (gnat_node));
      break;

    /***************************/
    /* Chapter 11: Exceptions: */
    /***************************/

    case N_Handled_Sequence_Of_Statements:
      /* If there are exeption handlers, start a new binding level that
	 we can exit (since each exception handler will do so).  Then
	 declare a variable to save the old __gnat_jmpbuf value and a
	 variable for our jmpbuf.  Call setjmp and handle each of the
	 possible exceptions if it returns one.

	 ??? We have a short-term kludge where we allow an N_Identifier
	 of a procedure to be in Node1.  In that case, exception
	 handlers aren't allowed.  We make an exception handler that
	 calls the specified function and does a re-raise.  We also
	 put a call to that function as a cleanup action for that block.  */

      if (Present (Exception_Handlers (gnat_node))
	  || Present (Node1 (gnat_node)))
	{
	  tree gnu_jmpsave_decl;
	  tree gnu_jmpbuf_decl;
	  tree gnu_cleanup_call = 0;
	  tree gnu_cleanup_decl;
	  tree gnu_call;
	  int moment = suspend_momentary ();

	  pushlevel (0);
	  expand_start_bindings (1);

	  gnu_jmpsave_decl = create_var_decl ("jmpbuf_save", NULL_PTR,
					      jmpbuf_ptr_type, jmpbuf_decl,
					      0, 0, 0, 0, 0);
	  gnu_jmpbuf_decl = create_var_decl ("jmp_buf", NULL_PTR, jmpbuf_type,
					     NULL_PTR, 0, 0, 0, 0, 0);

	  /* See if we are to call a function when exiting this block.  */
	  if (Present (Node1 (gnat_node)))
	    {
	      gnu_cleanup_call
		= build (CALL_EXPR, void_type_node,
			 build_unary_op (ADDR_EXPR, NULL_TREE,
					 gnat_to_gnu (Node1 (gnat_node))),
			 NULL_TREE, NULL_TREE);
	      TREE_SIDE_EFFECTS (gnu_cleanup_call) = 1;

	      gnu_cleanup_decl = create_var_decl ("cleanup", NULL_PTR,
						  integer_type_node,
						  NULL_PTR, 0, 0, 0, 0, 0);

	      expand_decl_cleanup (gnu_cleanup_decl, gnu_cleanup_call);
	    }

	  /* When we exit this block, restore the saved value.  */
	  expand_decl_cleanup (gnu_jmpsave_decl,
			       build_binary_op (MODIFY_EXPR, NULL_TREE,
						jmpbuf_decl,
						gnu_jmpsave_decl));

	  resume_momentary (moment);

	  /* Call setjmp and handle exceptions if it returns one.  */
	  gnu_call
	    = build (CALL_EXPR, integer_type_node,
		     build_unary_op (ADDR_EXPR, NULL_TREE, setjmp_decl),
		     chainon (NULL_TREE,
			      build_tree_list
			      (NULL_TREE,
			       build_unary_op (ADDR_EXPR, NULL_TREE,
					       gnu_jmpbuf_decl))),
		     NULL_TREE);
	  TREE_SIDE_EFFECTS (gnu_call) = 1;

	  set_lineno (gnat_node, 1);
	  expand_start_cond (gnu_call, 0);

	  /* Restore our incoming longjmp value before we do anything.  */
	  expand_expr_stmt (build_binary_op (MODIFY_EXPR, NULL_TREE,
					     jmpbuf_decl,
					     gnu_jmpsave_decl));

	  /* If we have a cleanup to do, emit it now.  */
	  if (gnu_cleanup_call)
	    expand_expr_stmt (gnu_cleanup_call);

	  /* Generate code for each exception handler.  The code at
	     N_Exception_Handler below does the real work.  */
	  if (Present (Exception_Handlers (gnat_node)))
	    for (gnat_temp = First (Exception_Handlers (gnat_node));
		 Present (gnat_temp); gnat_temp = Next (gnat_temp))
	      gnat_to_code (gnat_temp);

	  /* If none of the exception handlers did anything, re-raise.  */

	  gnu_call
	    = build (CALL_EXPR, void_type_node,
		     build_unary_op (ADDR_EXPR, NULL_TREE, longjmp_decl),
		     chainon (chainon (NULL_TREE,
				       build_tree_list (NULL_TREE,
							jmpbuf_decl)),
			      build_tree_list (NULL_TREE, integer_one_node)),
		     NULL_TREE);
	  TREE_SIDE_EFFECTS (gnu_call) = 1;
	  set_lineno (gnat_node, 1);
	  expand_expr_stmt (gnu_call);

	  /* End the "if" on setjmp.  Note that we have arranged things so 
	     control never returns here.  */

	  expand_end_cond ();

	  /* This is now immediately before the body proper.  Point
	     __gnat_jmpbuf to our jmp_buf.  */
	  expand_expr_stmt
	    (build_binary_op (MODIFY_EXPR, NULL_TREE, jmpbuf_decl,
			      build_unary_op (ADDR_EXPR, NULL_TREE,
					      gnu_jmpbuf_decl)));
	}

      /* Generate code for each statement in the block.  */
      push_momentary ();
      for (gnat_temp = First (Statements (gnat_node));
	   Present (gnat_temp); gnat_temp = Next (gnat_temp))
	{
	  gnat_to_code (gnat_temp);
	  clear_momentary ();
	}

      pop_momentary ();

      /* If we have handlers, close the block we made.  */
      if (Present (Node1 (gnat_node))
	  || Present (Exception_Handlers (gnat_node)))
	{
	  set_lineno (gnat_node, 1);
	  expand_end_bindings (getdecls (), 1, 0);
	  poplevel (kept_level_p (), 1, 0);
	}
      break;

    case N_Exception_Handler:
      {
	/* Unless this is "Others", make an "if" statement to select
	   the proper exceptions.  */
	tree gnu_choice = integer_zero_node;

	for (gnat_temp = First (Exception_Choices (gnat_node));
	     gnat_temp; gnat_temp = Next (gnat_temp))
	  {
	    if (Nkind (gnat_temp) == N_Others_Choice)
	      gnu_choice = integer_one_node;
	    else if (Nkind (gnat_temp) == N_Identifier
                     || Nkind (gnat_temp) == N_Expanded_Name)
	      {
		tree this_choice
		  = build_binary_op (EQ_EXPR, integer_type_node,
				     excptr_decl,
				     build_unary_op (ADDR_EXPR, NULL_TREE,
						     gnat_to_gnu (gnat_temp)));

		gnu_choice = build_binary_op (TRUTH_ORIF_EXPR,
					      integer_type_node,
					      gnu_choice, this_choice);
	      }
	    else
	      abort ();
	  }

	expand_start_cond (gnu_choice, 0);

	/* Generate code for the exception handler.  */
	push_momentary ();
	for (gnat_temp = First (Statements (gnat_node));
	     gnat_temp; gnat_temp = Next (gnat_temp))
	  {
	    gnat_to_code (gnat_temp);
	    clear_momentary ();
	  }

	/* At the end of the handler, exit the block.  We made this block
	   in N_Handled_Sequence_Of_Statements.  */
	pop_momentary ();
	expand_exit_something ();
	expand_end_cond ();
      }
      break;

    /*******************************/
    /* Chapter 12: Generic Units:  */
    /*******************************/

    case N_Generic_Package_Declaration:
    case N_Generic_Subprogram_Declaration:
    case N_Package_Instantiation:
    case N_Procedure_Instantiation:
    case N_Function_Instantiation:
      /* These nodes can appear on a declaration list but there is nothing to
	 to be done with them.  */
      break;


    /***************************************************/
    /* Chapter 13: Representation Clauses and	       */
    /*             Implementation-Dependent Features:  */
    /***************************************************/

    case N_Attribute_Definition_Clause:
      /* The only one we need deal with is for 'Address.  For the others, SEM
	 puts the information elsewhere.  */
      if (Get_Attribute_Id (Chars (Identifier (gnat_node))) != Attr_Address)
	break;

      /* Get the object being placed at a specified address and verify
	 that it has not yet been elaborated.  */
      gnat_temp = Entity (Name (gnat_node));
      if (! Is_Delayed (gnat_temp)
	  || present_gnu_tree (gnat_temp))
	abort ();

      /* Get the value to use as the address and save it as the 
	 equivalent for GNAT_TEMP.  When the object is frozen, 
	 gnat_to_gnu_entity will do the right thing. */
      gnu_expr = gnat_to_gnu (Expression (gnat_node));
      save_gnu_tree (gnat_temp, gnu_expr, 1);
      break;

    case N_Enumeration_Representation_Clause:
    case N_Record_Representation_Clause:
    case N_At_Clause:
      /* We do nothing with these.  SEM puts the information elsewhere.  */
      break;

    /***************************************************/
    /* Added Nodes	                               */
    /***************************************************/

    case N_Freeze_Entity:
      {
	gnat_tree gnat_entity = Entity (gnat_node);
	tree gnu_old
	  = present_gnu_tree (gnat_entity) ? get_gnu_tree (gnat_entity) : 0;
	tree gnu_new;
	tree gnu_init
	  = (Nkind (Parent (gnat_entity)) == N_Object_Declaration
	     && present_gnu_tree (Parent (gnat_entity)))
	    ? get_gnu_tree (Parent (gnat_entity)) : NULL_TREE;

	/* If GNAT_ENTITY is not delayed, ignore this node.  */
	if (! Is_Delayed (gnat_entity))
	  break;

	/* If this entity has an Address representation clause,
	   GNU_OLD is the address, so discard it here.  */
	if (Has_Address_Clause (gnat_entity))
	  gnu_old = 0;

	/* If we have a non-VOID_TYPE old tree, this node was never
	   delayed as it should have been.  */
	if (gnu_old != 0
	    && ! (TREE_CODE (gnu_old) == TYPE_DECL
		  && TREE_CODE (TREE_TYPE (gnu_old)) == VOID_TYPE))
	  abort ();

	/* Reset the saved tree, if any, and elaborate the object or
	   type for real.  If there is a full declaration, elaborate
	   it and copy the type to GNAT_ENTITY.  */
	if (gnu_old != 0)
	  {
	    save_gnu_tree (gnat_entity, NULL_TREE, 0);
	    if (IN (Ekind (gnat_entity), Incomplete_Kind)
		&& Present (Full_Declaration (gnat_entity))
		&& present_gnu_tree (Full_Declaration (gnat_entity))
		&& get_gnu_tree (Full_Declaration (gnat_entity)) == gnu_old)
	      save_gnu_tree (Full_Declaration (gnat_entity), NULL_TREE, 0);
	  }

	if (IN (Ekind (gnat_entity), Incomplete_Kind)
	    && Present (Full_Declaration (gnat_entity)))
	  {
	    gnu_new = gnat_to_gnu_entity (Full_Declaration (gnat_entity),
					  NULL_TREE, 0);
	    save_gnu_tree (gnat_entity, gnu_new, 0);
	  }
	else
	  gnu_new = gnat_to_gnu_entity (gnat_entity, gnu_init, 1);

	/* If we've made any pointers to the old version of this type, we have
	   to update them.  */
	if (gnu_old != 0 && TYPE_POINTER_TO (TREE_TYPE (gnu_old)) != 0)
	  {
	    TREE_TYPE (TYPE_POINTER_TO (TREE_TYPE (gnu_old)))
	      = TREE_TYPE (gnu_new);
	    TYPE_POINTER_TO (TREE_TYPE (gnu_new))
	      = TYPE_POINTER_TO (TREE_TYPE (gnu_old));
	  }
      }
      break;

    case N_Op_Concat:
    case N_Component_Association:
    case N_Extension_Aggregate:
    case N_Task_Body:
    default:
      post_error ("unsupported tree node~!", gnat_node);
      abort ();
    }

  /* Now convert the result to the proper type.  If the type is void or if
     we have no result, return error_mark_node to show we have no result.
     If the type of the result is correct or if we have a label (which doesn't
     have any well-defined type), return our result.  Also don't do the
     conversion if the "desired" type involves a PLACEHOLDER_EXPR in its size
     since those are the cases where the front end may have the type wrong due
     to "instantiating" the unconstrained record with discriminant values.
     Otherwise, convert the result to the proper type.  */

  if (TREE_CODE (gnu_result) == LABEL_DECL
      || (TYPE_SIZE (gnu_result_type) 
	  && TREE_CODE (TYPE_SIZE (gnu_result_type)) != INTEGER_CST
	  && contains_placeholder_p (TYPE_SIZE (gnu_result_type))))
    ;
  else if (gnu_result == error_mark_node
	   || gnu_result_type == void_type_node)
    gnu_result =  error_mark_node;
  else if (gnu_result_type != TREE_TYPE (gnu_result))
    gnu_result = convert (gnu_result_type, gnu_result);

  /* We don't need any NOP_EXPR or NON_LVALUE_EXPR on GNU_RESULT.  */
  while ((TREE_CODE (gnu_result) == NOP_EXPR
	  || TREE_CODE (gnu_result) == NON_LVALUE_EXPR)
	 && TREE_TYPE (TREE_OPERAND (gnu_result, 0)) == TREE_TYPE (gnu_result))
    gnu_result = TREE_OPERAND (gnu_result, 0);

  /* If this is an expression we are to only evaluate once, make a SAVE_EXPR
     and store it.  */
  if (IN (Nkind (gnat_node), N_Subexpr) && Evaluate_Once (gnat_node))
    {
      gnu_result = make_save_expr (gnu_result);
      save_gnu_tree (gnat_node, gnu_result, 1);
    }

  return gnu_result;
}

/* Emits an access check. GNU_EXPR is the expression that needs to be
   checked against the NULL pointer. */

static tree
emit_access_check (gnu_expr)
     tree gnu_expr;
{
  /* Checked expressions must be evaluated only once. */
  gnu_expr = make_save_expr (gnu_expr);

  return emit_check (build_binary_op (EQ_EXPR, integer_type_node,
				      gnu_expr,
				      convert (TREE_TYPE (gnu_expr),
					       integer_zero_node)),
		     gnu_expr);
}

/* Emits a discriminant check. GNU_EXPR is the expression to be checked and
   GNAT_NODE a N_Selected_Component node. */

static tree
emit_discriminant_check (gnu_expr, gnat_node)
     tree gnu_expr;
     gnat_tree gnat_node;
{
  gnat_tree gnat_discr_fct =
    Discriminant_Checking_Func (Entity (Selector_Name (gnat_node)));
  tree gnu_discr_fct;
  gnat_tree gnat_discr;
  tree gnu_actual_list = NULL_TREE;
  tree gnu_cond;

  if (! Present (gnat_discr_fct))
    return gnu_expr;
  /* abort (); */

  gnu_discr_fct = gnat_to_gnu_entity (gnat_discr_fct, NULL_TREE, 0); 

  /* Checked expressions must be evaluated only once. */
  gnu_expr = make_save_expr (gnu_expr);

  /* Create the list of the actual parameters as GCC expects it.
     This list is the list of the discriminant fields of the
     record expression to be discriminant checked. For documentation
     on what is the GCC format for this list see under the
     N_Function_Call case */
  for (gnat_discr = First_Discriminant (Etype (Entity
					       (Prefix (gnat_node))));
       Present (gnat_discr); gnat_discr = Next_Discriminant (gnat_discr))
    {
      tree gnu_discr = gnat_to_gnu_entity (gnat_discr, NULL_TREE, 0);
      /* the IDENTIFIER_NODE whose name is that of the discriminant */
      tree gnu_discr_id_node
	= get_identifier (Get_Name_String (Chars (gnat_discr)));

      gnu_actual_list
	= chainon (gnu_actual_list,
		   build_tree_list (NULL_TREE,
				    build_component_ref (gnu_expr,
							 gnu_discr_id_node,
							 gnu_discr)));
    }

  gnu_cond = build (CALL_EXPR,
		    TREE_TYPE (TREE_TYPE (gnu_discr_fct)),
		    build_unary_op (ADDR_EXPR,
				    NULL_TREE,
				    gnu_discr_fct),
		    gnu_actual_list,
		    NULL_TREE);
  TREE_SIDE_EFFECTS (gnu_cond) = 1;

  return emit_check (gnu_cond, gnu_expr);
}

/* Emit code for a range check. GNU_EXPR is the expression to be checked,
   GNAT_RANGE_TYPE the gnat type or subtype containing the bounds against
   which we have to check. */

static tree
emit_range_check (gnu_expr, gnat_range_type)
     tree gnu_expr;
     gnat_tree gnat_range_type;
{
  tree gnu_range_type = gnat_to_gnu_type (gnat_range_type);
  tree gnu_low  = TYPE_MIN_VALUE (gnu_range_type);
  tree gnu_high = TYPE_MAX_VALUE (gnu_range_type);

  /* If GNU_EXPR has an integral type that is narrower than GNU_RANGE_TYPE,
     we can't do anything since we might be truncating the bounds.  No
     check is needed in this case.  */
  if (INTEGRAL_TYPE_P (TREE_TYPE (gnu_expr))
      && (TYPE_PRECISION (TREE_TYPE (gnu_expr))
	  < TYPE_PRECISION (gnu_range_type)))
    return gnu_expr;

  /* Checked expressions must be evaluated only once. */
  gnu_expr = make_save_expr (gnu_expr);

  /* There's no good type to use here, so we might as well use
     integer_type_node.  */
  return emit_check
    (build_binary_op (TRUTH_ORIF_EXPR, integer_type_node,
		      build_binary_op (LT_EXPR, integer_type_node,
				       gnu_expr,
				       convert (TREE_TYPE (gnu_expr),
						gnu_low)),
		      build_binary_op (GT_EXPR, integer_type_node,
				       gnu_expr,
				       convert (TREE_TYPE (gnu_expr),
						gnu_high))),
     gnu_expr);
}

/* Emit code for an index check. GNU_ARRAY_OBJECT is the array object
   which we are about to index, GNU_EXPR is the index expression to be
   checked, GNU_LOW and GNU_HIGH are the lower and upper bounds
   against which GNU_EXPR has to be checked. Note that for index
   checking we cannot use the emit_range_check function (although very
   similar code needs to be generated in both cases) since for index
   checking the array type against which we are checking the indeces
   may be unconstrained and consequently we need to retrieve the
   actual index bounds from the array object itself
   (GNU_ARRAY_OBJECT). The place where we need to do that is in
   subprograms having unconstrained array formal parameters */

static tree
emit_index_check (gnu_array_object, gnu_expr, gnu_low, gnu_high)
     tree gnu_array_object;
     tree gnu_expr;
     tree gnu_low;
     tree gnu_high;
{
  /* Checked expressions must be evaluated only once. */
  gnu_expr = make_save_expr (gnu_expr);

  /* If GNU_LOW or GNU_HIGH are a PLACEHOLDER_EXPR, qualify them by
     the object we are handling. */
  if (TREE_CODE (gnu_low) != INTEGER_CST && contains_placeholder_p (gnu_low))
    gnu_low = build (WITH_RECORD_EXPR, TREE_TYPE (gnu_low),
		     gnu_low, gnu_array_object);

  if (TREE_CODE (gnu_high) != INTEGER_CST && contains_placeholder_p (gnu_high))
    gnu_high = build (WITH_RECORD_EXPR, TREE_TYPE (gnu_high),
		      gnu_high, gnu_array_object);

  /* There's no good type to use here, so we might as well use
     integer_type_node.   */
  return emit_check
    (build_binary_op (TRUTH_ORIF_EXPR, integer_type_node,
		      build_binary_op (LT_EXPR, integer_type_node,
				       gnu_expr,
				       convert (TREE_TYPE (gnu_expr),
						gnu_low)),
		      build_binary_op (GT_EXPR, integer_type_node,
				       gnu_expr,
				       convert (TREE_TYPE (gnu_expr),
						gnu_high))),
     gnu_expr);
}

/* Given GNU_COND which contains the condition corresponding to an access,
   discriminant or range check, of value GNU_EXPR, build a COND_EXPR
   that returns GNU_EXPR if GNU_COND is false and raises a 
   CONSTRAINT_ERROR if GNU_COND is true.  */

static tree
emit_check (gnu_cond, gnu_expr)
     tree gnu_cond;
     tree gnu_expr;
{
  tree gnu_call = build (CALL_EXPR, void_type_node,
			 build_unary_op (ADDR_EXPR, NULL_TREE,
					 raise_constraint_decl),
			 NULL_TREE, NULL_TREE);

  TREE_SIDE_EFFECTS (gnu_call) = 1;

  /* Use an outer COMPOUND_EXPR to make sure that GNU_EXPR will
     get evaluated in front of the comparison in case it ends
     up being a SAVE_EXPR.  */

  return build (COMPOUND_EXPR, TREE_TYPE (gnu_expr), gnu_expr,
		build (COND_EXPR, TREE_TYPE (gnu_expr), gnu_cond,
		       build (COMPOUND_EXPR, TREE_TYPE (gnu_expr),
			      gnu_call, gnu_expr),
		       gnu_expr));
}		

/* GNAT_ASSOC is a sub-part (possibly the front) of the Component_Associations
   of an N_Aggregate.  GNU_TYPE is the GCC type of the corresponding
   record.  GNU_SO_FAR is the part of the CONSTRUCTOR list before the
   variant, if any so far.  This list must contain the discriminants.
   Return a CONSTRUCTOR to initialize the record.

   This function is called recursively to handle variant parts.  We rely
   heavily on the fact that a variant must be the last thing in a record.

   We also assume that the front end has sorted all the fields and eliminated
   any multiple choices.  These assumptions are verified.  */

static tree
assoc_to_constructor (gnat_assoc, gnu_type, gnu_so_far)
     gnat_tree gnat_assoc;
     tree gnu_type;
     tree gnu_so_far;
{
  tree gnu_field, gnu_list;

  /* We test for GNU_FIELD being empty in the case where a variant
     was the last thing since we don't take things off GNAT_ASSOC in
     that case.  We check GNAT_ASSOC in case we have a variant, but it
     has no fields.  */

  for (gnu_field = TYPE_FIELDS (gnu_type), gnu_list = NULL_TREE;
       gnu_field != 0 && Present (gnat_assoc);
       gnat_assoc = Next (gnat_assoc), gnu_field = TREE_CHAIN (gnu_field))
    {
      gnat_tree gnat_field = First (Choices (gnat_assoc));
      char *field_name = Get_Name_String (Chars (gnat_field));
      tree gnu_expr;

      /* The expander is supposed to put a single component selector name
	 in every record component association */
      if (Next (gnat_field))
	abort ();

      /* There are only two valid possibilities: GNU_FIELD is either an
	 unnamed QUAL_UNION_TYPE that represents a variant part or it is
	 the desired field.  If it is a variant part, find the variant whose
	 DECL_QUALIFIER is true.  It might seem that we could just look
	 for the variant with the proper-named field, but we might have
	 an arbitrary number of nested variants before the first named
	 field, so this is simpler.  */

      if (DECL_NAME (gnu_field) == 0
	  && TREE_CODE (TREE_TYPE (gnu_field)) == QUAL_UNION_TYPE)
	{
	  tree gnu_variant;
	  tree gnu_discrims = gnu_so_far ? gnu_so_far : gnu_list;

	  for (gnu_variant = TYPE_FIELDS (TREE_TYPE (gnu_field));
	       gnu_variant; gnu_variant = TREE_CHAIN (gnu_variant))
	    {
	      tree gnu_qual = DECL_QUALIFIER (gnu_variant);
	      tree gnu_discrim;

	      for (gnu_discrim = gnu_discrims;
		   gnu_discrim; gnu_discrim = TREE_CHAIN (gnu_discrim))
		if (DECL_DISCRIMINANT_P (TREE_PURPOSE (gnu_discrim)))
		  gnu_qual = substitute_in_expr (gnu_qual,
						 TREE_PURPOSE (gnu_discrim),
						 TREE_VALUE (gnu_discrim));

	      /* If we found it, get the CONSTRUCTOR for the variant.
		 Then surround it with a CONSTRUCTOR for
		 the QUAL_UNION_TYPE.  */
	      if (integer_onep (gnu_qual))
		{
		  tree gnu_inner
		    = assoc_to_constructor (gnat_assoc,
					    TREE_TYPE (gnu_variant),
					    gnu_discrims);

		  gnu_expr = build_constructor (TREE_TYPE (gnu_field),
						tree_cons (gnu_variant,
							   gnu_inner,
							   NULL_TREE));
		  break;
		}
	    }
	}
      else if (0 == strcmp (field_name,
			    IDENTIFIER_POINTER (DECL_NAME (gnu_field))))
	{
	  gnu_expr = gnat_to_gnu (Expression (gnat_assoc));
	  /* Before assigning a value in an aggregate make sure range checks
	     are done if required */
	  if (Do_Range_Check (Expression (gnat_assoc)))
	    gnu_expr = emit_range_check (gnu_expr, Etype (gnat_field));
	}
      else
	abort ();

      /* Add the field and expression to the list.  */
      gnu_list = tree_cons (gnu_field, gnu_expr, gnu_list);
    }

  /* Return the final CONSTRUCTOR.  We need not reverse the fields, but
     do so for cleanliness.  */
  return build_constructor (gnu_type, nreverse (gnu_list));
}

/* Builds a possibly nested constructor for array aggregates. GNAT_EXPR
   is the first element of an array aggregate. It may itself be an
   aggregate (an array or record aggregate). GNU_ARRAY_TYPE is the gnu type
   corresponding to the array aggregate. GNAT_COMPONENT_TYPE is the type
   of the array component. It is needed for range checking. */

static tree
pos_to_constructor (gnat_expr, gnu_array_type, gnat_component_type)
     gnat_tree gnat_expr;
     tree gnu_array_type;
     gnat_tree gnat_component_type;     
{
  tree gnu_expr;
  tree gnu_expr_list = NULL_TREE;

  for ( ; Present (gnat_expr); gnat_expr = Next (gnat_expr))
    {
      /* if the expression is itself an array aggregate then build first
	 the innermost constructor */
      if (Nkind (gnat_expr) == N_Aggregate &&
	  TREE_CODE (TREE_TYPE (gnu_array_type)) == ARRAY_TYPE)
	gnu_expr = pos_to_constructor (First (Expressions (gnat_expr)),
				       TREE_TYPE (gnu_array_type),
				       gnat_component_type);
      else
	{
	  gnu_expr = gnat_to_gnu (gnat_expr);

	  /* before assigning the element to the array make sure it is
	     in range */
	  if (Do_Range_Check (gnat_expr))
	    gnu_expr = emit_range_check (gnu_expr, gnat_component_type);
	}
      gnu_expr_list = tree_cons (NULL_TREE, gnu_expr, gnu_expr_list);
    }

  return build_constructor (gnu_array_type, nreverse (gnu_expr_list));
}

/* EXP is to be treated as an array or record.  Handle the cases when it is
   an access object and perform the required dereferences.  */

static tree
maybe_implicit_deref (exp)
     tree exp;
{
  /* If the type is a pointer, dereference it.  */
  if (TREE_CODE (TREE_TYPE (exp)) == POINTER_TYPE
      || TYPE_FAT_POINTER_P (TREE_TYPE (exp)))
    exp = build_unary_op (INDIRECT_REF, NULL_TREE, exp);

  return exp;
}

/* Surround EXP with a SAVE_EXPR, but if it is an UNCONSTRAINED_ARRAY_REF,
   surround it's first operand instead.  */

static tree
make_save_expr (exp)
     tree exp;
{
  if (TREE_CODE (exp) == UNCONSTRAINED_ARRAY_REF)
    return build1 (UNCONSTRAINED_ARRAY_REF, TREE_TYPE (exp),
		   save_expr (TREE_OPERAND (exp, 0)));

  return save_expr (exp);
}

/* If EXP is an UNCONSTRAINED_ARRAY_REF, return an expression that refers
   to the underlying array.  */

static tree
maybe_unconstrained_array (exp)
     tree exp;
{
  if (TREE_CODE (exp) == UNCONSTRAINED_ARRAY_REF)
    exp = build_unary_op (INDIRECT_REF, NULL_TREE,
			  build_component_ref (TREE_OPERAND (exp, 0),
					       get_identifier ("p_array"),
					       NULL_TREE));

  return exp;
}

/* GNAT_UNIT is the Defining_Identifier for some package, either a
   spec or a body, BODY_P says which.  Make a function to be the
   elaboration routine for that object, perform the elaborations
   in GNU_ELAB_LIST, and then emit any statements in the global list
   FIRST_TO_ELAB followed by GNAT_STATEMENTS, which, if present, is a
   Handled_Sequence_Of_Statements. */

static void
build_package_elab (gnat_unit, body_p, gnu_elab_list, gnat_statements)
     gnat_tree gnat_unit;
     int body_p;
     tree gnu_elab_list;
     gnat_tree gnat_statements;
{
  tree gnu_decl;
  struct elab_list *elab;

  /* Set our file and line number to that of the object and set up the 
     elaboration routine.  */
  gnu_decl = create_subprog_decl (create_concat_name (gnat_unit,
						      body_p ?
						      "elabb" : "elabs"),
				  0, void_ftype, NULL_TREE, 0, 1, 0, 0);

  begin_subprog_body (gnu_decl);
  set_lineno (gnat_unit, 1);
  pushlevel (0);
  expand_start_bindings (0);

  /* Emit the assignments for the elaborations we have to do.  If there
     is no destination, this is just a call to execute some statement
     that was placed within the declarative region.  */
  for (; gnu_elab_list; gnu_elab_list = TREE_CHAIN (gnu_elab_list))
    if (TREE_PURPOSE (gnu_elab_list) == NULL_TREE)
      expand_expr_stmt (TREE_VALUE (gnu_elab_list));
    else
      expand_expr_stmt (build_binary_op (MODIFY_EXPR, NULL_TREE,
					 TREE_PURPOSE (gnu_elab_list),
					 TREE_VALUE (gnu_elab_list)));

  /* Process initialization for any bodies we encountered among the
     definitions.  */
  for (elab = first_to_elab; elab; elab = elab->next)
    gnat_to_code (elab->block);

  /* Now generate code for any actual statements in the body.  */
  if (Present (gnat_statements))
    gnat_to_code (gnat_statements);

  expand_end_bindings (getdecls (), 1, 0);
  poplevel (0, 0, 0);
  end_subprog_body ();
}

/* GNAT_UNIT is the Defining_Identifier for some subprogram.
   Make elaboration functions for the spec and body of it.
   Currently the function just returns -- eventually it may
   have some elaboration order checks in it.  */

static void
build_subprogram_elab (gnat_unit)
     gnat_tree gnat_unit;
{
  tree gnu_decl;

  /* Set our file and line number to that of the object and set up the 
     elaboration routine.  */
  gnu_decl = create_subprog_decl (create_concat_name (gnat_unit, "elabb"),
				  0, void_ftype, NULL_TREE, 0, 1, 0, 0);
  begin_subprog_body (gnu_decl);
  set_lineno (gnat_unit, 1);
  pushlevel (0);
  expand_start_bindings (0);

  expand_end_bindings (getdecls (), 1, 0);
  poplevel (0, 0, 0);
  end_subprog_body ();

  gnu_decl = create_subprog_decl (create_concat_name (gnat_unit, "elabs"),
				  0, void_ftype, NULL_TREE, 0, 1, 0, 0);
  begin_subprog_body (gnu_decl);
  set_lineno (gnat_unit, 1);
  pushlevel (0);
  expand_start_bindings (0);

  expand_end_bindings (getdecls (), 1, 0);
  poplevel (0, 0, 0);
  end_subprog_body ();
}

/* Determine the input_filename and the lineno from the source location
   (Sloc) of GNAT_NODE node.  Set the global variable input_filename and
   lineno.  If WRITE_NOTE_P is true, emit a line number note.  */

void
set_lineno (gnat_node, write_note_p)
     gnat_tree gnat_node;
     int write_note_p;
{
  extern Int Number_Units;
  extern struct Needed_File_Info *File_Info_Ptr;
  extern int **Lines_Ptrs;
  int current_unit_number;
  int Last_Line;
  Source_Ptr source_location = Sloc (gnat_node);
  int Lo = 0;
  int Hi;
  int Mid;

  /* If node not from source code, ignore.  */
  if (source_location < 0)
    return;

  for (current_unit_number = 0; current_unit_number < Number_Units;
       current_unit_number++)
    {
      if (source_location >= File_Info_Ptr[current_unit_number].First_Sloc
	  && source_location <= File_Info_Ptr[current_unit_number].Last_Sloc)
	break;
    }

  sloc_offset
    = source_location - File_Info_Ptr[current_unit_number].First_Sloc;

  if (current_unit_number == Number_Units)
    abort ();

  input_filename
    = Get_Name_String (File_Info_Ptr[current_unit_number].File_Name);

  Hi = File_Info_Ptr[current_unit_number].Last_Line - 1;

  for (;;)
    {
      Mid = (Lo + Hi) / 2;
      if (source_location < Lines_Ptrs[current_unit_number][Mid])
	Hi = Mid - 1;
      else if (Mid == Hi
	       || source_location < Lines_Ptrs[current_unit_number][Mid + 1])
	{
	  lineno = Mid + 1;
	  break;
	}
      else
	Lo = Mid + 1;
    }

  if (write_note_p)
    emit_line_note (input_filename, lineno);
}

/* Post an error message.  MSG is the error message, properly annotated.
   NODE is the node at which to post the error and the node to use for the
   "&" substitution.  */

void
post_error (msg, node)
     char *msg;
     gnat_tree node;
{
  struct template {int first, last; } temp = {1, strlen (msg)};
  struct fat_pointer { char *array; struct template *temp; } fp = {msg, &temp};

  errout__error_msg_n (fp, node);
}

/* Initialize the table that maps GNAT codes to GCC codes for simple
   binary and unary operations.  */

void
init_code_table ()
{
  gnu_codes[N_Op_And] = TRUTH_AND_EXPR;
  gnu_codes[N_Op_And_Then] = TRUTH_ANDIF_EXPR;
  gnu_codes[N_Op_Or] = TRUTH_OR_EXPR;
  gnu_codes[N_Op_Or_Else] = TRUTH_ORIF_EXPR;
  gnu_codes[N_Op_Xor] = TRUTH_XOR_EXPR;
  gnu_codes[N_Op_Eq] = EQ_EXPR;
  gnu_codes[N_Op_Ne] = NE_EXPR;
  gnu_codes[N_Op_Lt] = LT_EXPR;
  gnu_codes[N_Op_Le] = LE_EXPR;
  gnu_codes[N_Op_Gt] = GT_EXPR;
  gnu_codes[N_Op_Ge] = GE_EXPR;
  gnu_codes[N_Op_Add] = PLUS_EXPR;
  gnu_codes[N_Op_Subtract] = MINUS_EXPR;
  gnu_codes[N_Op_Multiply] = MULT_EXPR;
  gnu_codes[N_Op_Mod] = FLOOR_MOD_EXPR;
  gnu_codes[N_Op_Rem] = TRUNC_MOD_EXPR;
  gnu_codes[N_Op_Expon] = EXPON_EXPR;
  gnu_codes[N_Op_Minus] = NEGATE_EXPR;
  gnu_codes[N_Op_Abs] = ABS_EXPR;
  gnu_codes[N_Op_Not] = TRUTH_NOT_EXPR;
}
