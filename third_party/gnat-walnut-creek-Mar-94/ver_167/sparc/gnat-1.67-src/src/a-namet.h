/****************************************************************************/
/*									    */
/*			   GNAT COMPILER COMPONENTS			    */
/*									    */
/*				 N A M E T				    */
/*									    */
/*			       Specification				    */
/*									    */
/*			      $Revision: 1.12 $ 			    */
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

/* This is the C file that corresponds to the Ada package specification
   Namet. It was created manually from files namet.ads and namet.adb.

   This package contains routines for handling the names table. The table
   is used to store character strings for identifiers and operator symbols.
   The forms of the entries are as follows:

	Identifiers		Upper case letters folded to lower case.
				Character set encoding and folding conventions
				are the same as the source program.

	Operator symbols	Stored with surrounding quotes. All letters
				are lower case.

  The names are hashed so that a given name appears only once in the table.

   Note: character literals are not hashed into this table, since they can
   be represented directly using the appropriate Char_Code value.

   A tree node pointer is stored with each names table entry which is used
   to point to a chain of linked entities. The management of this chain
   used for visibility and identification processing, is fully described
   in the Entity_Table package.  */


/* Structure defining a names table entry.  */

struct Name_Entry
{
  Int Name_Chars_Index; /* Starting location of char in Name_Chars table. */
  Short Name_Len;	  /* Length of this name in characters. */
  Byte Byte_Info;	/* Byte value associated with this name */
  Byte Spare;		/* Unused */
  Name_Id Hash_Link;	/* Link to next entry in names table for same hash
			   code. Not accessed by C routines.  */
  Int Int_Info; 	/* Int value associated with this name */
};

/* Pointer to names table vector. This pointer is passed to the tree
   transformer and stored in a global location for access from here after
   subtracting Names_First_Entry so that Name_Id values can be used as
   subscripts into this table.	*/
extern struct Name_Entry *Names_Ptr;

/* Pointer to name characters table. This pointer is passed to the tree
   transformer and stored in a global location for access from here.  */
extern char *Name_Chars_Ptr;

/* The only function defined in the C header is Get_Name_String which returns
   a null terminated C string for the specified name. All the other subprograms
   in the Ada version have to do with building the names table or manipulating
   it in the semantic routines.  */

INLINE char *
Get_Name_String (Id)
     Name_Id Id;
{
  return Name_Chars_Ptr + Names_Ptr [Id].Name_Chars_Index + 1;
}

/* Define the function to return one of the numeric values below.  Note
   that it actually returns a char since an enumeration value of less
   than 256 entries is represented that way in Ada.  The operand is a Chars
   field value.  */

#define Get_Attribute_Id snames__get_attribute_id
extern char Get_Attribute_Id PROTO ((int));

/* Define the numeric values for the attributes.  */

#define  Attr_Access			    0
#define  Attr_Address			    1
#define  Attr_Adjacent			    2
#define  Attr_Aft			    3
#define  Attr_Alignment 		    4
#define  Attr_Bit_Order 		    5
#define  Attr_Body_Version		    6
#define  Attr_Callable			    7
#define  Attr_Caller			    8
#define  Attr_Ceiling			    9
#define  Attr_Component_Size		    10
#define  Attr_Compose			    11
#define  Attr_Constrained		    12
#define  Attr_Copy_Sign 		    13
#define  Attr_Count			    14
#define  Attr_Delta			    15
#define  Attr_Denorm			    16
#define  Attr_Digits			    17
#define  Attr_Emax			    18
#define  Attr_Epsilon			    19
#define  Attr_Exponent			    20
#define  Attr_External_Tag		    21
#define  Attr_First			    22
#define  Attr_First_Bit 		    23
#define  Attr_Floor			    24
#define  Attr_Fore			    25
#define  Attr_Fraction			    26
#define  Attr_Identity			    27
#define  Attr_Image			    28
#define  Attr_Input			    29
#define  Attr_Large			    30
#define  Attr_Last			    31
#define  Attr_Last_Bit			    32
#define  Attr_Leading_Part		    33
#define  Attr_Length			    34
#define  Attr_Machine			    35
#define  Attr_Machine_Emax		    36
#define  Attr_Machine_Emin		    37
#define  Attr_Machine_Mantissa		    38
#define  Attr_Machine_Overflows 	    39
#define  Attr_Machine_Radix		    40
#define  Attr_Machine_Rounds		    41
#define  Attr_Mantissa			    42
#define  Attr_Max			    43
#define  Attr_Max_Size_In_Storage_Elements  44
#define  Attr_Min			    45
#define  Attr_Model			    46
#define  Attr_Model_Emax		    47
#define  Attr_Model_Emin		    48
#define  Attr_Model_Epsilon		    49
#define  Attr_Model_Large		    50
#define  Attr_Model_Mantissa		    51
#define  Attr_Model_Small		    52
#define  Attr_Output			    53
#define  Attr_Pos			    54
#define  Attr_Position			    55
#define  Attr_Pred			    56
#define  Attr_Range			    57
#define  Attr_Read			    58
#define  Attr_Remainder 		    59
#define  Attr_Round			    60
#define  Attr_Rounding			    61
#define  Attr_Safe_Emax 		    62
#define  Attr_Safe_First		    63
#define  Attr_Safe_Large		    64
#define  Attr_Safe_Last 		    65
#define  Attr_Safe_Small		    66
#define  Attr_Scale			    67
#define  Attr_Signed_Zeros		    68
#define  Attr_Size			    69
#define  Attr_Small			    70
#define  Attr_Storage_Pool		    71
#define  Attr_Storage_Size		    72
#define  Attr_Succ			    73
#define  Attr_Tag			    74
#define  Attr_Terminated		    75
#define  Attr_Truncation		    76
#define  Attr_Unbiased_Rounding 	    77
#define  Attr_Unchecked_Access		    78
#define  Attr_Val			    79
#define  Attr_Valid			    80
#define  Attr_Value			    81
#define  Attr_Version			    82
#define  Attr_Wide_Image		    83
#define  Attr_Wide_Value		    84
#define  Attr_Width			    85
#define  Attr_Write			    86

#define  Attribute_Base 		    87
#define  Attribute_Class		    88
#define  Attribute_Standard_Access	    89

/* End of a-namet.h (C version of Namet package specification and body) */
