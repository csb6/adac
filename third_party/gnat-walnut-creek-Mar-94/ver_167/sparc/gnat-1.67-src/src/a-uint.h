/****************************************************************************/
/*									    */
/*			   GNAT COMPILER COMPONENTS			    */
/*									    */
/*			   U I N T _ S U P P O R T			    */
/*									    */
/*				Specification				    */
/*                               (C Version)                                */
/*									    */
/*			      $Revision: 1.10 $ 			    */
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

/* This file corresponds to the Ada package specification Uint_Support. It was
   created manually from the files uint_.ada, uint.ada  */

/* Support for universal integer arithmetic */

struct Uint_Entry
{
  Pos Length;
  Int Loc;
};

/* Obtain Int value from Uint input. For the moment, the result is assumed to
   fit in HOST_BITS_PER_INT bits, with no provision for out of range
   values.  */
#define UI_To_Int uintp__ui_to_int
extern Int UI_To_Int	PROTO((Uint));

#ifdef TREE_CODE   /* Don't lose if tree.h not included.  */
/* Similarly, but return a GCC INTEGER_CST.  Overflow is tested by the
   constant-folding used to build the node.  TYPE is the GCC type of the
   resulting node.  */
extern tree UI_To_gnu		PROTO((Uint, tree));
#endif

/* Universal integers are represented by the Uint type which is an index into
   the Uints_Ptr table containing Uint_Entry values.  A Uint_Entry contains an
   index and length for getting the "digits" of the universal integer from the
   Udigits_Ptr table.

   For efficiency, this method is used only for integer values larger than the
   constant Uint_Bias.  If a Uint is less than this constant, then it contains
   the integer value itself.  The origin of the Uints_Ptr table is adjusted so
   that a Uint value of Uint_Bias indexes the first element.  */

extern struct Uint_Entry *Uints_Ptr;
extern Int *Udigits_Ptr;
