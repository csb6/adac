/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*							                    */
/*			    U I N T _ S U P P O R T			    */
/*									    */
/*				    Body				    */
/*                              (C Version)                                 */
/*                                                                          */
/*                            $Revision: 1.13 $                             */
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

/* This file corresponds to the Ada package body Uint_Support. It was created
   manually from the file uint.ada. */

#include "config.h"
#include "tree.h"
#include "a-ada.h"
#include "a-types.h"
#include "a-atree.h"
#include "a-sinfo.h"
#include "a-einfo.h"
#include "a-namet.h"
#include "a-string.h"
#include "a-uint.h"

/* Universal integers are represented by the Uint type which is an index into
   the Uints_Ptr table containing Uint_Entry values.  A Uint_Entry contains an
   index and length for getting the "digits" of the universal integer from the
   Udigits_Ptr table.

   For efficiency, this method is used only for integer values larger than the
   constant Uint_Bias.  If a Uint is less than this constant, then it contains
   the integer value itself.  The origin of the Uints_Ptr table is adjusted so
   that a Uint value of Uint_Bias indexes the first element.  */

/* Similarly to UI_To_Int, but return a GCC INTEGER_CST.  Overflow is tested
   by the constant-folding used to build the node.  TYPE is the GCC type of the
   resulting node.  */

tree
UI_To_gnu (Input, type)
     Uint Input;
     tree type;
{
  tree gnu_ret;

  if (Input <= Uint_Direct_Last)
    gnu_ret = convert (type, build_int_2 (Input - Uint_Direct_Bias, 0));
  else
    {
      Int Idx =    Uints_Ptr[Input].Loc;
      Pos Length = Uints_Ptr[Input].Length;
      Int First = Udigits_Ptr[Idx];
      tree gnu_base = convert (type, build_int_2 (Base, 0));

      if (Length <= 0)
	abort ();

      gnu_ret = convert (type, build_int_2 (First, First < 0 ? -1 : 0));
      if (First < 0)
	for (Idx++, Length--; Length; Idx++, Length--)
	  gnu_ret = fold (build (MINUS_EXPR, type,
				 fold (build (MULT_EXPR, type,
					      gnu_ret, gnu_base)),
				 convert (type,
					  build_int_2 (Udigits_Ptr[Idx], 0))));
      else
	for (Idx++, Length--; Length; Idx++, Length--)
	  gnu_ret = fold (build (PLUS_EXPR, type,
				 fold (build (MULT_EXPR, type,
					      gnu_ret, gnu_base)),
				 convert (type,
					  build_int_2 (Udigits_Ptr[Idx], 0))));
    }

  /* We don't need any NOP_EXPR or NON_LVALUE_EXPR on GNU_RET.  */
  while ((TREE_CODE (gnu_ret) == NOP_EXPR
	  || TREE_CODE (gnu_ret) == NON_LVALUE_EXPR)
	 && TREE_TYPE (TREE_OPERAND (gnu_ret, 0)) == TREE_TYPE (gnu_ret))
    gnu_ret = TREE_OPERAND (gnu_ret, 0);

  return gnu_ret;
}
