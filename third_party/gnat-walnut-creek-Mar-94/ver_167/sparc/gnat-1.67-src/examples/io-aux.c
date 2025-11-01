/****************************************************************************/
/*                                                                          */
/*                         GNAT RUN-TIME COMPONENTS                         */
/*									    */
/*		        Text_IO Auxiliary C functions                       */
/*									    */
/*				    Body				    */
/*                                                                          */
/*                            $Revision: 1.1 $                              */
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
#include <stdio.h>
FILE *c_stdin () { return stdin;}
FILE *c_stdout () { return stdout;}
FILE *c_stderr () { return stderr;}

#if 0
----------------------
-- REVISION HISTORY --
----------------------

----------------------------
revision 1.1
date: Mon Dec 20 01:05:22 1993;  author: banner
Initial revision
----------------------------
** New changes after this line and before endif. **
#endif
