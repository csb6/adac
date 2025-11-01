/****************************************************************************/
/*									    */
/*			   GNAT COMPILER COMPONENTS			    */
/*						                            */
/*				  R T R E E                                 */
/*							                    */
/*				    Spec                                    */
/*									    */
/*			       $Revision: 1.9 $                             */
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

struct Needed_File_Info
{
  int File_Name;
  int First_Sloc;
  int Last_Sloc;
  int Last_Line;
};

extern int max_gnat_nodes;
extern struct Needed_File_Info *File_Info_Ptr;

extern Tree_Id read_gnat_tree PROTO((char * ));
