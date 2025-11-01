/****************************************************************************/
/*									    */
/*			   GNAT COMPILER COMPONENTS			    */
/*									    */
/*				   T R E E				    */
/*									    */
/*				   S p e c				    */
/*				 (C Version)				    */
/*									    */
/*			      $Revision: 1.20 $ 			    */
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

/* This is the C header corresponding to the Ada package specification for
   Tree. It also contains the implementations of inlined functions from the
   package body for Tree.  It was generated manually from TREE_.ADA and
   TREE.ADA and must be kept synchronized with changes in these packages.

   Note that only routines for reading the tree are included, since the tree
   transformer is not supposed to modify the tree in any way. */

/* Structures to define tree node.  */

/* The Ada record for a Node is a variant record discriminated by
   Is_Extension.  It contains a common area consisting of Nkind, Ekind,
   and a large number of flags.

   Note that the field names are all lower-case, while the access macros
   are mixed-case.  The first structure below corresponds to the variant
   part with is_extension = False and the second with is_extension = True.  */

struct Non_Extended
{
  Source_Ptr   sloc;
  Int	       link;
  Int	       field1;
  Int	       field2;
  Int	       field3;
  Int	       field4;
  Int	       field5;
};

/* The Following structure corresponds to variant with is_extension = True.  */
struct Extended
{
  Int	       field6;
  Int	       field7;
  Int	       field8;
  Int	       field9;
  Int	       field10;
  Int	       field11;
  Int	       field12;
};

struct Node
{
  Boolean      is_extension :  1;
  Boolean      in_list	    :  1;
  Boolean      rewrite_sub  :  1;
  Boolean      rewrite_ins  :  1;
  Boolean      flag1	    :  1;
  Boolean      flag2	    :  1;
  Boolean      flag3	    :  1;
  Boolean      flag4	    :  1;

  Boolean      flag5	    :  1;
  Boolean      flag6	    :  1;
  Boolean      flag7	    :  1;
  Boolean      flag8	    :  1;
  Boolean      flag9	    :  1;
  Boolean      flag10	    :  1;
  Boolean      flag11	    :  1;
  Boolean      flag12	    :  1;

  Boolean      flag13	    :  1;
  Boolean      flag14	    :  1;
  Boolean      flag15	    :  1;
  Boolean      flag16	    :  1;
  Boolean      flag17	    :  1;
  Boolean      flag18	    :  1;
  Boolean      flag19	    :  1;
  Boolean      flag20	    :  1;

  unsigned char kind	    :  8;

  union variant
    {
      struct Non_Extended NX;
      struct Extended EX;
    } V;
};

/* The actual tree is an array of nodes. The pointer to this array is passed
   as a parameter to the tree transformer procedure and stored in the global
   variable Nodes_Ptr after adjusting it by subtracting Node_First_Entry, so
   that Node_Id values can be used as subscripts.  */
extern struct Node *Nodes_Ptr;

/* Store the subscript of the last allocated Node_Id value. */
extern Node_Id Last_Node_Id;

/*  The following is the structure used for the lists table. Nodes in this
    table are used in three different ways:

    For node list headers:

	Lfield1 points to the first node in the list (Empty if none)
	Lfield2 points to the last node in the list (Empty if none)
	Lnode points to the parent (Empty if none)

    For element list headers:

	Lfield1 points to the first element in the list (No_Elmt if none)
	Lfield2 points to the last element in the list (No_Elmt if none)
	Lnode points to the parent (Empty if none)

   For elements in an element list:

	Lfield1 points to the previous element or to header at list start
	Lfield2 points to the next element or to header at list end
	Lnode contains pointer to referenced Val of element  */

struct List_Header
{
  Int Lfield1;
  Int Lfield2;
  Node_Id Lnode;
};

/* The list headers are stored in an array. The pointer to this array is
   passed as a parameter to the tree transformer procedure and stored in the
   global variable List_Headers_Ptr after adjusting it by subtracting
   List_First_Entry, so that List_Id values can be used as subscripts.	*/

extern struct List_Header *List_Headers_Ptr;

/* Store the subscript of the last allocated List_Id value.  */
extern List_Id Last_List_Id;

/* Node List Access Functions */

INLINE Node_Id
First (List)
     List_Id List;
{
  return List_Headers_Ptr [List].Lfield1;
}

INLINE Node_Id
Last (List)
     List_Id List;
{
  return List_Headers_Ptr [List].Lfield2;
}

INLINE Node_Id
Next (Node)
     Node_Id Node;
{
  Int Id = Nodes_Ptr [Node].V.NX.link;

  if (IN (Id, List_Range))
    return Empty;
  else return Id;
}

#define Prev atree__prev
extern Node_Id Prev PROTO((Node_Id));
#define Parent atree__parent
extern Node_Id Parent PROTO((Node_Id));

INLINE Boolean
Is_Empty_List (Id)
     List_Id Id;
{
  return (First (Id) == Empty);
}

INLINE Boolean
Is_Non_Empty_List (Id)
     List_Id Id;
{
  return (First (Id) != Empty);
}

INLINE Boolean
Is_List_Member (Node)
     Node_Id Node;
{
  return Nodes_Ptr [Node].in_list;
}

#define List_Containing atree__list_containing
extern List_Id List_Containing	PROTO((Node_Id));

/* Element List Access Functions:  */

INLINE Node_Id
Id_Of (Elmt)
     Elmt_Id Elmt;
{
  return List_Headers_Ptr [Elmt - Elmt_Bias].Lnode;
}

INLINE Elmt_Id
First_Elmt (List)
     Elist_Id List;
{
  return List_Headers_Ptr [List - Elist_Bias].Lfield1;
}

INLINE Elmt_Id
Last_Elmt (List)
     Elist_Id List;
{
  return List_Headers_Ptr [List - Elist_Bias].Lfield2;
}

INLINE Elmt_Id
Next_Elmt (Node)
     Elmt_Id Node;
{
  Int N = List_Headers_Ptr [Node - Elmt_Bias].Lfield2;

  if (IN (N, Elist_Range))
    return No_Elmt;
  else
    return N;
}

INLINE Elmt_Id
Prev_Elmt (Node)
     Elmt_Id Node;
{
  Int N = List_Headers_Ptr [Node - Elmt_Bias].Lfield1;

  if (IN (N, Elist_Range))
    return No_Elmt;
  else
    return N;
}

INLINE Boolean
Is_Empty_Elmt_List (Id)
     Elist_Id Id;
{
  return List_Headers_Ptr [Id - Elist_Bias].Lfield1 == No_Elmt;
}

INLINE Boolean
Is_Non_Empty_Elmt_List (Id)
     Elist_Id Id;
{
  return List_Headers_Ptr [Id - Elist_Bias].Lfield1 != No_Elmt;
}

/* Overloaded Functions:

   These functions are overloaded in the original Ada source, but there is
   only one corresponding C function, which works as described below.	*/

/* Type used for union of Node_Id, List_Id, Elist_Id. */
typedef Int Tree_Id;

/* These two functions can only be used for Node_Id and List_Id values and
   they work in the C version because Empty = No_List = 0.  */

INLINE Boolean
No (N)
     Tree_Id N;
{
  return N == Empty;
}

INLINE Boolean
Present (N)
     Tree_Id N;
{
  return N != Empty;
}

/* Test the range of N to distinguish between the cases of Node_Id, List_Id
   and Elist_Id arguments.  */
extern Node_Id Parent		PROTO((Tree_Id));

/* Node Access Functions:  */

#define Nkind(N) ((Node_Kind)(Nodes_Ptr [N].kind))
#define Ekind(N) ((Entity_Kind)(Nodes_Ptr [N + 1].kind))
#define Sloc(N) (Nodes_Ptr [N].V.NX.sloc)

#define Field1(N) (Nodes_Ptr [N].V.NX.field1)
#define Field2(N) (Nodes_Ptr [N].V.NX.field2)
#define Field3(N) (Nodes_Ptr [N].V.NX.field3)
#define Field4(N) (Nodes_Ptr [N].V.NX.field4)
#define Field5(N) (Nodes_Ptr [N].V.NX.Field5)
#define Field6(N) (Nodes_Ptr [N+1].V.EX.field6)
#define Field7(N) (Nodes_Ptr [N+1].V.EX.field7)
#define Field8(N) (Nodes_Ptr [N+1].V.EX.field8)
#define Field9(N) (Nodes_Ptr [N+1].V.EX.field9)
#define Field10(N) (Nodes_Ptr [N+1].V.EX.field10)
#define Field11(N) (Nodes_Ptr [N+1].V.EX.field11)
#define Field12(N) (Nodes_Ptr [N+1].V.EX.field12)

#define Node1(N) (Nodes_Ptr [N].V.NX.field1)
#define Node2(N) (Nodes_Ptr [N].V.NX.field2)
#define Node3(N) (Nodes_Ptr [N].V.NX.field3)
#define Node4(N) (Nodes_Ptr [N].V.NX.field4)
#define Node5(N) (Nodes_Ptr [N].V.NX.field5)
#define Node6(N) (Nodes_Ptr [N+1].V.EX.field6)
#define Node7(N) (Nodes_Ptr [N+1].V.EX.field7)
#define Node8(N) (Nodes_Ptr [N+1].V.EX.field8)
#define Node9(N) (Nodes_Ptr [N+1].V.EX.field9)
#define Node10(N) (Nodes_Ptr [N+1].V.EX.field10)
#define Node11(N) (Nodes_Ptr [N+1].V.EX.field11)
#define Node12(N) (Nodes_Ptr [N+1].V.EX.field12)

#define List1(N) (Nodes_Ptr [N].V.NX.field1)
#define List2(N) (Nodes_Ptr [N].V.NX.field2)
#define List3(N) (Nodes_Ptr [N].V.NX.field3)
#define List4(N) (Nodes_Ptr [N].V.NX.field4)
#define List5(N) (Nodes_Ptr [N].V.NX.field5)
#define List6(N) (Nodes_Ptr [N+1].V.EX.field6)
#define List7(N) (Nodes_Ptr [N+1].V.EX.field7)
#define List8(N) (Nodes_Ptr [N+1].V.EX.field8)
#define List9(N) (Nodes_Ptr [N+1].V.EX.field9)
#define List10(N) (Nodes_Ptr [N+1].V.EX.field10)
#define List11(N) (Nodes_Ptr [N+1].V.EX.field11)
#define List12(N) (Nodes_Ptr [N+1].V.EX.field12)

#define Elist1(N) (Nodes_Ptr [N].V.NX.field1)
#define Elist2(N) (Nodes_Ptr [N].V.NX.field2)
#define Elist3(N) (Nodes_Ptr [N].V.NX.field3)
#define Elist4(N) (Nodes_Ptr [N].V.NX.field4)
#define Elist5(N) (Nodes_Ptr [N].V.NX.field5)
#define Elist6(N) (Nodes_Ptr [N+1].V.EX.field6)
#define Elist7(N) (Nodes_Ptr [N+1].V.EX.field7)
#define Elist8(N) (Nodes_Ptr [N+1].V.EX.field8)
#define Elist9(N) (Nodes_Ptr [N+1].V.EX.field9)
#define Elist10(N) (Nodes_Ptr [N+1].V.EX.field10)
#define Elist11(N) (Nodes_Ptr [N+1].V.EX.field11)
#define Elist12(N) (Nodes_Ptr [N+1].V.EX.field12)

#define Name1(N) (Nodes_Ptr [N].V.NX.field1)
#define Name2(N) (Nodes_Ptr [N].V.NX.field2)
#define Name3(N) (Nodes_Ptr [N].V.NX.field3)
#define Name4(N) (Nodes_Ptr [N].V.NX.field4)
#define Name5(N) (Nodes_Ptr [N].V.NX.field5)

#define Char_Code1(N) (Nodes_Ptr [N].V.NX.field1 - Char_Code_Bias)
#define Char_Code2(N) (Nodes_Ptr [N].V.NX.field2 - Char_Code_Bias)
#define Char_Code3(N) (Nodes_Ptr [N].V.NX.field3 - Char_Code_Bias)
#define Char_Code4(N) (Nodes_Ptr [N].V.NX.field4 - Char_Code_Bias)
#define Char_Code5(N) (Nodes_Ptr [N].V.NX.field5 - Char_Code_Bias)

#define Str1(N) (Nodes_Ptr [N].V.NX.field1)
#define Str2(N) (Nodes_Ptr [N].V.NX.field2)
#define Str3(N) (Nodes_Ptr [N].V.NX.field3)
#define Str4(N) (Nodes_Ptr [N].V.NX.field4)
#define Str5(N) (Nodes_Ptr [N].V.NX.field5)

#define Uint1(N) (Nodes_Ptr [N].V.NX.field1)
#define Uint2(N) (Nodes_Ptr [N].V.NX.field2)
#define Uint3(N) (Nodes_Ptr [N].V.NX.field3)
#define Uint4(N) (Nodes_Ptr [N].V.NX.field4)
#define Uint5(N) (Nodes_Ptr [N].V.NX.field5)
#define Uint6(N) (Nodes_Ptr [N+1].V.EX.field6)
#define Uint7(N) (Nodes_Ptr [N+1].V.EX.field7)
#define Uint8(N) (Nodes_Ptr [N+1].V.EX.field8)
#define Uint9(N) (Nodes_Ptr [N+1].V.EX.field9)
#define Uint10(N) (Nodes_Ptr [N+1].V.EX.field10)
#define Uint11(N) (Nodes_Ptr [N+1].V.EX.field11)
#define Uint12(N) (Nodes_Ptr [N+1].V.EX.field12)

#define Flag1(N) (Nodes_Ptr [N].flag1)
#define Flag2(N) (Nodes_Ptr [N].flag2)
#define Flag3(N) (Nodes_Ptr [N].flag3)
#define Flag4(N) (Nodes_Ptr [N].flag4)
#define Flag5(N) (Nodes_Ptr [N].flag5)
#define Flag6(N) (Nodes_Ptr [N].flag6)
#define Flag7(N) (Nodes_Ptr [N].flag7)
#define Flag8(N) (Nodes_Ptr [N].flag8)
#define Flag9(N) (Nodes_Ptr [N].flag9)
#define Flag10(N) (Nodes_Ptr [N].flag10)
#define Flag11(N) (Nodes_Ptr [N].flag11)
#define Flag12(N) (Nodes_Ptr [N].flag12)
#define Flag13(N) (Nodes_Ptr [N].flag13)
#define Flag14(N) (Nodes_Ptr [N].flag14)
#define Flag15(N) (Nodes_Ptr [N].flag15)
#define Flag16(N) (Nodes_Ptr [N].flag16)
#define Flag17(N) (Nodes_Ptr [N].flag17)
#define Flag18(N) (Nodes_Ptr [N].flag18)
#define Flag19(N) (Nodes_Ptr [N].flag19)
#define Flag20(N) (Nodes_Ptr [N].flag20)
#define Flag21(N) (Nodes_Ptr [N+1].in_list)
#define Flag22(N) (Nodes_Ptr [N+1].rewrite_sub)
#define Flag23(N) (Nodes_Ptr [N+1].rewrite_ins)
#define Flag24(N) (Nodes_Ptr [N+1].flag1)
#define Flag25(N) (Nodes_Ptr [N+1].flag2)
#define Flag26(N) (Nodes_Ptr [N+1].flag3)
#define Flag27(N) (Nodes_Ptr [N+1].flag4)
#define Flag28(N) (Nodes_Ptr [N+1].flag5)
#define Flag29(N) (Nodes_Ptr [N+1].flag6)
#define Flag30(N) (Nodes_Ptr [N+1].flag7)
#define Flag31(N) (Nodes_Ptr [N+1].flag8)
#define Flag32(N) (Nodes_Ptr [N+1].flag9)
#define Flag33(N) (Nodes_Ptr [N+1].flag10)
#define Flag34(N) (Nodes_Ptr [N+1].flag11)
#define Flag35(N) (Nodes_Ptr [N+1].flag12)
#define Flag36(N) (Nodes_Ptr [N+1].flag13)
#define Flag37(N) (Nodes_Ptr [N+1].flag14)
#define Flag38(N) (Nodes_Ptr [N+1].flag15)
#define Flag39(N) (Nodes_Ptr [N+1].flag16)
#define Flag40(N) (Nodes_Ptr [N+1].flag17)
#define Flag41(N) (Nodes_Ptr [N+1].flag18)
#define Flag42(N) (Nodes_Ptr [N+1].flag19)
#define Flag43(N) (Nodes_Ptr [N+1].flag20)

/* End of a-atree.h (C version of Atree package specification) */
