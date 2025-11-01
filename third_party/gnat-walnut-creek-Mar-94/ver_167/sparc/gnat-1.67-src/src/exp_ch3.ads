------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 3                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.14 $                             --
--                                                                          --
--             Copyright (c) 1992,1993, NYU, All Rights Reserved            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms  of the GNU  General  Public  License  as  published  by the  Free --
-- Software  Foundation;  either version 2,  or (at your option)  any later --
-- version.  GNAT is distributed  in the hope  that it will be useful,  but --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANT- --
-- ABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public --
-- License  for  more details.  You should have received  a copy of the GNU --
-- General Public License along with GNAT;  see file COPYING. If not, write --
-- to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. --
--                                                                          --
------------------------------------------------------------------------------

--  Expand routines for chapter 3 constructs

with Types; use Types;

package Exp_Ch3 is

   procedure Expand_N_Object_Declaration         (N : Node_Id);
   procedure Expand_N_Variant_Part               (N : Node_Id);
   procedure Expand_N_Full_Type_Declaration      (N : Node_Id);

   procedure Build_Discr_Checking_Funcs (N : Node_Id);
   --  Builds function which checks whether the component name is consistent
   --  with the current discriminants. N is the full type declaration node,
   --  and the discriminant checking functions are inserted after this node.

   function Build_Initialization_Call
     (Loc : Source_Ptr; Id_Ref : Node_Id; Typ : Entity_Id) return Node_Id;
   --  Builds a call to the initialization procedure of the Id entity.
   --  Id_Ref is either a new reference to Id (for record fields), or an
   --  indexed component (for array elements). Loc is the source location for
   --  the constructed tree, and Typ is the type of the entity (the
   --  initialization procedure of the base type is the procedure that
   --  actually gets called).

   function Build_Record_Init_Proc
     (N : Node_Id; Pe : Entity_Id) return Node_Id;
   --  Build the initialization procedure for record types. N is the record
   --  type definition node, and Pe is an entity used to indicate whether the
   --  procedure should be made public or not (the procedure entity will be
   --  made public if and only the entity Pe is public). The result is Error
   --  if no record initialization procedure is required for the record type,
   --  and otherwise points to the constructed procedure body, which has
   --  already been analyzed. It is tthe caller's responsibility to insert
   --  the procedure into the proper declaration list. In addition, the
   --  Initialization field of the record type is set to reference the
   --  procedure. Note that it is not really an error for there to be
   --  no record initialization procedure, but this makes it easier for the
   --  caller to do a conditional insertion into the declaration list, since
   --  the list insertion routines ignore Error.

   function Expand_Dispatch_Table (Tagged_Type : Entity_Id) return List_Id;
   --  This function generates the code for the dispatch table associated
   --  with the Tagged_Type. This dispatch table is a record containing :
   --    - the inheritence depth (number of ascendants till the root type)
   --    - a pointer to a table containing the tags of all the ancestors
   --    - an access to each primitive operation of the tagged type
   --  The accesses to the primitives are used to implement the dispatching
   --  calls. The tags of ancestors and the inheritance depth are defined for
   --  the sake of the membership test.

   procedure Freeze_Type (N : Node_Id);
   --  This procedure executes the freezing actions associated with the
   --  given freeze type node N.
end Exp_Ch3;
