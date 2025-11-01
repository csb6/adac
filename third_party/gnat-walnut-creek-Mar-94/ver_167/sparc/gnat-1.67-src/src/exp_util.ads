------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ U T I L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.12 $                              --
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

--  Package containing utility procedures used throughout the expander

with Types; use Types;
package Exp_Util is

   Expander_Active : Boolean := False;
   --  A flag that indicates if expansion is active (True) or deactivated
   --  (False). When expansion is deactivated all calls to expander routines
   --  have no effect. Note that the initial setting of False is merely to
   --  prevent saving of an undefined value for an initial call to the
   --  Expander_Mode_Save_And_Set procedure.

   function Build_Call (Loc : Source_Ptr; E : Entity_Id) return Node_Id;      
   --  Build an N_Procedure_Call node that references the given entity        
   --  The call has no parameters. The first argument provides the location   
   --  information for the tree and for error messages. The call is properly  
   --  analyzed and resolved before it is returned.                           

   procedure Expander_Mode_Save_And_Set (Status : Boolean);
   --  Saves the current setting of the Expander_Active flag on an internal
   --  stack and then sets the flag to the given value.

   procedure Expander_Mode_Restore;
   --  Restores the setting of the Expander_Active flag using the top entry
   --  pushed onto the stack by Expander_Mode_Save_And_Reset, popping the
   --  stack, except that if any errors have been detected, then the state
   --  of the flag is left set to False.

   procedure Expand_Class_Allocator (N             : Node_Id;
                                     Acc_Type      : Entity_Id;
                                     Class_Subtype : Entity_Id);
   --  Uses Expand_Class_Subtype to expand a new class subtype then
   --  transforms "new T'Class'(Exp)" into:

   --    Acc_Type (new Equiv_T'(Equiv_T (Exp)))


   function Expand_Class_Subtype (Loc : Source_Ptr;
                                  Typ : Entity_Id;
                                  Exp : Node_Id) return List_Id;
   --  When a Class_Subtype value is encountered, that is to say in such 
   --  cases as:

   --    X: T'Class := Exp
   --    new T'Class'(Exp)

   --  this function generates the list of declarations defining a record
   --  of the following form :

   --    type anon is record
   --       _parent : Root_Type_Of (Typ) constrained with Exp discriminants;
   --       Extension : String (1 .. expr to match size of Exp);
   --    end record;

   --  This record is compatible with any value of T'Class thanks to the
   --  first field and has the same size as Exp thanks to the second field.
   --  This record is attached to the Subclass by its Equivalent_Type field.

   function Expand_Array_Equality 
     (Loc : Source_Ptr; Typ : Entity_Id; Lhs, Rhs : Node_Id) return Node_Id;
   --  Expand an array equality into an expression-action containing a local
   --  function implementing this equality, and a call to it. Loc is the
   --  location for the generated nodes. Typ is the type of the array, and
   --  Lhs, Rhs are the array expressions to be compared.

   function Expand_Record_Equality 
     (Loc : Source_Ptr; Typ : Entity_Id; Lhs, Rhs : Node_Id) return Node_Id;
   --  Expand a record equality into an expression that compares the fields
   --  individually to yield the required Boolean result. Loc is the location
   --  for the generated nodes. Typ is the type of the record, and Lhs, Rhs
   --  are the record expressions to be compared. Note that the case of
   --  variant records is not implemented yet ???.

   procedure Expand_Subtype_From_Expr (N : Node_Id; T : Entity_Id);
   --   Build subtype from initial value in object declarations
   --   where the object definition is an unconstrained type.

   procedure Expand_Tagged_Extension (Def : Node_Id); 
   --  Add a field _parent at the beginning of the record. This field
   --  contains a value of the Parent Type. It is used to implement inheritance
   --  Here is an example of expansion :

   --    type T2 (B, C : Int) is new T1 (A => B) with record
   --       _Parent : T1 (A => B);   <--- this is the expanded field
   --       D : Int;
   --    end;

   procedure Expand_Tagged_Root (Def : Node_Id);
   --  Add a field _Tag at the beginning of the record. This field carries the
   --  value of the access to the Dispatch table. It is converted to
   --  Ada-Tags.Tag type in order to be uniformely typed among descendants.
   --  It is converted back to an Access To Dispatch Table just before the
   --  dispatching call. This procedure is only called on the root type,
   --  the _Tag field being inherited by the descendent.

   procedure Protect_Statements (N : Node_Id; E : Entity_Id);
   --  This function protects the handled statement sequence of node N by
   --  adding a cleanup that is a call to the procedure referenced by the
   --  entity E. If necessary (if the handled statement sequence already has
   --  an exception handler, or a cleanup), an extra block is wrapped around.

   procedure Traceback_Store (N : Node_Id; Anal : Boolean := True);
   --  Constructs call node to the Store_Tb traceback store function and
   --  inserts the call before the node N. This construction and insertion
   --  happens only if Debug_Flag_B is set, otherwise the call has no effect.
   --  Also the insertion is suppressed if it duplicates an adjacent call,
   --  or if the unit containing the node N is not the main unit. Normally 
   --  the call is analyzed before it is inserted. If Anal is set to False
   --  the call to Analyze is suppressed in which case the caller must
   --  ensure that a subsequent call to Analyze is made for the node.

end Exp_Util;
