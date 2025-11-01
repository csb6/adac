------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M  - D I S P                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.5 $                              --
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

--  This package contains routines involved in tagged types and dynamic
--  dispatching.

with Types; use Types;
package Sem_Disp is

   procedure Check_Dispatching_Call (N : Node_Id);
   --  Check if a call is a dispatching call. The subprogram is known to
   --  be a dispatching operation. The call is dispatching if all the
   --  controlling actuals are dynamically tagged. This procedure is called
   --  after overload resolution, so the call is known to be unambiguous.

   procedure Check_Dispatching_Operation (Subp : Entity_Id);
   --  If a subprogram has a dispatching argument or result, add it to
   --  the list of primitive operations of the corresponding type.

   function Find_Controlling_Arg (N : Node_Id) return Node_Id;
   --  Returns the actual controlling argument if N is dynamically tagged,
   --  and Empty if it is not dynamically tagged.

   function Find_Dispatching_Type (Subp : Entity_Id) return Entity_Id;
   --  Check whether a subprogram is dispatching, and find the tagged
   --  type of the controlling argument or arguments.

   function Is_Dynamically_Tagged (N : Node_Id) return Boolean;
   --  Used to determine whether a call is dispatching, i.e. if is an
   --  an expression of a class_Wide type, or a call to a function with
   --  controlling result where at least one operand is dynamically tagged.

   function Is_Tag_Indeterminate (N : Node_Id) return Boolean;
   --  An expression is tag-indeterminate if it is a call that dispatches
   --  on result, and all controlling operands are also indeterminate.
   --  Such a function call may inherit a tag from an enclosing call.

   procedure Override_Dispatching_Operation (Prev_Op, New_Op : Entity_Id);
   --  Replace an implicit dispatching operation with an  explicit one.
   --  Prev_Op is an inherited primitive operation which is overriden by
   --  the explicit declaration of New_Op. Find the tagged type of which both
   --  are dispatching operations, and replace Prev_Op in the corresponding
   --  list. The replacement must be a the same position because both must
   --  appear in the same location in the dispatch table.

   procedure Propagate_Tag (Control : Node_Id; Actual : Node_Id);
   --  If a function call is tag-indeterminate,  its controlling argument is
   --  found in the context;  either an enclosing call, or the left-hand side
   --  of the enclosing assignment statement. The tag must be propagated 
   --  recursively to the tag-indeterminate actuals of the call.

end Sem_Disp;
