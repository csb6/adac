------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 9                               --
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

--  Expand routines for chapter 9 constructs

with Types; use Types;
package Exp_Ch9 is

   procedure Build_Activation_Chain_Entity (N : Node_Id);
   --  Given a declaration N of an object that is a task, or contains tasks
   --  (other than allocators to tasks) this routine ensures that an activation
   --  chain has been declared in the appropriate scope, building the required
   --  declaration for the chain variable if not. The name of this variable
   --  is always _Chain and it is accessed by name. This procedure also adds
   --  an appropriate call to Activate_Tasks to activate the tasks for this
   --  activation chain. It does not however deal with the call needed in the
   --  case of allocators to Expunge_Unactivated_Tasks, this is separately
   --  handled in the Expand_Task_Allocator routine.

   function Build_Call_With_Task (N : Node_Id; E : Entity_Id) return Node_Id;
   --  N is a node representing the name of a task or an access to a task.
   --  The value returned is a call to the function whose name is the entity
   --  E (typically a runtime routine entity obtained using RTE) with the
   --  Task_Id of the associated task as the parameter. The caller is
   --  responsible for analyzing and resolving the resulting tree.

   procedure Build_Master_Entity (E : Entity_Id);
   --  Given an entity E for the declaration of an object containing tasks
   --  or of a type declaration for an allocator whose designated type is a
   --  task or contains tasks, this routine marks the appropriate enclosing
   --  context as a master, and also declares a variable called _Master in
   --  the current declarative part which captures the value of Current_Master
   --  (if not already built by a prior call). We build this object (instead
   --  of just calling Current_Master) for two reasons. First it is clearly
   --  more efficient to call Current_Master only once for a bunch of tasks
   --  in the same declarative part, and second it makes things easier in
   --  generating the initialization routines, since they can just reference
   --  the object _Master by name, and they will get the proper Current_Master
   --  value at the outer level, and copy in the parameter value for the outer
   --  initialization call if the call is for a nested component). Note that
   --  in the case of nested packages, we only really need to make one such
   --  object at the outer level, but it is much easier to generate one per
   --  declarative part.

   procedure Build_Task_Activation_Call (N : Node_Id);
   --  This procedure is called for constructs that can be task activators
   --  i.e. task bodies, subprogram bodies, package bodies and blocks. If
   --  the construct is a task activator (as indicated by the non-empty
   --  setting of Activation_Chain_Entity, either in the construct, or, in
   --  the case of a package body, in its associated package spec), then
   --  a call to Activate_Tasks with this entity as the single parameter
   --  is inserted at the start of the statements of the activator.

   procedure Build_Task_Allocate_Block
     (Actions : List_Id; N : Node_Id; Args : List_Id);
   --  This routine is used in the case of allocators where the designated
   --  type is a task or contains tasks. In this case, the normal initialize
   --  call is replaced by:

   --    blockname : label;
   --    blockname : declare
   --       _Chain  : Activation_Chain;

   --       procedure _Expunge is
   --       begin
   --         Expunge_Unactivated_Tasks (_Chain);
   --       end;

   --    begin
   --       Init (Args);
   --       Activate_Tasks (_Chain);
   --    at end
   --       _Expunge;
   --    end;

   --  to get the task or tasks created and initialized. The expunge call
   --  ensures that any tasks that get created but not activated due to an
   --  exception are properly expunged (it has no effect in the normal case)
   --  The argument N is the allocator, and Args is the list of arguments
   --  for the initialization call, constructed by the caller, which uses
   --  the Master_Id of the access type as the _Master parameter, and _Chain
   --  (defined above) as the _Chain parameter.

   function Convert_Task (N : Node_Id; Typ : Entity_Id) return Node_Id;
   --  N is an expression of type Typ. If the type is not a task type then
   --  it is returned unchanged. If it is a task reference, Convert_Task
   --  creates an unchecked conversion node from this expression to the
   --  corresponding task record type value. We need this in any situation
   --  where the task is used, because the actual task object is an object
   --  of the corresponding record type, and manipulations on the task object
   --  actually manipulate the corresponding object of the record type.

   procedure Establish_Task_Master (N : Node_Id);
   --  Given a subprogram body, or a block statement, or a task body, this
   --  proccedure makes the necessary transformations required of a task
   --  master (add Enter_Master call at start, and establish a cleanup
   --  routine to make sure Complete_Master is called on exit).

   function Make_Task_Create_Call (Task_Rec : Entity_Id) return Node_Id;
   --  Given the entity of the record type created for a task type, build
   --  the call to Create_Task

   procedure Expand_N_Single_Task_Declaration  (N : Node_Id);
   procedure Expand_N_Task_Body                (N : Node_Id);
   procedure Expand_N_Task_Type_Declaration    (N : Node_Id);

end Exp_Ch9;
