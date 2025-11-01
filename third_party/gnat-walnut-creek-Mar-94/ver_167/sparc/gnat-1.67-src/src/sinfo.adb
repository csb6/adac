------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S I N F O                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.107 $                            --
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

with Atree;   use Atree;
with Comperr; use Comperr;
with Debug;   use Debug;
with Output;  use Output;

package body Sinfo is

   use Atree.Unchecked_Access;
   --  This package is one of the few packages which is allowed to make direct
   --  references to tree nodes (since it is in the business of providing a
   --  higher level of tree access which other clients are expected to use and
   --  which implements checks).

   use Atree_Private_Part;
   --  The only reason that we ask for direct access to the private part of
   --  the tree package is so that we can directly reference the Nkind field
   --  of nodes table entries. We do this since it helps the efficiency of
   --  the Sinfo debugging checks considerably (note that when we are checking
   --  Nkind values, we don't need to check for a valid node reference, because
   --  we will check that anyway when we reference the field).

   NT : Nodes.Table_Ptr renames Nodes.Table;
   --  A short hand abbreviation, useful for the debugging checks

   --------------------------
   -- Debugging Procedures --
   --------------------------

   --  These debugging procedures are called only if assertions are enabled

   procedure Dcheck_Bad_Nkind (N : Node_Id);
   --  Used to signal bad node kind for access to node N

   ----------------------
   -- Dcheck_Bad_Nkind --
   ----------------------

   procedure Dcheck_Bad_Nkind (N : Node_Id) is
   begin

      --  Note: in the following debugging message, we use the Nkind function
      --  (rather than get the Nkind value directly) quite deliberately. It
      --  means that if the reason that we got a bad Nkind was that N was not
      --  a proper node in the first place, we get the right error message.

      Compiler_Error;
      Write_Eol;
      Write_String ("Dcheck_Bad_Nkind failure: Nkind = ");
      Write_String (Node_Kind'Image (Nkind (N)));
      Write_Eol;
      Compiler_Abort (N);
   end Dcheck_Bad_Nkind;

   ----------------------------
   -- Field Access Functions --
   ----------------------------

   function Abort_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Requeue_Statement,
        Dcheck_Bad_Nkind (N));
      return Flag1 (N);
   end Abort_Present;

   function Abortable_Part
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Asynchronous_Select,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Abortable_Part;

   function Abstract_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Derived_Type_Definition
          or else NT (N).Nkind = N_Formal_Private_Type_Definition
          or else NT (N).Nkind = N_Private_Extension_Declaration
          or else NT (N).Nkind = N_Private_Type_Declaration
          or else NT (N).Nkind = N_Record_Definition,
        Dcheck_Bad_Nkind (N));
      return Flag4 (N);
   end Abstract_Present;

   function Accept_Name
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Accept_Statement,
        Dcheck_Bad_Nkind (N));
      return Node1 (N);
   end Accept_Name;

   function Accept_Statement
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Accept_Alternative,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Accept_Statement;

   function Actions
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Expression_Actions,
        Dcheck_Bad_Nkind (N));
      return List1 (N);
   end Actions;

   function Activation_Chain_Entity
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Block_Statement
          or else NT (N).Nkind = N_Generic_Package_Declaration
          or else NT (N).Nkind = N_Package_Declaration
          or else NT (N).Nkind = N_Subprogram_Body
          or else NT (N).Nkind = N_Task_Body,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Activation_Chain_Entity;

   function Acts_As_Spec
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit
          or else NT (N).Nkind = N_Subprogram_Body,
        Dcheck_Bad_Nkind (N));
      return Flag4 (N);
   end Acts_As_Spec;

   function Actual_Parameter
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Parameter_Association,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Actual_Parameter;

   function Aliased_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_Declaration
          or else NT (N).Nkind = N_Constrained_Array_Definition
          or else NT (N).Nkind = N_Object_Declaration
          or else NT (N).Nkind = N_Unconstrained_Array_Definition,
        Dcheck_Bad_Nkind (N));
      return Flag1 (N);
   end Aliased_Present;

   function All_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Access_To_Object_Definition,
        Dcheck_Bad_Nkind (N));
      return Flag1 (N);
   end All_Present;

   function Alternatives
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Case_Statement,
        Dcheck_Bad_Nkind (N));
      return List4 (N);
   end Alternatives;

   function Analyzed
      (N : Node_Id) return Boolean is
   begin
      return Flag19 (N);
   end Analyzed;

   function Array_Aggregate
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Enumeration_Representation_Clause,
        Dcheck_Bad_Nkind (N));
      return Node4 (N);
   end Array_Aggregate;

   function Assignment_OK
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Assignment_Statement,
        Dcheck_Bad_Nkind (N));
      return Flag5 (N);
   end Assignment_OK;

   function Bad_Is_Detected
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Subprogram_Body,
        Dcheck_Bad_Nkind (N));
      return Flag1 (N);
   end Bad_Is_Detected;

   function Body_Required
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit,
        Dcheck_Bad_Nkind (N));
      return Flag3 (N);
   end Body_Required;

   function Box_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Package_Declaration
          or else NT (N).Nkind = N_Formal_Subprogram_Declaration,
        Dcheck_Bad_Nkind (N));
      return Flag1 (N);
   end Box_Present;

   function Char_Literal_Value
      (N : Node_Id) return Char_Code is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Character_Literal,
        Dcheck_Bad_Nkind (N));
      return Char_Code2 (N);
   end Char_Literal_Value;

   function Chars
      (N : Node_Id) return Name_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Has_Chars,
        Dcheck_Bad_Nkind (N));
      return Name1 (N);
   end Chars;

   function Choice_Parameter
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Exception_Handler,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Choice_Parameter;

   function Choices
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_Association,
        Dcheck_Bad_Nkind (N));
      return List1 (N);
   end Choices;

   function Component_Associations
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Aggregate
          or else NT (N).Nkind = N_Extension_Aggregate,
        Dcheck_Bad_Nkind (N));
      return List4 (N);
   end Component_Associations;

   function Component_Clauses
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Record_Representation_Clause,
        Dcheck_Bad_Nkind (N));
      return List3 (N);
   end Component_Clauses;

   function Component_Declarations
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_List,
        Dcheck_Bad_Nkind (N));
      return List3 (N);
   end Component_Declarations;

   function Component_List
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Record_Definition
          or else NT (N).Nkind = N_Variant,
        Dcheck_Bad_Nkind (N));
      return Node1 (N);
   end Component_List;

   function Component_Name
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_Clause,
        Dcheck_Bad_Nkind (N));
      return Node1 (N);
   end Component_Name;

   function Condition
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Accept_Alternative
          or else NT (N).Nkind = N_Delay_Alternative
          or else NT (N).Nkind = N_Elsif_Part
          or else NT (N).Nkind = N_Entry_Body
          or else NT (N).Nkind = N_Exit_Statement
          or else NT (N).Nkind = N_If_Statement
          or else NT (N).Nkind = N_Iteration_Scheme
          or else NT (N).Nkind = N_Terminate_Alternative,
        Dcheck_Bad_Nkind (N));
      return Node5 (N);
   end Condition;

   function Constant_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Access_To_Object_Definition
          or else NT (N).Nkind = N_Object_Declaration,
        Dcheck_Bad_Nkind (N));
      return Flag2 (N);
   end Constant_Present;

   function Constraint
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Subtype_Indication,
        Dcheck_Bad_Nkind (N));
      return Node3 (N);
   end Constraint;

   function Constraints
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Index_Or_Discriminant_Constraint,
        Dcheck_Bad_Nkind (N));
      return List1 (N);
   end Constraints;

   function Context_Installed
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_With_Clause,
        Dcheck_Bad_Nkind (N));
      return Flag3 (N);
   end Context_Installed;

   function Context_Items
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit,
        Dcheck_Bad_Nkind (N));
      return List1 (N);
   end Context_Items;

   function Controlling_Argument
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Function_Call
          or else NT (N).Nkind = N_Procedure_Call_Statement,
        Dcheck_Bad_Nkind (N));
      return Node1 (N);
   end Controlling_Argument;

   function Corresponding_Body
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Generic_Package_Declaration
          or else NT (N).Nkind = N_Generic_Subprogram_Declaration
          or else NT (N).Nkind = N_Package_Body_Stub
          or else NT (N).Nkind = N_Package_Declaration
          or else NT (N).Nkind = N_Protected_Body_Stub
          or else NT (N).Nkind = N_Subprogram_Body_Stub
          or else NT (N).Nkind = N_Subprogram_Declaration
          or else NT (N).Nkind = N_Task_Body_Stub,
        Dcheck_Bad_Nkind (N));
      return Node5 (N);
   end Corresponding_Body;

   function Corresponding_Spec
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Package_Body
          or else NT (N).Nkind = N_Subprogram_Body
          or else NT (N).Nkind = N_Subprogram_Renaming_Declaration
          or else NT (N).Nkind = N_Task_Body
          or else NT (N).Nkind = N_With_Clause,
        Dcheck_Bad_Nkind (N));
      return Node5 (N);
   end Corresponding_Spec;

   function Debug_Statement
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Pragma,
        Dcheck_Bad_Nkind (N));
      return Node3 (N);
   end Debug_Statement;

   function Decimal
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Real_Literal,
        Dcheck_Bad_Nkind (N));
      return Flag2 (N);
   end Decimal;

   function Declarations
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Accept_Statement
          or else NT (N).Nkind = N_Block_Statement
          or else NT (N).Nkind = N_Entry_Body
          or else NT (N).Nkind = N_Package_Body
          or else NT (N).Nkind = N_Protected_Body
          or else NT (N).Nkind = N_Subprogram_Body
          or else NT (N).Nkind = N_Task_Body,
        Dcheck_Bad_Nkind (N));
      return List3 (N);
   end Declarations;

   function Default_Name
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Subprogram_Declaration,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Default_Name;

   function Defining_Identifier
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_Declaration
          or else NT (N).Nkind = N_Defining_Program_Unit_Name
          or else NT (N).Nkind = N_Discriminant_Specification
          or else NT (N).Nkind = N_Entry_Body
          or else NT (N).Nkind = N_Entry_Declaration
          or else NT (N).Nkind = N_Entry_Index_Specification
          or else NT (N).Nkind = N_Exception_Declaration
          or else NT (N).Nkind = N_Exception_Renaming_Declaration
          or else NT (N).Nkind = N_Formal_Object_Declaration
          or else NT (N).Nkind = N_Formal_Package_Declaration
          or else NT (N).Nkind = N_Formal_Type_Declaration
          or else NT (N).Nkind = N_Full_Type_Declaration
          or else NT (N).Nkind = N_Implicit_Label_Declaration
          or else NT (N).Nkind = N_Implicit_Type
          or else NT (N).Nkind = N_Incomplete_Type_Declaration
          or else NT (N).Nkind = N_Loop_Parameter_Specification
          or else NT (N).Nkind = N_Number_Declaration
          or else NT (N).Nkind = N_Object_Declaration
          or else NT (N).Nkind = N_Object_Renaming_Declaration
          or else NT (N).Nkind = N_Package_Body_Stub
          or else NT (N).Nkind = N_Parameter_Specification
          or else NT (N).Nkind = N_Private_Extension_Declaration
          or else NT (N).Nkind = N_Private_Type_Declaration
          or else NT (N).Nkind = N_Protected_Body
          or else NT (N).Nkind = N_Protected_Body_Stub
          or else NT (N).Nkind = N_Protected_Type_Declaration
          or else NT (N).Nkind = N_Single_Protected_Declaration
          or else NT (N).Nkind = N_Single_Task_Declaration
          or else NT (N).Nkind = N_Subtype_Declaration
          or else NT (N).Nkind = N_Task_Body
          or else NT (N).Nkind = N_Task_Body_Stub
          or else NT (N).Nkind = N_Task_Type_Declaration,
        Dcheck_Bad_Nkind (N));
      return Node1 (N);
   end Defining_Identifier;

   function Defining_Unit_Name
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Function_Instantiation
          or else NT (N).Nkind = N_Function_Specification
          or else NT (N).Nkind = N_Generic_Function_Renaming_Declaration
          or else NT (N).Nkind = N_Generic_Package_Renaming_Declaration
          or else NT (N).Nkind = N_Generic_Procedure_Renaming_Declaration
          or else NT (N).Nkind = N_Package_Body
          or else NT (N).Nkind = N_Package_Instantiation
          or else NT (N).Nkind = N_Package_Renaming_Declaration
          or else NT (N).Nkind = N_Package_Specification
          or else NT (N).Nkind = N_Procedure_Instantiation
          or else NT (N).Nkind = N_Procedure_Specification,
        Dcheck_Bad_Nkind (N));
      return Node1 (N);
   end Defining_Unit_Name;

   function Delay_Alternative
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Timed_Entry_Call,
        Dcheck_Bad_Nkind (N));
      return Node4 (N);
   end Delay_Alternative;

   function Delay_Statement
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Delay_Alternative,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Delay_Statement;

   function Delta_Expression
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Decimal_Fixed_Point_Definition
          or else NT (N).Nkind = N_Delta_Constraint
          or else NT (N).Nkind = N_Ordinary_Fixed_Point_Definition,
        Dcheck_Bad_Nkind (N));
      return Node3 (N);
   end Delta_Expression;

   function Denominator
      (N : Node_Id) return Uint is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Real_Literal,
        Dcheck_Bad_Nkind (N));
      return Uint4 (N);
   end Denominator;

   function Digits_Expression
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Decimal_Fixed_Point_Definition
          or else NT (N).Nkind = N_Digits_Constraint
          or else NT (N).Nkind = N_Floating_Point_Definition,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Digits_Expression;

   function Discrete_Choices
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Case_Statement_Alternative
          or else NT (N).Nkind = N_Variant,
        Dcheck_Bad_Nkind (N));
      return List4 (N);
   end Discrete_Choices;

   function Discrete_Range
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Slice,
        Dcheck_Bad_Nkind (N));
      return Node4 (N);
   end Discrete_Range;

   function Discrete_Subtype_Definition
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Entry_Declaration
          or else NT (N).Nkind = N_Entry_Index_Specification
          or else NT (N).Nkind = N_Loop_Parameter_Specification,
        Dcheck_Bad_Nkind (N));
      return Node4 (N);
   end Discrete_Subtype_Definition;

   function Discrete_Subtype_Definitions
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Constrained_Array_Definition,
        Dcheck_Bad_Nkind (N));
      return List2 (N);
   end Discrete_Subtype_Definitions;

   function Discriminant_Specifications
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Type_Declaration
          or else NT (N).Nkind = N_Full_Type_Declaration
          or else NT (N).Nkind = N_Incomplete_Type_Declaration
          or else NT (N).Nkind = N_Private_Extension_Declaration
          or else NT (N).Nkind = N_Private_Type_Declaration
          or else NT (N).Nkind = N_Protected_Type_Declaration
          or else NT (N).Nkind = N_Single_Protected_Declaration
          or else NT (N).Nkind = N_Single_Task_Declaration
          or else NT (N).Nkind = N_Task_Type_Declaration,
        Dcheck_Bad_Nkind (N));
      return List2 (N);
   end Discriminant_Specifications;

   function Discriminant_Type
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Discriminant_Specification,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Discriminant_Type;

   function Do_Access_Check
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Attribute_Reference
          or else NT (N).Nkind = N_Explicit_Dereference
          or else NT (N).Nkind = N_Indexed_Component
          or else NT (N).Nkind = N_Selected_Component
          or else NT (N).Nkind = N_Slice,
        Dcheck_Bad_Nkind (N));
      return Flag2 (N);
   end Do_Access_Check;

   function Do_Accessibility_Check
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Parameter_Specification,
        Dcheck_Bad_Nkind (N));
      return Flag3 (N);
   end Do_Accessibility_Check;

   function Do_Discriminant_Check
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Selected_Component,
        Dcheck_Bad_Nkind (N));
      return Flag3 (N);
   end Do_Discriminant_Check;

   function Do_Division_Check
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Op_Divide
          or else NT (N).Nkind = N_Op_Mod
          or else NT (N).Nkind = N_Op_Rem,
        Dcheck_Bad_Nkind (N));
      return Flag3 (N);
   end Do_Division_Check;

   function Do_Elaboration_Check
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Entry_Call_Statement
          or else NT (N).Nkind = N_Function_Call
          or else NT (N).Nkind = N_Procedure_Call_Statement,
        Dcheck_Bad_Nkind (N));
      return Flag2 (N);
   end Do_Elaboration_Check;

   function Do_Length_Check
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Assignment_Statement
          or else NT (N).Nkind = N_Op_And
          or else NT (N).Nkind = N_Op_Or
          or else NT (N).Nkind = N_Op_Xor
          or else NT (N).Nkind = N_Type_Conversion,
        Dcheck_Bad_Nkind (N));
      return Flag4 (N);
   end Do_Length_Check;

   function Do_Overflow_Check
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Op
          or else NT (N).Nkind = N_Type_Conversion,
        Dcheck_Bad_Nkind (N));
      return Flag2 (N);
   end Do_Overflow_Check;

   function Do_Range_Check
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Subexpr,
        Dcheck_Bad_Nkind (N));
      return Flag9 (N);
   end Do_Range_Check;

   function Do_Storage_Check
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Allocator
          or else NT (N).Nkind = N_Subprogram_Body,
        Dcheck_Bad_Nkind (N));
      return Flag2 (N);
   end Do_Storage_Check;

   function Do_Tag_Check
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Assignment_Statement
          or else NT (N).Nkind = N_Function_Call
          or else NT (N).Nkind = N_Procedure_Call_Statement
          or else NT (N).Nkind = N_Return_Statement
          or else NT (N).Nkind = N_Type_Conversion,
        Dcheck_Bad_Nkind (N));
      return Flag3 (N);
   end Do_Tag_Check;

   function Elaborate_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_With_Clause,
        Dcheck_Bad_Nkind (N));
      return Flag4 (N);
   end Elaborate_Present;

   function Elaborate_All_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_With_Clause,
        Dcheck_Bad_Nkind (N));
      return Flag1 (N);
   end Elaborate_All_Present;

   function Elaborate_Body_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit,
        Dcheck_Bad_Nkind (N));
      return Flag7 (N);
   end Elaborate_Body_Present;

   function Else_Statements
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Conditional_Entry_Call
          or else NT (N).Nkind = N_If_Statement
          or else NT (N).Nkind = N_Selective_Accept,
        Dcheck_Bad_Nkind (N));
      return List4 (N);
   end Else_Statements;

   function Elsif_Parts
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_If_Statement,
        Dcheck_Bad_Nkind (N));
      return List3 (N);
   end Elsif_Parts;

   function Enclosing_Variant
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Variant,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Enclosing_Variant;

   function Entity
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Op
          or else NT (N).Nkind = N_Attribute_Reference
          or else NT (N).Nkind = N_Character_Literal
          or else NT (N).Nkind = N_Expanded_Name
          or else NT (N).Nkind = N_Freeze_Entity
          or else NT (N).Nkind = N_Identifier
          or else NT (N).Nkind = N_Operator_Symbol,
        Dcheck_Bad_Nkind (N));
      return Node4 (N);
   end Entity;

   function Entry_Body_Formal_Part
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Entry_Body,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Entry_Body_Formal_Part;

   function Entry_Call_Alternative
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Conditional_Entry_Call
          or else NT (N).Nkind = N_Timed_Entry_Call,
        Dcheck_Bad_Nkind (N));
      return Node1 (N);
   end Entry_Call_Alternative;

   function Entry_Call_Statement
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Entry_Call_Alternative,
        Dcheck_Bad_Nkind (N));
      return Node1 (N);
   end Entry_Call_Statement;

   function Entry_Index
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Accept_Statement,
        Dcheck_Bad_Nkind (N));
      return Node5 (N);
   end Entry_Index;

   function Entry_Index_Specification
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Entry_Body_Formal_Part,
        Dcheck_Bad_Nkind (N));
      return Node1 (N);
   end Entry_Index_Specification;

   function Error_Posted
      (N : Node_Id) return Boolean is
   begin
      return Flag20 (N);
   end Error_Posted;

   function Etype
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
         or else NT (N).Nkind in N_Has_Etype,
        Dcheck_Bad_Nkind (N));
      return Node5 (N);
   end Etype;

   function Evaluate_Once
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Subexpr,
        Dcheck_Bad_Nkind (N));
      return Flag10 (N);
   end Evaluate_Once;

   function Exception_Choices
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Exception_Handler,
        Dcheck_Bad_Nkind (N));
      return List4 (N);
   end Exception_Choices;

   function Exception_Handlers
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Handled_Sequence_Of_Statements,
        Dcheck_Bad_Nkind (N));
      return List4 (N);
   end Exception_Handlers;

   function Explicit_Generic_Actual_Parameter
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Generic_Association,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Explicit_Generic_Actual_Parameter;

   function Expression
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Allocator
          or else NT (N).Nkind = N_Assignment_Statement
          or else NT (N).Nkind = N_At_Clause
          or else NT (N).Nkind = N_Attribute_Definition_Clause
          or else NT (N).Nkind = N_Attribute_Reference
          or else NT (N).Nkind = N_Case_Statement
          or else NT (N).Nkind = N_Code_Statement
          or else NT (N).Nkind = N_Component_Association
          or else NT (N).Nkind = N_Component_Declaration
          or else NT (N).Nkind = N_Delay_Relative_Statement
          or else NT (N).Nkind = N_Delay_Until_Statement
          or else NT (N).Nkind = N_Discriminant_Association
          or else NT (N).Nkind = N_Discriminant_Specification
          or else NT (N).Nkind = N_Expression_Actions
          or else NT (N).Nkind = N_Extension_Aggregate
          or else NT (N).Nkind = N_Formal_Object_Declaration
          or else NT (N).Nkind = N_Mod_Clause
          or else NT (N).Nkind = N_Modular_Type_Definition
          or else NT (N).Nkind = N_Number_Declaration
          or else NT (N).Nkind = N_Object_Declaration
          or else NT (N).Nkind = N_Parameter_Specification
          or else NT (N).Nkind = N_Parenthesized_Expression
          or else NT (N).Nkind = N_Pragma_Argument_Association
          or else NT (N).Nkind = N_Qualified_Expression
          or else NT (N).Nkind = N_Reference          
          or else NT (N).Nkind = N_Return_Statement
          or else NT (N).Nkind = N_Type_Conversion,
        Dcheck_Bad_Nkind (N));
      return Node3 (N);
   end Expression;

   function Expressions
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Aggregate
          or else NT (N).Nkind = N_Concat_Multiple
          or else NT (N).Nkind = N_Indexed_Component,
        Dcheck_Bad_Nkind (N));
      return List3 (N);
   end Expressions;

   function First_Bit
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_Clause,
        Dcheck_Bad_Nkind (N));
      return Node3 (N);
   end First_Bit;

   function First_Name
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_With_Clause,
        Dcheck_Bad_Nkind (N));
      return Flag5 (N);
   end First_Name;

   function First_Named_Actual
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Function_Call
          or else NT (N).Nkind = N_Procedure_Call_Statement,
        Dcheck_Bad_Nkind (N));
      return Node4 (N);
   end First_Named_Actual;

   function Following_Pragmas
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit,
        Dcheck_Bad_Nkind (N));
      return List3 (N);
   end Following_Pragmas;

   function Formal_Type_Definition
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Type_Declaration,
        Dcheck_Bad_Nkind (N));
      return Node3 (N);
   end Formal_Type_Definition;

   function Generic_Associations
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Package_Declaration
          or else NT (N).Nkind = N_Function_Instantiation
          or else NT (N).Nkind = N_Package_Instantiation
          or else NT (N).Nkind = N_Procedure_Instantiation,
        Dcheck_Bad_Nkind (N));
      return List3 (N);
   end Generic_Associations;

   function Generic_Formal_Declarations
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Generic_Package_Declaration
          or else NT (N).Nkind = N_Generic_Subprogram_Declaration,
        Dcheck_Bad_Nkind (N));
      return List3 (N);
   end Generic_Formal_Declarations;

   function Generic_Parent
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Package_Declaration
          or else NT (N).Nkind = N_Function_Specification
          or else NT (N).Nkind = N_Package_Specification
          or else NT (N).Nkind = N_Procedure_Specification,
        Dcheck_Bad_Nkind (N));
      return Node5 (N);
   end Generic_Parent;

   function Handled_Statement_Sequence
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Accept_Statement
          or else NT (N).Nkind = N_Block_Statement
          or else NT (N).Nkind = N_Entry_Body
          or else NT (N).Nkind = N_Package_Body
          or else NT (N).Nkind = N_Subprogram_Body
          or else NT (N).Nkind = N_Task_Body,
        Dcheck_Bad_Nkind (N));
      return Node4 (N);
   end Handled_Statement_Sequence;

   function Has_Created_Identifier
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Block_Statement
          or else NT (N).Nkind = N_Loop_Statement,
        Dcheck_Bad_Nkind (N));
      return Flag1 (N);
   end Has_Created_Identifier;

   function Has_No_Side_Effects
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Subexpr,
        Dcheck_Bad_Nkind (N));
      return Flag8 (N);
   end Has_No_Side_Effects;

   function Has_Priority_Pragma
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Protected_Definition
          or else NT (N).Nkind = N_Subprogram_Body
          or else NT (N).Nkind = N_Task_Definition,
        Dcheck_Bad_Nkind (N));
      return Flag6 (N);
   end Has_Priority_Pragma;

   function Has_Task_Stack_Size_Pragma
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Task_Definition,
        Dcheck_Bad_Nkind (N));
      return Flag5 (N);
   end Has_Task_Stack_Size_Pragma;

   function Has_Unknown_Discriminants
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Type_Declaration
          or else NT (N).Nkind = N_Incomplete_Type_Declaration
          or else NT (N).Nkind = N_Private_Extension_Declaration
          or else NT (N).Nkind = N_Private_Type_Declaration,
        Dcheck_Bad_Nkind (N));
      return Flag3 (N);
   end Has_Unknown_Discriminants;

   function High_Bound
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Range
          or else NT (N).Nkind = N_Real_Range_Specification
          or else NT (N).Nkind = N_Signed_Integer_Type_Definition,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end High_Bound;

   function Identifier
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_At_Clause
          or else NT (N).Nkind = N_Attribute_Definition_Clause
          or else NT (N).Nkind = N_Attribute_Reference
          or else NT (N).Nkind = N_Block_Statement
          or else NT (N).Nkind = N_Designator
          or else NT (N).Nkind = N_Enumeration_Representation_Clause
          or else NT (N).Nkind = N_Handled_Sequence_Of_Statements
          or else NT (N).Nkind = N_Label
          or else NT (N).Nkind = N_Loop_Statement
          or else NT (N).Nkind = N_Pragma
          or else NT (N).Nkind = N_Pragma_Argument_Association
          or else NT (N).Nkind = N_Record_Representation_Clause,
        Dcheck_Bad_Nkind (N));
      return Node1 (N);
   end Identifier;

   function Implicit_Types
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Record_Definition,
        Dcheck_Bad_Nkind (N));
      return List2 (N);
   end Implicit_Types;

   function Implicit_With
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_With_Clause,
        Dcheck_Bad_Nkind (N));
      return Flag2 (N);
   end Implicit_With;

   function In_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Object_Declaration
          or else NT (N).Nkind = N_Parameter_Specification,
        Dcheck_Bad_Nkind (N));
      return Flag1 (N);
   end In_Present;

   function Intval
      (N : Node_Id) return Uint is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Integer_Literal,
        Dcheck_Bad_Nkind (N));
      return Uint3 (N);
   end Intval;

   function Is_Evaluated
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Subexpr,
        Dcheck_Bad_Nkind (N));
      return Flag7 (N);
   end Is_Evaluated;

   function Is_Overloaded
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Subexpr,
        Dcheck_Bad_Nkind (N));
      return Flag5 (N);
   end Is_Overloaded;

   function Is_Static
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Subexpr,
        Dcheck_Bad_Nkind (N));
      return Flag6 (N);
   end Is_Static;

   function Is_Task_Master
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Block_Statement
          or else NT (N).Nkind = N_Subprogram_Body
          or else NT (N).Nkind = N_Task_Body,
        Dcheck_Bad_Nkind (N));
      return Flag5 (N);
   end Is_Task_Master;

   function Iteration_Scheme
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Loop_Statement,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Iteration_Scheme;

   function Label
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Implicit_Label_Declaration,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Label;

   function Last_Bit
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_Clause,
        Dcheck_Bad_Nkind (N));
      return Node4 (N);
   end Last_Bit;

   function Last_Name
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_With_Clause,
        Dcheck_Bad_Nkind (N));
      return Flag6 (N);
   end Last_Name;

   function Library_Unit
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit
          or else NT (N).Nkind = N_Package_Body_Stub
          or else NT (N).Nkind = N_Protected_Body_Stub
          or else NT (N).Nkind = N_Subprogram_Body_Stub
          or else NT (N).Nkind = N_Task_Body_Stub
          or else NT (N).Nkind = N_With_Clause,
        Dcheck_Bad_Nkind (N));
      return Node4 (N);
   end Library_Unit;

   function Left_Opnd
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Binary_Op,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Left_Opnd;

   function Literals
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Enumeration_Type_Definition,
        Dcheck_Bad_Nkind (N));
      return List1 (N);
   end Literals;

   function Limited_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Private_Type_Definition
          or else NT (N).Nkind = N_Private_Type_Declaration
          or else NT (N).Nkind = N_Record_Definition,
        Dcheck_Bad_Nkind (N));
      return Flag2 (N);
   end Limited_Present;

   function Loop_Parameter_Specification
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Iteration_Scheme,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Loop_Parameter_Specification;

   function Low_Bound
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Range
          or else NT (N).Nkind = N_Real_Range_Specification
          or else NT (N).Nkind = N_Signed_Integer_Type_Definition,
        Dcheck_Bad_Nkind (N));
      return Node1 (N);
   end Low_Bound;

   function Mod_Clause
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Record_Representation_Clause,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Mod_Clause;

   function More_Ids
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_Declaration
          or else NT (N).Nkind = N_Discriminant_Specification
          or else NT (N).Nkind = N_Exception_Declaration
          or else NT (N).Nkind = N_Formal_Object_Declaration
          or else NT (N).Nkind = N_Number_Declaration
          or else NT (N).Nkind = N_Object_Declaration
          or else NT (N).Nkind = N_Parameter_Specification,
        Dcheck_Bad_Nkind (N));
      return Flag5 (N);
   end More_Ids;

   function Name
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Assignment_Statement
          or else NT (N).Nkind = N_Attribute_Definition_Clause
          or else NT (N).Nkind = N_Defining_Program_Unit_Name
          or else NT (N).Nkind = N_Designator
          or else NT (N).Nkind = N_Entry_Call_Statement
          or else NT (N).Nkind = N_Exception_Renaming_Declaration
          or else NT (N).Nkind = N_Exit_Statement
          or else NT (N).Nkind = N_Formal_Package_Declaration
          or else NT (N).Nkind = N_Function_Call
          or else NT (N).Nkind = N_Function_Instantiation
          or else NT (N).Nkind = N_Generic_Function_Renaming_Declaration
          or else NT (N).Nkind = N_Generic_Package_Renaming_Declaration
          or else NT (N).Nkind = N_Generic_Procedure_Renaming_Declaration
          or else NT (N).Nkind = N_Goto_Statement
          or else NT (N).Nkind = N_Object_Renaming_Declaration
          or else NT (N).Nkind = N_Package_Instantiation
          or else NT (N).Nkind = N_Package_Renaming_Declaration
          or else NT (N).Nkind = N_Procedure_Call_Statement
          or else NT (N).Nkind = N_Procedure_Instantiation
          or else NT (N).Nkind = N_Raise_Statement
          or else NT (N).Nkind = N_Requeue_Statement
          or else NT (N).Nkind = N_Subprogram_Renaming_Declaration
          or else NT (N).Nkind = N_Subunit
          or else NT (N).Nkind = N_Variant_Part
          or else NT (N).Nkind = N_With_Clause,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Name;

   function Names
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Abort_Statement
          or else NT (N).Nkind = N_Use_Package_Clause,
        Dcheck_Bad_Nkind (N));
      return List2 (N);
   end Names;

   function Next_Named_Actual
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Parameter_Association,
        Dcheck_Bad_Nkind (N));
      return Node4 (N);
   end Next_Named_Actual;

   function Null_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_List
          or else NT (N).Nkind = N_Record_Definition,
        Dcheck_Bad_Nkind (N));
      return Flag3 (N);
   end Null_Present;

   function Null_Record_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Aggregate
          or else NT (N).Nkind = N_Extension_Aggregate,
        Dcheck_Bad_Nkind (N));
      return Flag2 (N);
   end Null_Record_Present;

   function Numerator
      (N : Node_Id) return Uint is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Real_Literal,
        Dcheck_Bad_Nkind (N));
      return Uint3 (N);
   end Numerator;

   function Object_Definition
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Object_Declaration,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Object_Definition;

   function Others_Discrete_Choices
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Others_Choice,
        Dcheck_Bad_Nkind (N));
      return List1 (N);
   end Others_Discrete_Choices;

   function Out_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Object_Declaration
          or else NT (N).Nkind = N_Parameter_Specification,
        Dcheck_Bad_Nkind (N));
      return Flag2 (N);
   end Out_Present;

   function Parameter_Associations
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Entry_Call_Statement
          or else NT (N).Nkind = N_Function_Call
          or else NT (N).Nkind = N_Procedure_Call_Statement,
        Dcheck_Bad_Nkind (N));
      return List3 (N);
   end Parameter_Associations;

   function Parameter_Specifications
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Accept_Statement
          or else NT (N).Nkind = N_Access_Function_Definition
          or else NT (N).Nkind = N_Access_Procedure_Definition
          or else NT (N).Nkind = N_Entry_Body_Formal_Part
          or else NT (N).Nkind = N_Entry_Declaration
          or else NT (N).Nkind = N_Function_Specification
          or else NT (N).Nkind = N_Procedure_Specification,
        Dcheck_Bad_Nkind (N));
      return List2 (N);
   end Parameter_Specifications;

   function Parameter_Type
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Parameter_Specification,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Parameter_Type;

   function Parens
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Subexpr,
        Dcheck_Bad_Nkind (N));
      return Flag1 (N);
   end Parens;

   function Parent_Spec
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Function_Instantiation
          or else NT (N).Nkind = N_Generic_Function_Renaming_Declaration
          or else NT (N).Nkind = N_Generic_Package_Declaration
          or else NT (N).Nkind = N_Generic_Package_Renaming_Declaration
          or else NT (N).Nkind = N_Generic_Procedure_Renaming_Declaration
          or else NT (N).Nkind = N_Generic_Subprogram_Declaration
          or else NT (N).Nkind = N_Package_Declaration
          or else NT (N).Nkind = N_Package_Instantiation
          or else NT (N).Nkind = N_Package_Renaming_Declaration
          or else NT (N).Nkind = N_Procedure_Instantiation
          or else NT (N).Nkind = N_Subprogram_Declaration
          or else NT (N).Nkind = N_Subprogram_Renaming_Declaration,
        Dcheck_Bad_Nkind (N));
      return Node4 (N);
   end Parent_Spec;

   function Position
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_Clause,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Position;

   function Pragma_Argument_Associations
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Pragma,
        Dcheck_Bad_Nkind (N));
      return List2 (N);
   end Pragma_Argument_Associations;

   function Preelaborable
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit,
        Dcheck_Bad_Nkind (N));
      return Flag2 (N);
   end Preelaborable;

   function Prefix
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Attribute_Reference
          or else NT (N).Nkind = N_Expanded_Name
          or else NT (N).Nkind = N_Explicit_Dereference
          or else NT (N).Nkind = N_Indexed_Component
          or else NT (N).Nkind = N_Selected_Component
          or else NT (N).Nkind = N_Slice,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Prefix;

   function Prev_Ids
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_Declaration
          or else NT (N).Nkind = N_Discriminant_Specification
          or else NT (N).Nkind = N_Exception_Declaration
          or else NT (N).Nkind = N_Formal_Object_Declaration
          or else NT (N).Nkind = N_Number_Declaration
          or else NT (N).Nkind = N_Object_Declaration
          or else NT (N).Nkind = N_Parameter_Specification,
        Dcheck_Bad_Nkind (N));
      return Flag6 (N);
   end Prev_Ids;

   function Private_Declarations
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Package_Specification
          or else NT (N).Nkind = N_Protected_Definition
          or else NT (N).Nkind = N_Task_Definition,
        Dcheck_Bad_Nkind (N));
      return List4 (N);
   end Private_Declarations;

   function Private_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit
          or else NT (N).Nkind = N_Formal_Derived_Type_Definition,
        Dcheck_Bad_Nkind (N));
      return Flag1 (N);
   end Private_Present;

   function Proper_Body
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Subunit,
        Dcheck_Bad_Nkind (N));
      return Node1 (N);
   end Proper_Body;

   function Protected_Definition
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Protected_Type_Declaration
          or else NT (N).Nkind = N_Single_Protected_Declaration,
        Dcheck_Bad_Nkind (N));
      return Node3 (N);
   end Protected_Definition;

   function Protected_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Access_Function_Definition
          or else NT (N).Nkind = N_Access_Procedure_Definition,
        Dcheck_Bad_Nkind (N));
      return Flag1 (N);
   end Protected_Present;

   function Range_Constraint
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Delta_Constraint
          or else NT (N).Nkind = N_Digits_Constraint,
        Dcheck_Bad_Nkind (N));
      return Node4 (N);
   end Range_Constraint;

   function Range_Expression
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Range_Constraint,
        Dcheck_Bad_Nkind (N));
      return Node4 (N);
   end Range_Expression;

   function Real_Range_Specification
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Decimal_Fixed_Point_Definition
          or else NT (N).Nkind = N_Floating_Point_Definition
          or else NT (N).Nkind = N_Ordinary_Fixed_Point_Definition,
        Dcheck_Bad_Nkind (N));
      return Node4 (N);
   end Real_Range_Specification;

   function Record_Extension_Part
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Derived_Type_Definition,
        Dcheck_Bad_Nkind (N));
      return Node3 (N);
   end Record_Extension_Part;

   function Redundant_Use
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Expanded_Name
          or else NT (N).Nkind = N_Identifier,
        Dcheck_Bad_Nkind (N));
      return Flag2 (N);
   end Redundant_Use;

   function Reverse_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Loop_Parameter_Specification,
        Dcheck_Bad_Nkind (N));
      return Flag1 (N);
   end Reverse_Present;

   function Right_Opnd
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Op,
        Dcheck_Bad_Nkind (N));
      return Node3 (N);
   end Right_Opnd;

   function Selective_Accept_Alternatives
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Selective_Accept,
        Dcheck_Bad_Nkind (N));
      return List1 (N);
   end Selective_Accept_Alternatives;

   function Selector_Name
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Expanded_Name
          or else NT (N).Nkind = N_Generic_Association
          or else NT (N).Nkind = N_Parameter_Association
          or else NT (N).Nkind = N_Selected_Component,
        Dcheck_Bad_Nkind (N));
      return Node3 (N);
   end Selector_Name;

   function Selector_Names
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Discriminant_Association,
        Dcheck_Bad_Nkind (N));
      return List1 (N);
   end Selector_Names;

   function Specification
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Abstract_Subprogram_Declaration
          or else NT (N).Nkind = N_Formal_Subprogram_Declaration
          or else NT (N).Nkind = N_Generic_Package_Declaration
          or else NT (N).Nkind = N_Generic_Subprogram_Declaration
          or else NT (N).Nkind = N_Package_Declaration
          or else NT (N).Nkind = N_Subprogram_Body
          or else NT (N).Nkind = N_Subprogram_Body_Stub
          or else NT (N).Nkind = N_Subprogram_Declaration
          or else NT (N).Nkind = N_Subprogram_Renaming_Declaration,
        Dcheck_Bad_Nkind (N));
      return Node1 (N);
   end Specification;

   function Statements
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Abortable_Part
          or else NT (N).Nkind = N_Accept_Alternative
          or else NT (N).Nkind = N_Case_Statement_Alternative
          or else NT (N).Nkind = N_Delay_Alternative
          or else NT (N).Nkind = N_Entry_Call_Alternative
          or else NT (N).Nkind = N_Exception_Handler
          or else NT (N).Nkind = N_Handled_Sequence_Of_Statements
          or else NT (N).Nkind = N_Loop_Statement
          or else NT (N).Nkind = N_Triggering_Alternative,
        Dcheck_Bad_Nkind (N));
      return List3 (N);
   end Statements;

   function Strval
      (N : Node_Id) return String_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Operator_Symbol
          or else NT (N).Nkind = N_String_Literal,
        Dcheck_Bad_Nkind (N));
      return Str3 (N);
   end Strval;

   function Subtype_Indication
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Access_To_Object_Definition
          or else NT (N).Nkind = N_Component_Declaration
          or else NT (N).Nkind = N_Constrained_Array_Definition
          or else NT (N).Nkind = N_Derived_Type_Definition
          or else NT (N).Nkind = N_Private_Extension_Declaration
          or else NT (N).Nkind = N_Subtype_Declaration
          or else NT (N).Nkind = N_Unconstrained_Array_Definition,
        Dcheck_Bad_Nkind (N));
      return Node4 (N);
   end Subtype_Indication;

   function Subtype_Mark
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Access_Definition
          or else NT (N).Nkind = N_Access_Function_Definition
          or else NT (N).Nkind = N_Formal_Derived_Type_Definition
          or else NT (N).Nkind = N_Formal_Object_Declaration
          or else NT (N).Nkind = N_Function_Specification
          or else NT (N).Nkind = N_Object_Renaming_Declaration
          or else NT (N).Nkind = N_Qualified_Expression
          or else NT (N).Nkind = N_Subtype_Indication
          or else NT (N).Nkind = N_Type_Conversion,
        Dcheck_Bad_Nkind (N));
      return Node4 (N);
   end Subtype_Mark;

   function Subtype_Marks
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Unconstrained_Array_Definition
          or else NT (N).Nkind = N_Use_Type_Clause,
        Dcheck_Bad_Nkind (N));
      return List2 (N);
   end Subtype_Marks;

   function Tagged_Present
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Private_Type_Definition
          or else NT (N).Nkind = N_Private_Type_Declaration
          or else NT (N).Nkind = N_Record_Definition,
        Dcheck_Bad_Nkind (N));
      return Flag1 (N);
   end Tagged_Present;

   function Task_Definition
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Single_Task_Declaration
          or else NT (N).Nkind = N_Task_Type_Declaration,
        Dcheck_Bad_Nkind (N));
      return Node3 (N);
   end Task_Definition;

   function Then_Statements
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Elsif_Part
          or else NT (N).Nkind = N_If_Statement,
        Dcheck_Bad_Nkind (N));
      return List2 (N);
   end Then_Statements;

   function Triggering_Alternative
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Asynchronous_Select,
        Dcheck_Bad_Nkind (N));
      return Node1 (N);
   end Triggering_Alternative;

   function Triggering_Statement
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Triggering_Alternative,
        Dcheck_Bad_Nkind (N));
      return Node1 (N);
   end Triggering_Statement;

   function Type_Definition
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Full_Type_Declaration,
        Dcheck_Bad_Nkind (N));
      return Node3 (N);
   end Type_Definition;

   function Unchecked_Conversion
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Type_Conversion,
        Dcheck_Bad_Nkind (N));
      return Flag11 (N);
   end Unchecked_Conversion;

   function Unit
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit,
        Dcheck_Bad_Nkind (N));
      return Node2 (N);
   end Unit;

   function Variant_Part
      (N : Node_Id) return Node_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_List,
        Dcheck_Bad_Nkind (N));
      return Node4 (N);
   end Variant_Part;

   function Variants
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Variant_Part,
        Dcheck_Bad_Nkind (N));
      return List1 (N);
   end Variants;

   function Visible_Declarations
      (N : Node_Id) return List_Id is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Package_Specification
          or else NT (N).Nkind = N_Protected_Definition
          or else NT (N).Nkind = N_Task_Definition,
        Dcheck_Bad_Nkind (N));
      return List2 (N);
   end Visible_Declarations;

   --------------------------
   -- Field Set Procedures --
   --------------------------

   procedure Set_Abort_Present
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Requeue_Statement,
        Dcheck_Bad_Nkind (N));
      Set_Flag1 (N, Val);
   end Set_Abort_Present;

   procedure Set_Abortable_Part
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Asynchronous_Select,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Abortable_Part;

   procedure Set_Abstract_Present
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Derived_Type_Definition
          or else NT (N).Nkind = N_Formal_Private_Type_Definition
          or else NT (N).Nkind = N_Private_Extension_Declaration
          or else NT (N).Nkind = N_Private_Type_Declaration
          or else NT (N).Nkind = N_Record_Definition,
        Dcheck_Bad_Nkind (N));
      Set_Flag4 (N, Val);
   end Set_Abstract_Present;

   procedure Set_Accept_Name
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Accept_Statement,
        Dcheck_Bad_Nkind (N));
      Set_Node1_With_Parent (N, Val);
   end Set_Accept_Name;

   procedure Set_Accept_Statement
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Accept_Alternative,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Accept_Statement;

   procedure Set_Actions
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Expression_Actions,
        Dcheck_Bad_Nkind (N));
      Set_List1_With_Parent (N, Val);
   end Set_Actions;

   procedure Set_Activation_Chain_Entity
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Block_Statement
          or else NT (N).Nkind = N_Generic_Package_Declaration
          or else NT (N).Nkind = N_Package_Declaration
          or else NT (N).Nkind = N_Subprogram_Body
          or else NT (N).Nkind = N_Task_Body,
        Dcheck_Bad_Nkind (N));
      Set_Node2 (N, Val); -- semantic node, no parent set
   end Set_Activation_Chain_Entity;

   procedure Set_Acts_As_Spec
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit
          or else NT (N).Nkind = N_Subprogram_Body,
        Dcheck_Bad_Nkind (N));
      Set_Flag4 (N, Val);
   end Set_Acts_As_Spec;

   procedure Set_Actual_Parameter
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Parameter_Association,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Actual_Parameter;

   procedure Set_Aliased_Present
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_Declaration
          or else NT (N).Nkind = N_Constrained_Array_Definition
          or else NT (N).Nkind = N_Object_Declaration
          or else NT (N).Nkind = N_Unconstrained_Array_Definition,
        Dcheck_Bad_Nkind (N));
      Set_Flag1 (N, Val);
   end Set_Aliased_Present;

   procedure Set_All_Present
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Access_To_Object_Definition,
        Dcheck_Bad_Nkind (N));
      Set_Flag1 (N, Val);
   end Set_All_Present;

   procedure Set_Alternatives
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Case_Statement,
        Dcheck_Bad_Nkind (N));
      Set_List4_With_Parent (N, Val);
   end Set_Alternatives;

   procedure Set_Analyzed
      (N : Node_Id; Val : Boolean) is
   begin
      Set_Flag19 (N, Val);
   end Set_Analyzed;

   procedure Set_Array_Aggregate
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Enumeration_Representation_Clause,
        Dcheck_Bad_Nkind (N));
      Set_Node4_With_Parent (N, Val);
   end Set_Array_Aggregate;

   procedure Set_Assignment_OK
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Assignment_Statement,
        Dcheck_Bad_Nkind (N));
      Set_Flag5 (N, Val);
   end Set_Assignment_OK;

   procedure Set_Bad_Is_Detected
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Subprogram_Body,
        Dcheck_Bad_Nkind (N));
      Set_Flag1 (N, Val);
   end Set_Bad_Is_Detected;

   procedure Set_Body_Required
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit,
        Dcheck_Bad_Nkind (N));
      Set_Flag3 (N, Val);
   end Set_Body_Required;

   procedure Set_Box_Present
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Package_Declaration
          or else NT (N).Nkind = N_Formal_Subprogram_Declaration,
        Dcheck_Bad_Nkind (N));
      Set_Flag1 (N, Val);
   end Set_Box_Present;

   procedure Set_Char_Literal_Value
      (N : Node_Id; Val : Char_Code) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Character_Literal,
        Dcheck_Bad_Nkind (N));
      Set_Char_Code2 (N, Val);
   end Set_Char_Literal_Value;

   procedure Set_Chars
      (N : Node_Id; Val : Name_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Has_Chars,
        Dcheck_Bad_Nkind (N));
      Set_Name1 (N, Val);
   end Set_Chars;

   procedure Set_Choice_Parameter
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Exception_Handler,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Choice_Parameter;

   procedure Set_Choices
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_Association,
        Dcheck_Bad_Nkind (N));
      Set_List1_With_Parent (N, Val);
   end Set_Choices;

   procedure Set_Component_Associations
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Aggregate
          or else NT (N).Nkind = N_Extension_Aggregate,
        Dcheck_Bad_Nkind (N));
      Set_List4_With_Parent (N, Val);
   end Set_Component_Associations;

   procedure Set_Component_Clauses
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Record_Representation_Clause,
        Dcheck_Bad_Nkind (N));
      Set_List3_With_Parent (N, Val);
   end Set_Component_Clauses;

   procedure Set_Component_Declarations
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_List,
        Dcheck_Bad_Nkind (N));
      Set_List3_With_Parent (N, Val);
   end Set_Component_Declarations;

   procedure Set_Component_List
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Record_Definition
          or else NT (N).Nkind = N_Variant,
        Dcheck_Bad_Nkind (N));
      Set_Node1_With_Parent (N, Val);
   end Set_Component_List;

   procedure Set_Component_Name
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_Clause,
        Dcheck_Bad_Nkind (N));
      Set_Node1_With_Parent (N, Val);
   end Set_Component_Name;

   procedure Set_Condition
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Accept_Alternative
          or else NT (N).Nkind = N_Delay_Alternative
          or else NT (N).Nkind = N_Elsif_Part
          or else NT (N).Nkind = N_Entry_Body
          or else NT (N).Nkind = N_Exit_Statement
          or else NT (N).Nkind = N_If_Statement
          or else NT (N).Nkind = N_Iteration_Scheme
          or else NT (N).Nkind = N_Terminate_Alternative,
        Dcheck_Bad_Nkind (N));
      Set_Node5_With_Parent (N, Val);
   end Set_Condition;

   procedure Set_Constant_Present
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Access_To_Object_Definition
          or else NT (N).Nkind = N_Object_Declaration,
        Dcheck_Bad_Nkind (N));
      Set_Flag2 (N, Val);
   end Set_Constant_Present;

   procedure Set_Constraint
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Subtype_Indication,
        Dcheck_Bad_Nkind (N));
      Set_Node3_With_Parent (N, Val);
   end Set_Constraint;

   procedure Set_Constraints
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Index_Or_Discriminant_Constraint,
        Dcheck_Bad_Nkind (N));
      Set_List1_With_Parent (N, Val);
   end Set_Constraints;

   procedure Set_Context_Installed
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_With_Clause,
        Dcheck_Bad_Nkind (N));
      Set_Flag3 (N, Val);
   end Set_Context_Installed;

   procedure Set_Context_Items
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit,
        Dcheck_Bad_Nkind (N));
      Set_List1_With_Parent (N, Val);
   end Set_Context_Items;

   procedure Set_Controlling_Argument
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Function_Call
          or else NT (N).Nkind = N_Procedure_Call_Statement,
        Dcheck_Bad_Nkind (N));
      Set_Node1 (N, Val); -- Note: semantic node, no parent set
   end Set_Controlling_Argument;

   procedure Set_Corresponding_Body
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Generic_Package_Declaration
          or else NT (N).Nkind = N_Generic_Subprogram_Declaration
          or else NT (N).Nkind = N_Package_Body_Stub
          or else NT (N).Nkind = N_Package_Declaration
          or else NT (N).Nkind = N_Protected_Body_Stub
          or else NT (N).Nkind = N_Subprogram_Body_Stub
          or else NT (N).Nkind = N_Subprogram_Declaration
          or else NT (N).Nkind = N_Task_Body_Stub,
        Dcheck_Bad_Nkind (N));
      Set_Node5 (N, Val); -- Semantic node, no parent set
   end Set_Corresponding_Body;

   procedure Set_Corresponding_Spec
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Package_Body
          or else NT (N).Nkind = N_Subprogram_Body
          or else NT (N).Nkind = N_Subprogram_Renaming_Declaration
          or else NT (N).Nkind = N_Task_Body
          or else NT (N).Nkind = N_With_Clause,
        Dcheck_Bad_Nkind (N));
      Set_Node5 (N, Val); -- Semantic node, no parent set
   end Set_Corresponding_Spec;

   procedure Set_Debug_Statement
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Pragma,
        Dcheck_Bad_Nkind (N));
      Set_Node3 (N, Val);  -- semantic node, no parent set
   end Set_Debug_Statement;

   procedure Set_Decimal
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Real_Literal,
        Dcheck_Bad_Nkind (N));
      Set_Flag2 (N, Val);
   end Set_Decimal;

   procedure Set_Declarations
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Accept_Statement
          or else NT (N).Nkind = N_Block_Statement
          or else NT (N).Nkind = N_Entry_Body
          or else NT (N).Nkind = N_Package_Body
          or else NT (N).Nkind = N_Protected_Body
          or else NT (N).Nkind = N_Subprogram_Body
          or else NT (N).Nkind = N_Task_Body,
        Dcheck_Bad_Nkind (N));
      Set_List3_With_Parent (N, Val);
   end Set_Declarations;

   procedure Set_Default_Name
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Subprogram_Declaration,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Default_Name;

   procedure Set_Defining_Identifier
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_Declaration
          or else NT (N).Nkind = N_Defining_Program_Unit_Name
          or else NT (N).Nkind = N_Discriminant_Specification
          or else NT (N).Nkind = N_Entry_Body
          or else NT (N).Nkind = N_Entry_Declaration
          or else NT (N).Nkind = N_Entry_Index_Specification
          or else NT (N).Nkind = N_Exception_Declaration
          or else NT (N).Nkind = N_Exception_Renaming_Declaration
          or else NT (N).Nkind = N_Formal_Object_Declaration
          or else NT (N).Nkind = N_Formal_Package_Declaration
          or else NT (N).Nkind = N_Formal_Type_Declaration
          or else NT (N).Nkind = N_Full_Type_Declaration
          or else NT (N).Nkind = N_Implicit_Label_Declaration
          or else NT (N).Nkind = N_Implicit_Type
          or else NT (N).Nkind = N_Incomplete_Type_Declaration
          or else NT (N).Nkind = N_Loop_Parameter_Specification
          or else NT (N).Nkind = N_Number_Declaration
          or else NT (N).Nkind = N_Object_Declaration
          or else NT (N).Nkind = N_Object_Renaming_Declaration
          or else NT (N).Nkind = N_Package_Body_Stub
          or else NT (N).Nkind = N_Parameter_Specification
          or else NT (N).Nkind = N_Private_Extension_Declaration
          or else NT (N).Nkind = N_Private_Type_Declaration
          or else NT (N).Nkind = N_Protected_Body
          or else NT (N).Nkind = N_Protected_Body_Stub
          or else NT (N).Nkind = N_Protected_Type_Declaration
          or else NT (N).Nkind = N_Single_Protected_Declaration
          or else NT (N).Nkind = N_Single_Task_Declaration
          or else NT (N).Nkind = N_Subtype_Declaration
          or else NT (N).Nkind = N_Task_Body
          or else NT (N).Nkind = N_Task_Body_Stub
          or else NT (N).Nkind = N_Task_Type_Declaration,
        Dcheck_Bad_Nkind (N));
      Set_Node1_With_Parent (N, Val);
   end Set_Defining_Identifier;

   procedure Set_Defining_Unit_Name
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Function_Instantiation
          or else NT (N).Nkind = N_Function_Specification
          or else NT (N).Nkind = N_Generic_Function_Renaming_Declaration
          or else NT (N).Nkind = N_Generic_Package_Renaming_Declaration
          or else NT (N).Nkind = N_Generic_Procedure_Renaming_Declaration
          or else NT (N).Nkind = N_Package_Body
          or else NT (N).Nkind = N_Package_Instantiation
          or else NT (N).Nkind = N_Package_Renaming_Declaration
          or else NT (N).Nkind = N_Package_Specification
          or else NT (N).Nkind = N_Procedure_Instantiation
          or else NT (N).Nkind = N_Procedure_Specification,
        Dcheck_Bad_Nkind (N));
      Set_Node1_With_Parent (N, Val);
   end Set_Defining_Unit_Name;

   procedure Set_Delay_Alternative
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Timed_Entry_Call,
        Dcheck_Bad_Nkind (N));
      Set_Node4_With_Parent (N, Val);
   end Set_Delay_Alternative;

   procedure Set_Delay_Statement
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Delay_Alternative,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Delay_Statement;

   procedure Set_Delta_Expression
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Decimal_Fixed_Point_Definition
          or else NT (N).Nkind = N_Delta_Constraint
          or else NT (N).Nkind = N_Ordinary_Fixed_Point_Definition,
        Dcheck_Bad_Nkind (N));
      Set_Node3_With_Parent (N, Val);
   end Set_Delta_Expression;

   procedure Set_Denominator
      (N : Node_Id; Val : Uint) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Real_Literal,
        Dcheck_Bad_Nkind (N));
      Set_Uint4 (N, Val);
   end Set_Denominator;

   procedure Set_Digits_Expression
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Decimal_Fixed_Point_Definition
          or else NT (N).Nkind = N_Digits_Constraint
          or else NT (N).Nkind = N_Floating_Point_Definition,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Digits_Expression;

   procedure Set_Discrete_Choices
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Case_Statement_Alternative
          or else NT (N).Nkind = N_Variant,
        Dcheck_Bad_Nkind (N));
      Set_List4_With_Parent (N, Val);
   end Set_Discrete_Choices;

   procedure Set_Discrete_Range
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Slice,
        Dcheck_Bad_Nkind (N));
      Set_Node4_With_Parent (N, Val);
   end Set_Discrete_Range;

   procedure Set_Discrete_Subtype_Definition
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Entry_Declaration
          or else NT (N).Nkind = N_Entry_Index_Specification
          or else NT (N).Nkind = N_Loop_Parameter_Specification,
        Dcheck_Bad_Nkind (N));
      Set_Node4_With_Parent (N, Val);
   end Set_Discrete_Subtype_Definition;

   procedure Set_Discrete_Subtype_Definitions
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Constrained_Array_Definition,
        Dcheck_Bad_Nkind (N));
      Set_List2_With_Parent (N, Val);
   end Set_Discrete_Subtype_Definitions;

   procedure Set_Discriminant_Specifications
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Type_Declaration
          or else NT (N).Nkind = N_Full_Type_Declaration
          or else NT (N).Nkind = N_Incomplete_Type_Declaration
          or else NT (N).Nkind = N_Private_Extension_Declaration
          or else NT (N).Nkind = N_Private_Type_Declaration
          or else NT (N).Nkind = N_Protected_Type_Declaration
          or else NT (N).Nkind = N_Single_Protected_Declaration
          or else NT (N).Nkind = N_Single_Task_Declaration
          or else NT (N).Nkind = N_Task_Type_Declaration,
        Dcheck_Bad_Nkind (N));
      Set_List2_With_Parent (N, Val);
   end Set_Discriminant_Specifications;

   procedure Set_Discriminant_Type
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Discriminant_Specification,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Discriminant_Type;

   procedure Set_Do_Access_Check
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Attribute_Reference
          or else NT (N).Nkind = N_Explicit_Dereference
          or else NT (N).Nkind = N_Indexed_Component
          or else NT (N).Nkind = N_Selected_Component
          or else NT (N).Nkind = N_Slice,
        Dcheck_Bad_Nkind (N));
      Set_Flag2 (N, Val);
   end Set_Do_Access_Check;

   procedure Set_Do_Accessibility_Check
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Parameter_Specification,
        Dcheck_Bad_Nkind (N));
      Set_Flag3 (N, Val);
   end Set_Do_Accessibility_Check;

   procedure Set_Do_Discriminant_Check
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Selected_Component,
        Dcheck_Bad_Nkind (N));
      Set_Flag3 (N, Val);
   end Set_Do_Discriminant_Check;

   procedure Set_Do_Division_Check
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Op_Divide
          or else NT (N).Nkind = N_Op_Mod
          or else NT (N).Nkind = N_Op_Rem,
        Dcheck_Bad_Nkind (N));
      Set_Flag3 (N, Val);
   end Set_Do_Division_Check;

   procedure Set_Do_Elaboration_Check
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Entry_Call_Statement
          or else NT (N).Nkind = N_Function_Call
          or else NT (N).Nkind = N_Procedure_Call_Statement,
        Dcheck_Bad_Nkind (N));
      Set_Flag2 (N, Val);
   end Set_Do_Elaboration_Check;

   procedure Set_Do_Length_Check
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Assignment_Statement
          or else NT (N).Nkind = N_Op_And
          or else NT (N).Nkind = N_Op_Or
          or else NT (N).Nkind = N_Op_Xor
          or else NT (N).Nkind = N_Type_Conversion,
        Dcheck_Bad_Nkind (N));
      Set_Flag4 (N, Val);
   end Set_Do_Length_Check;

   procedure Set_Do_Overflow_Check
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Op
          or else NT (N).Nkind = N_Type_Conversion,
        Dcheck_Bad_Nkind (N));
      Set_Flag2 (N, Val);
   end Set_Do_Overflow_Check;

   procedure Set_Do_Range_Check
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Subexpr,
        Dcheck_Bad_Nkind (N));
      Set_Flag9 (N, Val);
   end Set_Do_Range_Check;

   procedure Set_Do_Storage_Check
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Allocator
          or else NT (N).Nkind = N_Subprogram_Body,
        Dcheck_Bad_Nkind (N));
      Set_Flag2 (N, Val);
   end Set_Do_Storage_Check;

   procedure Set_Do_Tag_Check
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Assignment_Statement
          or else NT (N).Nkind = N_Function_Call
          or else NT (N).Nkind = N_Procedure_Call_Statement
          or else NT (N).Nkind = N_Return_Statement
          or else NT (N).Nkind = N_Type_Conversion,
        Dcheck_Bad_Nkind (N));
      Set_Flag3 (N, Val);
   end Set_Do_Tag_Check;

   procedure Set_Elaborate_Present
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_With_Clause,
        Dcheck_Bad_Nkind (N));
      Set_Flag4 (N, Val);
   end Set_Elaborate_Present;

   procedure Set_Elaborate_All_Present
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_With_Clause,
        Dcheck_Bad_Nkind (N));
      Set_Flag1 (N, Val);
   end Set_Elaborate_All_Present;

   procedure Set_Elaborate_Body_Present
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit,
        Dcheck_Bad_Nkind (N));
      Set_Flag7 (N, Val);
   end Set_Elaborate_Body_Present;

   procedure Set_Else_Statements
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Conditional_Entry_Call
          or else NT (N).Nkind = N_If_Statement
          or else NT (N).Nkind = N_Selective_Accept,
        Dcheck_Bad_Nkind (N));
      Set_List4_With_Parent (N, Val);
   end Set_Else_Statements;

   procedure Set_Elsif_Parts
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_If_Statement,
        Dcheck_Bad_Nkind (N));
      Set_List3_With_Parent (N, Val);
   end Set_Elsif_Parts;

   procedure Set_Enclosing_Variant
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Variant,
        Dcheck_Bad_Nkind (N));
      Set_Node2 (N, Val); -- semantic field, no parent pointer
   end Set_Enclosing_Variant;

   procedure Set_Entity
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Op
          or else NT (N).Nkind = N_Attribute_Reference
          or else NT (N).Nkind = N_Character_Literal
          or else NT (N).Nkind = N_Expanded_Name
          or else NT (N).Nkind = N_Freeze_Entity
          or else NT (N).Nkind = N_Identifier
          or else NT (N).Nkind = N_Operator_Symbol,
        Dcheck_Bad_Nkind (N));
      Set_Node4 (N, Val); -- Note: sem field, don't set parent
   end Set_Entity;

   procedure Set_Entry_Call_Alternative
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Conditional_Entry_Call
          or else NT (N).Nkind = N_Timed_Entry_Call,
        Dcheck_Bad_Nkind (N));
      Set_Node1_With_Parent (N, Val);
   end Set_Entry_Call_Alternative;

   procedure Set_Entry_Body_Formal_Part
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Entry_Body,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Entry_Body_Formal_Part;

   procedure Set_Entry_Call_Statement
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Entry_Call_Alternative,
        Dcheck_Bad_Nkind (N));
      Set_Node1_With_Parent (N, Val);
   end Set_Entry_Call_Statement;

   procedure Set_Entry_Index
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Accept_Statement,
        Dcheck_Bad_Nkind (N));
      Set_Node5_With_Parent (N, Val);
   end Set_Entry_Index;

   procedure Set_Entry_Index_Specification
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Entry_Body_Formal_Part,
        Dcheck_Bad_Nkind (N));
      Set_Node1_With_Parent (N, Val);
   end Set_Entry_Index_Specification;

   procedure Set_Error_Posted
      (N : Node_Id; Val : Boolean) is
   begin
      Set_Flag20 (N, Val);
   end Set_Error_Posted;

   procedure Set_Etype
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Has_Etype,
        Dcheck_Bad_Nkind (N));
      Set_Node5 (N, Val); -- Sem field, don't set parent
   end Set_Etype;

   procedure Set_Evaluate_Once
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Subexpr,
        Dcheck_Bad_Nkind (N));
      Set_Flag10 (N, Val);
   end Set_Evaluate_Once;

   procedure Set_Exception_Choices
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Exception_Handler,
        Dcheck_Bad_Nkind (N));
      Set_List4_With_Parent (N, Val);
   end Set_Exception_Choices;

   procedure Set_Exception_Handlers
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Handled_Sequence_Of_Statements,
        Dcheck_Bad_Nkind (N));
      Set_List4_With_Parent (N, Val);
   end Set_Exception_Handlers;

   procedure Set_Explicit_Generic_Actual_Parameter
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Generic_Association,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Explicit_Generic_Actual_Parameter;

   procedure Set_Expression
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Allocator
          or else NT (N).Nkind = N_Assignment_Statement
          or else NT (N).Nkind = N_At_Clause
          or else NT (N).Nkind = N_Attribute_Definition_Clause
          or else NT (N).Nkind = N_Attribute_Reference
          or else NT (N).Nkind = N_Case_Statement
          or else NT (N).Nkind = N_Code_Statement
          or else NT (N).Nkind = N_Component_Association
          or else NT (N).Nkind = N_Component_Declaration
          or else NT (N).Nkind = N_Delay_Relative_Statement
          or else NT (N).Nkind = N_Delay_Until_Statement
          or else NT (N).Nkind = N_Discriminant_Association
          or else NT (N).Nkind = N_Discriminant_Specification
          or else NT (N).Nkind = N_Expression_Actions
          or else NT (N).Nkind = N_Extension_Aggregate
          or else NT (N).Nkind = N_Formal_Object_Declaration
          or else NT (N).Nkind = N_Mod_Clause
          or else NT (N).Nkind = N_Modular_Type_Definition
          or else NT (N).Nkind = N_Number_Declaration
          or else NT (N).Nkind = N_Object_Declaration
          or else NT (N).Nkind = N_Parameter_Specification
          or else NT (N).Nkind = N_Parenthesized_Expression
          or else NT (N).Nkind = N_Pragma_Argument_Association
          or else NT (N).Nkind = N_Qualified_Expression
          or else NT (N).Nkind = N_Reference          
          or else NT (N).Nkind = N_Return_Statement
          or else NT (N).Nkind = N_Type_Conversion,
        Dcheck_Bad_Nkind (N));
      Set_Node3_With_Parent (N, Val);
   end Set_Expression;

   procedure Set_Expressions
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Aggregate
          or else NT (N).Nkind = N_Concat_Multiple
          or else NT (N).Nkind = N_Indexed_Component,
        Dcheck_Bad_Nkind (N));
      Set_List3_With_Parent (N, Val);
   end Set_Expressions;

   procedure Set_First_Bit
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_Clause,
        Dcheck_Bad_Nkind (N));
      Set_Node3_With_Parent (N, Val);
   end Set_First_Bit;

   procedure Set_First_Name
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_With_Clause,
        Dcheck_Bad_Nkind (N));
      Set_Flag5 (N, Val);
   end Set_First_Name;

   procedure Set_First_Named_Actual
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Function_Call
          or else NT (N).Nkind = N_Procedure_Call_Statement,
        Dcheck_Bad_Nkind (N));
      Set_Node4 (N, Val); -- Note: semantic node, parent pointer not set
   end Set_First_Named_Actual;

   procedure Set_Following_Pragmas
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit,
        Dcheck_Bad_Nkind (N));
      Set_List3_With_Parent (N, Val);
   end Set_Following_Pragmas;

   procedure Set_Formal_Type_Definition
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Type_Declaration,
        Dcheck_Bad_Nkind (N));
      Set_Node3_With_Parent (N, Val);
   end Set_Formal_Type_Definition;

   procedure Set_Generic_Associations
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Package_Declaration
          or else NT (N).Nkind = N_Function_Instantiation
          or else NT (N).Nkind = N_Package_Instantiation
          or else NT (N).Nkind = N_Procedure_Instantiation,
        Dcheck_Bad_Nkind (N));
      Set_List3_With_Parent (N, Val);
   end Set_Generic_Associations;

   procedure Set_Generic_Formal_Declarations
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Generic_Package_Declaration
          or else NT (N).Nkind = N_Generic_Subprogram_Declaration,
        Dcheck_Bad_Nkind (N));
      Set_List3_With_Parent (N, Val);
   end Set_Generic_Formal_Declarations;

   procedure Set_Generic_Parent
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Package_Declaration
          or else NT (N).Nkind = N_Function_Specification
          or else NT (N).Nkind = N_Package_Specification
          or else NT (N).Nkind = N_Procedure_Specification,
        Dcheck_Bad_Nkind (N));
      Set_Node5 (N, Val);
   end Set_Generic_Parent;

   procedure Set_Handled_Statement_Sequence
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Accept_Statement
          or else NT (N).Nkind = N_Block_Statement
          or else NT (N).Nkind = N_Entry_Body
          or else NT (N).Nkind = N_Package_Body
          or else NT (N).Nkind = N_Subprogram_Body
          or else NT (N).Nkind = N_Task_Body,
        Dcheck_Bad_Nkind (N));
      Set_Node4_With_Parent (N, Val);
   end Set_Handled_Statement_Sequence;

   procedure Set_Has_Created_Identifier
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Block_Statement
          or else NT (N).Nkind = N_Loop_Statement,
        Dcheck_Bad_Nkind (N));
      Set_Flag1 (N, Val);
   end Set_Has_Created_Identifier;

   procedure Set_Has_No_Side_Effects
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Subexpr,
        Dcheck_Bad_Nkind (N));
      Set_Flag8 (N, Val);
   end Set_Has_No_Side_Effects;

   procedure Set_Has_Priority_Pragma
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Protected_Definition
          or else NT (N).Nkind = N_Subprogram_Body
          or else NT (N).Nkind = N_Task_Definition,
        Dcheck_Bad_Nkind (N));
      Set_Flag6 (N, Val);
   end Set_Has_Priority_Pragma;

   procedure Set_Has_Task_Stack_Size_Pragma
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Task_Definition,
        Dcheck_Bad_Nkind (N));
      Set_Flag5 (N, Val);
   end Set_Has_Task_Stack_Size_Pragma;

   procedure Set_Has_Unknown_Discriminants
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Type_Declaration
          or else NT (N).Nkind = N_Incomplete_Type_Declaration
          or else NT (N).Nkind = N_Private_Extension_Declaration
          or else NT (N).Nkind = N_Private_Type_Declaration,
        Dcheck_Bad_Nkind (N));
      Set_Flag3 (N, Val);
   end Set_Has_Unknown_Discriminants;

   procedure Set_High_Bound
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Range
          or else NT (N).Nkind = N_Real_Range_Specification
          or else NT (N).Nkind = N_Signed_Integer_Type_Definition,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_High_Bound;

   procedure Set_Identifier
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_At_Clause
          or else NT (N).Nkind = N_Attribute_Definition_Clause
          or else NT (N).Nkind = N_Attribute_Reference
          or else NT (N).Nkind = N_Block_Statement
          or else NT (N).Nkind = N_Designator
          or else NT (N).Nkind = N_Enumeration_Representation_Clause
          or else NT (N).Nkind = N_Handled_Sequence_Of_Statements
          or else NT (N).Nkind = N_Label
          or else NT (N).Nkind = N_Loop_Statement
          or else NT (N).Nkind = N_Pragma
          or else NT (N).Nkind = N_Pragma_Argument_Association
          or else NT (N).Nkind = N_Record_Representation_Clause,
        Dcheck_Bad_Nkind (N));
      Set_Node1_With_Parent (N, Val);
   end Set_Identifier;

   procedure Set_Implicit_Types
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Record_Definition,
        Dcheck_Bad_Nkind (N));
      Set_List2_With_Parent (N, Val);
   end Set_Implicit_Types;

   procedure Set_Implicit_With
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_With_Clause,
        Dcheck_Bad_Nkind (N));
      Set_Flag2 (N, Val);
   end Set_Implicit_With;

   procedure Set_In_Present
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Object_Declaration
          or else NT (N).Nkind = N_Parameter_Specification,
        Dcheck_Bad_Nkind (N));
      Set_Flag1 (N, Val);
   end Set_In_Present;

   procedure Set_Intval
      (N : Node_Id; Val : Uint) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Integer_Literal,
        Dcheck_Bad_Nkind (N));
      Set_Uint3 (N, Val);
   end Set_Intval;

   procedure Set_Is_Evaluated
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Subexpr,
        Dcheck_Bad_Nkind (N));
      Set_Flag7 (N, Val);
   end Set_Is_Evaluated;

   procedure Set_Is_Overloaded
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Subexpr,
        Dcheck_Bad_Nkind (N));
      Set_Flag5 (N, Val);
   end Set_Is_Overloaded;

   procedure Set_Is_Static
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Subexpr,
        Dcheck_Bad_Nkind (N));
      Set_Flag6 (N, Val);
   end Set_Is_Static;

   procedure Set_Is_Task_Master
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Block_Statement
          or else NT (N).Nkind = N_Subprogram_Body
          or else NT (N).Nkind = N_Task_Body,
        Dcheck_Bad_Nkind (N));
      Set_Flag5 (N, Val);
   end Set_Is_Task_Master;

   procedure Set_Iteration_Scheme
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Loop_Statement,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Iteration_Scheme;

   procedure Set_Label
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Implicit_Label_Declaration,
        Dcheck_Bad_Nkind (N));
      Set_Node2 (N, Val);
   end Set_Label;

   procedure Set_Last_Bit
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_Clause,
        Dcheck_Bad_Nkind (N));
      Set_Node4_With_Parent (N, Val);
   end Set_Last_Bit;

   procedure Set_Last_Name
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_With_Clause,
        Dcheck_Bad_Nkind (N));
      Set_Flag6 (N, Val);
   end Set_Last_Name;

   procedure Set_Library_Unit
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit
          or else NT (N).Nkind = N_Package_Body_Stub
          or else NT (N).Nkind = N_Protected_Body_Stub
          or else NT (N).Nkind = N_Subprogram_Body_Stub
          or else NT (N).Nkind = N_Task_Body_Stub
          or else NT (N).Nkind = N_With_Clause,
        Dcheck_Bad_Nkind (N));
      Set_Node4 (N, Val); -- No parent, this is a library field
   end Set_Library_Unit;

   procedure Set_Left_Opnd
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Binary_Op,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Left_Opnd;

   procedure Set_Literals
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Enumeration_Type_Definition,
        Dcheck_Bad_Nkind (N));
      Set_List1_With_Parent (N, Val);
   end Set_Literals;

   procedure Set_Limited_Present
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Private_Type_Definition
          or else NT (N).Nkind = N_Private_Type_Declaration
          or else NT (N).Nkind = N_Record_Definition,
        Dcheck_Bad_Nkind (N));
      Set_Flag2 (N, Val);
   end Set_Limited_Present;

   procedure Set_Loop_Parameter_Specification
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Iteration_Scheme,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Loop_Parameter_Specification;

   procedure Set_Low_Bound
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Range
          or else NT (N).Nkind = N_Real_Range_Specification
          or else NT (N).Nkind = N_Signed_Integer_Type_Definition,
        Dcheck_Bad_Nkind (N));
      Set_Node1_With_Parent (N, Val);
   end Set_Low_Bound;

   procedure Set_Mod_Clause
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Record_Representation_Clause,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Mod_Clause;

   procedure Set_More_Ids
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_Declaration
          or else NT (N).Nkind = N_Discriminant_Specification
          or else NT (N).Nkind = N_Exception_Declaration
          or else NT (N).Nkind = N_Formal_Object_Declaration
          or else NT (N).Nkind = N_Number_Declaration
          or else NT (N).Nkind = N_Object_Declaration
          or else NT (N).Nkind = N_Parameter_Specification,
        Dcheck_Bad_Nkind (N));
      Set_Flag5 (N, Val);
   end Set_More_Ids;

   procedure Set_Name
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Assignment_Statement
          or else NT (N).Nkind = N_Attribute_Definition_Clause
          or else NT (N).Nkind = N_Defining_Program_Unit_Name
          or else NT (N).Nkind = N_Designator
          or else NT (N).Nkind = N_Entry_Call_Statement
          or else NT (N).Nkind = N_Exception_Renaming_Declaration
          or else NT (N).Nkind = N_Exit_Statement
          or else NT (N).Nkind = N_Formal_Package_Declaration
          or else NT (N).Nkind = N_Function_Call
          or else NT (N).Nkind = N_Function_Instantiation
          or else NT (N).Nkind = N_Generic_Function_Renaming_Declaration
          or else NT (N).Nkind = N_Generic_Package_Renaming_Declaration
          or else NT (N).Nkind = N_Generic_Procedure_Renaming_Declaration
          or else NT (N).Nkind = N_Goto_Statement
          or else NT (N).Nkind = N_Object_Renaming_Declaration
          or else NT (N).Nkind = N_Package_Instantiation
          or else NT (N).Nkind = N_Package_Renaming_Declaration
          or else NT (N).Nkind = N_Procedure_Call_Statement
          or else NT (N).Nkind = N_Procedure_Instantiation
          or else NT (N).Nkind = N_Raise_Statement
          or else NT (N).Nkind = N_Requeue_Statement
          or else NT (N).Nkind = N_Subprogram_Renaming_Declaration
          or else NT (N).Nkind = N_Subunit
          or else NT (N).Nkind = N_Variant_Part
          or else NT (N).Nkind = N_With_Clause,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Name;

   procedure Set_Names
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Abort_Statement
          or else NT (N).Nkind = N_Use_Package_Clause,
        Dcheck_Bad_Nkind (N));
      Set_List2_With_Parent (N, Val);
   end Set_Names;

   procedure Set_Next_Named_Actual
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Parameter_Association,
        Dcheck_Bad_Nkind (N));
      Set_Node4 (N, Val); -- Note: semantic node, no parent pointer needed
   end Set_Next_Named_Actual;

   procedure Set_Null_Present
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_List
          or else NT (N).Nkind = N_Record_Definition,
        Dcheck_Bad_Nkind (N));
      Set_Flag3 (N, Val);
   end Set_Null_Present;

   procedure Set_Null_Record_Present
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Aggregate
          or else NT (N).Nkind = N_Extension_Aggregate,
        Dcheck_Bad_Nkind (N));
      Set_Flag2 (N, Val);
   end Set_Null_Record_Present;

   procedure Set_Numerator
      (N : Node_Id; Val : Uint) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Real_Literal,
        Dcheck_Bad_Nkind (N));
      Set_Uint3 (N, Val);
   end Set_Numerator;

   procedure Set_Object_Definition
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Object_Declaration,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Object_Definition;

   procedure Set_Others_Discrete_Choices
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Others_Choice,
        Dcheck_Bad_Nkind (N));
      Set_List1_With_Parent (N, Val);
   end Set_Others_Discrete_Choices;

   procedure Set_Out_Present
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Object_Declaration
          or else NT (N).Nkind = N_Parameter_Specification,
        Dcheck_Bad_Nkind (N));
      Set_Flag2 (N, Val);
   end Set_Out_Present;

   procedure Set_Parameter_Associations
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Entry_Call_Statement
          or else NT (N).Nkind = N_Function_Call
          or else NT (N).Nkind = N_Procedure_Call_Statement,
        Dcheck_Bad_Nkind (N));
      Set_List3_With_Parent (N, Val);
   end Set_Parameter_Associations;

   procedure Set_Parameter_Specifications
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Accept_Statement
          or else NT (N).Nkind = N_Access_Function_Definition
          or else NT (N).Nkind = N_Access_Procedure_Definition
          or else NT (N).Nkind = N_Entry_Body_Formal_Part
          or else NT (N).Nkind = N_Entry_Declaration
          or else NT (N).Nkind = N_Function_Specification
          or else NT (N).Nkind = N_Procedure_Specification,
        Dcheck_Bad_Nkind (N));
      Set_List2_With_Parent (N, Val);
   end Set_Parameter_Specifications;

   procedure Set_Parameter_Type
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Parameter_Specification,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Parameter_Type;

   procedure Set_Parens
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
         or else NT (N).Nkind in N_Subexpr,
        Dcheck_Bad_Nkind (N));
      Set_Flag1 (N, Val);
   end Set_Parens;

   procedure Set_Parent_Spec
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Function_Instantiation
          or else NT (N).Nkind = N_Generic_Function_Renaming_Declaration
          or else NT (N).Nkind = N_Generic_Package_Declaration
          or else NT (N).Nkind = N_Generic_Package_Renaming_Declaration
          or else NT (N).Nkind = N_Generic_Procedure_Renaming_Declaration
          or else NT (N).Nkind = N_Generic_Subprogram_Declaration
          or else NT (N).Nkind = N_Package_Declaration
          or else NT (N).Nkind = N_Package_Instantiation
          or else NT (N).Nkind = N_Package_Renaming_Declaration
          or else NT (N).Nkind = N_Procedure_Instantiation
          or else NT (N).Nkind = N_Subprogram_Declaration
          or else NT (N).Nkind = N_Subprogram_Renaming_Declaration,
        Dcheck_Bad_Nkind (N));
      Set_Node4 (N, Val); -- semantic node, no parent pointer
   end Set_Parent_Spec;

   procedure Set_Position
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_Clause,
        Dcheck_Bad_Nkind (N));
      Set_Node2 (N, Val); -- semantic node, no parent pointer
   end Set_Position;

   procedure Set_Pragma_Argument_Associations
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Pragma,
        Dcheck_Bad_Nkind (N));
      Set_List2_With_Parent (N, Val);
   end Set_Pragma_Argument_Associations;

   procedure Set_Preelaborable
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit,
        Dcheck_Bad_Nkind (N));
      Set_Flag2 (N, Val);
   end Set_Preelaborable;

   procedure Set_Prefix
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Attribute_Reference
          or else NT (N).Nkind = N_Expanded_Name
          or else NT (N).Nkind = N_Explicit_Dereference
          or else NT (N).Nkind = N_Indexed_Component
          or else NT (N).Nkind = N_Selected_Component
          or else NT (N).Nkind = N_Slice,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Prefix;

   procedure Set_Prev_Ids
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_Declaration
          or else NT (N).Nkind = N_Discriminant_Specification
          or else NT (N).Nkind = N_Exception_Declaration
          or else NT (N).Nkind = N_Formal_Object_Declaration
          or else NT (N).Nkind = N_Number_Declaration
          or else NT (N).Nkind = N_Object_Declaration
          or else NT (N).Nkind = N_Parameter_Specification,
        Dcheck_Bad_Nkind (N));
      Set_Flag6 (N, Val);
   end Set_Prev_Ids;

   procedure Set_Private_Declarations
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Package_Specification
          or else NT (N).Nkind = N_Protected_Definition
          or else NT (N).Nkind = N_Task_Definition,
        Dcheck_Bad_Nkind (N));
      Set_List4_With_Parent (N, Val);
   end Set_Private_Declarations;

   procedure Set_Private_Present
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit
          or else NT (N).Nkind = N_Formal_Derived_Type_Definition,
        Dcheck_Bad_Nkind (N));
      Set_Flag1 (N, Val);
   end Set_Private_Present;

   procedure Set_Proper_Body
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Subunit,
        Dcheck_Bad_Nkind (N));
      Set_Node1_With_Parent (N, Val);
   end Set_Proper_Body;

   procedure Set_Protected_Definition
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Protected_Type_Declaration
          or else NT (N).Nkind = N_Single_Protected_Declaration,
        Dcheck_Bad_Nkind (N));
      Set_Node3_With_Parent (N, Val);
   end Set_Protected_Definition;

   procedure Set_Protected_Present
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Access_Function_Definition
          or else NT (N).Nkind = N_Access_Procedure_Definition,
        Dcheck_Bad_Nkind (N));
      Set_Flag1 (N, Val);
   end Set_Protected_Present;

   procedure Set_Range_Constraint
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Delta_Constraint
          or else NT (N).Nkind = N_Digits_Constraint,
        Dcheck_Bad_Nkind (N));
      Set_Node4_With_Parent (N, Val);
   end Set_Range_Constraint;

   procedure Set_Range_Expression
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Range_Constraint,
        Dcheck_Bad_Nkind (N));
      Set_Node4_With_Parent (N, Val);
   end Set_Range_Expression;

   procedure Set_Real_Range_Specification
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Decimal_Fixed_Point_Definition
          or else NT (N).Nkind = N_Floating_Point_Definition
          or else NT (N).Nkind = N_Ordinary_Fixed_Point_Definition,
        Dcheck_Bad_Nkind (N));
      Set_Node4_With_Parent (N, Val);
   end Set_Real_Range_Specification;

   procedure Set_Record_Extension_Part
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Derived_Type_Definition,
        Dcheck_Bad_Nkind (N));
      Set_Node3_With_Parent (N, Val);
   end Set_Record_Extension_Part;

   procedure Set_Redundant_Use
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Expanded_Name
          or else NT (N).Nkind = N_Identifier,
        Dcheck_Bad_Nkind (N));
      Set_Flag2 (N, Val);
   end Set_Redundant_Use;

   procedure Set_Reverse_Present
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Loop_Parameter_Specification,
        Dcheck_Bad_Nkind (N));
      Set_Flag1 (N, Val);
   end Set_Reverse_Present;

   procedure Set_Right_Opnd
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind in N_Op,
        Dcheck_Bad_Nkind (N));
      Set_Node3_With_Parent (N, Val);
   end Set_Right_Opnd;

   procedure Set_Selective_Accept_Alternatives
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Selective_Accept,
        Dcheck_Bad_Nkind (N));
      Set_List1_With_Parent (N, Val);
   end Set_Selective_Accept_Alternatives;

   procedure Set_Selector_Name
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Expanded_Name
          or else NT (N).Nkind = N_Generic_Association
          or else NT (N).Nkind = N_Parameter_Association
          or else NT (N).Nkind = N_Selected_Component,
        Dcheck_Bad_Nkind (N));
      Set_Node3_With_Parent (N, Val);
   end Set_Selector_Name;

   procedure Set_Selector_Names
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Discriminant_Association,
        Dcheck_Bad_Nkind (N));
      Set_List1_With_Parent (N, Val);
   end Set_Selector_Names;

   procedure Set_Specification
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Abstract_Subprogram_Declaration
          or else NT (N).Nkind = N_Formal_Subprogram_Declaration
          or else NT (N).Nkind = N_Generic_Package_Declaration
          or else NT (N).Nkind = N_Generic_Subprogram_Declaration
          or else NT (N).Nkind = N_Package_Declaration
          or else NT (N).Nkind = N_Subprogram_Body
          or else NT (N).Nkind = N_Subprogram_Body_Stub
          or else NT (N).Nkind = N_Subprogram_Declaration
          or else NT (N).Nkind = N_Subprogram_Renaming_Declaration,
        Dcheck_Bad_Nkind (N));
      Set_Node1_With_Parent (N, Val);
   end Set_Specification;

   procedure Set_Statements
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Abortable_Part
          or else NT (N).Nkind = N_Accept_Alternative
          or else NT (N).Nkind = N_Case_Statement_Alternative
          or else NT (N).Nkind = N_Delay_Alternative
          or else NT (N).Nkind = N_Entry_Call_Alternative
          or else NT (N).Nkind = N_Exception_Handler
          or else NT (N).Nkind = N_Handled_Sequence_Of_Statements
          or else NT (N).Nkind = N_Loop_Statement
          or else NT (N).Nkind = N_Triggering_Alternative,
        Dcheck_Bad_Nkind (N));
      Set_List3_With_Parent (N, Val);
   end Set_Statements;

   procedure Set_Strval
      (N : Node_Id; Val : String_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Operator_Symbol
          or else NT (N).Nkind = N_String_Literal,
        Dcheck_Bad_Nkind (N));
      Set_Str3 (N, Val);
   end Set_Strval;

   procedure Set_Subtype_Indication
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Access_To_Object_Definition
          or else NT (N).Nkind = N_Component_Declaration
          or else NT (N).Nkind = N_Constrained_Array_Definition
          or else NT (N).Nkind = N_Derived_Type_Definition
          or else NT (N).Nkind = N_Private_Extension_Declaration
          or else NT (N).Nkind = N_Subtype_Declaration
          or else NT (N).Nkind = N_Unconstrained_Array_Definition,
        Dcheck_Bad_Nkind (N));
      Set_Node4_With_Parent (N, Val);
   end Set_Subtype_Indication;

   procedure Set_Subtype_Mark
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Access_Definition
          or else NT (N).Nkind = N_Access_Function_Definition
          or else NT (N).Nkind = N_Formal_Derived_Type_Definition
          or else NT (N).Nkind = N_Formal_Object_Declaration
          or else NT (N).Nkind = N_Function_Specification
          or else NT (N).Nkind = N_Object_Renaming_Declaration
          or else NT (N).Nkind = N_Qualified_Expression
          or else NT (N).Nkind = N_Subtype_Indication
          or else NT (N).Nkind = N_Type_Conversion,
        Dcheck_Bad_Nkind (N));
      Set_Node4_With_Parent (N, Val);
   end Set_Subtype_Mark;

   procedure Set_Subtype_Marks
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Unconstrained_Array_Definition
          or else NT (N).Nkind = N_Use_Type_Clause,
        Dcheck_Bad_Nkind (N));
      Set_List2_With_Parent (N, Val);
   end Set_Subtype_Marks;

   procedure Set_Tagged_Present
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Formal_Private_Type_Definition
          or else NT (N).Nkind = N_Private_Type_Declaration
          or else NT (N).Nkind = N_Record_Definition,
        Dcheck_Bad_Nkind (N));
      Set_Flag1 (N, Val);
   end Set_Tagged_Present;

   procedure Set_Task_Definition
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Single_Task_Declaration
          or else NT (N).Nkind = N_Task_Type_Declaration,
        Dcheck_Bad_Nkind (N));
      Set_Node3_With_Parent (N, Val);
   end Set_Task_Definition;

   procedure Set_Then_Statements
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Elsif_Part
          or else NT (N).Nkind = N_If_Statement,
        Dcheck_Bad_Nkind (N));
      Set_List2_With_Parent (N, Val);
   end Set_Then_Statements;

   procedure Set_Triggering_Alternative
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Asynchronous_Select,
        Dcheck_Bad_Nkind (N));
      Set_Node1_With_Parent (N, Val);
   end Set_Triggering_Alternative;

   procedure Set_Triggering_Statement
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Triggering_Alternative,
        Dcheck_Bad_Nkind (N));
      Set_Node1_With_Parent (N, Val);
   end Set_Triggering_Statement;

   procedure Set_Type_Definition
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Full_Type_Declaration,
        Dcheck_Bad_Nkind (N));
      Set_Node3_With_Parent (N, Val);
   end Set_Type_Definition;

   procedure Set_Unchecked_Conversion
      (N : Node_Id; Val : Boolean) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Type_Conversion,
        Dcheck_Bad_Nkind (N));
      Set_Flag11 (N, Val);
   end Set_Unchecked_Conversion;

   procedure Set_Unit
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit,
        Dcheck_Bad_Nkind (N));
      Set_Node2_With_Parent (N, Val);
   end Set_Unit;

   procedure Set_Variant_Part
      (N : Node_Id; Val : Node_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Component_List,
        Dcheck_Bad_Nkind (N));
      Set_Node4_With_Parent (N, Val);
   end Set_Variant_Part;

   procedure Set_Variants
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Variant_Part,
        Dcheck_Bad_Nkind (N));
      Set_List1_With_Parent (N, Val);
   end Set_Variants;

   procedure Set_Visible_Declarations
      (N : Node_Id; Val : List_Id) is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Package_Specification
          or else NT (N).Nkind = N_Protected_Definition
          or else NT (N).Nkind = N_Task_Definition,
        Dcheck_Bad_Nkind (N));
      Set_List2_With_Parent (N, Val);
   end Set_Visible_Declarations;

end Sinfo;
