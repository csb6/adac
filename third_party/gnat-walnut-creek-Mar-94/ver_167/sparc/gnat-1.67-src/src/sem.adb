------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  S E M                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.160 $                            --
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

with Atree;     use Atree;
with Comperr;   use Comperr;
with Debug;     use Debug;
with Debug_Sem; use Debug_Sem;
with Errout;    use Errout;
with Expand;   
with Exp_Util;  use Exp_Util;
with Lib;       use Lib;
with Output;    use Output;
with Sem_Attr;  use Sem_Attr;
with Sem_Ch3;   use Sem_Ch3;
with Sem_Ch4;   use Sem_Ch4;
with Sem_Ch5;   use Sem_Ch5;
with Sem_Ch6;   use Sem_Ch6;
with Sem_Ch7;   use Sem_Ch7;
with Sem_Ch8;   use Sem_Ch8;
with Sem_Ch9;   use Sem_Ch9;
with Sem_Ch10;  use Sem_Ch10;
with Sem_Ch11;  use Sem_Ch11;
with Sem_Ch12;  use Sem_Ch12;
with Sem_Ch13;  use Sem_Ch13;
with Sem_Prag;  use Sem_Prag;
with Sem_Util;  use Sem_Util;
with Sinfo;     use Sinfo;
with Sinput;    use Sinput;
with Stand;     use Stand;

package body Sem is

   ----------------
   -- Local Data --
   ----------------

   --  Implicit_Types is a stack which holds the list headers corresponding
   --  to lists of Implicit_Type_List. Since the Semantics procedure is called
   --  recursively, it is necessary to save the current Implicit_Type_List.
   --  Semantics saves the Implicit_Type_List on the top of this stack and
   --  initializes a new Implicit_Type_List to be an empty list. Before
   --  returning it restores Implicit_Type_List and and pops the stack.

   package Implicit_Types is new Table (
      Component_Type => List_Id,
      Index_Type     => Int,
      Low_Bound      => 0,
      Initial        => 50,
      Increment      => 100,
      Table_Name     => "Sem.Implicit_Types");

   use Implicit_Types;

   ---------------
   -- Semantics --
   ---------------

   procedure Semantics (Cunit : Node_Id) is
      Save_Ada_83_Mode : constant Boolean := Ada_83;
      --  Saves state of Ada 83 mode switch for restore on exit (since it may
      --  have been reset by occurrences of the Ada_83 or Ada_9X pragmas)

      Save_Transient_Scopes_Present : Boolean := Transient_Scopes_Present;
      --  A flag used to save the current setting of the flag that shows
      --  whether transient scopes are present. This setting is restored
      --  on exit (allows for proper recursive entry to Semantics)

   begin
      Compiler_State := Analyzing;
      Transient_Scopes_Present := False;

      Expander_Mode_Save_And_Set
        (Operating_Mode = Generate_Code or Debug_Flag_X);

      --  Only do analysis of unit that has not already been analyzed

      if not Analyzed (Cunit) then

         --  Initialize Ada 83 and Ada 9X mode from compiler switches

         Ada_83 := Ada_83_Switch;
         Ada_9X := not Ada_83;

         --  Push a scope stack entry for Standard on the bottom of the stack.
         --  Note that we don't need to remove this on exit, since the scope
         --  stack is local to Sem in any case and disappears on exit.

         Implicit_Types.Increment_Last;
         Implicit_Types.Table (Implicit_Types.Last) := Implicit_Type_List;
         Implicit_Type_List := New_List;
         New_Scope (Standard_Standard);
         Scope_Suppress := Suppress_Options;

         --  Now analyze the top level compilation unit node

         Analyze (Cunit);

         if Current_Scope /= Standard_Standard 
           and then Cunit /= File.Table (Main_Unit).Cunit 
         then
            Compiler_Error;
            Write_Str ("scope mismatch on exit from compilation");
            Write_Eol;
         end if;
         Pop_Scope; -- pop entry for Standard

         Implicit_Type_List := Implicit_Types.Table (Implicit_Types.Last);
         Implicit_Types.Decrement_Last;
      end if;

      --  Restore settings of Ada 83 and 9X mode switches to entry values
      --  and also the setting of the transient scopes flag.

      Ada_83 := Save_Ada_83_Mode;
      Ada_9X := not Ada_83;
      Expander_Mode_Restore;
      Transient_Scopes_Present := Save_Transient_Scopes_Present;

   exception

      --  The exception Subunit_Found is raised when we are done prematurely
      --  with the unit because it turned out that we were analyzing it only
      --  for the benefit of a subunit, and the rest of our unit was
      --  irrelevant. The action in this case is simply to return. Nothing
      --  else needs to be done.

      when Subunit_Found =>
         Ada_83 := Save_Ada_83_Mode;
         Ada_9X := not Ada_83;
         Expander_Mode_Restore;
         Transient_Scopes_Present := Save_Transient_Scopes_Present;
         return;
   end Semantics;

   -------------
   -- Analyze --
   -------------

   procedure Analyze (N : Node_Id) is
   begin
      pragma Debug (Debug_A_Entry ("analyzing  ", N));

      --  Immediate return if already analyzed

      if Analyzed (N) then
         pragma Debug
           (Debug_A_Exit ("analyzing  ", N, "  (done, analyzed already)"));
         return;
      end if;

      --  Otherwise processing depends on the node kind. Note: some entries
      --  in this table generate a Compiler_Abort. This means that the node
      --  is always analyzed within the semantic chapter routines, and there
      --  should never be a case of making a call to the main Analyze routine
      --  for the particular node kind. For example, an N_Access_Definition
      --  node appears only in the context of a type declaration, and is
      --  processed by the analyze routine for type declarations.

      case Nkind (N) is

      when N_Abortable_Part                => Analyze_Abortable_Part (N);

      when N_Abort_Statement               => Analyze_Abort_Statement (N);

      when N_Abstract_Subprogram_Declaration =>
         Analyze_Abstract_Subprogram_Declaration (N);

      when N_Accept_Alternative            => Analyze_Accept_Alternative (N);

      when N_Accept_Statement              => Analyze_Accept_Statement (N);

      when N_Access_Definition             => Compiler_Abort;

      when N_Access_Function_Definition    => Compiler_Abort;

      when N_Access_Procedure_Definition   => Compiler_Abort;

      when N_Access_To_Object_Definition   => Compiler_Abort;

      when N_Aggregate                     => Analyze_Aggregate (N);

      when N_Allocator                     => Analyze_Allocator (N);

      when N_Assignment_Statement          => Analyze_Assignment (N);

      when N_Asynchronous_Select           => Analyze_Asynchronous_Select (N);

      when N_At_Clause                     => Analyze_At_Clause (N);

      when N_Attribute_Reference           => Analyze_Attribute (N);

      when N_Attribute_Definition_Clause   =>
         Analyze_Attribute_Definition_Clause (N);

      when N_Block_Statement               => Analyze_Block_Statement (N);

      when N_Case_Statement                => Analyze_Case_Statement (N);

      when N_Case_Statement_Alternative    => Compiler_Abort;

      when N_Character_Literal             => Set_Etype (N, Any_Character);

      when N_Code_Statement                => Analyze_Code_Statement (N);

      when N_Compilation_Unit              => Analyze_Compilation_Unit (N);

      when N_Component_Association         => Compiler_Abort;

      when N_Component_Clause              => Analyze_Component_Clause (N);

      when N_Component_Declaration         =>
         Analyze_Component_Declaration (N);

      when N_Component_List                => Compiler_Abort;

      when N_Concat_Multiple               => Compiler_Abort;

      when N_Conditional_Entry_Call        =>
         Analyze_Conditional_Entry_Call (N);

      when N_Constrained_Array_Definition  => Compiler_Abort;

      when N_Decimal_Fixed_Point_Definition => Compiler_Abort;

      when N_Defining_Character_Literal    => Compiler_Abort;

      when N_Defining_Identifier           => Compiler_Abort;

      when N_Defining_Operator_Symbol      => Compiler_Abort;

      when N_Defining_Program_Unit_Name    => Compiler_Abort;

      when N_Delay_Alternative             => Analyze_Delay_Alternative (N);

      when N_Delay_Relative_Statement      => Analyze_Delay_Relative (N);

      when N_Delay_Until_Statement         => Analyze_Delay_Until (N);

      when N_Delta_Constraint              => Compiler_Abort;

      when N_Derived_Type_Definition       => Compiler_Abort;

      when N_Designator                    => Compiler_Abort;

      when N_Digits_Constraint             => Compiler_Abort;

      when N_Discriminant_Association      => Compiler_Abort;

      when N_Discriminant_Specification    => Compiler_Abort;

      when N_Elsif_Part                    => Compiler_Abort;

      when N_Empty                         => Compiler_Abort;

      when N_Entry_Body                    => Analyze_Entry_Body (N);

      when N_Entry_Body_Formal_Part        => 
         Analyze_Entry_Body_Formal_Part (N);

      when N_Entry_Call_Alternative        =>
         Analyze_Entry_Call_Alternative (N);

      when N_Entry_Call_Statement          => Analyze_Entry_Call (N);

      when N_Entry_Declaration             => Analyze_Entry_Declaration (N);

      when N_Entry_Index_Specification     =>
         Analyze_Entry_Index_Specification (N);

      when N_Enumeration_Representation_Clause =>
         Analyze_Enumeration_Representation_Clause (N);

      when N_Enumeration_Type_Definition   => Compiler_Abort;

      when N_Error                         => Compiler_Abort;

      when N_Exception_Declaration         =>
         Analyze_Exception_Declaration (N);

      when N_Exception_Handler             => Compiler_Abort;

      when N_Exception_Renaming_Declaration => Analyze_Exception_Renaming (N);

      when N_Exit_Statement                => Analyze_Exit_Statement (N);

      when N_Expanded_Name                 => null;

      when N_Explicit_Dereference          => Analyze_Explicit_Dereference (N);

      when N_Expression_Actions            => Analyze_Expression_Actions (N);

      when N_Extension_Aggregate           =>
         Unimplemented (N, "extension aggregates");

      when N_Floating_Point_Definition     => Compiler_Abort;

      when N_Formal_Decimal_Fixed_Point_Definition => Compiler_Abort;

      when N_Formal_Derived_Type_Definition => Compiler_Abort;

      when N_Formal_Discrete_Type_Definition => Compiler_Abort;

      when N_Formal_Floating_Point_Definition => Compiler_Abort;

      when N_Formal_Modular_Type_Definition => Compiler_Abort;

      when N_Formal_Object_Declaration     =>
         Analyze_Formal_Object_Declaration (N);

      when N_Formal_Ordinary_Fixed_Point_Definition => Compiler_Abort;

      when N_Formal_Package_Declaration    => Analyze_Formal_Package (N);

      when N_Formal_Private_Type_Definition => Compiler_Abort;

      when N_Formal_Signed_Integer_Type_Definition => Compiler_Abort;

      when N_Formal_Subprogram_Declaration => Analyze_Formal_Subprogram (N);

      when N_Formal_Type_Declaration       =>
         Analyze_Formal_Type_Declaration (N);

      when N_Freeze_Entity                 => null;

      when N_Full_Type_Declaration         => Analyze_Type_Declaration (N);

      when N_Function_Call                 => Analyze_Function_Call (N);

      when N_Function_Instantiation        =>
         Analyze_Function_Instantiation (N);

      when N_Function_Specification        => Compiler_Abort;

      when N_Generic_Association           => Analyze_Generic_Association (N);

      when N_Generic_Function_Renaming_Declaration =>
         Unimplemented (N, "generic renaming declarations");

      when N_Generic_Package_Declaration   =>
         Analyze_Generic_Package_Declaration (N);

      when N_Generic_Package_Renaming_Declaration =>
         Unimplemented (N, "generic renaming declarations");

      when N_Generic_Procedure_Renaming_Declaration =>
         Unimplemented (N, "generic renaming declarations");

      when N_Generic_Subprogram_Declaration =>
         Analyze_Generic_Subprogram_Declaration (N);

      when N_Goto_Statement                => Analyze_Goto_Statement (N);

      when N_Handled_Sequence_Of_Statements => Analyze_Handled_Statements (N);

      when N_Identifier                    => Find_Simple_Name (N);

      when N_Implicit_Type                 => Analyze_Implicit_Type (N);

      when N_If_Statement                  => Analyze_If_Statement (N);

      when N_Implicit_Label_Declaration    =>
         Analyze_Implicit_Label_Declaration (N);

      when N_Incomplete_Type_Declaration   => Analyze_Incomplete_Type_Decl (N);

      when N_Indexed_Component             => Find_Name (N);

      when N_Index_Or_Discriminant_Constraint => Compiler_Abort;

      when N_Integer_Literal               => Set_Etype (N, Universal_Integer);

      when N_Iteration_Scheme              => Compiler_Abort;

      when N_Label                         => Compiler_Abort;

      when N_Loop_Parameter_Specification  => Compiler_Abort;

      when N_Loop_Statement                => Analyze_Loop_Statement (N);

      when N_Mod_Clause                    => Analyze_Mod_Clause (N);

      when N_Modular_Type_Definition       => Compiler_Abort;

      when N_Null                          => Analyze_Null (N);

      when N_Null_Statement                => null;

      when N_Number_Declaration            => Analyze_Number_Declaration (N);

      when N_Object_Declaration            => Analyze_Object_Declaration (N);

      when N_Object_Renaming_Declaration   => Analyze_Object_Renaming (N);

      when N_Operator_Symbol               => Analyze_Operator_Symbol (N);

      when N_Op_Abs                        => Analyze_Unary_Op (N);

      when N_Op_Add                        => Analyze_Arithmetic_Op (N);

      when N_Op_And                        => Analyze_Logical_Op (N);

      when N_Op_And_Then                   => Analyze_Short_Circuit (N);

      when N_Op_Concat                     => Analyze_Concatenation (N);

      when N_Op_Divide                     => Analyze_Arithmetic_Op (N);

      when N_Op_Eq                         => Analyze_Equality_Op (N);

      when N_Op_Expon                      => Analyze_Arithmetic_Op (N);

      when N_Op_Ge                         => Analyze_Comparison_Op (N);

      when N_Op_Gt                         => Analyze_Comparison_Op (N);

      when N_Op_In                         => Analyze_Membership_Op (N);

      when N_Op_Le                         => Analyze_Comparison_Op (N);

      when N_Op_Lt                         => Analyze_Comparison_Op (N);

      when N_Op_Minus                      => Analyze_Unary_Op (N);

      when N_Op_Mod                        => Analyze_Arithmetic_Op (N);

      when N_Op_Multiply                   => Analyze_Arithmetic_Op (N);

      when N_Op_Ne                         => Analyze_Equality_Op (N);

      when N_Op_Not                        => Analyze_Negation (N);

      when N_Op_Not_In                     => Analyze_Membership_Op (N);

      when N_Op_Or                         => Analyze_Logical_Op (N);

      when N_Op_Or_Else                    => Analyze_Short_Circuit (N);

      when N_Op_Plus                       => Analyze_Unary_Op (N);

      when N_Op_Rem                        => Analyze_Arithmetic_Op (N);

      when N_Op_Subtract                   => Analyze_Arithmetic_Op (N);

      when N_Op_Xor                        => Analyze_Logical_Op (N);

      when N_Ordinary_Fixed_Point_Definition => Compiler_Abort;

      when N_Others_Choice                 => null;

      when N_Package_Body                  => Analyze_Package_Body (N);

      when N_Package_Body_Stub             => Analyze_Package_Body_Stub (N);

      when N_Package_Declaration           => Analyze_Package_Declaration (N);

      when N_Package_Instantiation         =>
         Analyze_Package_Instantiation (N);

      when N_Package_Renaming_Declaration  => Analyze_Package_Renaming (N);

      when N_Package_Specification         =>
         Analyze_Package_Specification (N);

      when N_Parameter_Association         => Analyze (Actual_Parameter (N));

      when N_Parameter_Specification       => Compiler_Abort;

      when N_Parenthesized_Expression =>
         Analyze_N_Parenthesized_Expression (N);

      when N_Pragma                        => Analyze_Pragma (N);

      when N_Pragma_Argument_Association   => Compiler_Abort;

      when N_Private_Extension_Declaration =>
         Analyze_Private_Extension_Declaration (N);

      when N_Private_Type_Declaration      =>
         Analyze_Private_Type_Declaration (N);

      when N_Procedure_Call_Statement      => Analyze_Procedure_Call (N);

      when N_Procedure_Specification       => Compiler_Abort;

      when N_Procedure_Instantiation       =>
         Analyze_Procedure_Instantiation (N);

      when N_Protected_Body                => Analyze_Protected_Body (N);

      when N_Protected_Body_Stub           => Compiler_Abort;

      when N_Protected_Definition          => Analyze_Protected_Definition (N);

      when N_Protected_Type_Declaration    => Analyze_Protected_Type (N);

      when N_Qualified_Expression          => Analyze_Qualified_Expression (N);

      when N_Raise_Statement               => Analyze_Raise_Statement (N);

      when N_Range                         => Analyze_Range (N);

      when N_Range_Constraint              =>
         Analyze_Range (Range_Expression (N));

      when N_Real_Literal                  => Set_Etype (N, Universal_Real);

      when N_Real_Range_Specification      => Compiler_Abort;

      when N_Record_Definition             => Compiler_Abort;

      when N_Record_Representation_Clause  =>
         Analyze_Record_Representation_Clause (N);

      when N_Reference                     => Analyze_Reference (N);

      when N_Requeue_Statement             => Analyze_Requeue (N);

      when N_Return_Statement              => Analyze_Return_Statement (N);

      when N_Selected_Component            => Find_Selected_Component (N);

      when N_Selective_Accept              => Analyze_Selective_Accept (N);

      when N_Signed_Integer_Type_Definition
                                           => Compiler_Abort;

      when N_Single_Protected_Declaration  => Analyze_Single_Protected (N);

      when N_Single_Task_Declaration       => Analyze_Single_Task (N);

      when N_Slice                         => Find_Name (N);

      when N_String_Literal                => Set_Etype (N, Any_String);

      when N_Subprogram_Body               => Analyze_Subprogram_Body (N);

      when N_Subprogram_Body_Stub          => Analyze_Subprogram_Body_Stub (N);

      when N_Subprogram_Declaration        =>
         Analyze_Subprogram_Declaration (N);

      when N_Subprogram_Renaming_Declaration
                                           => Analyze_Subprogram_Renaming (N);

      when N_Subtype_Declaration           => Analyze_Subtype_Declaration (N);

      when N_Subtype_Indication            => Analyze_Subtype_Indication (N);

      when N_Subunit                       => Analyze_Subunit (N);

      when N_Task_Body                     => Analyze_Task_Body (N);

      when N_Task_Body_Stub                => Analyze_Concurrent_Body_Stub (N);

      when N_Task_Definition               => Analyze_Task_Definition (N);

      when N_Task_Type_Declaration         => Analyze_Task_Type (N);

      when N_Terminate_Alternative         =>
         Analyze_Terminate_Alternative (N);

      when N_Timed_Entry_Call              => Analyze_Timed_Entry_Call (N);

      when N_Triggering_Alternative        => Analyze_Asynchronous_Select (N);

      when N_Type_Conversion               => Analyze_Conversion (N);

      when N_Unconstrained_Array_Definition => Compiler_Abort;

      when N_Unused_At_Start               => Compiler_Abort;

      when N_Unused_At_End                 => Compiler_Abort;

      when N_Use_Package_Clause            => Analyze_Use_Package (N);

      when N_Use_Type_Clause               => Analyze_Use_Type (N);

      when N_Variant                       => Compiler_Abort;

      when N_Variant_Part                  => Analyze_Variant_Part (N);

      when N_With_Clause                   => Analyze_With_Clause (N);

      end case;

      pragma Debug (Debug_A_Exit ("analyzing  ", N, "  (done)"));

      --  Now that we have analyzed the node, we call the expander to
      --  perform possible expansion. This is done only for nodes that
      --  are not subexpressions, because in the case of subexpressions,
      --  we don't have the type yet, and the expander will need to know
      --  the type before it can do its job. For subexpression nodes, the
      --  call to the expander happens in the Resolve routine in chapter 4.

      --  The Analyzed flag is also set at this point for non-subexpression
      --  nodes (in the case of subexpression nodes, we can't set the flag
      --  yet, since resolution and expansion have not yet been completed)

      if Nkind (N) not in N_Subexpr then
         Expand (N);
         Set_Analyzed (N, True);
      end if;

   end Analyze;

end Sem;
