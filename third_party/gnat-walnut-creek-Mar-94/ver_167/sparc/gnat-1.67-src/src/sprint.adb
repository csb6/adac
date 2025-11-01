------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S P R I N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.76 $                             --
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
with Csets;   use Csets;
with Debug;   use Debug;
with Einfo;   use Einfo;
with Lib;     use Lib;
with Namet;   use Namet;
with Opt;     use Opt;
with Output;  use Output;
with Sinfo;   use Sinfo;
with Stand;   use Stand;
with Stringt; use Stringt;
with Uintp;   use Uintp;
with Uname;   use Uname;

package body Sprint is

   Indent : Int := 0;
   --  Number of columns for current line output indentation

   Indent_Annull_Flag : Boolean := False;
   --  Set True if subsequent Write_Indent call to be ignored, gets reset
   --  by this call, so it is only active to suppress a single indent call.

   -----------------------
   --  Local Procedures --
   -----------------------

   procedure Indent_Annull;
   --  Causes following call to Write_Indent to be ignored. This is used when
   --  a higher level node wants to stop a lower level node from starting a
   --  new line, when it would otherwise be inclined to do so (e.g. the case
   --  of an accept statement called from an accept alternative with a guard)

   procedure Indent_Begin;
   --  Increase indentation level

   procedure Indent_End;
   --  Decrease indentation level

   procedure Sprint_Node_Actual (Node : Node_Id);
   --  This routine prints its node argument. It is a lower level routine than
   --  Sprint_Node, in that it does not bother about rewritten trees.

   procedure Write_Ekind (E : Entity_Id);
   --  Write the String corresponding to the Ekind without "E_".

   procedure Write_Id (Id : Node_Id);
   --  Write identifier name

   function Write_Identifiers (Node : Node_Id) return Boolean;
   --  Handle node where the grammar has a list of defining identifiers, but
   --  the tree has a separate declaration for each identifier. Handles the
   --  printing of the defining identifier, and returns True if the type and
   --  initialization information is to be printed, False if it is to be
   --  skipped (the latter case happens when printing defining identifiers
   --  other than the first in the original tree output case).

   procedure Write_Implicit_Def (E : Entity_Id);
   --  Write the definition of the implicit type E according to its Ekind

   procedure Write_Indent;
   --  Start a new line and write indent spacing

   function Write_Indent_Identifiers (Node : Node_Id) return Boolean;
   --  Like Write_Identifiers except that each new printed declaration
   --  is at the start of a new line.

   procedure Write_Indent_String (S : String);
   --  Start a new line and write indent spacing followed by given string

   procedure Write_Param_Specs (N : Node_Id);
   --  Output parameter specifications for node (which is either a function
   --  or procedure specification with a Parameter_Specifications field

   procedure Write_Real_Value (Node : Node_Id);
   --  Used to output real value from N_Real_Literal or N_Real_Value node

   procedure Write_Rewrite_String (S : String);
   --  Writes out a string (typically containing { or }) for a node created
   --  by rewriting the tree. Suppressed if Debug_Flag_G is set, since in this
   --  case we don't specially mark nodes created by rewriting).

   procedure Write_Uint (U : Uint);
   --  Used to output a Uint value

   -------------------
   -- Indent_Annull --
   -------------------

   procedure Indent_Annull is
   begin
      Indent_Annull_Flag := True;
   end Indent_Annull;

   ------------------
   -- Indent_Begin --
   ------------------

   procedure Indent_Begin is
   begin
      Indent := Indent + 3;
   end Indent_Begin;

   ----------------
   -- Indent_End --
   ----------------

   procedure Indent_End is
   begin
      Indent := Indent - 3;
   end Indent_End;

   -----------------
   -- Source_Dump --
   -----------------

   procedure Source_Dump (S : Char) is
      Max_Unit : Unit_Number_Type;

      procedure Underline;
      --  Put underline under string we just printed

      procedure Underline is
         Col : constant Int := Column;

      begin
         Write_Eol;

         while Col > Column loop
            Write_Char ('-');
         end loop;

         Write_Eol;
      end Underline;

   --  Start of processing for Source_Dump

   begin
      if Debug_Flag_Z
        and then (S = 'S' or else Operating_Mode = Check_Syntax)
      then
         Write_Eol;
         Write_String ("Source recreated from tree of Standard (spec)");

         if S = 'P' then
            Write_Str (" after parsing");
         else
            Write_Str (" after semantics");
         end if;

         Underline;
         Sprint_Node (Standard_Package_Node);
         Write_Eol;
      end if;

      if (Debug_Flag_S and then
            (S = 'S' or else Operating_Mode = Check_Syntax))
        or else
         (Debug_Flag_W and then
            (S = 'P' and then Operating_Mode /= Check_Syntax))
      then
         if Debug_Flag_F then
            Max_Unit := File.Last;
         else
            Max_Unit := Main_Unit;
         end if;

         for U in Main_Unit .. Max_Unit loop
            if File.Table (U).Source /= null then
               Write_Eol;
               Write_String ("Source recreated from tree for ");
               Write_Unit_Name (File.Table (U).Unit_Name);

               if S = 'P' then
                  Write_Str (" after parsing ");
               else
                  Write_Str (" after semantics ");
               end if;

               Underline;
               Sprint_Node (File.Table (U).Cunit);
               Write_Eol;
            end if;
         end loop;
      end if;
   end Source_Dump;

   ---------------------
   -- Sprint_Bar_List --
   ---------------------

   procedure Sprint_Bar_List (List : List_Id) is
      Node : Node_Id;

   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);

         loop
            Sprint_Node (Node);
            Node := Next (Node);
            exit when Node = Empty;
            Write_String (" | ");

            if Column > 70 then
               Write_Indent_String (" ");
            end if;
         end loop;
      end if;
   end Sprint_Bar_List;

   -----------------------
   -- Sprint_Comma_List --
   -----------------------

   procedure Sprint_Comma_List (List : List_Id) is
      Node : Node_Id;

   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);

         loop
            Sprint_Node (Node);
            Node := Next (Node);
            exit when Node = Empty;
            Write_String (", ");

            if Column > 70 then
               Write_Indent_String (" ");
            end if;
         end loop;
      end if;
   end Sprint_Comma_List;

   --------------------------
   -- Sprint_Indented_List --
   --------------------------

   procedure Sprint_Indented_List (List : List_Id) is
   begin
      Indent_Begin;
      Sprint_Node_List (List);
      Indent_End;
   end Sprint_Indented_List;

   -----------------
   -- Sprint_Node --
   -----------------

   procedure Sprint_Node (Node : Node_Id) is
   begin
      if Is_Rewrite_Insertion (Node) then
         if not Debug_Flag_O then
            Write_Rewrite_String ("{");
            Sprint_Node_Actual (Node);
            Write_Rewrite_String ("}");
         end if;

      elsif Is_Rewrite_Substitution (Node) then
         if Debug_Flag_G then
            Sprint_Node_Actual (Node);
         elsif Debug_Flag_O then
            Sprint_Node_Actual (Original_Node (Node));
         else
            Sprint_Node_Actual (Original_Node (Node));
            Write_Rewrite_String ("{");
            Sprint_Node_Actual (Node);
            Write_Rewrite_String ("}");
         end if;

      else
         Sprint_Node_Actual (Node);
      end if;
   end Sprint_Node;

   ------------------------
   -- Sprint_Node_Actual --
   ------------------------

   procedure Sprint_Node_Actual (Node : Node_Id) is

      Paren_Flag : Boolean := False;
      --  Set true if closing parenthesis needed at end

      procedure Do_Parens;
      --  Used for nodes that have a Parens field. If the Parens field is set
      --  True, then a left paren is output at the time of call, and Paren_Flag
      --  is set True, which will cause a right paren to be output on exit from
      --  the Print_Node procedure.

      procedure Do_Parens is
      begin
         if Parens (Node) then
            Write_Char ('(');
            Paren_Flag := True;
         end if;
      end Do_Parens;

   begin
      if Node = Empty then
         return;
      end if;

      case Nkind (Node) is

         when N_Abort_Statement =>
            Write_Indent_String ("abort ");
            Sprint_Comma_List (Names (Node));
            Write_Char (';');

         when N_Abortable_Part =>
            Sprint_Indented_List (Statements (Node));

         when N_Abstract_Subprogram_Declaration =>
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_String (" is abstract;");

         when N_Accept_Alternative =>
            if Present (Condition (Node)) then
               Write_Indent;
               Write_String ("when ");
               Sprint_Node (Condition (Node));
               Write_String (" => ");
               Indent_Annull;
            end if;

            Sprint_Node (Accept_Statement (Node));
            Sprint_Node_List (Statements (Node));

         when N_Accept_Statement =>
            Write_Indent_String ("accept ");
            Write_Id (Accept_Name (Node));

            if Present (Entry_Index (Node)) then
               Write_String (" (");
               Sprint_Node (Entry_Index (Node));
               Write_String (")");
            end if;

            Write_Param_Specs (Node);

            if Present (Handled_Statement_Sequence (Node)) then
               Write_String (" do");
               Sprint_Node (Handled_Statement_Sequence (Node));
               Write_Indent_String ("end ");
               Write_Id (Accept_Name (Node));
            end if;

            Write_Char (';');

         when N_Access_Definition =>
            Write_String ("access ");
            Sprint_Node (Subtype_Mark (Node));

         when N_Access_Function_Definition =>
            Write_String ("access ");

            if Protected_Present (Node) then
               Write_String ("protected ");
            end if;

            Write_String ("function");
            Write_Param_Specs (Node);
            Write_String (" return ");
            Sprint_Node (Subtype_Mark (Node));

         when N_Access_Procedure_Definition =>
            Write_String ("access ");

            if Protected_Present (Node) then
               Write_String ("protected ");
            end if;

            Write_String ("procedure");
            Write_Param_Specs (Node);

         when N_Access_To_Object_Definition =>
            Write_String ("access ");

            if All_Present (Node) then
               Write_String ("all ");
            elsif Constant_Present (Node) then
               Write_String ("constant ");
            end if;

            Sprint_Node (Subtype_Indication (Node));

         when N_At_Clause =>
            Write_Indent_String ("for ");
            Write_Id (Identifier (Node));
            Write_String (" use at ");
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Aggregate =>
            Do_Parens;

            if Null_Record_Present (Node) then
               Write_String ("(null record)");

            else
               Write_Char ('(');

               if List_Present (Expressions (Node)) then
                  Sprint_Comma_List (Expressions (Node));

                  if List_Present (Component_Associations (Node)) then
                     Write_String (", ");
                  end if;
               end if;

               if List_Present (Component_Associations (Node)) then
                  Sprint_Comma_List (Component_Associations (Node));
               end if;

               Write_Char (')');
            end if;

         when N_Mod_Clause =>
            Write_String ("at mod ");
            Sprint_Node (Expression (Node));

         when N_Allocator =>
            Do_Parens;
            Write_String ("new ");
            Sprint_Node (Expression (Node));

         when N_Assignment_Statement =>
            Write_Indent;
            Sprint_Node (Name (Node));
            Write_String (" := ");
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Asynchronous_Select =>
            Write_Indent_String ("select");
            Indent_Begin;
            Sprint_Node (Triggering_Alternative (Node));
            Indent_End;
            Write_Indent_String ("then abort");
            Sprint_Node (Abortable_Part (Node));
            Write_Indent_String ("end select;");

         when N_Attribute_Definition_Clause =>
            Write_Indent_String ("for ");
            Write_Id (Identifier (Node));
            Write_String (" use ");
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Attribute_Reference =>
            Do_Parens;
            Sprint_Node (Prefix (Node));
            Write_Char (''');
            Write_Id (Identifier (Node));

            if Present (Expression (Node)) then
               Write_String (" (");
               Sprint_Node (Expression (Node));
               Write_Char (')');
            end if;

         when N_Block_Statement =>
            Write_Indent;

            if Present (Identifier (Node))
              and then (not Has_Created_Identifier (Node)
                          or else not Debug_Flag_O)
            then
               Write_Rewrite_String ("{");
               Write_Id (Identifier (Node));
               Write_String (" : ");
               Write_Rewrite_String ("}");
            end if;

            if List_Present (Declarations (Node)) then
               Write_String ("declare");
               Sprint_Indented_List (Declarations (Node));
               Write_Indent;
            end if;

            Write_String ("begin");
            Sprint_Node (Handled_Statement_Sequence (Node));
            Write_Indent_String ("end");

            if Present (Identifier (Node))
              and then (not Has_Created_Identifier (Node)
                          or else not Debug_Flag_O)
            then
               Write_Rewrite_String ("{");
               Write_Char (' ');
               Write_Id (Identifier (Node));
               Write_Rewrite_String ("}");
            end if;

            Write_Char (';');

         when N_Case_Statement =>
            Write_Indent_String ("case ");
            Sprint_Node (Expression (Node));
            Write_String (" is");
            Sprint_Indented_List (Alternatives (Node));
            Write_Indent_String ("end case;");

         when N_Case_Statement_Alternative =>
            Write_Indent_String ("when ");
            Sprint_Bar_List (Discrete_Choices (Node));
            Write_String (" =>");
            Sprint_Indented_List (Statements (Node));

         when N_Character_Literal =>
            Do_Parens;
            Write_Char (''');

            if Char_Literal_Value (Node) in 16#20# .. 16#7E# then
               Write_Char (Char'Val (Char_Literal_Value (Node)));
            else
               Write_Str ("{Char_Code = ");
               Write_Int (Int (Char_Literal_Value (Node)));
               Write_Char ('}');
            end if;

            Write_Char (''');

         when N_Code_Statement =>
            Write_Indent;
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Compilation_Unit =>
            Sprint_Node_List (Context_Items (Node));

            if Private_Present (Node) then
               Write_Indent_String ("private ");
               Indent_Annull;
            end if;

            Sprint_Node (Unit (Node));
            Sprint_Opt_Node_List (Following_Pragmas (Node));

         when N_Component_Association =>
            Sprint_Bar_List (Choices (Node));
            Write_String (" => ");
            Sprint_Node (Expression (Node));

         when N_Component_Clause =>
            Write_Indent;
            Sprint_Node (Component_Name (Node));
            Write_String (" at ");
            Sprint_Node (Position (Node));
            Write_String (" range ");
            Sprint_Node (First_Bit (Node));
            Write_String (" .. ");
            Sprint_Node (Last_Bit (Node));
            Write_Char (';');

         when N_Component_Declaration =>
            if Write_Indent_Identifiers (Node) then
               Write_String (" : ");

               if Aliased_Present (Node) then
                  Write_String ("aliased ");
               end if;

               Sprint_Node (Subtype_Indication (Node));

               if Present (Expression (Node)) then
                  Write_String (" := ");
                  Sprint_Node (Expression (Node));
               end if;

               Write_Char (';');
            end if;

         when N_Component_List =>
            if Null_Present (Node) then
               Indent_Begin;
               Write_Indent_String ("null");
               Write_Char (';');
               Indent_End;

            else
               Sprint_Indented_List (Component_Declarations (Node));
               Sprint_Node (Variant_Part (Node));
            end if;

         when N_Concat_Multiple =>
            Do_Parens;

            declare
               Expr : Node_Id;

            begin
               Expr := First (Expressions (Node));

               loop
                  Sprint_Node (Expr);
                  Expr := Next (Expr);
                  exit when No (Expr);
                  Write_String (" && ");
               end loop;
            end;

         when N_Conditional_Entry_Call =>
            Write_Indent_String ("select");
            Indent_Begin;
            Sprint_Node (Entry_Call_Alternative (Node));
            Indent_End;
            Write_Indent_String ("else");
            Sprint_Indented_List (Else_Statements (Node));
            Write_Indent_String ("end select;");

         when N_Constrained_Array_Definition =>
            Write_String ("array ");
            Sprint_Paren_Comma_List (Discrete_Subtype_Definitions (Node));
            Write_String (" of ");

            if Aliased_Present (Node) then
               Write_String ("aliased ");
            end if;

            Sprint_Node (Subtype_Indication (Node));

         when N_Decimal_Fixed_Point_Definition =>
            Write_String ("digits ");
            Sprint_Node (Digits_Expression (Node));
            Write_String (" delta ");
            Sprint_Node (Delta_Expression (Node));
            Sprint_Opt_Node (Real_Range_Specification (Node));

         when N_Defining_Character_Literal =>
            Write_Name (Chars (Node));

         when N_Defining_Identifier =>
            Write_Id (Node);

         when N_Defining_Operator_Symbol =>
            Write_Name (Chars (Node));

         when N_Defining_Program_Unit_Name =>
            Sprint_Node (Name (Node));
            Write_Char ('.');
            Write_Id (Defining_Identifier (Node));

         when N_Delay_Alternative =>
            if Present (Condition (Node)) then
               Write_Indent;
               Write_String ("when ");
               Sprint_Node (Condition (Node));
               Write_String (" => ");
               Indent_Annull;
            end if;

            Sprint_Node (Delay_Statement (Node));
            Sprint_Node_List (Statements (Node));

         when N_Delay_Relative_Statement =>
            Write_Indent_String ("delay ");
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Delay_Until_Statement =>
            Write_Indent_String ("delay until ");
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Delta_Constraint =>
            Write_String ("delta ");
            Sprint_Node (Delta_Expression (Node));
            Sprint_Opt_Node (Range_Constraint (Node));

         when N_Derived_Type_Definition =>
            if Abstract_Present (Node) then
               Write_String ("abstract ");
            end if;

            Write_String ("new ");
            Sprint_Node (Subtype_Indication (Node));

            if Present (Record_Extension_Part (Node)) then
               Write_String (" with ");
               Sprint_Node (Record_Extension_Part (Node));
            end if;

         when N_Designator =>
            Sprint_Node (Name (Node));
            Write_Char ('.');
            Write_Id (Identifier (Node));

         when N_Digits_Constraint =>
            Write_String ("digits ");
            Sprint_Node (Digits_Expression (Node));
            Sprint_Opt_Node (Range_Constraint (Node));

         when N_Discriminant_Association =>
            if List_Present (Selector_Names (Node)) then
               Sprint_Bar_List (Selector_Names (Node));
               Write_String (" => ");
            end if;

            Sprint_Node (Expression (Node));

         when N_Discriminant_Specification =>
            if Write_Identifiers (Node) then
               Write_String (" : ");
               Sprint_Node (Discriminant_Type (Node));

               if Present (Expression (Node)) then
                  Write_String (" := ");
                  Sprint_Node (Expression (Node));
               end if;
            end if;

         when N_Elsif_Part =>
            Write_Indent_String ("elsif ");
            Sprint_Node (Condition (Node));
            Sprint_Indented_List (Then_Statements (Node));

         when N_Empty =>
            null;

         when N_Entry_Body =>
            Write_Indent_String ("entry ");
            Write_Id (Defining_Identifier (Node));
            Sprint_Node (Entry_Body_Formal_Part (Node));
            Write_String (" when ");
            Sprint_Node (Condition (Node));

            Write_String (" is");
            Sprint_Indented_List (Declarations (Node));
            Write_Indent_String ("begin");
            Sprint_Node (Handled_Statement_Sequence (Node));
            Write_Indent_String ("end ");
            Write_Id (Defining_Identifier (Node));
            Write_Char (';');

         when N_Entry_Body_Formal_Part =>
            if Present (Entry_Index_Specification (Node)) then
               Write_String (" (");
               Sprint_Node (Entry_Index_Specification (Node));
               Write_Char (')');
            end if;

            Write_Param_Specs (Node);

         when N_Entry_Call_Alternative =>
            Sprint_Node (Entry_Call_Statement (Node));
            Sprint_Node_List (Statements (Node));

         when N_Entry_Call_Statement =>
            Write_Indent;
            Sprint_Node (Name (Node));
            Sprint_Opt_Paren_Comma_List (Parameter_Associations (Node));
            Write_Char (';');

         when N_Entry_Declaration =>
            Write_Indent_String ("entry ");
            Write_Id (Defining_Identifier (Node));

            if Present (Discrete_Subtype_Definition (Node)) then
               Write_String (" (");
               Sprint_Node (Discrete_Subtype_Definition (Node));
               Write_String (")");
            end if;

            Write_Param_Specs (Node);
            Write_Char (';');

         when N_Entry_Index_Specification =>
            Write_String ("for ");
            Write_Id (Defining_Identifier (Node));
            Write_String (" in ");
            Sprint_Node (Discrete_Subtype_Definition (Node));

         when N_Enumeration_Representation_Clause =>
            Write_Indent_String ("for ");
            Write_Id (Identifier (Node));
            Write_String (" use ");
            Sprint_Node (Array_Aggregate (Node));
            Write_Char (';');

         when N_Enumeration_Type_Definition =>

            --  Skip attempt to print Literals field if it's not there and
            --  we are in package Standard (case of Character, which is
            --  handled specially (without an explicit literals list).

            if Sloc (Node) /= Standard_Location
              or else List_Present (Literals (Node))
            then
               Sprint_Paren_Comma_List (Literals (Node));
            end if;

         when N_Error =>
            Write_String ("<error>");

         when N_Exception_Declaration =>
            if Write_Indent_Identifiers (Node) then
               Write_String (" : exception;");
            end if;

         when N_Exception_Handler =>
            Write_Indent_String ("when ");

            if Present (Choice_Parameter (Node)) then
               Sprint_Node (Choice_Parameter (Node));
               Write_String (": ");
            end if;

            Sprint_Bar_List (Exception_Choices (Node));
            Write_String (" => ");
            Sprint_Indented_List (Statements (Node));

         when N_Exception_Renaming_Declaration =>
            Write_Indent;
            Sprint_Node (Defining_Identifier (Node));
            Write_String (" : exception renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Exit_Statement =>
            Write_Indent_String ("exit");
            Sprint_Opt_Node (Name (Node));

            if Present (Condition (Node)) then
               Write_String (" when ");
               Sprint_Node (Condition (Node));
            end if;

            Write_Char (';');

         when N_Explicit_Dereference =>
            Do_Parens;
            Sprint_Node (Prefix (Node));
            Write_String (".all");

         when N_Expression_Actions =>
            Write_Str (" [");
            Sprint_Indented_List (Actions (Node));
            Indent_Begin;
            Write_Indent;
            Sprint_Node (Expression (Node));
            Indent_End;
            Write_Char (']');

         when N_Extension_Aggregate =>
            Do_Parens;
            Write_Char ('(');
            Sprint_Node (Expression (Node));
            Write_String (" with ");

            if Null_Record_Present (Node) then
               Write_String ("null record");
            else
               Sprint_Comma_List (Component_Associations (Node));
            end if;

            Write_Char (')');

         when N_Floating_Point_Definition =>
            Write_String ("digits ");
            Sprint_Node (Digits_Expression (Node));
            Sprint_Opt_Node (Real_Range_Specification (Node));

         when N_Formal_Decimal_Fixed_Point_Definition =>
            Write_String ("delta <> digits <>");

         when N_Formal_Derived_Type_Definition =>
            Write_String ("new ");
            Sprint_Node (Subtype_Mark (Node));

            if Private_Present (Node) then
               Write_String (" with private");
            end if;

         when N_Formal_Discrete_Type_Definition =>
            Write_String ("<>");

         when N_Formal_Floating_Point_Definition =>
            Write_String ("digits <>");

         when N_Formal_Modular_Type_Definition =>
            Write_String ("mod <>");

         when N_Formal_Object_Declaration =>
            if Write_Indent_Identifiers (Node) then
               Write_String (" : ");

               if In_Present (Node) then
                  Write_String ("in ");
               end if;

               if Out_Present (Node) then
                  Write_String ("out ");
               end if;

               Sprint_Node (Subtype_Mark (Node));

               if Present (Expression (Node)) then
                  Write_String (" := ");
                  Sprint_Node (Expression (Node));
               end if;

               Write_Char (';');
            end if;

         when N_Formal_Ordinary_Fixed_Point_Definition =>
            Write_String ("delta <>");

         when N_Formal_Package_Declaration =>
            Write_Indent_String ("with package ");
            Write_Id (Defining_Identifier (Node));
            Write_String (" is new ");
            Sprint_Node (Name (Node));
            Write_String (" (<>);");

         when N_Formal_Private_Type_Definition =>
            if Abstract_Present (Node) then
               Write_String ("abstract ");
            end if;

            if Tagged_Present (Node) then
               Write_String ("tagged ");
            end if;

            if Limited_Present (Node) then
               Write_String ("limited ");
            end if;

            Write_String ("private");

         when N_Formal_Signed_Integer_Type_Definition =>
            Write_String ("range <>");

         when N_Formal_Subprogram_Declaration =>
            Write_Indent_String ("with ");
            Sprint_Node (Specification (Node));

            if Box_Present (Node) then
               Write_String (" is <>");
            elsif Present (Name (Node)) then
               Write_String (" is ");
               Sprint_Node (Name (Node));
            end if;

            Write_Char (';');

         when N_Formal_Type_Declaration =>
            Write_Indent_String ("type ");
            Write_Id (Defining_Identifier (Node));

            if List_Present (Discriminant_Specifications (Node)) then
               Sprint_Paren_Comma_List
                 (Discriminant_Specifications (Node));
            elsif Has_Unknown_Discriminants (Node) then
               Write_String ("(<>)");
            end if;

            Write_String (" is ");
            Sprint_Node (Formal_Type_Definition (Node));
            Write_Char (';');

         when N_Freeze_Entity =>
            if not Debug_Flag_O then
               Write_Indent;
               Write_Rewrite_String ("{");
               Write_String ("freeze ");
               Write_Id (Entity (Node));
               Write_Char (';');
               Write_Rewrite_String ("}");
            end if;

         when N_Full_Type_Declaration =>
            Write_Indent_String ("type ");
            Write_Id (Defining_Identifier (Node));
            Sprint_Opt_Paren_Comma_List
              (Discriminant_Specifications (Node));
            Write_String (" is ");
            Sprint_Node (Type_Definition (Node));
            Write_Char (';');

         when N_Function_Call =>
            Sprint_Node (Name (Node));
            Sprint_Opt_Paren_Comma_List (Parameter_Associations (Node));

         when N_Function_Instantiation =>
            Write_Indent_String ("function ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_String (" is new ");
            Sprint_Node (Name (Node));
            Sprint_Opt_Paren_Comma_List (Generic_Associations (Node));

         when N_Function_Specification =>
            Write_String ("function ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Param_Specs (Node);
            Write_String (" return ");
            Sprint_Node (Subtype_Mark (Node));

         when N_Generic_Association =>
            if Present (Selector_Name (Node)) then
               Sprint_Node (Selector_Name (Node));
               Write_String (" => ");
            end if;

            Sprint_Node (Explicit_Generic_Actual_Parameter (Node));

         when N_Generic_Function_Renaming_Declaration =>
            Write_Indent_String ("generic function ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_String (" renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Generic_Package_Declaration =>
            Write_Indent_String ("generic ");
            Sprint_Indented_List (Generic_Formal_Declarations (Node));
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_Char (';');

         when N_Generic_Package_Renaming_Declaration =>
            Write_Indent_String ("generic package ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_String (" renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Generic_Procedure_Renaming_Declaration =>
            Write_Indent_String ("generic procedure ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_String (" renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Generic_Subprogram_Declaration =>
            Write_Indent_String ("generic ");
            Sprint_Indented_List (Generic_Formal_Declarations (Node));
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_Char (';');

         when N_Goto_Statement =>
            Write_Indent_String ("goto ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Handled_Sequence_Of_Statements =>
            Sprint_Indented_List (Statements (Node));

            if List_Present (Exception_Handlers (Node)) then
               Write_Indent_String ("exception");
               Indent_Begin;
               Sprint_Node_List (Exception_Handlers (Node));
               Indent_End;
            end if;

            if Present (Identifier (Node)) then
               Write_Indent_String ("at end");
               Indent_Begin;
               Write_Indent;
               Sprint_Node (Identifier (Node));
               Write_Char (';');
               Indent_End;
            end if;

         when N_Identifier =>
            Do_Parens;
            Write_Id (Node);

         when N_If_Statement =>
            Write_Indent_String ("if ");
            Sprint_Node (Condition (Node));
            Write_String (" then");
            Sprint_Indented_List (Then_Statements (Node));
            Sprint_Opt_Node_List (Elsif_Parts (Node));

            if List_Present (Else_Statements (Node)) then
               Write_Indent_String ("else");
               Sprint_Indented_List (Else_Statements (Node));
            end if;

            Write_Indent_String ("end if;");

         when N_Implicit_Label_Declaration =>
            if not Debug_Flag_O then
               Write_Indent;
               Write_Rewrite_String ("{");
               Write_Id (Defining_Identifier (Node));
               Write_String (" : label");
               Write_Rewrite_String ("}");
            end if;

         when N_Implicit_Type =>
            if not Debug_Flag_O then
               Write_Indent;
               Write_Rewrite_String ("{");
               Write_String ("implicit ");
               Write_Implicit_Def (Defining_Identifier (Node));
               Write_Rewrite_String ("}");
            end if;

         when N_Incomplete_Type_Declaration =>
            Write_Indent_String ("type ");
            Write_Id (Defining_Identifier (Node));

            if List_Present (Discriminant_Specifications (Node)) then
               Sprint_Paren_Comma_List
                 (Discriminant_Specifications (Node));
            elsif Has_Unknown_Discriminants (Node) then
               Write_String ("(<>)");
            end if;

            Write_Char (';');

         when N_Index_Or_Discriminant_Constraint =>
            Sprint_Paren_Comma_List (Constraints (Node));

         when N_Indexed_Component =>
            Do_Parens;
            Sprint_Node (Prefix (Node));
            Sprint_Opt_Paren_Comma_List (Expressions (Node));

         when N_Integer_Literal =>
            Do_Parens;
            Write_Uint (Intval (Node));

         when N_Iteration_Scheme =>
            if Present (Condition (Node)) then
               Write_String ("while ");
               Sprint_Node (Condition (Node));
            else
               Write_String ("for ");
               Sprint_Node (Loop_Parameter_Specification (Node));
            end if;

            Write_Char (' ');

         when N_Label =>
            Write_Indent_String ("<<");
            Write_Id (Identifier (Node));
            Write_String (">>");

         when N_Loop_Parameter_Specification =>
            Write_Id (Defining_Identifier (Node));
            Write_String (" in ");

            if Reverse_Present (Node) then
               Write_String ("reverse ");
            end if;

            Sprint_Node (Discrete_Subtype_Definition (Node));

         when N_Loop_Statement =>
            Write_Indent;

            if Present (Identifier (Node))
              and then (not Has_Created_Identifier (Node)
                          or else not Debug_Flag_O)
            then
               Write_Rewrite_String ("{");
               Write_Id (Identifier (Node));
               Write_String (" : ");
               Write_Rewrite_String ("}");
               Sprint_Node (Iteration_Scheme (Node));
               Write_String ("loop");
               Sprint_Indented_List (Statements (Node));
               Write_Indent_String ("end loop ");
               Write_Rewrite_String ("{");
               Write_Id (Identifier (Node));
               Write_Rewrite_String ("}");
               Write_Char (';');

            else
               Sprint_Node (Iteration_Scheme (Node));
               Write_String ("loop");
               Sprint_Indented_List (Statements (Node));
               Write_Indent_String ("end loop;");
            end if;

         when N_Modular_Type_Definition =>
            Write_String ("mod ");
            Sprint_Node (Expression (Node));

         when N_Null =>
            Do_Parens;
            Write_String ("null");

         when N_Null_Statement =>
            Write_Indent_String ("null;");

         when N_Number_Declaration =>
            if Write_Indent_Identifiers (Node) then
               Write_String (" : constant ");
               Write_String (" := ");
               Sprint_Node (Expression (Node));
            end if;

         when N_Object_Declaration =>
            if Write_Indent_Identifiers (Node) then
               Write_String (" : ");

               if Aliased_Present (Node) then
                  Write_String ("aliased ");
               end if;

               if Constant_Present (Node) then
                  Write_String ("constant ");
               end if;

               Sprint_Node (Object_Definition (Node));

               if Present (Expression (Node)) then
                  Write_String (" := ");
                  Sprint_Node (Expression (Node));
               end if;

               Write_Char (';');
            end if;

         when N_Object_Renaming_Declaration =>
            Write_Indent;
            Sprint_Node (Defining_Identifier (Node));
            Write_String (" : ");
            Sprint_Node (Subtype_Mark (Node));
            Write_String (" renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Op_Abs =>
            Do_Parens;
            Write_String ("abs ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Add =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" + ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_And =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" and ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_And_Then =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" and then ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Concat =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" & ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Divide =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" / ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Eq =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" = ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Expon =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" ** ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Ge =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" >= ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Gt =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" > ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_In =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" in ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Le =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" <= ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Lt =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" < ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Minus =>
            Do_Parens;
            Write_Char ('-');
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Mod =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" mod ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Multiply =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" * ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Ne =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" /= ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Not =>
            Do_Parens;
            Write_String ("not ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Not_In =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" not in ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Or =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" or ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Or_Else =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" or else ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Plus =>
            Do_Parens;
            Write_Char ('+');
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Rem =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" rem ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Subtract =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" - ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Xor =>
            Do_Parens;
            Sprint_Node (Left_Opnd (Node));
            Write_String (" xor ");
            Sprint_Node (Right_Opnd (Node));

         when N_Operator_Symbol =>
            Get_Name_String (Chars (Node));

            for I in 1 .. Name_Len loop
               Write_Char (Name_Buffer (I));
            end loop;

         when N_Ordinary_Fixed_Point_Definition =>
            Write_String ("delta ");
            Sprint_Node (Delta_Expression (Node));
            Sprint_Opt_Node (Real_Range_Specification (Node));

         when N_Others_Choice =>
            Write_String ("others");

         when N_Package_Body =>
            Write_Indent_String ("package body ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_String (" is");
            Sprint_Indented_List (Declarations (Node));

            if Present (Handled_Statement_Sequence (Node)) then
               Write_Indent_String ("begin");
               Sprint_Node (Handled_Statement_Sequence (Node));
            end if;

            Write_Indent_String ("end ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Char (';');

         when N_Package_Body_Stub =>
            Write_Indent_String ("package_body ");
            Sprint_Node (Defining_Identifier (Node));
            Write_String (" is separate;");

         when N_Package_Declaration =>
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_Char (';');

         when N_Package_Instantiation =>
            Write_Indent_String ("package ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_String (" is new ");
            Sprint_Node (Name (Node));
            Sprint_Opt_Paren_Comma_List (Generic_Associations (Node));

         when N_Package_Renaming_Declaration =>
            Write_Indent_String ("package ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_String (" renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Package_Specification =>
            Write_String ("package ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_String (" is");
            Sprint_Indented_List (Visible_Declarations (Node));

            if List_Present (Private_Declarations (Node)) then
               Write_Indent_String ("private");
               Sprint_Indented_List (Private_Declarations (Node));
            end if;

            Write_Indent_String ("end ");
            Sprint_Node (Defining_Unit_Name (Node));

         when N_Parameter_Association =>
            Sprint_Node (Selector_Name (Node));
            Write_String (" =>");
            Sprint_Node (Actual_Parameter (Node));

         when N_Parameter_Specification =>
            if Write_Identifiers (Node) then
               Write_String (" : ");

               if In_Present (Node) then
                  Write_String ("in ");
               end if;

               if Out_Present (Node) then
                  Write_String ("out ");
               end if;

               Sprint_Node (Parameter_Type (Node));

               if Present (Expression (Node)) then
                  Write_String (" := ");
                  Sprint_Node (Expression (Node));
               end if;
            end if;

         when N_Parenthesized_Expression =>
            Do_Parens;
            Write_Char ('(');
            Sprint_Node (Expression (Node));
            Write_Char (')');

         when N_Pragma =>
            Write_Indent_String ("pragma ");
            Write_Id (Identifier (Node));

            if List_Present (Pragma_Argument_Associations (Node)) then
               Sprint_Opt_Paren_Comma_List
                 (Pragma_Argument_Associations (Node));
            end if;

            Write_Char (';');

         when N_Pragma_Argument_Association =>
            if Present (Identifier (Node)) then
               Write_Id (Identifier (Node));
               Write_String (" => ");
            end if;

            Sprint_Node (Expression (Node));

         when N_Private_Type_Declaration =>
            Write_Indent_String ("type ");
            Write_Id (Defining_Identifier (Node));

            if List_Present (Discriminant_Specifications (Node)) then
               Sprint_Paren_Comma_List
                 (Discriminant_Specifications (Node));
            elsif Has_Unknown_Discriminants (Node) then
               Write_String ("(<>)");
            end if;

            Write_String (" is ");

            if Tagged_Present (Node) then
               Write_String ("tagged ");
            end if;

            if Limited_Present (Node) then
               Write_String ("limited ");
            end if;

            Write_String ("private;");

         when N_Private_Extension_Declaration =>
            Write_Indent_String ("type ");
            Write_Id (Defining_Identifier (Node));

            if List_Present (Discriminant_Specifications (Node)) then
               Sprint_Paren_Comma_List
                 (Discriminant_Specifications (Node));
            elsif Has_Unknown_Discriminants (Node) then
               Write_String ("(<>)");
            end if;

            Write_String (" is new ");
            Sprint_Node (Subtype_Indication (Node));
            Write_String (" with private;");

         when N_Procedure_Call_Statement =>
            Write_Indent;
            Sprint_Node (Name (Node));
            Sprint_Opt_Paren_Comma_List (Parameter_Associations (Node));
            Write_Char (';');

         when N_Procedure_Instantiation =>
            Write_Indent_String ("procedure ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_String (" is new ");
            Sprint_Node (Name (Node));
            Sprint_Opt_Paren_Comma_List (Generic_Associations (Node));

         when N_Procedure_Specification =>
            Write_String ("procedure ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Param_Specs (Node);

         when N_Protected_Body =>
            Write_Indent_String ("protected body ");
            Write_Id (Defining_Identifier (Node));
            Write_String (" is");
            Sprint_Indented_List (Declarations (Node));
            Write_Indent_String ("end ");
            Write_Id (Defining_Identifier (Node));
            Write_Char (';');

         when N_Protected_Body_Stub =>
            Write_Indent_String ("protected body ");
            Write_Id (Defining_Identifier (Node));
            Write_String (" is separate;");

         when N_Protected_Definition =>
            Sprint_Indented_List (Visible_Declarations (Node));

            if List_Present (Private_Declarations (Node)) then
               Write_Indent_String ("private");
               Sprint_Indented_List (Private_Declarations (Node));
            end if;

            Write_Indent_String ("end ");
            Write_Id (Defining_Identifier (Parent (Node)));

         when N_Protected_Type_Declaration =>
            Write_Indent_String ("protected type ");
            Write_Id (Defining_Identifier (Node));
            Sprint_Opt_Paren_Comma_List
              (Discriminant_Specifications (Node));
            Write_String (" is");
            Sprint_Node (Protected_Definition (Node));
            Write_Char (';');

         when N_Qualified_Expression =>
            Do_Parens;
            Sprint_Node (Subtype_Mark (Node));
            Write_String ("'");
            Sprint_Node (Expression (Node));

         when N_Raise_Statement =>
            Write_Indent_String ("raise ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Range =>
            Sprint_Node (Low_Bound (Node));
            Write_String (" .. ");
            Sprint_Node (High_Bound (Node));

         when N_Range_Constraint =>
            Write_String ("range ");
            Sprint_Node (Range_Expression (Node));

         when N_Real_Literal =>
            Do_Parens;
            Write_Real_Value (Node);

         when N_Real_Range_Specification =>
            Write_String ("range ");
            Sprint_Node (Low_Bound (Node));
            Write_String (" .. ");
            Sprint_Node (High_Bound (Node));

         when N_Record_Definition =>
            if Abstract_Present (Node) then
               Write_String ("abstract ");
            end if;

            if Tagged_Present (Node) then
               Write_String ("tagged ");
            end if;

            if Limited_Present (Node) then
               Write_String ("limited ");
            end if;

            if Null_Present (Node) then
               Write_String ("null record");

            else
               Write_String ("record");

               if not Debug_Flag_O 
                  and then List_Present (Implicit_Types (Node))
               then
                  Sprint_Indented_List (Implicit_Types (Node));
               end if;

               Sprint_Node (Component_List (Node));
               Write_Indent_String ("end record");
            end if;

         when N_Record_Representation_Clause =>
            Write_Indent_String ("for ");
            Sprint_Node (Identifier (Node));
            Write_String (" use record ");

            if Present (Mod_Clause (Node)) then
               Sprint_Node (Mod_Clause (Node));
            end if;

            Sprint_Indented_List (Component_Clauses (Node));
            Write_Indent_String ("end record;");

         when N_Reference =>
            Sprint_Node (Expression (Node));
            Write_String ("'reference");

         when N_Requeue_Statement =>
            Write_Indent_String ("requeue ");
            Sprint_Node (Name (Node));

            if Abort_Present (Node) then
               Write_String (" with abort");
            end if;

            Write_Char (';');

         when N_Return_Statement =>
            if Present (Expression (Node)) then
               Write_Indent_String ("return ");
               Sprint_Node (Expression (Node));
               Write_Char (';');
            else
               Write_Indent_String ("return;");
            end if;

         when N_Selective_Accept =>
            Write_Indent_String ("select");

            declare
               Alt_Node : Node_Id;
            begin
               Alt_Node := First (Selective_Accept_Alternatives (Node));

               loop
                  Indent_Begin;
                  Sprint_Node (Alt_Node);
                  Indent_End;
                  Alt_Node := Next (Alt_Node);
                  exit when Alt_Node = Empty;
                  Write_Indent_String ("or");
               end loop;
            end;

            if List_Present (Else_Statements (Node)) then
               Write_Indent_String ("else");
               Sprint_Indented_List (Else_Statements (Node));
            end if;

            Write_Indent_String ("end select;");

         when N_Signed_Integer_Type_Definition =>
            Write_String ("range ");
            Sprint_Node (Low_Bound (Node));
            Write_String (" .. ");
            Sprint_Node (High_Bound (Node));

         when N_Single_Protected_Declaration =>
            Write_Indent_String ("protected ");
            Write_Id (Defining_Identifier (Node));

            if Present (Protected_Definition (Node)) then
               Write_String (" is");
               Sprint_Node (Protected_Definition (Node));
            end if;

            Write_Char (';');

         when N_Single_Task_Declaration =>
            Write_Indent_String ("task ");
            Write_Id (Defining_Identifier (Node));

            if Present (Task_Definition (Node)) then
               Write_String (" is");
               Sprint_Node (Task_Definition (Node));
            end if;

            Write_Char (';');

         when N_Selected_Component | N_Expanded_Name =>
            Do_Parens;
            Sprint_Node (Prefix (Node));
            Write_Char ('.');
            Sprint_Node (Selector_Name (Node));

         when N_Slice =>
            Do_Parens;
            Sprint_Node (Prefix (Node));
            Write_String (" (");
            Sprint_Node (Discrete_Range (Node));
            Write_Char (')');

         when N_String_Literal =>
            Do_Parens;
            Write_String_Table_Entry (Strval (Node));

         when N_Subprogram_Body =>
            Write_Indent;
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_String (" is");
            Sprint_Indented_List (Declarations (Node));
            Write_Indent_String ("begin");
            Sprint_Node (Handled_Statement_Sequence (Node));
            Write_Indent_String ("end ");
            Sprint_Node
              (Defining_Unit_Name (Specification (Node)));
            Write_Char (';');

            if Is_List_Member (Node)
              and then Present (Next (Node))
              and then Nkind (Next (Node)) /= N_Subprogram_Body
            then
               Write_Indent;
            end if;

         when N_Subprogram_Body_Stub =>
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_String (" is separate;");

         when N_Subprogram_Declaration =>
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_Char (';');

         when N_Subprogram_Renaming_Declaration =>
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_String (" ranames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Subtype_Declaration =>
            Write_Indent_String ("subtype ");
            Write_Id (Defining_Identifier (Node));
            Write_String (" is ");
            Sprint_Node (Subtype_Indication (Node));
            Write_Char (';');

         when N_Subtype_Indication =>
            Sprint_Node (Subtype_Mark (Node));
            Write_Char (' ');
            Sprint_Node (Constraint (Node));

         when N_Subunit =>
            Write_Indent_String ("separate (");
            Sprint_Node (Name (Node));
            Write_Char (')');
            Write_Eol;
            Sprint_Node (Proper_Body (Node));

         when N_Task_Body =>
            Write_Indent_String ("task body ");
            Write_Id (Defining_Identifier (Node));
            Write_String (" is");
            Sprint_Indented_List (Declarations (Node));
            Write_Indent_String ("begin");
            Sprint_Node (Handled_Statement_Sequence (Node));
            Write_Indent_String ("end ");
            Write_Id (Defining_Identifier (Node));
            Write_Char (';');

         when N_Task_Body_Stub =>
            Write_Indent_String ("task body ");
            Write_Id (Defining_Identifier (Node));
            Write_String (" is separate;");

         when N_Task_Definition =>
            Sprint_Indented_List (Visible_Declarations (Node));

            if List_Present (Private_Declarations (Node)) then
               Write_Indent_String ("private");
               Sprint_Indented_List (Private_Declarations (Node));
            end if;

            Write_Indent_String ("end ");
            Write_Id (Defining_Identifier (Parent (Node)));

         when N_Task_Type_Declaration =>
            Write_Indent_String ("task type ");
            Write_Id (Defining_Identifier (Node));
            Sprint_Opt_Paren_Comma_List
              (Discriminant_Specifications (Node));
            Write_String (" is");
            Sprint_Node (Task_Definition (Node));
            Write_Char (';');

         when N_Terminate_Alternative =>
            if Present (Condition (Node)) then
               Write_Indent;
               Write_String ("when ");
               Sprint_Node (Condition (Node));
               Write_String (" => terminate;");
            else
               Write_String ("terminate;");
            end if;

            Sprint_Node_List (Statements (Node));

         when N_Timed_Entry_Call =>
            Write_Indent_String ("select");
            Indent_Begin;
            Sprint_Node (Entry_Call_Alternative (Node));
            Indent_End;
            Write_Indent_String ("or");
            Indent_Begin;
            Sprint_Node (Delay_Alternative (Node));
            Indent_End;
            Write_Indent_String ("end select;");

         when N_Triggering_Alternative =>
            Sprint_Node (Triggering_Statement (Node));
            Sprint_Node_List (Statements (Node));

         when N_Type_Conversion =>
            Do_Parens;
            Sprint_Node (Subtype_Mark (Node));

            if Unchecked_Conversion (Node) then
               Write_String ("!(");
            else
               Write_String (" (");
            end if;

            Sprint_Node (Expression (Node));
            Write_Char (')');

         when N_Unconstrained_Array_Definition =>
            Write_String ("array (");

            declare
               Node1 : Node_Id := First (Subtype_Marks (Node));
            begin
               loop
                  Sprint_Node (Node1);
                  Write_String (" range <>");
                  Node1 := Next (Node1);
                  exit when Node1 = Empty;
                  Write_String (", ");
               end loop;
            end;

            Write_String (") of ");

            if Aliased_Present (Node) then
               Write_String ("aliased ");
            end if;

            Sprint_Node (Subtype_Indication (Node));

         when N_Unused_At_Start | N_Unused_At_End =>
            Write_Indent_String ("***** Error, unused node encountered *****");
            Write_Eol;

         when N_Use_Package_Clause =>
            Write_Indent_String ("use ");
            Sprint_Comma_List (Names (Node));
            Write_Char (';');

         when N_Use_Type_Clause =>
            Write_Indent_String ("use type ");
            Sprint_Comma_List (Subtype_Marks (Node));
            Write_Char (';');

         when N_Variant =>
            Write_Indent_String ("when ");
            Sprint_Bar_List (Discrete_Choices (Node));
            Write_String (" => ");
            Sprint_Node (Component_List (Node));

         when N_Variant_Part =>
            Indent_Begin;
            Write_Indent_String ("case ");
            Sprint_Node (Name (Node));
            Write_String (" is ");
            Sprint_Indented_List (Variants (Node));

         when N_With_Clause =>
            if First_Name (Node) or else not Debug_Flag_O then
               Write_Indent_String ("with ");
            else
               Write_String (", ");
            end if;

            Sprint_Node (Name (Node));

            if Last_Name (Node) or else not Debug_Flag_O then
               Write_Char (';');
            end if;

      end case;

      if Paren_Flag then
         Write_Char (')');
      end if;

   end Sprint_Node_Actual;

   ----------------------
   -- Sprint_Node_List --
   ----------------------

   procedure Sprint_Node_List (List : List_Id) is
      Node : Node_Id;

   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);

         loop
            Sprint_Node (Node);
            Node := Next (Node);
            exit when Node = Empty;
         end loop;
      end if;
   end Sprint_Node_List;

   ---------------------
   -- Sprint_Opt_Node --
   ---------------------

   procedure Sprint_Opt_Node (Node : Node_Id) is
   begin
      if Present (Node) then
         Write_Char (' ');
         Sprint_Node (Node);
      end if;
   end Sprint_Opt_Node;

   --------------------------
   -- Sprint_Opt_Node_List --
   --------------------------

   procedure Sprint_Opt_Node_List (List : List_Id) is
   begin
      if List_Present (List) then
         Sprint_Node_List (List);
      end if;
   end Sprint_Opt_Node_List;

   ---------------------------------
   -- Sprint_Opt_Paren_Comma_List --
   ---------------------------------

   procedure Sprint_Opt_Paren_Comma_List (List : List_Id) is
   begin
      if List_Present (List) then
         Write_Char (' ');
         Sprint_Paren_Comma_List (List);
      end if;
   end Sprint_Opt_Paren_Comma_List;

   -----------------------------
   -- Sprint_Paren_Comma_List --
   -----------------------------

   procedure Sprint_Paren_Comma_List (List : List_Id) is
   begin
      if Is_Non_Empty_List (List) then
         Write_Char ('(');
         Sprint_Comma_List (List);
         Write_Char (')');
      end if;
   end Sprint_Paren_Comma_List;

   -----------------
   -- Write_Ekind --
   -----------------

   procedure Write_Ekind (E : Entity_Id) is
      Ucase : Boolean;
      S     : constant String := Entity_Kind'Image (Ekind (E));

   begin
      Ucase := True;

      for I in S'First + 2 .. S'Last loop
         if Ucase then
            Write_Char (Fold_Upper (To_Char (S (I))));
         else
            Write_Char (Fold_Lower (To_Char (S (I))));
         end if;

         Ucase := (S (I) = '_');
      end loop;

   end Write_Ekind;

   --------------
   -- Write_Id --
   --------------

   procedure Write_Id (Id : Node_Id) is
   begin
      Write_Name (Chars (Id));
   end Write_Id;

   -----------------------
   -- Write_Identifiers --
   -----------------------

   function Write_Identifiers (Node : Node_Id) return Boolean is
   begin
      --  If we are printing the original tree, and this is not the first
      --  defining identifier in the list, then we must output a comma to
      --  separate this name from the list.

      if Debug_Flag_O and then Prev_Ids (Node) then
         Write_String (", ");
      end if;

      Sprint_Node (Defining_Identifier (Node));

      --  The remainder of the declaration must be printed unless we are
      --  printing the original tree and this is not the last identifier

      return
         not Debug_Flag_O or else not More_Ids (Node);

   end Write_Identifiers;

   ------------------------
   -- Write_Implicit_Def --
   ------------------------

   procedure Write_Implicit_Def (E : Entity_Id) is
      Ind : Node_Id;

   begin
      case Ekind (E) is
         when E_Array_Subtype =>
            Write_String ("subtype ");
            Write_Id (E);
            Write_String (" is ");
            Write_Id (Base_Type (E));
            Write_String (" (");

            Ind := First_Index (E);

            while Present (Ind) loop
               Sprint_Node (Ind);
               Ind := Next_Index (Ind);

               if Present (Ind) then
                  Write_String (", ");
               end if;
            end loop;

            Write_String (");");

         when E_Integer_Subtype | E_Enumeration_Subtype =>
            Write_String ("subtype ");
            Write_Id (E);
            Write_String (" is ");
            Write_Id (Etype (E));
            Write_String (" range ");
            Sprint_Node (Scalar_Range (E));
            Write_String (";");

         when others =>
            Write_String ("type ");
            Write_Id (E);
            Write_String (" is <");
            Write_Ekind (E);
            Write_String (">;");
      end case;

   end Write_Implicit_Def;

   ------------------
   -- Write_Indent --
   ------------------

   procedure Write_Indent is
   begin
      if Indent_Annull_Flag then
         Indent_Annull_Flag := False;
      else
         Write_Eol;
         for I in 1 .. Indent loop
            Write_Char (' ');
         end loop;
      end if;
   end Write_Indent;

   ------------------------------
   -- Write_Indent_Identifiers --
   ------------------------------

   function Write_Indent_Identifiers (Node : Node_Id)
     return Boolean is
   begin
      --  We need to start a new line for every node, except in the case
      --  where we are printing the original tree and this is not the first
      --  defining identifier in the list.

      if not Debug_Flag_O or else not Prev_Ids (Node) then
         Write_Indent;

      --  If printing original tree and this is not the first defining
      --  identifier in the list, then the previous call to this procedure
      --  printed only the name, and we add a comma to separate the names.

      else
         Write_String (", ");
      end if;

      Sprint_Node (Defining_Identifier (Node));

      --  The remainder of the declaration must be printed unless we are
      --  printing the original tree and this is not the last identifier

      return
         not Debug_Flag_O or else not More_Ids (Node);

   end Write_Indent_Identifiers;

   -------------------------
   -- Write_Indent_String --
   -------------------------

   procedure Write_Indent_String (S : String) is
   begin
      Write_Indent;
      Write_String (S);
   end Write_Indent_String;

   -----------------------
   -- Write_Param_Specs --
   -----------------------

   procedure Write_Param_Specs (N : Node_Id) is
      Specs : List_Id;
      Spec  : Node_Id;

   begin
      Specs := Parameter_Specifications (N);

      if List_Present (Specs) then
         Write_String (" (");
         Spec := First (Specs);

         loop
            Sprint_Node (Spec);
            Spec := Next (Spec);
            exit when Spec = Empty;

            --  Add semicolon, unless we are printing original tree and the
            --  next specification is part of a list (but not the first
            --  element of that list)

            if not Debug_Flag_O or else not Prev_Ids (Spec) then
               Write_String ("; ");
            end if;

            if Column > 70 then
               Write_Indent_String (" ");
            end if;
         end loop;

         Write_Char (')');
      end if;
   end Write_Param_Specs;

   ----------------------
   -- Write_Real_Value --
   ----------------------

   procedure Write_Real_Value (Node : Node_Id) is
   begin
      if Decimal (Node) then
         Write_Uint (Numerator (Node));
         Write_String (".0");

         if Denominator (Node) /= Uint_0 then
            Write_String ("E");
            Write_Uint (Denominator (Node));
         end if;

      else
         Write_Char ('[');
         Write_Uint (Numerator (Node));
         Write_String (" / ");
         Write_Uint (Denominator (Node));
         Write_Char (']');
      end if;
   end Write_Real_Value;

   --------------------------
   -- Write_Rewrite_String --
   --------------------------

   procedure Write_Rewrite_String (S : String) is
   begin
      if not Debug_Flag_G then
         Write_String (S);
      end if;
   end Write_Rewrite_String;

   ----------------
   -- Write_Uint --
   ----------------

   procedure Write_Uint (U : Uint) is
   begin
      Get_Name_String (UI_Image (U));

      for I in 1 .. Name_Len loop
         Write_Char (Name_Buffer (I));
      end loop;
   end Write_Uint;

end Sprint;
