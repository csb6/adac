------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               E X P A N D                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.35 $                             --
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
with Debug_Sem; use Debug_Sem;
with Exp_Attr;  use Exp_Attr;
with Exp_Ch2;   use Exp_Ch2;
with Exp_Ch3;   use Exp_Ch3;
with Exp_Ch4;   use Exp_Ch4;
with Exp_Ch5;   use Exp_Ch5;
with Exp_Ch6;   use Exp_Ch6;
with Exp_Ch7;   use Exp_Ch7;
with Exp_Ch8;   use Exp_Ch8;
with Exp_Ch9;   use Exp_Ch9;
with Exp_Ch10;  use Exp_Ch10;
with Exp_Ch11;  use Exp_Ch11;
with Exp_Ch12;  use Exp_Ch12;
with Exp_Ch13;  use Exp_Ch13;
with Exp_Prag;  use Exp_Prag;
with Exp_Util;  use Exp_Util;
with Opt;       use Opt;
with Output;    use Output;
with Sinfo;     use Sinfo;
with Sinput;    use Sinput;

procedure Expand (N : Node_Id) is
begin
   if Expander_Active then
      pragma Debug (Debug_A_Entry ("expanding  ", N));

      --  Processing depends on node kind. For full details on the expansion
      --  activity required in each case, see bodies of corresponding routines

      case Nkind (N) is

         when N_Allocator =>
            Expand_N_Allocator (N);

         when N_Assignment_Statement =>
            Expand_N_Assignment_Statement (N);

         when N_Attribute_Definition_Clause =>
            Expand_N_Attribute_Definition_Clause (N);

         when N_Attribute_Reference =>
            Expand_N_Attribute_Reference (N);

         when N_Block_Statement =>
            Expand_N_Block_Statement (N);

         when N_Case_Statement =>
            Expand_N_Case_Statement (N);

         when N_Expanded_Name =>
            Expand_N_Expanded_Name (N);

         when N_Freeze_Entity =>
            Expand_N_Freeze_Entity (N);

         when N_Full_Type_Declaration =>
            Expand_N_Full_Type_Declaration (N);

         when N_Function_Call =>
            Expand_N_Function_Call (N);

         when N_If_Statement =>
            Expand_N_If_Statement (N);

         when N_Identifier =>
            Expand_N_Identifier (N);

         when N_Object_Declaration =>
            Expand_N_Object_Declaration (N);

         when N_Op_And => 
            Expand_N_Op_And (N);

         when N_Op_Eq =>
            Expand_N_Op_Eq (N);

         when N_Op_Expon =>
            Expand_N_Op_Expon (N);

         when N_Op_Ge => 
            Expand_N_Op_Ge (N);

         when N_Op_Gt =>
            Expand_N_Op_Gt (N);

         when N_Op_In =>
            Expand_N_Op_In (N);

         when N_Op_Le =>
            Expand_N_Op_Le (N);

         when N_Op_Lt =>
            Expand_N_Op_Lt (N);

         when N_Op_Not =>
            Expand_N_Op_Not (N);

         when N_Op_Not_In =>
            Expand_N_Op_Not_In (N);

         when N_Op_Or =>
            Expand_N_Op_Or (N);

         when N_Op_Xor =>
            Expand_N_Op_Xor (N);

         when N_Package_Body =>
            Expand_N_Package_Body (N);

         when N_Parenthesized_Expression =>
            Expand_N_Parenthesized_Expression (N);

         when N_Pragma =>
            Expand_N_Pragma (N);

         when N_Procedure_Call_Statement =>
            Expand_N_Procedure_Call_Statement (N);

         when N_Slice =>
            Expand_N_Slice (N);

         when N_Subprogram_Body =>
            Expand_N_Subprogram_Body (N);

         when N_Single_Task_Declaration =>
            Expand_N_Single_Task_Declaration (N);

         when N_Task_Body =>
            Expand_N_Task_Body (N);

         when N_Task_Type_Declaration =>
            Expand_N_Task_Type_Declaration (N);

         when N_Variant_Part =>
            Expand_N_Variant_Part (N);

         --  For all other node kinds, no expansion activity is required

         when others => null;

      end case;

      pragma Debug (Debug_A_Exit ("expanding ", N, "  (done)"));
   end if;
end Expand;
