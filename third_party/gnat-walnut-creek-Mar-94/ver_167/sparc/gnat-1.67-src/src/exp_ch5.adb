------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 5                               --
--                                                                          --
--                                 B o d y                                  --
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

with Atree;    use Atree;
with Einfo;    use Einfo;
with Exp_Ch9;  use Exp_Ch9;
with Exp_Util; use Exp_Util;
with Nmake;    use Nmake;
with Rtsfind;  use Rtsfind;
with Sinfo;    use Sinfo;
with Sem;      use Sem;
with Sem_Util; use Sem_Util;
with Snames;   use Snames;
with Tbuild;   use Tbuild;

package body Exp_Ch5 is

   -----------------------------------
   -- Expand_N_Assignment_Statement --
   -----------------------------------

   --  For tagged types re-assign the original Tag of the target.
   --  ??? Could be implemented more efficiently as a kind of record slice 
   --  assignment but there no simple way to express this for the back-end.

   procedure Expand_N_Assignment_Statement (N : Node_Id) is
      Local_Tag_Name : Name_Id; 
      Loc            : constant Source_Ptr := Sloc (N);
      New_Assign     : Node_Id; 
      New_Name       : Node_Id;
      New_Exp        : Node_Id;
      Statements     : List_Id;

   begin
      if Is_Tagged_Type (Etype (Name (N))) then

         --  Generate the code

         --    declare
         --       tag__nn : Ada.Tags.Tag := lhs._tag;
         --    begin
         --       <if lhs is class-wide and Tag_Checks are on>
         --           if tag__nn /= rhs._tag then
         --              raise constraint_Error;
         --           end if;
         --
         --       lhs := rhs;
         --       lhs._tag := tag__nn;
         --    end;

         New_Name := New_Copy (Name (N));
         New_Exp  := New_Copy (Expression (N));
         New_Assign := New_Copy (N);
         Set_Name (New_Assign, New_Name);
         Set_Expression (New_Assign, New_Exp);

         --  ??? New_Name may contain some side-effect expression so it should 
         --  be evauate once. But if this flag is set, the assignment does
         --  not work at all so it is removed till further investigation.
         --  Set_Evaluate_Once (New_Name, True);

         Set_Evaluate_Once (New_Exp, True);
         Local_Tag_Name := New_Internal_Name ("tag");

         --  New_Assign is marked as already analyzed in order to avoid 
         --  a recursive re-expansion.

         Set_Analyzed (New_Assign, True); 

         Statements := New_List;

         if Is_Class_Type (Etype (New_Name))
              and then (not Tag_Checks_Suppressed (Etype (New_Name)))
         then
            Append_To (Statements,
              Make_If_Statement (Loc,
                Condition =>
                  Make_Op_Ne (Loc,
                    Left_Opnd => Make_Identifier (Loc, Local_Tag_Name),
                    Right_Opnd =>
                      Make_Selected_Component (Loc,
                        Prefix => New_Exp, 
                        Selector_Name => Make_Identifier (Loc, Name_uTag))),
                Then_Statements => New_List_1 (New_Constraint_Error (Loc))));
         end if;       

         Append_To (Statements, New_Assign);
         Append_To (Statements,
           Make_Assignment_Statement (Loc,
             Name => Make_Selected_Component (Loc,
                       Prefix => New_Name,
                       Selector_Name => Make_Identifier (Loc, Name_uTag)),
             Expression => Make_Identifier (Loc, Local_Tag_Name)));

         Rewrite_Substitute_Tree (N, 
           Make_Block_Statement (Loc,
             Identifier => Empty,
             Declarations => New_List_1 (Make_Object_Declaration (Loc,
               Defining_Identifier => 
                 Make_Defining_Identifier (Loc, Local_Tag_Name),
               Object_Definition => New_Reference_To (RTE (RE_Tag), Loc),
               Expression =>
                 Make_Selected_Component (Loc,
                   Prefix => New_Name,
                   Selector_Name => Make_Identifier (Loc, Name_uTag)))),
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
               Statements => Statements)));

         Analyze (N);
      end if;

   end Expand_N_Assignment_Statement;

   ------------------------------
   -- Expand_N_Block_Statement --
   ------------------------------

   --  Establish master if the block is a master
   --  Activate tasks if the block is an activator

   procedure Expand_N_Block_Statement (N : Node_Id) is
   begin
      Build_Task_Activation_Call (N);

      if Is_Task_Master (N) then
         Establish_Task_Master (N);
      end if;
   end Expand_N_Block_Statement;

   -----------------------------
   -- Expand_N_Case_Statement --
   -----------------------------

   --  If the last alternative is not an Others choice replace it with an
   --  N_Others_Choice. Note that we do not bother to call Analyze on the
   --  modified case statement, since it's only effect would be to compute
   --  the contents of the Others_Discrete_Choices node laboriously, and of
   --  course we already know the list of choices that corresponds to the
   --  others choice (it's the list we are replacing!)

   procedure Expand_N_Case_Statement (N : Node_Id) is
      Altnode     : constant Node_Id := Last (Alternatives (N));
      Others_Node : Node_Id;

   begin
      if Nkind (First (Discrete_Choices (Altnode))) /= N_Others_Choice then
         Others_Node := Make_Others_Choice (Sloc (Altnode));
         Set_Others_Discrete_Choices
           (Others_Node, Discrete_Choices (Altnode));
         Set_Discrete_Choices (Altnode, New_List_1 (Others_Node));
      end if;
   end Expand_N_Case_Statement;

   ---------------------------
   -- Expand_N_If_Statement --
   ---------------------------

   --  Add traceback before the IF. In addition, if an ELSE/ELSIF parts
   --  are present, add traceback calls at the start of the THEN, ELSIF
   --  and ELSE statements, so we know which way control went.

   procedure Expand_N_If_Statement (N : Node_Id) is
      Elsf : Node_Id;

   begin
      Traceback_Store (N);

      Traceback_Store (First (Then_Statements (N)));

      if List_Present (Elsif_Parts (N)) then
         Elsf := First (Elsif_Parts (N));

         while Present (Elsf) loop
            Traceback_Store (First (Then_Statements (Elsf)));
            Elsf := Next (Elsf);
         end loop;
      end if;

      if List_Present (Else_Statements (N)) then
         Traceback_Store (First (Else_Statements (N)));
      end if;
   end Expand_N_If_Statement;

end Exp_Ch5;
