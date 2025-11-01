------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 X R E F                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.9 $                              --
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
with Comperr;  use Comperr;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Xref_Tab; use Xref_Tab;
with Lib;      use Lib;
with Namet;    use Namet;
with Osint;    use Osint;
with Output;   use Output;
with Sinfo;    use Sinfo;
with Sprint;   use Sprint;
with Stand;    use Stand;
with Unchecked_Deallocation;

package body Xref is

   use Unchecked_Access;


   --  File suffixes  --
   ---------------------

   Spec_REQ_Suffix : constant Str (1 .. 2) := ".r";
   Body_REQ_Suffix : constant Str (1 .. 3) := ".br";
   Org_Spec_Suffix : constant Str (1 .. 4) := ".org";


   --  Other data  --
   ------------------

   type Phase_Type is (Add_Entities_And_Refs, Unmark_Refs);  
   Phase : Phase_Type;
   --  Used to signal the Traverse_Node procedure what we're doing.
   --  Add_Entities_And_REfs : normal mode to build the Entity_Tables.
   --  Unmark_Refs           : special mode for multipass removal of unused
   --                          nodes within the program tree.

   type Node_Numbers_Type is array (Node_Id range <>) of Boolean;
   type Access_Node_Numbers_Type is access Node_Numbers_Type;
   Node_Numbers : Access_Node_Numbers_Type;
   --  Array of flags used within Traverse_Node. Set to True if the node
   --  has been visited.

   type List_Numbers_Type is array (List_Id range <>) of Boolean;
   type Access_List_Numbers_Type is access List_Numbers_Type;
   List_Numbers : Access_List_Numbers_Type;
   --  Array of flags used within Traverse_Node. Set to True if the list
   --  has been visited.

   type Entity_Association_Type is array (Node_Id range <>) of Entity_Acc;
   type Access_Entity_Association_Type is access Entity_Association_Type;
   Entity_Association : Access_Entity_Association_Type;
   --  Array to associate the entities in the program tree with their
   --  Corresponding data structures.

   type File_Record is record
      Create_REQ : Boolean       := False;
      Etbl       : Entity_Table_Acc;
   end record;
   type File_Array_Type is array (Unit_Number_Type range <>) of File_Record;
   type File_Array_Acc is access File_Array_Type;
   Loaded_Files : File_Array_Acc;
   --  Array that associates the loaded files with the Entity_Tables.
   --  Create_REQ is used in Write_Spec_REQs to build the REQ for a withed
   --  spec.

   type Ref_Array_Type is array (Int range <>) of Node_Id;
   type Ref_Array_Acc is access Ref_Array_Type;
   Ref_Buffer : Ref_Array_Acc;
   Ref_Length : Int;
   --  Array to buffer the found references until all entities are added
   --  to our entity lists.



   --  Local subprograms  --
   -------------------------

   procedure Allocate_Memory;
   --  Allocates the memory on the heap which is needed for the dynamic
   --  arrays.

   procedure Collect_Withs (Unum : Unit_Number_Type);
   --  Collect with clauses in the context clause of the given compilation
   --  unit.

   procedure Deallocate_Memory;
   --  Frees the memory on the heap which is needed for the dynamic arrays.

   procedure Init_Numbers;
   --  Clear Node_Numbers and List_Numbers to False (= unmarked).
   --  Use Init_Numbers before each call of Traverse_Node_Hierarchic.

   procedure Link_Subunits (The_Unit : Unit_Number_Type);
   --  Links the main Entity_Table with its loaded subunits.

   procedure Remove_Entities (The_Etbl : in     Entity_Table_Acc;
                              Found    : in out Boolean);
   --  Looks for entities with no references and removes both the fitting node
   --  and all its parents which also get redundant. This is done by a loop
   --  to provide succesive dead code removal.

   procedure Set_Spec_REQ_Flags (The_Etbl : Entity_Table_Acc);
   --  Sets the Create_REQ fields to create a required interface for all
   --  withed specs as well as to select the transitive specs.

   procedure Traverse_Node_Hierarchic (N : Node_Id);
   --  Recursive procedure traversing the program tree.
   --  Can be used to add information to the Entity_Tables as well as
   --  to remove references of a dead program part (for dead code elimination).
   --  Before each call of Traverse_Node_Hierarchic the procedure
   --  Init_Numbers must be called to initialize the node and list flags.
   --
   --  If an entity declaration is found, then the corresponding entity
   --  information gets immediately added to the fitting Entity_Table.
   --  Every reference which is found is buffered in Ref_Buffer to avoid
   --  the case of adding a reference pointing to an entity that doesn't yet
   --  appear in the fitting Entity_Table.

   procedure Traverse_Node_Linear;
   --  Linear procedure traversing the program tree.
   --  Can only be used to add information to the Entity_Tables.
   --  Is much faster than Traverse_Node_Hierarchic.
   --  
   --  If an entity declaration is found, then the corresponding entity
   --  information gets immediately added to the fitting Entity_Table.
   --  Every reference which is found is buffered in Ref_Buffer to avoid
   --  the case of adding a reference pointing to an entity that doesn't yet
   --  appear in the fitting Entity_Table.

   procedure Write_Org (The_Etbl : Entity_Table_Acc);
   --  Writes the original source of the given Entity_Table (GNAT -dw conform).
   --  Used to compare the required interfaces with it and then to create the
   --  correct provided interface. File suffix changes to Org_Spec_Suffix.

   procedure Write_Body_REQs;
   --  Writes the REQs for the body and its subunits the disk.
   --  The file name of a Body_REQ is the file name of the compilation unit,
   --  the suffix '.adb' replaced by Body_REQ_Suffix.

   procedure Write_Spec_REQs;
   --  Writes the REQs (one for each withed spec) to the disk.
   --  The file name of a Spec_REQ is the file name of the withed
   --  compilation unit, the suffix '.ads' replaced by 
   --  '.' & the name of the withing unit & Body_REQ_Suffix.
   --  e.g.   with Pack1;
   --         procedure Main is ...
   --  then we have the source file name main.adb and would get pack1.main.r
   --  with the Spec_REQ for Pack1 inside.


   ---------------------
   -- Allocate_Memory --
   ---------------------

   procedure Allocate_Memory is
   begin

      --  Flags for Traverse_Node_Hierarchic.

      Node_Numbers := new Node_Numbers_Type (First_Node_Id .. Last_Node_Id);
      List_Numbers := new List_Numbers_Type (First_List_Id .. Last_List_Id);

      --  Pointers for direct entity access.

      Entity_Association := new Entity_Association_Type
        (Last_Standard_Node_Id + 1 .. Last_Node_Id);

      --  Array to associate the loaded files with their corresponding
      --  Entity_Table.

      Loaded_Files := new File_Array_Type   (File.First    .. File.Last);

      --  Buffer for found references.

      Ref_Buffer   := new Ref_Array_Type    (1             .. 
        Int (Last_Node_Id - Last_Standard_Node_Id));
      Ref_Length   := 0;

   end Allocate_Memory;


   -------------------
   -- Collect_Withs --
   -------------------

   procedure Collect_Withs (Unum : Unit_Number_Type) is
      Item        : Node_Id;
      Withed_Unit : Unit_Number_Type;

   begin
      Item := First (Context_Items (File.Table (Unum).Cunit));
      while Present (Item) loop

         if Nkind (Item) = N_With_Clause then
            Withed_Unit := Get_Cunit_Unit_Number (Library_Unit (Item));
            Add_With (Loaded_Files (Unum).Etbl, 
              Loaded_Files (Withed_Unit).Etbl);
         end if;

         Item := Next (Item);
      end loop;
   end Collect_Withs;

   -----------------------
   -- Deallocate_Memory --
   -----------------------

   procedure Deallocate_Memory is

      procedure Free is new Unchecked_Deallocation (Node_Numbers_Type,
        Access_Node_Numbers_Type);

      procedure Free is new Unchecked_Deallocation (List_Numbers_Type,
        Access_List_Numbers_Type);

      procedure Free is new Unchecked_Deallocation (Entity_Association_Type,
        Access_Entity_Association_Type);

      procedure Free is new Unchecked_Deallocation (File_Array_Type,
        File_Array_Acc);

      procedure Free is new Unchecked_Deallocation (Ref_Array_Type,
        Ref_Array_Acc);

   begin

      Free (Node_Numbers);
      Free (List_Numbers);
      Free (Entity_Association);
      Free (Loaded_Files);
      Free (Ref_Buffer);

   end Deallocate_Memory;


   -----------------
   -- Gather_Info --
   -----------------

   procedure Gather_Info (Top : Node_Id) is

      Spec        : Unit_Number_Type;
      Unum        : Unit_Number_Type;
      To_Unum     : Unit_Number_Type;
      To_Entity   : Entity_Id;
      Ref_Node    : Node_Id;
      Main_Etbl   : Entity_Table_Acc;

   begin

      Allocate_Memory;   
      --  Allocation for the dynamic arrays.

      --  Associate the loaded files with the Entity_Tables.
      --  Ignore Zombies (e.g. the spec for a body acting as spec)!

      for I in Loaded_Files'range loop
         if File.Table (I).Source /= null then
            Add_Etbl (First_Etbl, Last_Etbl, I, Loaded_Files (I).Etbl);
         end if;
      end loop;

      Main_Etbl := Loaded_Files (Main_Unit).Etbl;

      Main_Etbl.RU := True;
      --  We mark the main Entity_Table as a 'required unit', that is
      --  we create a fully detailed Xref for it.

      --  Look for with clauses within the loaded files.
      --  Ignore Zombies (e.g. the spec for a body acting as spec) and
      --  do this only once ('Examined' signals a previous examination)!

      for I in Loaded_Files'range loop
         if File.Table (I).Source /= null
           and then not Loaded_Files (I).Etbl.Examined then
            Collect_Withs (I);
         end if;
      end loop;

      --  Here we link the multiple parts of an object
      --  (i.e. Spec/Body/Subunits) to give correct With_Warnings.

      case Main_Etbl.Status is

         when A_Body =>

            Spec := Get_Cunit_Unit_Number (Library_Unit (Top));
            if Spec_REQs_Flag then
               Write_Org (Loaded_Files (Spec).Etbl);
            end if;
            Loaded_Files (Spec).Create_REQ := True;

            --  In multifile mode we create full output for this spec.
            --  Otherwise we print only what is used.

            if Number_Of_Files > 1 then
               Loaded_Files (Spec).Etbl.RU := True;

            else
               Loaded_Files (Spec).Etbl.Status := Withed_Spec;
            end if;        

            Loaded_Files (Spec).Etbl.Successor := Main_Etbl;
            Main_Etbl.Predecessor := Loaded_Files (Spec).Etbl;
            Link_Subunits (Main_Unit);

         when Sub_Body =>

            Unum := Get_Cunit_Unit_Number (Library_Unit (Top));
            Loaded_Files (Unum).Etbl.RU := True;

            if Loaded_Files (Unum).Etbl.Status = A_Body then
               Spec := Get_Cunit_Unit_Number
                 (Library_Unit (File.Table (Unum).Cunit));
               if Spec_REQs_Flag then
                  Write_Org (Loaded_Files (Spec).Etbl);
               end if;

               --  In multifile mode we create full output for this spec.
               --  Otherwise we print only what is used.

               if Number_Of_Files > 1 then
                  Loaded_Files (Spec).Etbl.RU := True;

               else
                  Loaded_Files (Spec).Etbl.Status := Withed_Spec;
               end if;        

               Loaded_Files (Spec).Etbl.Successor :=
                 Loaded_Files (Unum).Etbl;
               Loaded_Files (Unum).Etbl.Predecessor := 
                 Loaded_Files (Spec).Etbl;
            end if;
            Link_Subunits (Unum);

         when Body_As_Spec =>

            Link_Subunits (Main_Unit);

         when A_Spec | Withed_Spec =>

            if Spec_REQs_Flag then
               Write_Org (Main_Etbl);
            end if;

      end case;

      Set_Spec_REQ_Flags (Main_Etbl);

      --  Search and add the entities to the entity tables.

      Phase := Add_Entities_And_Refs;
      Traverse_Node_Linear;

      --  Take the references from the buffer and add them to the
      --  fitting entities.

      for I in 1 .. Ref_Length loop
         Ref_Node := Ref_Buffer (I);
         Unum     := Get_Sloc_Unit_Number (Sloc (Ref_Node));
         To_Unum   := Get_Sloc_Unit_Number (Sloc (Entity (Ref_Node)));

         if not Loaded_Files (Unum).Etbl.Examined then
            To_Entity := Entity (Ref_Node);
            Add_Reference (Entity_Association (To_Entity),
              Loaded_Files (Unum).Etbl, Ref_Node);

         elsif Spec_REQs_Flag
           and then Loaded_Files (Unum).Create_REQ
           and then Unum = To_Unum then
            To_Entity := Entity (Ref_Node);
            Update_Reference (Entity_Association (To_Entity),
              Loaded_Files (Unum).Etbl, Ref_Node);

         end if;
      end loop;

      --  Write the Spec_REQs.

      if Spec_REQs_Flag then      
         Write_Spec_REQs;
      end if;

      --  Write the Body_REQs.

      if Body_REQs_Flag and then 
        Loaded_Files (Main_Unit).Etbl.Status in Body_As_Spec .. Sub_Body then
         Write_Body_REQs;
      end if;

      --  Mark the units to be Examined.

      for I in Loaded_Files'range loop
         if Loaded_Files (I).Etbl /= null then
            Loaded_Files (I).Etbl.Examined := True;
         end if;
      end loop;

      Deallocate_Memory;
      --  Deallocation for the dynamic arrays.

   end Gather_Info;


   ------------------
   -- Init_Numbers --
   ------------------

   procedure Init_Numbers is
   begin

      --  Set node flags to unvisited (except those of Standard).

      for I in Node_Numbers'First .. Last_Standard_Node_Id loop
         Node_Numbers (I) := True;
      end loop;
      for I in Last_Standard_Node_Id + 1 .. Node_Numbers'Last loop
         Node_Numbers (I) := False;
      end loop;

      --  Set list flags to unvisited (except those of Standard).

      for I in List_Numbers'First .. Last_Standard_List_Id loop
         List_Numbers (I) := True;
      end loop;
      for I in Last_Standard_List_Id + 1 .. List_Numbers'Last loop
         List_Numbers (I) := False;
      end loop;
   end Init_Numbers;   


   ---------------------
   -- Initialize_Info --
   ---------------------

   procedure Initialize_Xref is
   begin
      With_Warnings       := Debug_Flag_1 or Debug_Flag_2
                          or Debug_Flag_3 or Debug_Flag_4;

      Entity_Warnings     := Debug_Flag_2 or Debug_Flag_3
                          or Debug_Flag_4;

      Xref_Flag           := Debug_Flag_3 or Debug_Flag_4;

      Entity_Info_In_Xref := Debug_Flag_4;

      Spec_REQs_Flag      := Debug_Flag_Q;

      Body_REQs_Flag      := Debug_Flag_B;

   end Initialize_Xref;


   -------------------
   -- Link_Subunits --
   ------------------- 

   procedure Link_Subunits (The_Unit : Unit_Number_Type) is

   Etbl_Tmp  : Entity_Table_Acc := Loaded_Files (The_Unit).Etbl;
   Top_Node  : constant Node_Id := File.Table (The_Unit).Cunit;

   begin
      for I in Loaded_Files'range loop
         if Loaded_Files (I).Etbl /= null
           and then Loaded_Files (I).Etbl.Kind = Sub
           and then I /= The_Unit
           and then Library_Unit (File.Table (I).Cunit) = Top_Node
           and then Loaded_Files (I).Etbl.Predecessor = null
         then
            while Etbl_Tmp.Successor /= null loop
               Etbl_Tmp := Etbl_Tmp.Successor;
            end loop;

            Etbl_Tmp.Successor := Loaded_Files (I).Etbl;
            Loaded_Files (I).Etbl.Predecessor := Etbl_Tmp;
         end if;
      end loop;
   end Link_Subunits;


   ---------------------
   -- Remove_Entities --
   ---------------------

   procedure Remove_Entities (The_Etbl : in     Entity_Table_Acc;
                              Found    : in out Boolean) is

      E_Tmp : Entity_Acc;
      --  To store the current entity within the loop.


   --------------------
   -- Remove_Element --
   --------------------

   procedure Remove_Element (The_Node : Node_Id) is
   --  Removes the given node and also the surrounding Ada construct.

   L_Tmp : List_Id;
   P_Tmp : Node_Id;

   begin

      if Nkind (The_Node) in N_Entity then
      --  If we get a N_Defining_Identifier, N_Defining_Character_Literal or
      --  N_Defining_Operator_Symbol, then we have to distinguish between the
      --  various kinds of entities.

         case Ekind (The_Node) is

            --  We don't remove void entities since they are used internally

            when E_Void =>
               null;

            --  Processing for variable (this is slightly different from the
            --  processing for constant, is that really correct ???)

            when E_Variable =>

               P_Tmp := Parent (The_Node);
               if Is_List_Member (The_Node) then
                  L_Tmp := List_Containing (The_Node);

                  Traverse_Node_Hierarchic (The_Node);
                  --  To cancel the references in the subtree of The_Node.

                  Remove (The_Node);
                  if Is_Empty_List (L_Tmp) then
                     Remove_Element (P_Tmp);
                  end if;

               else
                  Traverse_Node_Hierarchic (The_Node);
                  --  To cancel the references in the subtree of The_Node.

                  Remove_Element (P_Tmp);
               end if;

            --  Don't do anything with components, since it is very tricky
            --  to find a hidden reference for a record component (e.g. in
            --  an aggregate, all components are implicitly referenced).

            when E_Component =>
               null;

            --  Main processing for most cases

            when E_Constant                |
                 E_Named_Integer           |
                 E_Named_Real              |
                 E_Enumeration_Type        |
                 E_Enumeration_Subtype     |
                 E_Character_Type          |
                 E_Boolean_Type            |
                 E_Integer_Type            |
                 E_Integer_Subtype         |
                 E_Modular_Type            |
                 E_Modular_Subtype         |
                 E_Float_Type              |
                 E_Float_Subtype           |
                 E_Fixed_Type              |
                 E_Fixed_Subtype           |
                 E_Decimal_Type            |
                 E_Decimal_Subtype         |
                 E_Array_Type              |
                 E_Array_Subtype           |
                 E_String_Type             |
                 E_String_Subtype          |
                 E_String_Literal_Subtype  |
                 E_Enum_Table_Type         |
                 E_Slice_Subtype           |
                 E_Record_Type             |
                 E_Class_Subtype           |
                 E_Class_Type              |
                 E_Record_Subtype          |
                 E_Access_Type             |
                 E_Access_Subtype          |
                 E_General_Access_Type     |
                 E_Subprogram_Type         |
                 E_Incomplete_Type         |
                 E_Limited_Type            |
                 E_Task_Type               |
                 E_Task_Subtype            |
                 E_Protected_Type          |
                 E_Protected_Subtype       |
                 E_Exception_Type          |
                 E_Discriminant            |
                 E_Loop                    |
                 E_Loop_Parameter          |
                 E_Block                   |
                 E_Label                   |
                 E_Entry                   | 
                 E_Protected_Object        |
                 E_Entry_Family            | 
                 E_Exception               =>

               if Is_List_Member (The_Node) then
                  L_Tmp := List_Containing (The_Node);
                  P_Tmp := Parent (The_Node);

                  Traverse_Node_Hierarchic (The_Node);
                  --  To cancel the references in the subtree of The_Node.

                  Remove (The_Node);
                  if Is_Empty_List (L_Tmp) then
                     Remove_Element (P_Tmp);
                  end if;

               else
                  Traverse_Node_Hierarchic (The_Node);
                  --  To cancel the references in the subtree of The_Node.

                  P_Tmp := Parent (The_Node);
                  Remove_Element (P_Tmp);
               end if;

            --  Private type processing

            when E_Private_Type |
                 E_Limited_Private_Type =>

               Remove_Element (Full_Declaration (The_Node));

               if Is_List_Member (The_Node) then
                  L_Tmp := List_Containing (The_Node);
                  P_Tmp := Parent (The_Node);

                  Traverse_Node_Hierarchic (The_Node);
                  --  To cancel the references in the subtree of The_Node.

                  Remove (The_Node);
                  if Is_Empty_List (L_Tmp) then
                     Remove_Element (P_Tmp);
                  end if;

               else
                  Traverse_Node_Hierarchic (The_Node);
                  --  To cancel the references in the subtree of The_Node.

                  P_Tmp := Parent (The_Node);
                  Remove_Element (P_Tmp);
               end if;

            when E_Enumeration_Literal =>
               null;
               --  Since it's very tricky to find a hidden reference 
               --  for an enumeration literal we do nothing
               --  (e.g. in a loop from type'first to type'last all literals
               --   are automatically referenced).

            when E_Function  |
                 E_Operator  |
                 E_Procedure |
                 E_Package   =>

               if Scope (The_Node) > Last_Standard_Node_Id then
               --  Compilation unit nodes are not removed since
               --  this causes a lot of trouble.

                  Traverse_Node_Hierarchic (The_Node);
                  --  To cancel the references in the subtree of The_Node.

                  P_Tmp := Parent (The_Node);
                  Remove_Element (P_Tmp);
                  --  There are only two possibilities in this case:
                  --  Either the parent is a N_Function_Specification or
                  --  a N_Procedure_Specification.

               end if;

            --  Cannot delete a parameter of a subprogram (or at least not 
            --  easily because it would have all sorts of consequences)

            when E_In_Parameter       |
                 E_Out_Parameter      |
                 E_In_Out_Parameter   =>

               null;

            --  For the moment, forget the case of generics and their formal
            --  parameters. Might try to do something later here ??? 

            when E_Generic_Package              |
                 E_Generic_Function             |
                 E_Generic_Procedure            |
                 E_Generic_In_Parameter         |
                 E_Generic_In_Out_Parameter     |
                 E_Formal_Derived_Type          |
                 E_Formal_Generic_Package       =>

               null;

            --  Body entities have no significance semantically, so ignore them

            when E_Task_Body        |
                 E_Subprogram_Body  |
                 E_Package_Body     |
                 E_Protected_Body   => 

               null;

            --  Internal allocator and access types should never appear
            --  at the stage a cross-reference is being computed

            when E_Allocator_Type          |
                 E_Access_Subprogram_Type  |
                 E_Anonymous_Access_Type   =>

               Compiler_Abort;
         end case;

      else
         case Nkind (The_Node) is

            when N_Empty =>
               null;

            when N_Function_Specification .. N_Procedure_Specification =>
               P_Tmp := Parent (The_Node);
               Remove_Element (P_Tmp);
               --  There are only two possibilities in this case:
               --  Either the parent is a N_Subprogram_Declaration or
               --  a N_Subprogram_Body.

            when N_Component_List | N_Record_Definition =>
               null;
               --  These are intermediaire nodes.
               --  In this cases we don't remove the parent
               --  (e.g. N_Record_Definition for a N_Component_List)
               --  since the Record identifier is treated separately.

            when N_Object_Declaration =>
               if Is_List_Member (The_Node) then
                  L_Tmp := List_Containing (The_Node);
                  P_Tmp := Parent (The_Node);

                  Traverse_Node_Hierarchic (The_Node);
                  --  To cancel the references in the subtree of The_Node.

                  Remove (The_Node);

                  --  Here we do *not* remove the parent since then we
                  --  would delete the whole subprogram.
               end if;

            when others =>
               if Is_List_Member (The_Node) then
                  L_Tmp := List_Containing (The_Node);
                  P_Tmp := Parent (The_Node);

                  Traverse_Node_Hierarchic (The_Node);
                  --  To cancel the references in the subtree of The_Node.

                  Remove (The_Node);
                  if Is_Empty_List (L_Tmp) then
                     Remove_Element (P_Tmp);
                  end if;

               else
                  Traverse_Node_Hierarchic (The_Node);
                  --  To cancel the references in the subtree of The_Node.

                  P_Tmp := Parent (The_Node);
                  Remove_Element (P_Tmp);

               end if;

         end case;
      end if;     

   end Remove_Element; 



   --------------------
   -- Remove_Pragmas --
   --------------------

   procedure Remove_Pragmas (The_Entity : Entity_Acc) is
   --  Removes all the pragma nodes within the program tree which
   --  apply on the given entity.

      R_Tmp : Ref_Acc := First (The_Entity);

   begin
      while not Is_Null (R_Tmp) loop

         if Is_Pragma (R_Tmp) then
            Remove_Element (Parent (Parent (The_Node (R_Tmp))));
         end if;
         R_Tmp := Next (R_Tmp);

      end loop;
   end Remove_Pragmas;


   --  begin of Remove_Entities --  

   begin
      E_Tmp := The_Etbl.First_Entity;
      Found := False;

      Phase := Unmark_Refs;
      Init_Numbers;   

      while not Is_Null (E_Tmp) loop

         if Number_Of_Marks (E_Tmp) = 0
           and then Give_Warning (E_Tmp) then

            --  Update the Entity_Table:

            Found := True;
            Mark_Entity (E_Tmp);
            --  We mark the entity in our entity table for the next pass
            --  to say: Don't remove this entity twice!

            --  Update the program tree:
            --
            --  Since we need E_Tmp to find the pragmas, we have to
            --  remove the pragmas here rather than in Remove_Element 

            if Entity_Type (E_Tmp) in E_Function .. E_Procedure then
               Remove_Pragmas (E_Tmp);
            end if;

            Remove_Element (Entity_Node (E_Tmp));
            --  Here we remove the entities with no references.

         end if;
         E_Tmp := Next (E_Tmp);      

      end loop;
   end Remove_Entities;


   -------------------------
   -- Set_Spec_REQ_Flags  --
   -------------------------

   procedure Set_Spec_REQ_Flags (The_Etbl : Entity_Table_Acc) is

   Etbl_Tmp    : Entity_Table_Acc;
   With_Clause : With_Acc;

   begin

      --  Mark a spec withed by The_Etbl or a predecessor.

      Etbl_Tmp := The_Etbl;   
      while Etbl_Tmp /= null loop

         With_Clause := Etbl_Tmp.First_With;
         while With_Clause /= null loop

            if With_Clause.Withed_Etbl.Kind /= Genr then
               Loaded_Files (Get_Cunit_Unit_Number 
                 (With_Clause.Withed_Etbl.Top_Node)).Create_REQ := True;
            end if;
            --  We don't create a REQ for a generic.

            With_Clause.Withed_Etbl.Status := Withed_Spec;
            With_Clause := With_Clause.Next_With;
         end loop;

         Etbl_Tmp := Etbl_Tmp.Predecessor;
      end loop;

      --  Mark a spec withed by a successor.

      Etbl_Tmp := The_Etbl.Successor;
      while Etbl_Tmp /= null loop

         With_Clause := Etbl_Tmp.First_With;
         while With_Clause /= null loop

            if With_Clause.Withed_Etbl.Kind /= Genr then
               Loaded_Files (Get_Cunit_Unit_Number 
                 (With_Clause.Withed_Etbl.Top_Node)).Create_REQ := True;
            end if;
            --  We don't create a REQ for a generic.

            With_Clause.Withed_Etbl.Status := Withed_Spec;
            With_Clause := With_Clause.Next_With;
         end loop;

         Etbl_Tmp := Etbl_Tmp.Successor;
      end loop;

   end Set_Spec_REQ_Flags;


   ------------------------------
   -- Traverse_Node_Hierarchic --
   ------------------------------

   procedure Traverse_Node_Hierarchic (N : Node_Id) is

      Kind        : Node_Kind;
      Unum        : Unit_Number_Type;
      To_Unum     : Unit_Number_Type;
      To_Entity   : Entity_Id;
      --  Only to reduce the length of some function calls.

      procedure Visit_List (L : List_Id);
      procedure Visit_Descendent (D : Int);
      pragma Inline (Visit_Descendent);


      ----------------
      -- Visit_List --
      ----------------

      procedure Visit_List (L : List_Id) is
      --  Visits the nodes of a list in the program tree

         N : Node_Id;

      begin
         if List_Numbers (L) = True then
            return;
            --  List is already visited

         else   
            List_Numbers (L) := True;
         end if;

         N := First (L);

         if N /= Empty then
            while Next (N) /= Empty loop
               Traverse_Node_Hierarchic (N);
               N := Next (N);
            end loop;
         end if;

         Traverse_Node_Hierarchic (N);
      end Visit_List;


      ----------------------
      -- visit_Descendent --
      ----------------------

      procedure Visit_Descendent (D : Int) is
      begin
         if D in Node_Range then
            if D = Int (Empty)
              or else D = Int (Error)
              or else (Parent (Node_Id (D)) /= Empty
                        and then Parent (Node_Id (D)) /= N)
            then
               return;
            else
               Traverse_Node_Hierarchic (Node_Id (D));
            end if;

         elsif D in List_Range then
            if D = Int (No_List)
              or else Is_Empty_List (List_Id (D))
              or else (List_Parent (List_Id (D)) /= Empty
                        and then List_Parent (List_Id (D)) /= N)
            then
               return;
            else
               Visit_List (List_Id (D));
            end if;

         end if;
      end Visit_Descendent;


   --  Start of Traverse_Node_Hierarchic

   begin
      if N = Empty then
         return;
      end if;

      if Node_Numbers (N) = True then
         return; -- already visited
      else
         Node_Numbers (N) := True;
      end if;

      Kind := Nkind (N);      

      case Phase is

         when Add_Entities_And_Refs =>

            --  Look for named numbers or other constructs which get
            --  transformed into the corresponding litterals during
            --  semantics.

            if Is_Rewrite_Substitution (N) then 
               Traverse_Node_Hierarchic (Original_Node (N));
            end if;

            if Kind in N_Entity
              and then Ekind (N)  /= E_Void
              and then not Is_Internal (N) 
              and then Parent (N) /= Empty then
               Unum := Get_Sloc_Unit_Number (Sloc (N));

               if Loaded_Files (Unum).Etbl.Examined then
                  Update_Entity (Loaded_Files (Unum).Etbl,
                    N, Entity_Association (N));
               else
                  Add_Entity (Loaded_Files (Unum).Etbl,
                    N, Entity_Association (N));
               end if;       

            elsif Kind = N_Identifier then
               if Nkind (Parent (N)) = N_Range
                 and then Parent (Parent (N)) = Empty then
                  --  We don't accept things like an internal range
                  --  declaration.

                  null;
               elsif Entity (N) > Last_Standard_Node_Id then
               --  We suppress references to standard entities (e.g. integer)

                  Ref_Length := Ref_Length + 1;
                  Ref_Buffer (Ref_Length) := N;
               end if;

            elsif (Kind in N_Op
              or else Kind = N_Attribute_Reference
              or else Kind = N_Character_Literal
              or else Kind = N_Expanded_Name
              or else Kind = N_Operator_Symbol)
              and then Entity (N) > Last_Standard_Node_Id then

               Ref_Length := Ref_Length + 1;
               Ref_Buffer (Ref_Length) := N;

            end if;

         when Unmark_Refs =>

            --  Look for named numbers or other constructs which get
            --  transformed into the corresponding litterals during
            --  semantics.

            if Is_Rewrite_Substitution (N) then 
               Traverse_Node_Hierarchic (Original_Node (N));
            end if;

            if Kind in N_Entity
              and then Ekind (N)  /= E_Void
              and then not Is_Internal (N) 
              and then Parent (N) /= Empty then

               --  We mark the entity and all its descendents in our entity 
               --  table for the next pass to say:
               --  Don't remove this entity twice! It's already removed.

               Unum := Get_Sloc_Unit_Number (Sloc (N));
               Mark_Entity (In_E_List (Loaded_Files (Unum).Etbl, N));

            elsif Kind = N_Identifier then
               if Nkind (Parent (N)) = N_Range
                 and then Parent (Parent (N)) = Empty then
                  --  We don't accept things like an internal range
                  --  declaration.

                  null;
               elsif Entity (N) > Last_Standard_Node_Id then
               --  We suppress references to standard entities (e.g. integer)

                  To_Unum   := Get_Sloc_Unit_Number (Sloc (Entity (N)));
                  To_Entity := Entity (N);
                  Unmark_Reference (Entity_Association (To_Entity), N);
               end if;

            elsif (Kind in N_Op
              or else Kind = N_Attribute_Reference
              or else Kind = N_Character_Literal
              or else Kind = N_Expanded_Name
              or else Kind = N_Operator_Symbol)
              and then Entity (N) > Last_Standard_Node_Id then

               To_Unum   := Get_Sloc_Unit_Number (Sloc (Entity (N)));
               To_Entity := Entity (N);
               Unmark_Reference (Entity_Association (To_Entity), N);
            end if;

      end case;

      Visit_Descendent (Field1 (N));
      Visit_Descendent (Field2 (N));
      Visit_Descendent (Field3 (N));
      Visit_Descendent (Field4 (N));
      Visit_Descendent (Field5 (N));

      if Has_Extension (N) then
         Visit_Descendent (Field6 (N));
         Visit_Descendent (Field7 (N));
         Visit_Descendent (Field8 (N));
         Visit_Descendent (Field9 (N));
         Visit_Descendent (Field10 (N));
         Visit_Descendent (Field11 (N));
         Visit_Descendent (Field12 (N));
      end if;
   end Traverse_Node_Hierarchic;


   --------------------------
   -- Traverse_Node_Linear --
   --------------------------

   procedure Traverse_Node_Linear is

      Kind        : Node_Kind;
      E_Kind      : Entity_Kind;
      Unum        : Unit_Number_Type;
      --  Only to reduce the length of some function calls.

      Skip_Next   : Boolean := False;

   begin
      for N in Last_Standard_Node_Id + 1 .. Last_Node_Id loop

         if N = Empty then
            null;

         elsif Skip_Next then
            Skip_Next := False;

         else

            Kind := Nkind (N);      

            --  Look for named numbers or other constructs which get
            --  transformed into the corresponding litterals during
            --  semantics.

            if Kind in N_Entity then

               if not Has_Extension (N) then
                  null;
                  --  Zombie node (unreachable from the top node).

               else
                  Skip_Next := True;
                  --  Skip the extended fields node which follows
                  --  immediately.

                  E_Kind := Ekind (N);

                  --  Special treatment for some ticklish cases:
                  --
                  --  To avoid double entities within the cross
                  --  references we have to skip some nodes.
                  --  The completions for private types can be marked as
                  --  internal since we have a pointer to them.
                  --  The completions for deferred constants don't have a
                  --  pointer to their completion but they have an empty
                  --  scope node since they don't pass semantics.


                  if E_Kind = E_Private_Type then
                     Set_Is_Internal (Full_Declaration (N));
                  end if;

                  if (E_Kind = E_Constant and then Scope (N) = Empty)
                    or else E_Kind = E_Void
                    or else Is_Internal (N)
                    or else Parent (N) = Empty then

                     null;
                  else

                     Unum := Get_Sloc_Unit_Number (Sloc (N));
                     if Loaded_Files (Unum).Etbl.Examined then

                        --  Update the entity if its already in the
                        --  Entity_Table but with a wrong Node_Id.

                        Update_Entity (Loaded_Files (Unum).Etbl,
                        N, Entity_Association (N));
                     else

                        --  Add the entity to the end of the Entity_Table.

                        Add_Entity (Loaded_Files (Unum).Etbl,
                          N, Entity_Association (N));
                     end if;       
                  end if;
               end if;

            elsif Kind = N_Identifier then
               if Nkind (Parent (N)) = N_Range
                 and then Parent (Parent (N)) = Empty then
                  --  We don't accept things like an internal range
                  --  declaration.

                  null;
               elsif Entity (N) > Last_Standard_Node_Id then
               --  We suppress references to standard entities (e.g. integer)

                  Ref_Length := Ref_Length + 1;
                  Ref_Buffer (Ref_Length) := N;
               end if;

            elsif (Kind in N_Op
              or else Kind = N_Attribute_Reference
              or else Kind = N_Character_Literal
              or else Kind = N_Expanded_Name
              or else Kind = N_Operator_Symbol)
              and then Entity (N) > Last_Standard_Node_Id then

               Ref_Length := Ref_Length + 1;
               Ref_Buffer (Ref_Length) := N;

            end if;
         end if;

      end loop;
   end Traverse_Node_Linear;


   ---------------
   -- Write_Org --
   ---------------

   procedure Write_Org (The_Etbl : Entity_Table_Acc) is

   begin

      Name_Len := The_Etbl.File_Name'Length;
      Name_Buffer (1 .. Name_Len) := The_Etbl.File_Name.all;
      Name_Buffer (Name_Len - 3 .. 
                   Name_Len - 4 + Org_Spec_Suffix'Length) := Org_Spec_Suffix;
      Name_Buffer (Name_Len - 3 + Org_Spec_Suffix'Length) := NUL;
      --  In an earlier version of Gnat this was done automatically

      Create_Req_Output;

      Sprint_Node (The_Etbl.Top_Node);

      Write_Eol;      
      Set_Standard_Output;
      Close_Xref_Output;

   end Write_Org;                              


   ---------------------
   -- Write_Body_REQs --
   ---------------------

   procedure Write_Body_REQs is

      Main_Etbl  : Entity_Table_Acc := Loaded_Files (Main_Unit).Etbl;   
      Etbl_Tmp_1 : Entity_Table_Acc;
      Etbl_Tmp_2 : Entity_Table_Acc;

      First : Boolean;
      Found : Boolean;

      ---------------
      -- Open_File --
      ---------------

      procedure Open_File (The_Etbl : Entity_Table_Acc) is

      begin

         --  Here we build the file name of the Body_REQ.
         --  The file name is the file name of the body,
         --  the suffix '.adb' changed into Body_REQ_Suffix !

         Name_Len := The_Etbl.File_Name.all'Length;   
         Name_Buffer (1 .. Name_Len) := The_Etbl.File_Name.all;
         Name_Buffer (Name_Len - 3 .. Name_Len - 4 + Body_REQ_Suffix'Length)
           := Body_REQ_Suffix;
         Name_Buffer (Name_Len - 3 + Body_REQ_Suffix'Length) := NUL;

         Create_Req_Output;         
      end Open_File;


      ----------------
      -- Close_File --
      ----------------

      procedure Close_File is
      begin
         Write_Eol;
         Set_Standard_Output;
         Close_Xref_Output;
      end Close_File;


   begin

      Etbl_Tmp_1 := Main_Etbl;
      Etbl_Tmp_2 := Main_Etbl;
      First      := True;

      while Etbl_Tmp_1 /= null loop

         --  First we have to mark the body and its subunits across each
         --  others.

         while Etbl_Tmp_2 /= null loop

            if First then                                          
               Clear_And_Mark_Xrefs (Etbl_Tmp_1, Etbl_Tmp_2);
               First := False;
            else
               Mark_Xrefs (Etbl_Tmp_1, Etbl_Tmp_2);
            end if;

            Etbl_Tmp_2 := Etbl_Tmp_2.Successor;
         end loop;

         --  Then remove the entities with no references step by step
         --  until all the remaining entities have one or more 
         --  references.
         --  This must be done within a loop because after having 
         --  removed some entities an earlier defined entity may become
         --  removable.

         --  Write_Org (Etbl_Tmp_1);  can be inserted to measure dead code!

         Found := True;
         while Found loop
            Remove_Entities (Etbl_Tmp_1, Found);
         end loop;

         Open_File (Etbl_Tmp_1);
--  CC
--  don't touch the indentation now. 
--  has to be fixed later
--
--       Xsprint.Indent := 0;
         Sprint_Node (Etbl_Tmp_1.Top_Node);
         Close_File;

         Etbl_Tmp_1 := Etbl_Tmp_1.Successor;         
         Etbl_Tmp_2 := Main_Etbl;
         First      := True;
      end loop;
   end Write_Body_REQs;


   ---------------------
   -- Write_Spec_REQs --
   ---------------------

   procedure Write_Spec_REQs is

      Buffer : Str (1 .. 100);
      Length : Int;
      Found  : Boolean;

      Etbl_Tmp   : Entity_Table_Acc;

      ---------------
      -- Open_File --
      ---------------

      procedure Open_File (I : Unit_Number_Type) is

      begin

         --  Here we build the file name of the Spec_REQ.
         --  The file name is the file name of the withed unit,
         --  the suffix changed from '.ads' into
         --  '.' & withing_unit_name & Spec_REQ_Suffix !

         Get_Name_String (File.Table (Main_Unit).File_Name);
         Buffer (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);
         Length := Name_Len - 3;

         Get_Name_String (File.Table (I).File_Name);
         Name_Buffer (Name_Len + 1 .. Length + Name_Len) :=
            Buffer (1 .. Length);
         Length := Length + Name_Len - 4;

         Name_Buffer (Length + 1 .. Length + Spec_REQ_Suffix'Length) 
           := Spec_REQ_Suffix;
         Name_Buffer (Length + Spec_REQ_Suffix'Length + 1) := NUL;

         Create_Req_Output;
      end Open_File;


      ----------------
      -- Close_File --
      ----------------

      procedure Close_File is
      begin
         Write_Eol;
         Set_Standard_Output;
         Close_Xref_Output;
      end Close_File;


   begin
      for I in Loaded_Files'range loop

         if Loaded_Files (I).Create_REQ then

            --  First we mark all the entities which are used by the withed
            --  spec itself.

            Clear_And_Mark_Xrefs (Loaded_Files (I).Etbl,
              Loaded_Files (I).Etbl);

            --  Then we mark all the entities which are used by the main object
            --  (Main_Unit, all predecessors and all successors).

            Etbl_Tmp := Loaded_Files (Main_Unit).Etbl;   
            while Etbl_Tmp /= null loop
               Mark_Xrefs (Loaded_Files (I).Etbl, Etbl_Tmp);
               Etbl_Tmp := Etbl_Tmp.Predecessor;
            end loop;

            Etbl_Tmp := Loaded_Files (Main_Unit).Etbl.Successor;
            while Etbl_Tmp /= null loop
               Mark_Xrefs (Loaded_Files (I).Etbl, Etbl_Tmp);
               Etbl_Tmp := Etbl_Tmp.Successor;
            end loop;

            --  Then remove the entities with no references step by step
            --  until all the remaining entities have one or more 
            --  references.
            --  This must be done within a loop because after having 
            --  removed some entities an earlier defined entity may become
            --  removable.

            Found := True;
            while Found loop
               Remove_Entities (Loaded_Files (I).Etbl, Found);
            end loop;

            Open_File (I);
--  CC
--  don't touch the indentation now. 
--  has to be fixed later
--
--            Xsprint.Indent := 0;
            Sprint_Node (File. Table (I).Cunit);
            Close_File;
         end if;

      end loop;
   end Write_Spec_REQs;


   ----------------
   -- Write_Xref --
   ----------------

   procedure Write_Xref is

      Etbl_Tmp : Entity_Table_Acc;

   begin
      if Xref_Flag then

         --  File and screen output

         Create_Xref_Output; 
         Etbl_Tmp := First_Etbl;
         while Etbl_Tmp /= null loop

            if Etbl_Tmp.RU then
               Writ (Etbl_Tmp, Full_Xref);
            elsif Etbl_Tmp.Status = Withed_Spec then
               Writ (Etbl_Tmp, Smart_Xref);
            end if;

            Etbl_Tmp := Etbl_Tmp.Next_Etbl;
         end loop;
         Close_Xref_Output;

      elsif With_Warnings then

         --  Only screen output

         Etbl_Tmp := First_Etbl;
         while Etbl_Tmp /= null loop

            if Etbl_Tmp.RU then
               Writ (Etbl_Tmp, Full_Only_Screen);
            elsif Etbl_Tmp.Status = Withed_Spec then
               Writ (Etbl_Tmp, Smart_Only_Screen);
            end if;

            Etbl_Tmp := Etbl_Tmp.Next_Etbl;
         end loop;
      end if;

   end Write_Xref;
end Xref;
