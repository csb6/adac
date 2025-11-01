------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P A R . L O A D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.16 $                             --
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

--  The Lib_Load procedure loads all units that are definitely required before
--  it makes any sense at all to proceed with semantic analysis, including
--  with'ed units, corresponding specs for bodies, parents of child specs,
--  and parents of subunits. All these units are loaded and pointers installed
--  in the tree as described in the spec of package Lib.

with Fname; use Fname;
with Uname; use Uname;
separate (Par)
procedure Load is

   File_Name : File_Name_Type;
   --  Name of file for current unit, derived from unit name

   Cur_Unum : Unit_Number_Type := Scan_Unit;
   --  Unit number of unit that we just finished parsing. Note that we need
   --  to capture this, because Scan_Unit will change as we parse new sources.

   Cunit : constant Node_Id := File.Table (Cur_Unum).Cunit;
   --  Compilation unit node for current compilation unit

   With_Cunit : Node_Id;
   --  Compilation unit node for withed unit

   Context_Node : Node_Id;
   --  Next node in context items list

   With_Node : Node_Id;
   --  N_With_Clause node

   Spec_Name : Unit_Name_Type;
   --  Unit name of required spec

   Body_Name : Unit_Name_Type;
   --  Unit name of corresponding body

   Unum : Unit_Number_Type;
   --  Unit number of loaded unit

   Spec_Node : Node_Id;
   --  Package spec node

begin
   --  Don't do any loads if we already had a fatal error

   if File.Table (Cur_Unum).Fatal_Error then
      return;
   end if;

   --  First step, make sure that the unit name matches the file name
   --  Do this only if no errors have been detected, since otherwise it
   --  may well not be interesting, and moreover, blowups are possible
   --  if no good name was found.

   File_Name := Get_File_Name (File.Table (Cur_Unum).Unit_Name);

   if File_Name /= File.Table (Cur_Unum).File_Name then
      Error_Msg_Name_1 := File_Name;
      Error_Msg
        ("?file name does not match unit name, should be{", Sloc (Cunit));
   end if;

   --  Now we load with'ed units, loop through context items

   Context_Node := First (Context_Items (Cunit));
   while Present (Context_Node) loop

      if Nkind (Context_Node) = N_With_Clause then
         With_Node := Context_Node;
         Spec_Name := Get_Unit_Name (With_Node);

         Unum := Lib.Load (Spec_Name, False, With_Node);

         --  If we find the unit, then set spec pointer in the N_With_Clause
         --  to point to the compilation unit for the spec. Remember that
         --  the Load routine itself sets our Fatal_Error flag if the loaded
         --  unit gets a fatal error, so we don't need to worry about that.

         if Unum /= No_Unit then
            Set_Library_Unit (With_Node, File.Table (Unum).Cunit);

         --  If the spec isn't found, then try finding the corresponding
         --  body, since it is possible that we have a subprogram body
         --  that is acting as a spec (since no spec is present).

         else
            Body_Name := Get_Body_Name (Spec_Name);
            Unum := Lib.Load (Body_Name, False, With_Node);

            --  If we got a subprogram body, then mark that we are using
            --  the body as a spec in the file table, and set the spec
            --  pointer in the N_With_Clause to point to the body entity.

            if Unum /= No_Unit
              and then Nkind (Unit (File.Table (Unum).Cunit)) =
                                                    N_Subprogram_Body
            then
               File.Table (Unum).Body_Spec := True;
               With_Cunit := File.Table (Unum).Cunit;
               Set_Library_Unit (With_Node, With_Cunit);
               Set_Acts_As_Spec (With_Cunit, True);
               Set_Library_Unit (With_Cunit, With_Cunit);

               --  If we couldn't find the body, or if it wasn't a body spec
               --  then we are in trouble. We make one more call to Load to
               --  require the spec. We know it will fail of course, the
               --  purpose is to generate the required error message (we prefer
               --  that this message refer to the missing spec, not the body)

            else
               Unum := Lib.Load (Spec_Name, True, With_Node);
            end if;
         end if;
      end if;

      Context_Node := Next (Context_Node);
   end loop;

   --  If current unit is a body, load its corresponding spec

   if Nkind (Unit (Cunit)) = N_Package_Body
     or else Nkind (Unit (Cunit)) = N_Subprogram_Body
   then
      Spec_Name := Get_Spec_Name (File.Table (Cur_Unum).Unit_Name);
      Unum := Lib.Load (Spec_Name, False, Cunit);

      --  If we successfully load the unit, then set the spec pointer. Once
      --  again note that if the loaded unit has a fatal error, Load will
      --  have set our Fatal_Error flag to propagate this condition.

      if Unum /= No_Unit then
         Set_Library_Unit (Cunit, File.Table (Unum).Cunit);

      --  If we don't find the spec, then if we have a subprogram body, we
      --  are still OK, we just have a case of a body acting as its own spec

      elsif Nkind (Unit (Cunit)) = N_Subprogram_Body then
         Set_Acts_As_Spec (Cunit, True);
         Set_Library_Unit (Cunit, Cunit);

      --  Otherwise we do have an error, repeat the load request for the spec
      --  with Required set True to generate an appropriate error message.

      else
         Unum := Lib.Load (Spec_Name, True, Cunit);
      end if;

   --  If current unit is a child unit spec, load its parent

   elsif Nkind (Unit (Cunit)) = N_Package_Declaration
     or else Nkind (Unit (Cunit)) =  N_Subprogram_Declaration
     or else Nkind (Unit (Cunit)) in N_Generic_Declaration
     or else Nkind (Unit (Cunit)) in N_Generic_Instantiation
     or else Nkind (Unit (Cunit)) in N_Renaming_Declaration
   then
      Spec_Name := Get_Parent_Spec_Name (File.Table (Cur_Unum).Unit_Name);

      if Spec_Name /= No_Name then
         Unum := Lib.Load (Spec_Name, True, Cunit);

         if Unum /= No_Unit then
            Set_Parent_Spec (Unit (Cunit), File.Table (Unum).Cunit);
         end if;
      end if;

   --  If current unit is a subunit, then load its parent body

   elsif Nkind (Unit (Cunit)) = N_Subunit then
      Body_Name := Get_Parent_Body_Name (File.Table (Cur_Unum).Unit_Name);
      Unum := Lib.Load (Body_Name, True, Cunit);

      if Unum /= No_Unit then
         Set_Library_Unit (Cunit, File.Table (Unum).Cunit);
      end if;
   end if;
end Load;
