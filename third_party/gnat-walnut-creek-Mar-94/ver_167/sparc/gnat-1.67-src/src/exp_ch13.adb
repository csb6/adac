------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ C H 1 3                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.8 $                              --
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
with Exp_Ch3;  use Exp_Ch3;
with Nmake;    use Nmake;
with Rtsfind;  use Rtsfind;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Tbuild;   use Tbuild;

package body Exp_Ch13 is

   ------------------------------------------
   -- Expand_N_Attribute_Definition_Clause --
   ------------------------------------------

   --  Expansion action depends on attribute involved

   procedure Expand_N_Attribute_Definition_Clause (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      case Get_Attribute_Id (Chars (Identifier (N))) is

         --  Storage_Size

         when Attribute_Storage_Size =>

            --  If the type is a task type, then assign the value of the
            --  storage size to the Size variable associated with the task.

            --    task___Size := expression

            if Ekind (Etype (Name (N))) = E_Task_Type then
               Rewrite_Substitute_Tree (N,
                 Make_Assignment_Statement (Loc,
                   Name => Make_Identifier (Loc, 
                     Chars => New_External_Name (
                       Chars (Etype (Name (N))), "size")),
                   Expression =>
                     Make_Type_Conversion (Loc,
                       Subtype_Mark =>
                         New_Reference_To (RTE (RE_Size_Type), Loc),
                       Expression => Expression (N))));

            --  Other types for Storage_Size attribute need no expansion

            else
               null;
            end if;

         --  Other attributes require no expansion

         when others => null;

      end case;

   end Expand_N_Attribute_Definition_Clause;

   ----------------------------
   -- Expand_N_Freeze_Entity --
   ----------------------------

   procedure Expand_N_Freeze_Entity (N : Node_Id) is
      E : Entity_Id := Entity (N);

   begin
      --  Freeze the type if it is a record type, task type, or array type.
      --  Note that we explicitly check for a type, rather than use the
      --  Is_xxx_Type predicates, because these also include subtypes and
      --  there are no freeze actions for subtypes.

      if Ekind (E) = E_Record_Type 
        or else Ekind (E) = E_Task_Type 
        or else Ekind (E) = E_Array_Type
      then
         Freeze_Type (N);
      end if;
   end Expand_N_Freeze_Entity;

end Exp_Ch13;
