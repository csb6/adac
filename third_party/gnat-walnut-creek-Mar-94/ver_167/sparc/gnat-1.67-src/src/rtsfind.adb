------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              R T S F I N D                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.20 $                             --
--                                                                          --
--             Copyright (c) 1992,1993, NYU, All Rights Reserved            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms  of the GNU  General  Public  License  as  published  by the  Free --
-- Software  Foundation;  either version 2,  or (at your option)  any later --
-- version.  GNAT is distributed  in the hope  that it will be  useful, but --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANT- --
-- ABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public --
-- License  for  more details.  You should have received  a copy of the GNU --
-- General Public License along with GNAT;  see file COPYING. If not, write --
-- to the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Casing;   use Casing;
with Comperr;  use Comperr;
with Csets;    use Csets;
with Einfo;    use Einfo;
with Excep;    use Excep;
with Fname;    use Fname;
with Lib;      use Lib;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Output;   use Output;
with Sem;      use Sem;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Tbuild;   use Tbuild;

package body Rtsfind is

   ----------------
   -- Unit table --
   ----------------

   --  The unit table has one entry for each unit included in the definition
   --  of the type RTU_Id in the spec. The table entries are initialized in
   --  Initialize_Rtsfind to set the Entity field to Empty, indicating that
   --  the corresponding unit has not yet been loaded. The fields are set
   --  when a unit is loaded to contain the defining entity for the unit,
   --  the unit name, and the unit number.

   type RT_Unit_Table_Record is record
      Entity : Entity_Id;
      Uname  : Unit_Name_Type;
      Unum   : Unit_Number_Type;
   end record;

   RT_Unit_Table : array (RTU_Id) of RT_Unit_Table_Record;

   --------------------------
   -- Runtime Entity Table --
   --------------------------

   --  There is one entry in the runtime entity table for each entity that is
   --  included in the definition of the RE_Id type in the spec. The entries
   --  are set by Initialize_Rtsfind to contain Empty, indicating that the
   --  entity has not yet been located. Once the entity is located for the
   --  first time, its ID is stored in this array, so that subsequent calls
   --  for the same entity can be satisfied immediately.

   RE_Table : array (RE_Id) of Entity_Id;

   ------------------------
   -- Initialize_Rtsfind --
   ------------------------

   procedure Initialize_Rtsfind is
   begin
      --  Initialize the unit table

      for I in RTU_Id loop
         RT_Unit_Table (I).Entity := Empty;
      end loop;

      for I in RE_Id loop
         RE_Table (I) := Empty;
      end loop;
   end Initialize_Rtsfind;

   ---------
   -- RTE --
   ---------

   function RTE (E : RE_Id) return Entity_Id is
      U_Id : constant RTU_Id := RE_Unit_Table (E);
      U    : RT_Unit_Table_Record renames RT_Unit_Table (U_Id);

      Lib_Unit : Node_Id;
      Pkg_Ent  : Entity_Id;
      Ename    : Name_Id;
      Withn    : Node_Id;
      Loaded   : Boolean;

      --  Internal procedure called if we can't find the entity. The
      --  parameter is a detailed error message that is to be given

      procedure Failure (S : Str) is
         Ent_Name : constant String := RE_Id'Image (E);

      begin
         Set_Standard_Error;

         Write_String ("fatal error: runtime library configuration error");
         Write_Eol;

         Write_String ("cannot locate """);

         for I in 4 .. Ent_Name'Length loop
            Name_Buffer (Int (I) - 3) := To_Char (Ent_Name (I));
         end loop;

         Name_Len := Ent_Name'Length - 3;
         Set_Casing (Mixed_Case);
         Write_Str (Name_Buffer (1 .. Name_Len));
         Write_String (""" in file """);
         Write_Name (Get_File_Name (U.Uname));
         Write_String (""" (");
         Write_Str (S);
         Write_Char (')');
         Write_Eol;
         Set_Standard_Output;
         raise Unrecoverable_Error;
      end Failure;

   --  Start of processing for RTE

   begin
      --  Immediate return if entity previously located

      if Present (RE_Table (E)) then
         return RE_Table (E);
      end if;

      --  Load unit if unit table entry not previously built

      if No (U.Entity) then

         --  Build unit name from the enumeration literal name

         declare
            Uname_Chars : constant String := RTU_Id'Image (U_Id);
            Copy_From   : Int;
            Copy_To     : Int;

         begin
            --  Note the test for lower case letters here is clearly not
            --  necessary, it reflects a bug that was present at one time
            --  in Gigi and should disappear when this bug is fixed. TBSL.

            if Uname_Chars (4) = '_' then
               Copy_From := 5;
               Copy_To := 1;

            elsif Uname_Chars (4) = 'A' or else Uname_Chars (4) = 'a' then
               Copy_From := 6;
               Name_Buffer (1 .. 4) := "ada.";
               Copy_To := 5;

            elsif Uname_Chars (4) = 'S' or else Uname_Chars (4) = 's' then
               Copy_From := 6;
               Name_Buffer (1 .. 7) := "system.";
               Copy_To := 8;

            else
               Compiler_Abort;
            end if;

            for I in Integer (Copy_From) .. Uname_Chars'Last loop
               Name_Buffer (Int (I) - (Copy_From - Copy_To)) :=
                 Fold_Lower (To_Char (Uname_Chars (I)));
            end loop;

            Name_Len := Int (Uname_Chars'Length) - (Copy_From - Copy_To);
         end;

         --  Add %s at end for spec

         Name_Buffer (Name_Len + 1) := '%';
         Name_Buffer (Name_Len + 2) := 's';
         Name_Len := Name_Len + 2;

         U.Uname := Name_Find;
         Loaded := Is_Loaded (U.Uname);
         U.Unum := Load (U.Uname, False, Empty);

         if U.Unum = No_Unit then
            Failure ("unit not found");
         elsif File.Table (U.Unum).Fatal_Error then
            Failure ("parser errors");
         end if;

         --  Make sure that the unit is analyzed

         if not Analyzed (File.Table (U.Unum).Cunit) then
            Semantics (File.Table (U.Unum).Cunit);

            if File.Table (U.Unum).Fatal_Error then
               Failure ("semantic errors");
            end if;
         end if;

         Lib_Unit := Unit (File.Table (U.Unum).Cunit);
         U.Entity := Defining_Unit_Simple_Name (Specification (Lib_Unit));

         --  Add to with list if we loaded the unit

         if not Loaded then
            Withn :=
              Make_With_Clause (Standard_Location,
                Name => New_Reference_To (U.Entity, Standard_Location));
            Set_Library_Unit          (Withn, File.Table (U.Unum).Cunit);
            Set_Corresponding_Spec    (Withn, U.Entity);
            Set_First_Name            (Withn, True);
            Set_Implicit_With         (Withn, True);
            Mark_Rewrite_Insertion (Withn);
            Append (Withn, Context_Items (File.Table (Main_Unit).Cunit));
         end if;
      end if;

      --  Merge here with unit loaded

      Lib_Unit := Unit (File.Table (U.Unum).Cunit);

      --  In the subprogram case, we are all done

      if Nkind (Lib_Unit) = N_Subprogram_Declaration
        or else Nkind (Lib_Unit) = N_Subprogram_Body
        or else Nkind (Lib_Unit) = N_Procedure_Instantiation
        or else Nkind (Lib_Unit) = N_Function_Instantiation
      then
         RE_Table (E) := U.Entity;
         return RE_Table (E);

      --  Otherwise search the package entity chain for the entity we want

      else
         declare
            RE_Name_Chars : constant String := RE_Id'Image (E);

         begin
            --  Copy name skipping initial RE_ characters

            for I in 4 .. RE_Name_Chars'Last loop
               Name_Buffer (Int (I - 3)) :=
                 Fold_Lower (To_Char (RE_Name_Chars (I)));
            end loop;

            Name_Len := Int (RE_Name_Chars'Length - 3);
            Ename := Name_Find;
         end;

         Pkg_Ent := First_Entity (U.Entity);

         while Present (Pkg_Ent) loop
            if Ename = Chars (Pkg_Ent) then
               RE_Table (E) := Pkg_Ent;
               return Pkg_Ent;
            end if;

            Pkg_Ent := Next_Entity (Pkg_Ent);
         end loop;

         --  If we didn't find the unit we want, something is wrong!

         Failure ("entity undefined");
      end if;

   end RTE;
end Rtsfind;
