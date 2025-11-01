------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T 1 D R V                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.4 $                              --
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
with Csets;    use Csets;
with Back_End;
with Errcount; use Errcount;
with Errout;   use Errout;
with Excep;    use Excep;
with Frontend;
with Gnatvsn;  use Gnatvsn;
with Lib;      use Lib;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Par;
with Sinfo;    use Sinfo;
with System.Assertions;
with Types;    use Types;
with Usage;

procedure Gnat1drv is
   Main_Unit_Node : Node_Id;
   --  Compilation unit node for main unit

   Main_Kind : Node_Kind;
   --  Kind of main compilation unit node.

begin
   Initialize_OS_Interface (Compiler);
   Initialize_Char_Tables;

   if Verbose_Mode or Full_List then
      Write_Eol;
      Write_String ("NYU GNAT Compiler Version ");
      Write_String (Gnat_Version_String);
      Write_String (" (C) Copyright NYU, 1992-1993");
      Write_Eol;
   end if;

   Frontend;

   if Errors_Detected /= 0 then
      Finalize_Error_Output;
      Exit_Program (E_Errors);

   elsif Operating_Mode /= Generate_Code then
      Finalize_Lib;
      Finalize_Namet;
      return;
   end if;

   --  Check for unit that generates no code, and if so, generate
   --  warning message and suppress expander and code generation.

   Main_Unit_Node := File.Table (Main_Unit).Cunit;
   Main_Kind := Nkind (Unit (Main_Unit_Node));

   --  Generate code for subprogram bodies only if they have
   --  a corresponding non-generic subprogram declaration. Note
   --  that the check for No (Library_Unit) here is a defensive
   --  check that should not be necessary, since the Library_Unit
   --  field should always be set properly.

   if Main_Kind = N_Subprogram_Body
     and then (No (Library_Unit (Main_Unit_Node))
        or else Nkind (Unit (Library_Unit (Main_Unit_Node))) /=
                   N_Generic_Subprogram_Declaration)
   then
      null;

   --  Generate code for package bodies only if they have
   --  a corresponding non-generic package declaration

   elsif Main_Kind = N_Package_Body
     and then (No (Library_Unit (Main_Unit_Node))
        or else Nkind (Unit (Library_Unit (Main_Unit_Node))) /=
                   N_Generic_Package_Declaration)
   then
      null;

   --  Generate code for package declarations that do not
   --  require a corresponding body

   elsif Main_Kind = N_Package_Declaration
     and then not Body_Required (Main_Unit_Node)
   then
      null;

   --  Compilation units that are renamings do not require
   --  bodies either.

   elsif Main_Kind = N_Package_Renaming_Declaration
     or else Main_Kind = N_Subprogram_Renaming_Declaration
   then
      null;

   --  In all other cases (specs which have bodies, and generics)
   --  we cannot generate code and we generate a warning message.
   --  Note that generic instantiations are gone at this stage
   --  since they have been replaced by their instances.

   else
      Write_String ("No code generated for this unit");
      Write_Eol;
      return;
   end if;

   File.Table (Main_Unit).Generate_Code := True;

   --  If we have a corresponding spec, then we need object
   --  code for the spec unit as well

   if Nkind (Unit (Main_Unit_Node)) in N_Unit_Body
     and then not Acts_As_Spec (Main_Unit_Node)
   then
      File.Table
        (Get_Cunit_Unit_Number
          (Library_Unit
             (Main_Unit_Node))).Generate_Code := True;
   end if;

   --  Generate back end tables and library information

   Back_End;

   --  Only write the library if the backend did not generate any error
   --  messages. Otherwise signal errors to the driver program so that 
   --  there will be no attempt to generate an object file.

   if Errors_Detected /= 0 then
      Finalize_Error_Output;
      Exit_Program (E_Errors);
   else
      Lib.Writ;
      Finalize_Lib;
      Finalize_Namet;
   end if;

exception
   when Unrecoverable_Error =>
      Finalize_Error_Output;
      Set_Standard_Error;
      Write_String ("compilation abandoned");
      Write_Eol;
      Set_Standard_Output;
      Exit_Program (E_Errors);

   --  If we get an Assert failure, then we don't want to raise an unhandled
   --  exception, but rather to use the normal compilation abort mechanism

   when System.Assertions.Assert_Failure =>
      Compiler_Error;
      Write_String ("internal error: assert failure");
      Compiler_Abort;

end Gnat1drv;
