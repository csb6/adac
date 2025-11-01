------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              F R O N T D R V                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.5 $                              --
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

with Csets;       use Csets;
with Comperr;     use Comperr;
with Errcount;    use Errcount;
with Errout;      use Errout;
with Xref_Tab;    use Xref_Tab;
with Excep;       use Excep;
with Frontend;
with Gnatvsn;     use Gnatvsn;
with Xref;        use Xref;
with Lib;         use Lib;
with Namet;       use Namet;
with Opt;         use Opt;
with Osint;       use Osint;
with Output;      use Output;
with Par;
with System.Assertions;
with Types;       use Types;
with Usage;

procedure Frontdrv is

   Total_Warnings : Nat := 0;
   --  Counts total warnings in all files

   Total_Errors : Nat := 0;
   --  Counts total errors in all files

begin
   Initialize_OS_Interface (Compiler);
   Initialize_Xref;
   Initialize_Char_Tables;

   if Verbose_Mode or Full_List then
      Write_Eol;
      Write_Eol;
      Write_String ("GNAT Front End and Cross Reference Tool Version ");
      Write_String (Gnat_Version_String);
      Write_Eol;  
   end if;

   --  Output usage information if no files

   if not More_Source_Files then
      Usage;
      Exit_Program (E_Fatal);
   end if;

   --  Loop through files

   while More_Source_Files loop
      begin

         Frontend;

         --  Update total error counts

         Total_Warnings := Total_Warnings + Warnings_Detected;
         Total_Errors := Total_Errors + Errors_Detected;
         Finalize_Error_Output;

         --  Let the Xref gather what it needs

         if Total_Errors = 0 
              and then (With_Warnings or Spec_REQs_Flag or Body_REQs_Flag) 
         then
            Gather_Info (File.Table (Main_Unit).Cunit);
         end if;

      exception
         when Unrecoverable_Error =>
            Total_Warnings := Total_Warnings + Warnings_Detected;
            Total_Errors := Total_Errors + Errors_Detected;
            Finalize_Error_Output;
            Set_Standard_Error;
            Write_String ("compilation of ");
            Write_Name (File.Table (Main_Unit).File_Name);
            Write_String (" abandoned");
            Write_Eol;
            Set_Standard_Output;


         --  If we get an Assert failure, then we don't want to raise an 
         --  unhandled exception, but rather to use the normal compilation
         --  abort mechanism

         when System.Assertions.Assert_Failure =>
            Compiler_Error;
            Write_String ("internal error: assert failure");
            Compiler_Abort;
      end;

      Finalize_Lib;
      Finalize_Namet;
   end loop;

   if Total_Errors = 0 and With_Warnings then
      Write_Xref;
   end if;

   --  All done. Set proper exit status

   if Total_Errors > 0 then
      Exit_Program (E_Errors);
   else
      Exit_Program (E_Success);
   end if;

end Frontdrv;
