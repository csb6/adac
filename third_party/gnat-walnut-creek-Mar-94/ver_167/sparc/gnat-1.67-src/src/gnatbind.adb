------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T B I N D                              --
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

with ALI;      use ALI;
with Bcheck;   use Bcheck;
with Binde;    use Binde;
with Binderr;  use Binderr;
with Bindgen;  use Bindgen;
with Bindusg;
with Butil;    use Butil;
with Errcount; use Errcount;
with Excep;    use Excep;
with Gnatvsn;  use Gnatvsn;
with Libfmt;   use Libfmt;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Types;    use Types;

procedure Gnatbind is

   Total_Errors : Nat := 0;
   --  Counts total errors in all files

   Total_Warnings : Nat := 0;
   --  Total warnings in all files

   Main_Lib_File : File_Name_Type;
   --  Current main library file

begin
   Initialize_OS_Interface (Binder);

   if Verbose_Mode then
      Write_Eol;
      Write_String ("NYU GNAT Binder Version ");
      Write_String (Gnatbind_Version_String);
      Write_String (" (C) NYU, 1992-1993, All Rights Reserved");
      Write_Eol;
   end if;

   --  Output usage information if no files

   if not More_Lib_Files then
      Bindusg;
      Exit_Program (E_Fatal);
   end if;

   --  The block here is to catch the Unrecoverable_Error exception in the
   --  case where we exceed the maximum number of permissible errors or some
   --  other unrecoverable error occurs.

   begin

      --  Carry out package initializations. These are initializations which
      --  might logically be performed at elaboration time, but Namet at
      --  least can't be done that way (because it is used in the Compiler),
      --  and we decide to be consistent. Like elaboration, the order in
      --  which these calls are made is in some cases important.

      Initialize_Namet;
      Initialize_Binderr;
      Initialize_ALI;

      if Verbose_Mode then
         Write_Eol;
      end if;

      --  Input ALI files

      while More_Lib_Files loop
         Main_Lib_File := Next_Main_Lib_File;

         if Verbose_Mode then
            if Check_Only then
               Write_String ("Checking: ");
            else
               Write_String ("Binding: ");
            end if;

            Write_Name (Main_Lib_File);
            Write_Eol;
         end if;

         Read_ALI (Main_Lib_File, No_File);
      end loop;

      --  Build source file from the ALI files we have read in

      Set_Source_Table;

      --  Check that main library file is a suitable main program

      if Bind_Main_Program
        and then ALIs.Table (ALIs.First).Main_Program = None
      then
         Error_Msg_Name_1 := Main_Lib_File;
         Error_Msg ("% does not contain a unit that can be a main program");
      end if;

      --  Perform consistency checks

      Check_Versions;
      Check_Consistency;

      --  Complete bind if no errors

      if Errors_Detected = 0 then
         Find_Elab_Order;

         if Errors_Detected = 0 then
            if Elab_Order_Output then
               Write_Eol;
               Write_String ("ELABORATION ORDER");
               Write_Eol;

               for I in Elab_Order.First .. Elab_Order.Last loop
                  Write_String ("   ");
                  Write_Unit_Name (Unit.Table (Elab_Order.Table (I)).Uname);
                  Write_Eol;
               end loop;

               Write_Eol;
            end if;

            if not Check_Only then
               Gen_Output_File;
            end if;
         end if;
      end if;

      Total_Errors := Total_Errors + Errors_Detected;
      Total_Warnings := Total_Warnings + Warnings_Detected;

   exception
      when Unrecoverable_Error =>
         Total_Errors := Total_Errors + Errors_Detected;
         Total_Warnings := Total_Warnings + Warnings_Detected;
   end;

   --  All done. Set proper exit status. 

   Finalize_Binderr;
   Finalize_Namet;

   if Total_Errors > 0 then
      Exit_Program (E_Errors);
   elsif Total_Warnings > 0 then
      Exit_Program (E_Warnings);
   else
      Exit_Program (E_Success);
   end if;

end Gnatbind;
