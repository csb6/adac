------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              B I N D E R R                               --
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

with Butil;            use Butil;
with Debug;            use Debug;
with Errcount;         use Errcount;
with Excep;            use Excep;
with Namet;            use Namet;
with Opt;              use Opt;
with Osint;            use Osint;
with Output;           use Output;
with System.Traceback; use System.Traceback;

package body Binderr is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Error_Msg_Output (Msg : Str);
   --  Output given message, with insertions, to current message output file

   ------------------
   -- Binder_Abort --
   ------------------

   procedure Binder_Abort is
   begin
      --  Case of errors have been detected. In this case, we guess that the
      --  abort is caused by previous errors, and we don't make too much fuss
      --  about it, since we want to let the programmer fix the errors first.

      --  This treatment is disabled by debug flag -dk so that we can force
      --  a traceback when we are trying to get rid of cascaded bombs!

      if not Debug_Flag_K and then Errors_Detected /= 0 then
         raise Unrecoverable_Error;

      --  Otherwise we have a real abort. First generate software traceback
      --  if we are doing software tracebacks. Then do a system level abort.

      else
         if Tracebacks_Stored then
            Output_Traceback;

            --  We exit softly if we generated a software traceback, to be
            --  sure that buffers get flushed etc. The assumption is that
            --  we would not be doing a software traceback if the abort
            --  gave useful information!

            Exit_Program (E_Errors);

         else
            Exit_Program (E_Abort);
         end if;
      end if;

   end Binder_Abort;

   ------------------
   -- Binder_Error --
   ------------------

   procedure Binder_Error is
   begin
      --  Case of errors have been detected. In this case, we guess that the
      --  abort is caused by previous errors, and we don't make too much fuss
      --  about it, since we want to let the programmer fix the errors first.

      --  This treatment is disabled by debug flag -dk so that we can force
      --  a traceback when we are trying to get rid of cascaded bombs!

      if not Debug_Flag_K and then Errors_Detected /= 0 then
         raise Unrecoverable_Error;

      --  Otherwise we have a real abort. In the case of Binder_Error, as
      --  opposed to Binder_Abort, we return to the caller to generate
      --  additional diagnostic information (which will go to standard error)

      else
         Set_Standard_Error;
         return;
      end if;

   end Binder_Error;

   ---------------
   -- Error_Msg --
   ---------------

   procedure Error_Msg (Msg : Str) is
   begin
      if Msg (Msg'First) = '?' then
         if Warning_Mode = Suppress then
            return;
         end if;

         if Warning_Mode = Treat_As_Error then
            Errors_Detected := Errors_Detected + 1;
         else
            Warnings_Detected := Warnings_Detected + 1;
         end if;

      else
         Errors_Detected := Errors_Detected + 1;
      end if;

      if Brief_Output or else (not Verbose_Mode) then
         Set_Standard_Error;
         Error_Msg_Output (Msg);
         Set_Standard_Output;
      end if;

      if Verbose_Mode then
         if Errors_Detected + Warnings_Detected = 0 then
            Write_Eol;
         end if;

         Error_Msg_Output (Msg);
      end if;

      if Warnings_Detected + Errors_Detected > Maximum_Errors then
         raise Unrecoverable_Error;
      end if;

   end Error_Msg;

   ----------------------
   -- Error_Msg_Output --
   ----------------------

   procedure Error_Msg_Output (Msg : Str) is
      Use_Second_Name : Boolean := False;

   begin
      if Warnings_Detected + Errors_Detected > Maximum_Errors then
         Write_String ("error: maximum errors exceeded");
         Write_Eol;
         return;
      end if;

      if Msg (Msg'First) = '?' then
         if Warning_Mode = Suppress then
            return;
         else
            Write_String ("warning: ");
         end if;

      else
         Write_String ("error: ");
      end if;

      for I in Msg'range loop
         if Msg (I) = '%' then

            if Use_Second_Name then
               Get_Name_String (Error_Msg_Name_2);
            else
               Use_Second_Name := True;
               Get_Name_String (Error_Msg_Name_1);
            end if;

            Write_Char ('"');
            Write_Str (Name_Buffer (1 .. Name_Len));
            Write_Char ('"');

         elsif Msg (I) = '&' then
            Write_Char ('"');

            if Use_Second_Name then
               Write_Unit_Name (Error_Msg_Name_2);
            else
               Use_Second_Name := True;
               Write_Unit_Name (Error_Msg_Name_1);
            end if;

            Write_Char ('"');

         elsif Msg (I) /= '?' then
            Write_Char (Msg (I));
         end if;
      end loop;

      Write_Eol;
   end Error_Msg_Output;

   ----------------------
   -- Finalize_Binderr --
   ----------------------

   procedure Finalize_Binderr is
   begin
      --  Message giving number of errors detected (verbose mode only)

      if Verbose_Mode then
         Write_Eol;

         if Errors_Detected = 0 then
            Write_String ("No errors");

         elsif Errors_Detected = 1 then
            Write_String ("1 error");

         else
            Write_Int (Errors_Detected);
            Write_String (" errors");
         end if;

         if Warnings_Detected = 1 then
            Write_String (", 1 warning");

         elsif Warnings_Detected > 1 then
            Write_String (", ");
            Write_Int (Warnings_Detected);
            Write_String (" warnings");
         end if;

         Write_Eol;
      end if;
   end Finalize_Binderr;

   ------------------------
   -- Initialize_Binderr --
   ------------------------

   procedure Initialize_Binderr is
   begin
      Errors_Detected := 0;
      Warnings_Detected := 0;
   end Initialize_Binderr;

end Binderr;
