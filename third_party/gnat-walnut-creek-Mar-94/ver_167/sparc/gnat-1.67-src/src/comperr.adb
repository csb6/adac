------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              C O M P E R R                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.7 $                              --
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

--  This package contains routines called when a fatal internal compiler
--  error is detected. Calls to these routines cause termination of the
--  current compilation with appropriate error output.

pragma Ada_9X;
with Debug;            use Debug;
with Errcount;         use Errcount;
with Excep;            use Excep;
with Osint;            use Osint;
with Output;           use Output;
with System.Traceback; use System.Traceback;
with Treepr;           use Treepr;

package body Comperr is

   --------------------
   -- Compiler_Abort --
   --------------------

   procedure Compiler_Abort is
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

   end Compiler_Abort;

   procedure Compiler_Abort (N : Node_Id) is
   begin
      Compiler_Error;
      Print_Tree_Node (N);
      Compiler_Abort;
   end Compiler_Abort;

   --------------------
   -- Compiler_Error --
   --------------------

   procedure Compiler_Error is
   begin
      --  Case of errors have been detected. In this case, we guess that the
      --  abort is caused by previous errors, and we don't make too much fuss
      --  about it, since we want to let the programmer fix the errors first.

      --  This treatment is disabled by debug flag -dk so that we can force
      --  a traceback when we are trying to get rid of cascaded bombs!

      if not Debug_Flag_K and then Errors_Detected /= 0 then
         raise Unrecoverable_Error;

      --  Otherwise we have a real abort. In the case of Compiler_Error, as
      --  opposed to Compiler_Abort, we return to the caller to generate
      --  additional diagnostic information (which will go to standard error)

      else
         Set_Standard_Error;
         return;
      end if;

   end Compiler_Error;

end Comperr;
