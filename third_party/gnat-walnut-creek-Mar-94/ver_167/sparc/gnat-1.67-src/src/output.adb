------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               O U T P U T                                --
--                                                                          --
--                                 B o d y                                  --
--                              (UNIX Version)                              --
--                                                                          --
--                            $Revision: 1.14 $                             --
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

with Unixlib; use Unixlib;

package body Output is

--  This version of the package written for operation under UNIX, using
--  the Alsys System_Environment package for OS interface functions.

   Current_Column : Int := 1;
   --  Current column number

   FD : Unix_FD := Unix_Standout;
   --  File descriptor for current output

   -------------
   --  Column --
   -------------

   function Column return Int is
   begin
      return Current_Column;
   end Column;

   ------------------------
   -- Set_Standard_Error --
   ------------------------

   procedure Set_Standard_Error is
   begin
      FD := Unix_Standerr;
   end Set_Standard_Error;

   -------------------------
   -- Set_Standard_Output --
   -------------------------

   procedure Set_Standard_Output is
   begin
      FD := Unix_Standout;
   end Set_Standard_Output;

   ----------------
   -- Write_Char --
   ----------------

   procedure Write_Char (C : Char) is
   begin
      Unix_Write (FD, C'Address, 1);
      Current_Column := Current_Column + 1;
   end Write_Char;

   ---------------
   -- Write_Eol --
   ---------------

   procedure Write_Eol is
   begin
      Write_Char (LF);
      Current_Column := 1;
   end Write_Eol;

   ---------------
   -- Write_Int --
   ---------------

   procedure Write_Int (I : Int) is
   begin

      if I < 0 then
         Write_Char ('-');
         Write_Int ( -I);

      else
         if I > 9 then
            Write_Int (I / 10);
         end if;

         Write_Char (Char'VAL ((I mod 10) + 48));
      end if;
   end Write_Int;

   ---------------
   -- Write_Str --
   ---------------

   procedure Write_Str (S : Str) is
   begin
      Unix_Write (FD, S'Address, S'length);
      Current_Column := Current_Column + S'Length;
   end Write_Str;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String (S : String) is
   begin
      Unix_Write (FD, S'Address, S'length);
      Current_Column := Current_Column + S'Length;
   end Write_String;

   --------------------------
   -- Debugging Procedures --
   --------------------------

   procedure ws (S : String) is
   begin
      Write_String (S);
      Write_Eol;
   end ws;

   procedure w (S : Str) is
   begin
      Write_Str (S);
      Write_Eol;
   end w;

   procedure w (I : Int) is
   begin
      Write_Int (I);
      Write_Eol;
   end w;

   procedure w (B : Boolean) is
   begin
      if B then
         ws ("True");
      else
         ws ("False");
      end if;
   end w;

   procedure ws (L : Str; S : String) is
   begin
      Write_Str (L);
      Write_Char (' ');
      ws (S);
   end ws;

   procedure w (L : Str; S : Str) is
   begin
      Write_Str (L);
      Write_Char (' ');
      w (S);
   end w;

   procedure w (L : Str; I : Int) is
   begin
      Write_Str (L);
      Write_Char (' ');
      w (I);
   end w;

   procedure w (L : Str; B : Boolean) is
   begin
      Write_Str (L);
      Write_Char (' ');
      w (B);
   end w;

end Output;
