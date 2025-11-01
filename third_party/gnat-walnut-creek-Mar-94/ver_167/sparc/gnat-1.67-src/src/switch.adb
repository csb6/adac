------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S W I T C H                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.41 $                             --
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

--  Option switch scanning for both the compiler and the binder

--  Note: this version of the package should be usable in both Unix and DOS

with Debug;  use Debug;
with Osint;  use Osint;
with Opt;    use Opt;
with Output; use Output;
with Sysid;  use Sysid;
with Types;  use Types;

package body Switch is

   Bad_Switch : exception;
   --  Exception raised if bad switch encountered

   -------------------
   -- Scan_Switches --
   -------------------

   procedure Scan_Switches (Switch_Chars : String) is
      Ptr  : Integer := Switch_Chars'First;
      Max  : Integer := Switch_Chars'Last;
      C    : Character;

      Switches : String (Switch_Chars'range);
      --  Copy of switches which is actually scanned. In MS/DOS mode, this
      --  string is folded to lower case so that upper case switches are
      --  recognized as equivalent to the lower case switches.

      function Scan_Int return Pos;
      --  Scan positive integer parameter for switch. On entry, Ptr points
      --  just past the switch character, on exit it points past the last
      --  digit of the integer value.

      --------------
      -- Scan_Int --
      --------------

      function Scan_Int return Pos is
         Val : Int := 0;

      begin
         if Ptr > Max or else Switches (Ptr) not in '0' .. '9' then
            raise Bad_Switch;
         end if;

         while Ptr <= Max and then Switches (Ptr) in '0' .. '9' loop
            Val := Val * 10 +
              Character'Pos (Switches (Ptr)) - Character'Pos ('0');
            Ptr := Ptr + 1;

            if Val > 999 then
               raise Bad_Switch;
            end if;
         end loop;

         return Val;
      end Scan_Int;

      -------------------
      -- Scan_Switches --
      -------------------

   begin
      --  In MS/DOS mode, fold switch string to lower case

      for I in Switches'range loop
         if MS_DOS and then Switch_Chars (I) in 'A' .. 'Z' then
            Switches (I) :=
                 Character'Val (Character'Pos (Switch_Chars (I)) - 32);
         else
            Switches (I) := Switch_Chars (I);
         end if;
      end loop;

      --  Skip past the initial character (must be the switch character)

      if Ptr = Max then
         raise Bad_Switch;
      else
         Ptr := Ptr + 1;
      end if;

      --  Loop to scan through switches given in switch string

      while Ptr <= Max loop
         C := Switches (Ptr);

         --  Processing for -a switch

         if C = 'a' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               Assertions_Enabled := True;
            elsif Program = Binder then
               All_Sources := True;
               Check_Source_Files := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -b switch

         elsif C = 'b' then
            if Program = Compiler or else Program = Binder then
               Ptr := Ptr + 1;
               Brief_Output := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -c switch

         elsif C = 'c' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               Operating_Mode := Check_Semantics;
            elsif Program = Binder then
               Check_Only := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -d switch

         elsif C = 'd' then

            --  Note: for the debug switch, the remaining characters in this
            --  switch field must all be debug flags, since all valid switch
            --  characters are also valid debug characters.

            --  Loop to scan out debug flags

            while Ptr < Max loop
               Ptr := Ptr + 1;
               C := Switches (Ptr);
               exit when C = Ascii.NUL or else C = '/' or else C = '-';

               if C in '1' .. '9' or else C in 'a' .. 'z' then
                  Set_Debug_Flag (To_Char (C));
               else
                  raise Bad_Switch;
               end if;
            end loop;

            return;

         --  Processing for -e switch

         elsif C = 'e' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               Immediate_Errors := True;
            elsif Program = Binder then
               Elab_Dependency_Output := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -f switch

         elsif C = 'f' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               All_Errors_Mode := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -g switch

         elsif C = 'g' then
            if Program = Compiler then
               Ptr := Ptr + 1;
               RM_Column_Check := True;
               GNAT_Style_Check := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -i switch

         elsif C = 'i' then
            if Program = Compiler or else Program = Binder then
               if Ptr = Max then
                  raise Bad_Switch;
               end if;

               Ptr := Ptr + 1;
               C := Switches (Ptr);

               if C = '1' or else
                  C = '2' or else
                  C = '3' or else
                  C = '4' or else
                  C = 'p' or else
                  C = 'f' or else
                  C = 'n'
               then
                  Identifier_Character_Set := To_Char (C);
                  Ptr := Ptr + 1;
               else
                  raise Bad_Switch;
               end if;

            else
               raise Bad_Switch;
            end if;

         elsif C = 'k' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               Maximum_File_Name_Length := Scan_Int;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -l switch

         elsif C = 'l' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               Full_List := True;
            elsif Program = Binder then
               Elab_Order_Output := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -m switch

         elsif C = 'm' then
            Ptr := Ptr + 1;

            if Program = Compiler or else Program = Binder then
               Maximum_Errors := Scan_Int;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -n switch

         elsif C = 'n' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               Inline_Active := False;
            elsif Program = Binder then
               Bind_Main_Program := False;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -o switch

         elsif C = 'o' then
            Ptr := Ptr + 1;

            if Program = Binder then
               Output_Filename_Present := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -p switch

         elsif C = 'p' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               Suppress_Options.Access_Checks        := True;
               Suppress_Options.Accessibility_Checks := True;
               Suppress_Options.Discriminant_Checks  := True;
               Suppress_Options.Division_Checks      := True;
               Suppress_Options.Index_Checks         := True;
               Suppress_Options.Length_Checks        := True;
               Suppress_Options.Overflow_Checks      := True;
               Suppress_Options.Range_Checks         := True;
               Suppress_Options.Division_Checks      := True;
               Suppress_Options.Length_Checks        := True;
               Suppress_Options.Range_Checks         := True;
               Suppress_Options.Storage_Checks       := True;
               Suppress_Options.Tag_Checks           := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -r switch

         elsif C = 'r' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               RM_Column_Check := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -s switch

         elsif C = 's' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               Operating_Mode := Check_Syntax;
            elsif Program = Binder then
               Check_Source_Files := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -t switch

         elsif C = 't' then
            Ptr := Ptr + 1;

            if Program = Binder then
               Ignore_Time_Stamp_Errors := True;
            elsif Program = Compiler then
               Try_Semantics := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -u switch

         elsif C = 'u' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               List_Units := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -v switch

         elsif C = 'v' then
            Ptr := Ptr + 1;

            if Program = Compiler or else Program = Binder then
               Verbose_Mode := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -w switch

         elsif C = 'w' then
            Ptr := Ptr + 1;

            if Program = Compiler or else Program = Binder then
               C := Switches (Ptr);

               if C = 's' then
                  Warning_Mode := Suppress;
               elsif C = 'e' then
                  Warning_Mode := Treat_As_Error;
               else
                  raise Bad_Switch;
               end if;

               Ptr := Ptr + 1;

            else
               raise Bad_Switch;
            end if;

         --  Processing for -83 switch

         elsif C = '8' then

            if Program = Compiler then
               if Ptr = Max then
                  raise Bad_Switch;
               end if;

               Ptr := Ptr + 1;

               if Switches (Ptr) /= '3' then
                  raise Bad_Switch;
               else
                  Ptr := Ptr + 1;
                  Ada_83_Switch := True;
                  Ada_9X := False;
                  Ada_83 := True;
               end if;

            else
               raise Bad_Switch;
            end if;

         --  Ignore extra switch character

         elsif C = '/' or else C = '-' then
            Ptr := Ptr + 1;

         --  Anything else is an error (illegal switch character)

         else
            raise Bad_Switch;
         end if;

      end loop;

   exception
      when Bad_Switch =>
         Write_String ("Invalid switch");
         Write_Eol;
         Exit_Program (E_Fatal);

   end Scan_Switches;

end Switch;
