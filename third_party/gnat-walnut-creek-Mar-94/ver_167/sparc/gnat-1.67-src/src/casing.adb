------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 C A S E                                  --
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

with Csets; use Csets;
with Namet; use Namet;
with Scans; use Scans;
with Types; use Types;
package body Casing is

   ----------------------------
   -- Determine_Token_Casing --
   ----------------------------

   function Determine_Token_Casing return Casing_Type is
      All_Lower : Boolean := True;
      --  Set False if upper case letter found

      All_Upper : Boolean := True;
      --  Set False if lower case letter found

      Decisive : Boolean := False;
      --  Set True if at least one instance of letter not after underline

      After_Und : Boolean := True;
      --  True at start of string, and after an underline character

   begin
      for S in Token_Ptr .. Scan_Ptr - 1 loop
         if Source (S) = '_' or else Source (S) = '.' then
            After_Und := True;

         elsif Is_Lower_Case_Letter (Source (S)) then
            All_Upper := False;

            if not After_Und then
               Decisive := True;
            else
               After_Und := False;
            end if;

         elsif Is_Upper_Case_Letter (Source (S)) then
            All_Lower := False;

            if not After_Und then
               Decisive := True;
            else
               After_Und := False;
            end if;
         end if;
      end loop;

      --  Now we can figure out the result from the flags we set in that loop

      if All_Lower then
         return All_Lower_Case;

      elsif not Decisive then
         return Unknown;

      elsif All_Upper then
         return All_Upper_Case;
      else
         return Mixed_Case;
      end if;
   end Determine_Token_Casing;

   ----------------
   -- Set_Casing --
   ----------------

   procedure Set_Casing (C : Casing_Type; D : Casing_Type := Mixed_Case) is

      Actual_Casing : Casing_Type;
      --  Set from C or D as appropriate

      After_Und : Boolean := True;
      --  True at start of string, and after an underline character. Note that
      --  for the puroses of mixed case mode, we treat < and period the same
      --  way as underline (deals with special names like Any_Type, and also
      --  with unit names which have embedded periods).

   begin
      if C /= Unknown then
         Actual_Casing := C;
      else
         Actual_Casing := D;
      end if;

      for I in 1 .. Name_Len loop
         if Name_Buffer (I) = '_'
           or else Name_Buffer (I) = '.'
           or else Name_Buffer (I) = '-'
           or else Name_Buffer (I) = '<'
         then
            After_Und := True;

         elsif Is_Lower_Case_Letter (Name_Buffer (I)) then
            if Actual_Casing = All_Upper_Case
              or else (After_Und and then Actual_Casing = Mixed_Case)
            then
               Name_Buffer (I) := Fold_Upper (Name_Buffer (I));
            end if;

            After_Und := False;

         elsif Is_Upper_Case_Letter (Name_Buffer (I)) then
            if Actual_Casing = All_Lower_Case
              or else (not After_Und and then Actual_Casing = Mixed_Case)
            then
               Name_Buffer (I) := Fold_Lower (Name_Buffer (I));
            end if;

            After_Und := False;

         end if;
      end loop;
   end Set_Casing;

end Casing;
