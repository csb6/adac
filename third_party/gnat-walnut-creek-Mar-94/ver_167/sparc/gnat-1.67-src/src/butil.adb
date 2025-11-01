------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                B U T I L                                 --
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

with Namet;  use Namet;
with Output; use Output;
package body Butil is

   -----------
   -- Later --
   -----------

   function Later (T1, T2 : Time_Stamp_Type) return Boolean is
   begin
      for I in T1'range loop
         if T1 (I) > T2 (I) then
            return True;
         elsif T1 (I) < T2 (I) then
            return False;
         end if;
      end loop;

      return False;
   end Later;

   ----------------
   -- Uname_Less --
   ----------------

   function Uname_Less (U1, U2 : Unit_Name_Type) return Boolean is
   begin
      Get_Name_String (U1);

      declare
         U1_Name : constant Str (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);
         Min_Length : Nat;

      begin
         Get_Name_String (U2);

         if Name_Len < U1_Name'Last then
            Min_Length := Name_Len;
         else
            Min_Length := U1_Name'Last;
         end if;

         for I in 1 .. Min_Length loop
            if U1_Name (I) > Name_Buffer (I) then
               return False;
            elsif U1_Name (I) < Name_Buffer (I) then
               return True;
            end if;
         end loop;

         return U1_Name'Last < Name_Len;
      end;
   end Uname_Less;

   ---------------------
   -- Write_Unit_Name --
   ---------------------

   procedure Write_Unit_Name (U : Unit_Name_Type) is
   begin
      Get_Name_String (U);
      Write_Str (Name_Buffer (1 .. Name_Len - 2));

      if Name_Buffer (Name_Len) = 's' then
         Write_Str (" (spec)");
      else
         Write_Str (" (body)");
      end if;

      Name_Len := Name_Len + 5;
   end Write_Unit_Name;

end Butil;
