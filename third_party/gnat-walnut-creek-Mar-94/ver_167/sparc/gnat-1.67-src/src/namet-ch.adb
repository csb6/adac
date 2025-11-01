------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             N A M E T . C H                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.3 $                             --
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

separate (Namet)
function CH return Hash_Index_Type is
   subtype Int_1_12 is Int range 1 .. 12;
   --  Used to avoid when others on case jump below

   Even_Name_Len : Int;
   --  Last even numbered position (used for >12 case)


begin

   --  Special test for 12 (rather than counting on a when others for the
   --  case statement below) avoids some Ada compilers converting the case
   --  statement into successive jumps.

   --  The case of a name longer than 12 characters is handled by taking the
   --  first 6 odd numbered characters and the last 6 even numbered characters

   if Name_Len > 12 then
      Even_Name_Len := (Name_Len) / 2 * 2;

      return ((((((((((((
        Char'Pos (Name_Buffer (01))) * 2 +
        Char'Pos (Name_Buffer (Even_Name_Len - 10))) * 2 +
        Char'Pos (Name_Buffer (03))) * 2 +
        Char'Pos (Name_Buffer (Even_Name_Len - 08))) * 2 +
        Char'Pos (Name_Buffer (05))) * 2 +
        Char'Pos (Name_Buffer (Even_Name_Len - 06))) * 2 +
        Char'Pos (Name_Buffer (07))) * 2 +
        Char'Pos (Name_Buffer (Even_Name_Len - 04))) * 2 +
        Char'Pos (Name_Buffer (09))) * 2 +
        Char'Pos (Name_Buffer (Even_Name_Len - 02))) * 2 +
        Char'Pos (Name_Buffer (11))) * 2 +
        Char'Pos (Name_Buffer (Even_Name_Len))) mod Hash_Num;
   end if;

   --  For the cases of 1-12 characters, all characters participate in the
   --  hash. The positioning is randomized, with the bias that characters
   --  later on participate fully (i.e. are added towards the right side).

   case (Int_1_12 (Name_Len)) is

      when 1 =>
         return
            Char'Pos (Name_Buffer (1));

      when 2 =>
         return ((
           Char'Pos (Name_Buffer (1))) * 64 +
           Char'Pos (Name_Buffer (2))) mod Hash_Num;

      when 3 =>
         return (((
           Char'Pos (Name_Buffer (1))) * 16 +
           Char'Pos (Name_Buffer (3))) * 16 +
           Char'Pos (Name_Buffer (2))) mod Hash_Num;

      when 4 =>
         return ((((
           Char'Pos (Name_Buffer (1))) * 8 +
           Char'Pos (Name_Buffer (2))) * 8 +
           Char'Pos (Name_Buffer (3))) * 8 +
           Char'Pos (Name_Buffer (4))) mod Hash_Num;

      when 5 =>
         return (((((
           Char'Pos (Name_Buffer (4))) * 8 +
           Char'Pos (Name_Buffer (1))) * 4 +
           Char'Pos (Name_Buffer (3))) * 4 +
           Char'Pos (Name_Buffer (5))) * 8 +
           Char'Pos (Name_Buffer (2))) mod Hash_Num;

      when 6 =>
         return ((((((
           Char'Pos (Name_Buffer (5))) * 4 +
           Char'Pos (Name_Buffer (1))) * 4 +
           Char'Pos (Name_Buffer (4))) * 4 +
           Char'Pos (Name_Buffer (2))) * 4 +
           Char'Pos (Name_Buffer (6))) * 4 +
           Char'Pos (Name_Buffer (3))) mod Hash_Num;

      when 7 =>
         return (((((((
           Char'Pos (Name_Buffer (4))) * 4 +
           Char'Pos (Name_Buffer (3))) * 4 +
           Char'Pos (Name_Buffer (1))) * 4 +
           Char'Pos (Name_Buffer (2))) * 2 +
           Char'Pos (Name_Buffer (5))) * 2 +
           Char'Pos (Name_Buffer (7))) * 2 +
           Char'Pos (Name_Buffer (6))) mod Hash_Num;

      when 8 =>
         return ((((((((
           Char'Pos (Name_Buffer (2))) * 4 +
           Char'Pos (Name_Buffer (1))) * 4 +
           Char'Pos (Name_Buffer (3))) * 2 +
           Char'Pos (Name_Buffer (5))) * 2 +
           Char'Pos (Name_Buffer (7))) * 2 +
           Char'Pos (Name_Buffer (6))) * 2 +
           Char'Pos (Name_Buffer (4))) * 2 +
           Char'Pos (Name_Buffer (8))) mod Hash_Num;

      when 9 =>
         return (((((((((
           Char'Pos (Name_Buffer (2))) * 4 +
           Char'Pos (Name_Buffer (1))) * 4 +
           Char'Pos (Name_Buffer (3))) * 4 +
           Char'Pos (Name_Buffer (4))) * 2 +
           Char'Pos (Name_Buffer (8))) * 2 +
           Char'Pos (Name_Buffer (7))) * 2 +
           Char'Pos (Name_Buffer (5))) * 2 +
           Char'Pos (Name_Buffer (6))) * 2 +
           Char'Pos (Name_Buffer (9))) mod Hash_Num;

      when 10 =>
         return ((((((((((
           Char'Pos (Name_Buffer (01))) * 2 +
           Char'Pos (Name_Buffer (02))) * 2 +
           Char'Pos (Name_Buffer (08))) * 2 +
           Char'Pos (Name_Buffer (03))) * 2 +
           Char'Pos (Name_Buffer (04))) * 2 +
           Char'Pos (Name_Buffer (09))) * 2 +
           Char'Pos (Name_Buffer (06))) * 2 +
           Char'Pos (Name_Buffer (05))) * 2 +
           Char'Pos (Name_Buffer (07))) * 2 +
           Char'Pos (Name_Buffer (10))) mod Hash_Num;

      when 11 =>
         return (((((((((((
           Char'Pos (Name_Buffer (05))) * 2 +
           Char'Pos (Name_Buffer (01))) * 2 +
           Char'Pos (Name_Buffer (06))) * 2 +
           Char'Pos (Name_Buffer (09))) * 2 +
           Char'Pos (Name_Buffer (07))) * 2 +
           Char'Pos (Name_Buffer (03))) * 2 +
           Char'Pos (Name_Buffer (08))) * 2 +
           Char'Pos (Name_Buffer (02))) * 2 +
           Char'Pos (Name_Buffer (10))) * 2 +
           Char'Pos (Name_Buffer (04))) * 2 +
           Char'Pos (Name_Buffer (11))) mod Hash_Num;

      when 12 =>
         return ((((((((((((
           Char'Pos (Name_Buffer (03))) * 2 +
           Char'Pos (Name_Buffer (02))) * 2 +
           Char'Pos (Name_Buffer (05))) * 2 +
           Char'Pos (Name_Buffer (01))) * 2 +
           Char'Pos (Name_Buffer (06))) * 2 +
           Char'Pos (Name_Buffer (04))) * 2 +
           Char'Pos (Name_Buffer (08))) * 2 +
           Char'Pos (Name_Buffer (11))) * 2 +
           Char'Pos (Name_Buffer (07))) * 2 +
           Char'Pos (Name_Buffer (09))) * 2 +
           Char'Pos (Name_Buffer (10))) * 2 +
           Char'Pos (Name_Buffer (12))) mod Hash_Num;

   end case;
end CH;
