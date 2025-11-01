------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . I M G _ L L F                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $                              --
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

--  This is a little messed up for now, since we can't assume that the
--  accessible sprintf routine handles long long float. We print the right
--  number of digits, but the precision may get degraded. On almost all
--  machines Long_Long_Float is the same as Long_Float so it doesn't matter
--  but notably for the Intel x86, this will need fixing later on (TBSL).

pragma Ada_9X;
with Ttypes; use Ttypes;
function System.Img_LLF (V : Long_Long_Float; B : Address) return Natural is

   procedure sprintf (Target, Fmt : Address;
                      Prec        : Natural;
                      Val         : Long_Float;
                      Length_Ptr  : Address);
   pragma Import (C, sprintf);

   Fmt    : constant String := "% .*e%n" & Ascii.NUL;
   Length : aliased Natural;
begin
   sprintf (B, Fmt'Address, Standard_Long_Long_Float_Digits, Long_Float (V),
     Length'Address);
   return Length;
end System.Img_LLF;
