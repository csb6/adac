------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . I M G _ L L I                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $                              --
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

--  Note that we can't use printf for this function, because we don't know
--  that the accessible version of printf supports GCC long long types.

pragma Ada_9X;
with System.Storage_Elements; use System.Storage_Elements;
function System.Img_LLI (V : Long_Long_Integer; B : Address) return Natural is

   package Cnv is new Address_To_Access_Conversions (Character);
   use Cnv;

   Ptr : Address := B;

   procedure Putc (C : Character) is
   begin
      To_Pointer (Ptr).all := C;
      Ptr := Ptr + Storage_Offset (1);
   end Putc;

   procedure Digs (V : Long_Long_Integer) is
   begin
      if V >= 10 then
         Digs (V / 10);
      end if;

      Putc (Character'Val (V mod 10 + 48));
   end Digs;

begin
   if V < 0 then
      Putc ('-');
      Digs (-V);
   else
      Putc (' ');
      Digs (V);
   end if;

   return Natural (Ptr - B);

end System.Img_LLI;
