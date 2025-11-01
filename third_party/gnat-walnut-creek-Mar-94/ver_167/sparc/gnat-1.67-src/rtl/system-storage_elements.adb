------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S T O R A G E _ E L E M E N T S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.3 $                              --
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

pragma Ada_9X;
package body System.Storage_Elements is

   --  Address arithmetic

   function "+" (Left : Address; Right : Storage_Offset) return Address is
   begin
      return Left + Address (Right);
   end "+";

   function "+" (Left : Storage_Offset; Right : Address) return Address is
   begin
      return Address (Left) + Right;
   end "+";

   function "-" (Left : Address; Right : Storage_Offset) return Address is
   begin
      return Left - Address (Right);
   end "-";

   function "-" (Left, Right : Address) return Storage_Offset is
   begin
      return Storage_Offset (Left) - Storage_Offset (Right);
   end "-";

   function "mod" (Left : Address; Right : Storage_Offset)
     return Storage_Offset is
   begin
      return Storage_Offset (Left) mod Right;
   end "mod";

   --  Conversion to/from integers

   function To_Address (Value : Integer_Address) return Address is
   begin
      return Address (Value);
   end To_Address;

   function To_Integer (Value : Address) return Integer_Address is
   begin
      return Integer_Address (Value);
   end To_Integer;

end System.Storage_Elements;
