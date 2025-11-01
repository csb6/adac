------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S T O R A G E _ E L E M E N T S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.4 $                              --
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
with Unchecked_Conversion;
package System.Storage_Elements is
   --  pragma Pure (System.Storage_Elements);

   type Storage_Offset is new Integer;
   subtype Storage_Count is Storage_Offset range 0 .. Storage_Offset'Last;
   subtype Storage_Index is Storage_Offset range 1 .. Storage_Offset'Last;

   type Storage_Element is mod 2 ** 8;
   for Storage_Element'Size use Storage_Unit;

   type Storage_Array is
     array (Storage_Index range <>) of aliased Storage_Element;
   --  for Storage_Array'Component_Size use Storage_Unit;

   --  Address arithmetic

   function "+" (Left : Address; Right : Storage_Offset) return Address;
   pragma Convention (Intrinsic, "+");
   pragma Inline ("+");

   function "+" (Left : Storage_Offset; Right : Address) return Address;
   pragma Convention (Intrinsic, "+");
   pragma Inline ("+");

   function "-" (Left : Address; Right : Storage_Offset) return Address;
   pragma Convention (Intrinsic, "-");
   pragma Inline ("-");

   function "-" (Left, Right : Address) return Storage_Offset;
   pragma Convention (Intrinsic, "-");
   pragma Inline ("-");

   function "mod" (Left : Address; Right : Storage_Offset)
     return Storage_Offset;
   pragma Convention (Intrinsic, "mod");
   pragma Inline ("mod");

   --  Conversion to/from integers

   type Integer_Address is mod 2 ** 32;

   function To_Address (Value : Integer_Address) return Address;
   pragma Convention (Intrinsic, To_Address);
   pragma Inline (To_Address);

   function To_Integer (Value : Address) return Integer_Address;
   pragma Convention (Intrinsic, To_Integer);
   pragma Inline (To_Integer);

   --  Peek/poke functionality

   generic
      type Object (<>) is limited private;

   package Address_To_Access_Conversions is
      type Object_Pointer is access all Object;

      function To_Pointer is new
        Unchecked_Conversion (Address, Object_Pointer);
      pragma Convention (Intrinsic, To_Pointer);

      function To_Address is new
        Unchecked_Conversion (Object_Pointer, Address);
      pragma Convention (Intrinsic, To_Address);
   end Address_To_Access_Conversions;

end System.Storage_Elements;
