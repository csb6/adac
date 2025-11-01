------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               T T Y P E S                                --
--                                                                          --
--                                 S p e c                                  --
--                             (32-bit Version)
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

--  This module contains values that describe the predefined (standard)
--  target types that are provided. Note that it is essential that all
--  references to Size values and other attributes of types use this
--  package when talking about target types (rather than using the
--  attributes directly which would give information about host types).

package Ttypes is

   --  Note: GNAT always supplies all the following integer and float types,
   --  but depending on the machine, some of the types may be identical. For
   --  example, on some machines, Short_Float may be the same as Float, and
   --  Long_Long_Float may be the same as Long_Float.

   Standard_Short_Short_Integer_Size   : constant := 8;
   --  Standard.Short_Short_Integer'Size

   Standard_Short_Short_Integer_Width  : constant := 4;
   --  Standard.Short_Short_Integer'Width

   Standard_Short_Integer_Size         : constant := 16;
   --  Standard.Short_Integer'Size

   Standard_Short_Integer_Width        : constant := 6;
   --  Standard.Short_Integer'Width

   Standard_Integer_Size               : constant := 32;
   --  Standard.Integer'Size

   Standard_Integer_Width              : constant := 11;
   --  Standard.Integer'Width

   Standard_Long_Integer_Size          : constant := 32;
   --  Standard.Long_Integer'Size

   Standard_Long_Integer_Width         : constant := 11;
   --  Standard.Long_Integer'Width

   Standard_Long_Long_Integer_Size     : constant := 64;
   --  Standard.Long_Long_Integer'Size

   Standard_Long_Long_Integer_Width    : constant := 21;
   --  Standard.Long_Long_Integer'Width

   Standard_Short_Float_Size           : constant := 32;
   --  Standard.Short_Float'Size

   Standard_Short_Float_Digits         : constant := 6;
   --  Standard.Short_Float'Digits

   Standard_Float_Size                 : constant := 32;
   --  Standard.Float'Size

   Standard_Float_Digits               : constant := 6;
   --  Standard.Float'Digits

   Standard_Long_Float_Size            : constant := 64;
   --  Standard.Long_Float'Size

   Standard_Long_Float_Digits          : constant := 15;
   --  Standard.Long_Float'Digits

   Standard_Long_Long_Float_Size       : constant := 64;
   --  Standard.Long_Long_Float'Size

   Standard_Long_Long_Float_Digits     : constant := 15;
   --  Standard.Long_Long_Float'Digits

   Standard_Character_Size             : constant := 8;
   --  Standard.Character'Size

   System_Address_Size                 : constant := 32;
   --  System.Address'Size (also size of all thin pointers)

   System_Max_Binary_Modulus_Power     : constant := 64;
   --  System.Max_Binary_Modulus := 2 ** System_Max_Binary_Modulus_Power

   System_Max_Nonbinary_Modulus_Power  : constant := 32;
   --  System.Max_Nonbinary_Modulus := 
   --    2 ** System_Max_Nonbinary_Modulus_Power - 1

   System_Storage_Unit                 : constant := 8;
   --  System.Storage_Unit                                                     

   System_Word_Size                    : constant := 32;
   --  System.Word_Size

   --  Note: there is no specific control over the representation of
   --  enumeration types. The convention used is that if an enumeration
   --  type has fewer than 2**(Character'Size) elements, then the size
   --  used is Character'Size, otherwise Integer'Size is used.

   Ones_Complement                     : constant Boolean := False;
   --  Set True if integers are represented in ones complement form. The 
   --  current version of GCC does not support ones complement, so this
   --  constant will always be False, but we retain the control for
   --  possible future implementation

end Ttypes;
