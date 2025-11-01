------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S Y S T E M                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.11 $                             --
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
package System is
   --  pragma Pure (System);

   type Name is (GNAT);
   System_Name : constant Name := GNAT;

   --  System-Dependent Named Numbers

   Min_Int                         : constant := - (2#1#E63);
   Max_Int                         : constant := (2#1#E63) - 1;
   Max_Binary_Modulus              : constant := 2#1#E32;
   Max_Nonbinary_Modulus           : constant := 2#1#E16 - 1;
   Max_Base_Digits                 : constant := 15;
   Max_Digits                      : constant := 15;
   Max_Mantissa                    : constant := 31;
   Fine_Delta                      : constant := 2#1.0#E-31;
   Tick                            : constant := 1.0;

   --  Storage related Declarations

   type Address is private;
   Null_Address : constant Address;

   Storage_Unit : constant := 8;
   Word_Size    : constant := 32;

   Memory_Size  : constant := 2#1#E32;

   --  Address comparison

   function "<"  (Left, Right : Address) return Boolean;
   function "<=" (Left, Right : Address) return Boolean;
   function ">"  (Left, Right : Address) return Boolean;
   function ">=" (Left, Right : Address) return Boolean;
   function "="  (Left, Right : Address) return Boolean;

   pragma Import (Intrinsic, "<");
   pragma Import (Intrinsic, "<=");
   pragma Import (Intrinsic, ">");
   pragma Import (Intrinsic, ">=");
   pragma Import (Intrinsic, "=");

   --  Declarations for real time Annexe (Annexe H)

   subtype Any_Priority is Integer             range 0  .. 31;
   subtype Priority is Any_Priority            range 0  .. 30;
   subtype Interrupt_Priority is Any_Priority  range 31 .. 31;

   Default_Priority : constant Priority := 
                        (Priority'First + Priority'Last) / 2;

private

   type Address is mod 2#1#E32;
   Null_Address : constant Address := 0;

end System;
