------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M _ E N V I R O N M E N T                  --
--                                                                          --
--                                 B o d y                                  --
--                              (UNIX Version)                              --
--                                                                          --
--                            $Revision: 1.6 $                              --
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

--  This package contains some functions to access arg. on the command line
with System; 
package body System_Environment is 

   Args : array (0 .. Arg_Count - 1) of A_String;

   procedure Fill_Arg (A : System.Address; Arg_Num : Integer);
   pragma Interface (C, Fill_Arg);

   function Len_Arg (Arg_Num : Integer) return Integer;
   pragma Interface (C, Len_Arg);

   function Arg_Value (Arg_Num : Natural) return A_String is
   begin
      return Args (Arg_Num);
   end Arg_Value;

begin
   for I in Args'range loop
      Args (I) := new String (1 .. Len_Arg (I));
      Fill_Arg (Args (I).all'Address, I);
   end loop;
end System_Environment;
