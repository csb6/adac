------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                         S Y S T E M . I M G _ C                          --
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

pragma Ada_9X;
with System.Storage_Elements; use System.Storage_Elements;
function System.Img_C (V : Character; B : Address) return Natural is

   package Cnv is new Address_To_Access_Conversions (Character);
   use Cnv;

begin
   --  TBSL, the code in the case of control characters is dubious. The result
   --  returned in these cases is a three character string '?' where ? is the
   --  control character in question. This is not actually wrong, but is not
   --  the most desirable implementation dependent result

   To_Pointer (B + Storage_Offset (0)).all := ''';
   To_Pointer (B + Storage_Offset (1)).all := V;
   To_Pointer (B + Storage_Offset (2)).all := ''';
   return 3;

end System.Img_C;
