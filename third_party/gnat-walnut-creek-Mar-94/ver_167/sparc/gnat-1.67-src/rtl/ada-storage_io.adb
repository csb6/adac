------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       A D A . S T O R A G E _ I O                        --
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

with Unchecked_Conversion;
package body Ada.Storage_IO is

   package Buffer_Ops is new
     System.Storage_Elements.Address_To_Access_Conversions (Buffer_Type);

   package Element_Ops is new
     System.Storage_Elements.Address_To_Access_Conversions (Element_Type);

   ----------
   -- Read --
   ----------

   procedure Read (Buffer : in  Buffer_Type; Item : out Element_Type) is
   begin
      Element_Ops.To_Pointer (Item'Address).all :=
        Element_Ops.To_Pointer (Buffer'Address).all;
   end Read;
 

   -----------
   -- Write --
   -----------

   procedure Write (Buffer : out Buffer_Type; Item : in  Element_Type) is
   begin
      Element_Ops.To_Pointer (Buffer'Address).all :=
        Element_Ops.To_Pointer (Item'Address).all;
   end Write;

end Ada.Storage_IO;
