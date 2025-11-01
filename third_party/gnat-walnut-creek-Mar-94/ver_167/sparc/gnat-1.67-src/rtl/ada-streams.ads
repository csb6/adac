------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                          A D A . S T R E A M S                           --
--                                                                          --
--                                 S p e c                                  --
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

with System.Storage_Elements;                                        
with Ada.Tags;                                                       
with Ada.Exceptions;                                                 

package Ada.Streams is                                               

   pragma Pure (Streams);
                                                                    
   type Root_Stream_Type is abstract tagged limited private;        
                                                                    
   procedure Read (
     Stream : in out Root_Stream_Type;                              
     Item   : out System.Storage_Elements.Storage_Array;            
                                                                    
     Last   : out System.Storage_Elements.Storage_Offset) is abstract;
                                                                    
   procedure Write (
     Stream : in out Root_Stream_Type;                              
     Item   : in System.Storage_Elements.Storage_Array) is abstract;
                                                                    
private                                                              
   --  Dummy definition for now (body not implemented yet) ???

   type Root_Stream_Type is tagged null record;

end Ada.Streams;                                                     
