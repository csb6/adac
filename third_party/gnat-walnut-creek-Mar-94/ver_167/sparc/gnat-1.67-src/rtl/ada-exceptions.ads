------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       A D A . E X C E P T I O N S                        --
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

package Ada.Exceptions is

   type Exception_Occurrence is private;
   Null_Occurrence : constant Exception_Occurrence;

   function Exception_Name        (X : Exception_Occurrence) return String;
   function Exception_Message     (X : Exception_Occurrence) return String;
   function Exception_Information (X : Exception_Occurrence) return String;

   procedure Reraise_Occurrence   (X : Exception_Occurrence);

private
   --  Dummy definitions for now (body not implemented yet) ???

   type Exception_Occurrence is new Integer; 

   Null_Occurrence : constant Exception_Occurrence := 0;

end Ada.Exceptions;
