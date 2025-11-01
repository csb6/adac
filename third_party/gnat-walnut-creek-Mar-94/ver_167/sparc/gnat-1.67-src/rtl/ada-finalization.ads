------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     A D A . F I N A L I Z A T I O N                      --
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
with System.Finalization_Implementation;
use System;

package Ada.Finalization is

   type Controlled is abstract
     new Finalization_Implementation.Root_Controlled with null record;

   procedure Initialize (Object : in out Controlled);
   procedure Split      (Object : in out Controlled) is abstract;
   procedure Finalize   (Object : in out Controlled) is abstract;

   Root_Part : Finalization_Implementation.Root_Controlled
     renames Finalization_Implementation.Root_Part;

   type Limited_Controlled is abstract
     new Finalization_Implementation.Root_Limited_Controlled with null record;

   procedure Initialize (Object : in out Limited_Controlled);
   procedure Finalize   (Object : in out Limited_Controlled) is abstract;

end Ada.Finalization;

