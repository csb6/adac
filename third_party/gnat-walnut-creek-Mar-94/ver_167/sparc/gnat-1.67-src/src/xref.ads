------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                   X R E F                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $                              --
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

--  The Xref package contains the routines for gathering all definition/use
--  information (entities and references to these entities). It creats a cross-
--  reference list as well as the REQs (required interfaces) for the withed
--  specs.

with Types; use Types;

package Xref is

   procedure Initialize_Xref;
   --  Sets the output flags in Etable to give the correct output.

   procedure Gather_Info (Top : Node_Id);
   --  Main procedure to build the entity tables and the REQs.
   --  Looks for new entities and new references, and - if found - adds
   --  them to the fitting entity table.

   procedure Write_Xref;
   --  Creates a cross reference list, containing all the entities and their
   --  corresponding references, which are found in the loaded files.
   --  The references are printed by file name and line numbers of their
   --  source location.

end Xref;
