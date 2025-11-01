------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 3                              --
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

with Types; use Types;
package Sem_Ch13 is
   procedure Analyze_At_Clause                          (N : Node_Id);
   procedure Analyze_Attribute_Definition_Clause        (N : Node_Id);
   procedure Analyze_Enumeration_Representation_Clause  (N : Node_Id);
   procedure Analyze_Record_Representation_Clause       (N : Node_Id);
   procedure Analyze_Component_Clause                   (N : Node_Id);
   procedure Analyze_Code_Statement                     (N : Node_Id);
   procedure Analyze_Mod_Clause                         (N : Node_Id);

   function Freeze_Entity (E : Entity_Id) return List_Id;
   --  Freeze an entity, and return Freeze nodes, to be inserted at the
   --  point of call.

   function Freeze_All return List_Id;
   --  Before a non-instance body, or at the end of a declarative part
   --  freeze all entities therein that are not yet frozen.

   procedure Validate_Unchecked_Conversion (N : Node_Id; Act_Unit : Entity_Id);
   --  Validate a call to unchecked conversion. N is the node for the actual
   --  instantiation, which is used only for error messages. Act_Unit is the
   --  entity for the instantiation, from which the actual types etc for this
   --  instantiation can be determined.

end Sem_Ch13;
