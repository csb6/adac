------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 2                              --
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

with Types; use Types;
package Sem_Ch12 is
   procedure Analyze_Generic_Package_Declaration        (N : Node_Id);
   procedure Analyze_Generic_Subprogram_Declaration     (N : Node_Id);
   procedure Analyze_Package_Instantiation              (N : Node_Id);
   procedure Analyze_Procedure_Instantiation            (N : Node_Id);
   procedure Analyze_Function_Instantiation             (N : Node_Id);
   procedure Analyze_Generic_Association                (N : Node_Id);
   procedure Analyze_Formal_Object_Declaration          (N : Node_Id);
   procedure Analyze_Formal_Type_Declaration            (N : Node_Id);
   procedure Analyze_Formal_Subprogram                  (N : Node_Id);
   procedure Analyze_Formal_Package                     (N : Node_Id);

   function Instantiate_Record (Parent_Type, Derived_Type : Entity_Id)
                                        return Node_Id;
   --  The process of record type derivation is identical to that of
   --  a parameterless generic instantiation: create new instances of
   --  each entity, and copy an arbitrary tree replacing terminal nodes
   --  with their instances. This procedure is called for type derivation
   --  and it reuses the generic instantiation machinery that otherwise
   --  applies to compilation units.

end Sem_Ch12;
