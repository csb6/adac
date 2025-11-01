------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 6                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.5 $                              --
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
package Sem_Ch6 is

   procedure Analyze_Abstract_Subprogram_Declaration    (N : Node_Id);
   procedure Analyze_Function_Call                      (N : Node_Id);
   procedure Analyze_Operator_Symbol                    (N : Node_Id);
   procedure Analyze_Procedure_Call                     (N : Node_Id);
   procedure Analyze_Subprogram_Declaration             (N : Node_Id);
   procedure Analyze_Subprogram_Body                    (N : Node_Id);

   function Analyze_Spec (N : Node_Id) return Entity_Id;
   --  Analyze subprogram specification in both subprogram declarations
   --  and body declarations.

   function Find_Corresponding_Spec (N : Node_Id) return Entity_Id;
   --  Use the subprogram specification in the body to retrieve the previous
   --  subprogram declaration, if any.

   function Mode_Conformant (S1, S2 : Entity_Id) return Boolean;
   --  Determine whether two callable entities (subprograms, entries,
   --  literals) are mode conformant, i.e. have the same type-result
   --  profile, and corresponding parameters have the same mode.

   procedure New_Overloaded_Entity (S : Entity_Id);
   --  Process new overloaded entity. Overloaded entities are created
   --  by enumeration type declarations, subprogram specifications,
   --  entry declarations, and (implicitly) by type derivations.

   procedure Process_Formals (S : Entity_Id; T : List_Id);
   --  Enter the formals in the scope of the subprogram or entry, and 
   --  analyze default expressions if any.

   procedure Set_Formal_Mode (Formal_Id : Entity_Id);
   --  Set proper Ekind to reflect formal mode (in, out, in out)

   function Type_Conformant (S1, S2 : Entity_Id) return Boolean;
   --  Determine whether two callable entities (subprograms, entries,
   --  literals) are type conformant, i.e. have same type-result profile

end Sem_Ch6;
