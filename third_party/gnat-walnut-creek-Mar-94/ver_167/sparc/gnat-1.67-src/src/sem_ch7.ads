------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 7                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.8 $                              --
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
package Sem_Ch7  is

   procedure Analyze_Package_Body                       (N : Node_Id);
   procedure Analyze_Package_Declaration                (N : Node_Id);
   procedure Analyze_Package_Specification              (N : Node_Id);
   procedure Analyze_Private_Type_Declaration           (N : Node_Id);


   procedure End_Package_Scope (P : Entity_Id);
   --  At the end of a package declaration or body, declarations in the
   --  visible part are no longer directly visible, and declarations in
   --  the private part are not visible at all. For inner packages, place
   --  visible entities at the end of their homonym chains. For compilation
   --  units, make all entities invisible. In both cases, exchange private
   --  and visible declarations to restore order of elaboration.

   procedure Install_Visible_Declarations (P : Entity_Id);
   procedure Install_Private_Declarations (P : Entity_Id);

   --  On entrance to a package body, make declarations in package spec
   --  directly visible, and place them at the beginning of their
   --  homonym chains. This is done both for internal packages and for
   --  library packages. In the case of internal packages, the local
   --  entities are already on visibility chains, behind all directly
   --  visible declarations, and the previous entries must be removed
   --  from the chain.
   --  When compiling the body of a package,  both routines are called in
   --  succession. When compiling the body of a child package, the call
   --  to Install_Private_Declaration is immediate for private children,
   --  but is deffered until the compilation of the  private part of the
   --  child for public child packages.

   procedure Install_Package_Entity (Id : Entity_Id);
   --  Basic procedure for the previous two. Places one entity on its
   --  visibility chain, and recurses on the visible part if the entity
   --  is an inner package.

   function Unit_Requires_Body (P : Entity_Id) return Boolean;
   --  Check if a unit requires a body. A specification requires a body
   --  if it contains declarations that require completion in a body.

   function Is_Fully_Visible (Type_Id : Entity_Id) return Boolean;
   --  Indicates whether the Full Declaration of a private type is visible.

   procedure Mark_Implicit_Private_Decls (Impl_List : List_Id);
   --  Iterate through the list of implicit types and set the Is_Private
   --  flag for those that depend on private types.

end Sem_Ch7;
