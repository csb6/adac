------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 3                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.14 $                              --
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
package Sem_Ch3  is
   procedure Analyze_Component_Declaration              (N : Node_Id);
   procedure Analyze_Implicit_Type                      (N : Node_Id);
   procedure Analyze_Number_Declaration                 (N : Node_Id);
   procedure Analyze_Declarations                       (L : List_Id);
   procedure Analyze_Incomplete_Type_Decl               (N : Node_Id);
   procedure Analyze_Object_Declaration                 (N : Node_Id);
   procedure Analyze_Subtype_Declaration                (N : Node_Id);
   procedure Analyze_Subtype_Indication                 (N : Node_Id);
   procedure Analyze_Type_Declaration                   (N : Node_Id);
   procedure Analyze_Variant_Part                       (N : Node_Id);
   procedure Analyze_Private_Extension_Declaration      (N : Node_Id);

   function Access_Definition (N : Node_Id) return Entity_Id;
   --  An access definition defines a general access type for a formal
   --  parameter.  The procedure is called when processing formals, when
   --  the current scope is the subprogram. The Implicit type is placed
   --  in the enclosing scope, so that the only entities defined in the
   --  spec are the formals themselves.

   procedure Array_Type_Declaration (T : in out Entity_Id; Def : Node_Id);
   --  Process an array type declaration. If the array is constrained, we
   --  create an implicit parent array type, with the same index types and
   --  component type.

   procedure Access_Type_Declaration (T : Entity_Id; Def : Node_Id);
   --  Process an access type declaration

   procedure Check_Completion;
   --  At the end of a declarative part,  verify that all entities that
   --  require completion have received one.

   procedure Install_Implicit_Types (N : Node_Id);
   --  Insert the list of implicit types where it has to be :
   --    - before the concerned declaration, or
   --    - in the anonymous list in a Record Definition

   function Is_Derived_Type (Type_Id : Entity_Id) return Boolean;
   --  Determine if given entity is for a derived type

   function Is_Unconstrained_Type (E : Entity_Id) return Boolean;
   --  Determines if given entity is for an unconstrained array or
   --  record type (note that scalar types like Integer, which are
   --  technically unconstrained are not considered to be so for
   --  the purpose of this test)

   procedure Make_Index (I : Node_Id; 
                         Related_Id : Entity_Id := Empty; 
                         Postfix : Nat := 1);
   --  Process an index that is given in an array declaration, an entry
   --  family declaration or a loop iteration. The index is given by an
   --  index declaration (a 'box'), or by a discrete range. The later can
   --  be the name of a discrete type, or a subtype indication.

   procedure Make_Class_Type (T : Entity_Id);
   --  A Class_Type is created for each tagged type definition.
   --  The attributes of a class wide type are inherited from
   --  those of the type T. If T is introduced by a private declaration,
   --  the corresponding Class_Type is created at the same time, and
   --  therefore there is a private and a full declaration for the class
   --  type as well.

   function Process_Subtype (S          : Node_Id;
                             Related_Id : Entity_Id := Empty;
                             Suffix     : Str := "") return Entity_Id;
   --  Process a subtype indication and return corresponding entity. The 2
   --  last parameters are used to build the associated Implicit type name.

   procedure Process_The_Discriminants (N : Node_Id);
   --  Process discriminants of a type with discriminants

   function New_Class_Subtype (N : Node_Id; Ctyp : Entity_Id) return Entity_Id;
   --  Ctype is a Class type. This function defines the implicit type 
   --  corresponding to a new Class Subtype (a constrained class type). 
   --  see Expand_Class_Subtype for further details.

end Sem_Ch3;
