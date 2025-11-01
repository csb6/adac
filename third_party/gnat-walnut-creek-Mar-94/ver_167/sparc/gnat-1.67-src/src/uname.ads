------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                U N A M E                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.12 $                             --
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
package Uname is

   ---------------------------
   -- Unit Name Conventions --
   ---------------------------

   --  Units are associated with a unique ASCII name as follows. First we
   --  have the fully expanded name of the unit, with lower case letters and
   --  periods. Following this is one of the following suffixes:

   --    %s  for package/subprogram/generic declarations (specs)
   --    %b  for package/subprogram bodies

   --  The general rule is that the active library (i.e. the units required
   --  by the current unit being compiled) cannot contain duplicate units,
   --  defined as units with identical names according to the above rule.

   --  Unit names are stored in the names table, and referred to by the
   --  corresponding Name_Id values.

   --  Types as a synonym for Name_Id and is used to indicate that a Name_Id
   --  holding a unit name (as defined here) is expected.

   --  Note: as far as possible the conventions for unit names are encapsulated
   --  in this package. The one exception is that package Fname, which provides
   --  conversion routines from unit names to file names must be aware of the
   --  precise conventions that are used.

   -------------------
   -- Display Names --
   -------------------

   --  For display purposes, unit names are printed out with the suffix
   --  " (body)" for a body and " (spec)" for a spec. These formats are
   --  used for the Write_Unit_Name and Get_Unit_Name_String subprograms.

   -----------------
   -- Subprograms --
   -----------------

   function Name_To_Unit_Name (N : Name_Id) return Unit_Name_Type;
   --  Given the Id of the Ada name of a unit, this function returns the
   --  corresponding unit name of the spec (by appending %s to the name).

   function Get_Unit_Name (N : Node_Id) return Unit_Name_Type;
   --  This procedure is passed one of the following nodes:

      --  N_Subprogram_Declaration         (spec) cases
      --  N_Package_Declaration
      --  N_Generic_Declaration
      --  N_With_Clause
      --  N_Function_Instantiation
      --  N_Package_Instantiation
      --  N_Procedure_Instantiation
      --  N_Pragma (Elaborate case)

      --  N_Package_Body                   (body) cases
      --  N_Subprogram_Body

      --  N_Subprogram_Body_Stub           (subunit) cases
      --  N_Package_Body_Stub
      --  N_Task_Body_Stub
      --  N_Protected_Body_Stub
      --  N_Subunit

   function Get_Spec_Name (N : Unit_Name_Type) return Unit_Name_Type;
   --  Given the name of a body, this function returns the name of the
   --  corresponding spec, i.e. characters %b replaced by %s

   function Get_Body_Name (N : Unit_Name_Type) return Unit_Name_Type;
   --  Given the name of a spec, this function returns the name of the
   --  corresponding body, i.e. characters %s replaced by %b

   function Get_Parent_Body_Name (N : Unit_Name_Type) return Unit_Name_Type;
   --  Given the name of a subunit, returns the name of the parent body.

   function Get_Parent_Spec_Name (N : Unit_Name_Type) return Unit_Name_Type;
   --  Given the name of a child unit spec or body, returns the unit name
   --  of the parent spec. Returns No_Name if the given name is not the name
   --  of a child unit.

   procedure Get_Unit_Name_String (N : Unit_Name_Type);
   --  Places the display name of the unit in Name_Buffer and sets Name_Len
   --  to the length of the stored name, i.e. it uses the same interface as
   --  the Get_Name_String routine in the Namet package.

   procedure Write_Unit_Name (N : Unit_Name_Type);
   --  Given a unit name, this procedure writes the display name to the
   --  standard output file. Name_Buffer and Name_Len are set as described
   --  above for the Get_Unit_Name_String call on return.

   function Is_Spec_Name (N : Unit_Name_Type) return Boolean;
   --  Returns True iff the given name is the unit name of a specification
   --  (i.e. if it ends with the characters %s).

   function Is_Body_Name (N : Unit_Name_Type) return Boolean;
   --  Returns True iff the given name is the unit name of a body (i.e. if
   --  it ends with the characters %b).

   function Is_Child_Name (N : Unit_Name_Type) return Boolean;
   --  Returns True iff the given name is a child unit name (of either a
   --  body or a spec).

   function Uname_Lt (Left, Right : Unit_Name_Type) return Boolean;
   function Uname_Gt (Left, Right : Unit_Name_Type) return Boolean;
   function Uname_Le (Left, Right : Unit_Name_Type) return Boolean;
   function Uname_Ge (Left, Right : Unit_Name_Type) return Boolean;
   --  These functions perform lexicographic ordering of unit names. The
   --  ordering is suitable for printing, and is not quite a straightforward
   --  comparison of the names, since the convention is that specs appear
   --  before bodies. Note that the standard = and /= operators work fine
   --  because all unit names are hashed into the name table, so if two names
   --  are the same, they always have the same Name_Id value.

end Uname;
