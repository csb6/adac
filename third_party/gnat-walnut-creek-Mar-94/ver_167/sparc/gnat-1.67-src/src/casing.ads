------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               C A S I N G                                --
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

package Casing is

   --  This package contains data and subprograms to support the feature that
   --  recognizes the letter case styles used in the source program being
   --  compiled, and uses this information for error message formatting, and
   --  for recognizing reserved words that are misused as identifiers.

   -------------------------------
   -- Case Control Declarations --
   -------------------------------

   type Casing_Type is (All_Upper_Case, All_Lower_Case, Mixed_Case, Unknown);
   --  This type is used to describe the casing style for identifiers/keywords

   ------------------------------
   -- Case Control Subprograms --
   ------------------------------

   function Determine_Token_Casing return Casing_Type;
   --  Determines the casing style of the current token, which is either a
   --  keyword or an identifier. For identifiers, the result may be Unknown
   --  (for example, the identifiers X, Y_3, M4, and A_B do not sufficiently
   --  distinguish between All_Upper_Case and Mixed_Case). This subprogram
   --  can be called only during parsing.

   procedure Set_Casing (C : Casing_Type; D : Casing_Type := Mixed_Case);
   --  Takes the name stored in the first Name_Len positions of Name_Buffer
   --  and modifies it to be consistent with the casing given by C,
   --  or, if C = Unknown, then with the casing given by D.

end Casing;
