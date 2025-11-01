------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                B U T I L                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.3 $                              --
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
package Butil is

--  This package contains utility routines for the binder

   function Later (T1, T2 : Time_Stamp_Type) return Boolean;
   --  Return True if T1 represents a later time than T2

   function Uname_Less (U1, U2 : Unit_Name_Type) return Boolean;
   --  Determines if the unit name U1 is alphabetically before U2

   procedure Write_Unit_Name (U : Unit_Name_Type);
   --  Output unit name with (body) or (spec) after as required. On return
   --  Name_Len is set to the number of characters which were output.

end Butil;
