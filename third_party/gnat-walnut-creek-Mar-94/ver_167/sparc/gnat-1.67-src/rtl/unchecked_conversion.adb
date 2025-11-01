------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   U N C H E C K E D _ C O N V E R S I O N                --
--                                                                          --
--                                 B o d y                                  --
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

--  Obviously the body below is incorrect, since it is simply a checked
--  conversion. Indeed it is not possible to write a correct body for
--  this generic function in Ada. The approach we take is to provide this
--  bogus body, which is read in as a generic template using the normal
--  mechanisms, then there is a special circuit (in Sem_Ch11) which checks
--  for unchecked conversion and sets the Unchecked_Conversion flag in the
--  N_Conversion node in the template.

function Unchecked_Conversion (Source_Object : Source) return Target is
begin
   return Target (Source_Object);
end Unchecked_Conversion;
