------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                F N A M E                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.7 $                              --
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

--  This package defines the association between source file names and
--  unit names as defined in package Uname.

with Types; use Types;
package Fname is

   --  Note: this package spec does not depend on the Uname spec in the Ada
   --  sense, but the comments and description of the semantics do depend on
   --  the conventions established by Uname, which explains the "with Uname".

   ---------------------------
   -- File Name Conventions --
   ---------------------------

   --  GNAT requires that there be a one to one correspondence between source
   --  file names (as used in the Osint package interface) and unit names as
   --  defined by the Uname package. This correspondence is defined by the
   --  two subprograms defined here in the Fname package.

   --  The body of this package is potentially system dependent, since file
   --  naming conventions do differ from operating system to operating system.
   --  However, the code in the body of Fname does not typically require any
   --  operating system interface, and furthermore, we choose a convention
   --  that is likely to be widely implementable, and certainly is one that
   --  can be shared between Unix, DOS, NT, Mac OS and OS/2.

   --  Since we do expect this convention to be followed widely, and since
   --  Osint depends on the convention, it is described here in the Spec.
   --  However, no unit (other than Osint) in any way depends on the choices
   --  described here.

   --  Unit names are the Ada names, with all upper case letters, and a suffix
   --  that is either %b or %s for bodies and specs respectively. This is the
   --  convention described and implemented in package Uname.

   --  Source file names are obtained by taking the unit name, excluding the
   --  %b or %s, and replacing all upper case letters with lower case and
   --  periods with minus signs. The extension is either ads or adb for a
   --  spec or body respectively.

   --  The rules in Ada 83 mode are identical, except identical, except that in
   --  the case of subunit names with more than two components, the inner names
   --  and their associated dashes are omitted (this corresponds to the Ada/83
   --  rule that subunits have to be unique in their first and last names).

   --  Examples of these rules are

   --    Unit name        File name (9X)      File name (83)

   --    PACKGE1%s        packge.ads          package.ads
   --    PACKGE1%b        packge.adb          packge.adb
   --    SCN.NLIT%b       scn-nlit.adb        scn-nlit.adb
   --    CHILD.PKG%s      child-pkg.ads       child-pkg.ads
   --    XYZ.ARG.LMS%b    xyz-arg-lms.adb     xyz-lms.adb
   --    ABC.DEF.GHI%s    abc-def-ghi.ads     (no child units)

   --  Note that the file name does *not* include the directory name. The
   --  management of directories is completely hidden in the Osint body.

   -----------------
   -- Subprograms --
   -----------------

   function Get_File_Name (Uname : Unit_Name_Type) return File_Name_Type;
   --  This function returns the file name that corresponds to a given unit
   --  name. The caller is responsible for ensuring that the unit name meets
   --  the requirements given in package Uname.

end Fname;
