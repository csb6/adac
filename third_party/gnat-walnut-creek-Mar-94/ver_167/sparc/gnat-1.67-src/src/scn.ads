------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  S C N                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.9 $                              --
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
package Scn is

--  This package contains the lexical analyzer routines

   procedure Initialize_Scanner (Unit : Unit_Number_Type);
   --  Initialize lexical scanner for scanning a new file. The caller has
   --  completed the construction of the File.Table entry for the specified
   --  Unit (and the Source field is known to be non-null).

   procedure Scan;
   --  Scan scans out the next token, and advances the scan state accordingly
   --  (see package Scan_State for details). If the scan encounters an illegal
   --  token, then an error message is issued pointing to the bad character,
   --  and Scan returns a reasonable substitute token of some kind.

   function Scan_First_Char return Source_Ptr;
   --  This routine returns the position in Source of the first non-blank
   --  character on the current line, used for certain error recovery actions.

   procedure Scan_Reserved_Identifier (Force_Msg : Boolean);
   --  This procedure is called to convert the current token, which the caller
   --  has checked is for a reserved word, to an equivalent identifier. This is
   --  of course only used in error situations where the parser can detect that
   --  a reserved word is being used as an identifier. An appropriate error
   --  message, pointing to the token, is also issued if either this is the
   --  first occurrence of misuse of this identifier, or if Force_Msg is True.

   procedure GNAT_Column_Check (P : Source_Ptr);
   --  Check to see if P points to the first non-blank character of the
   --  current line, and if so, check that it is in an appropriate column.
   --  Called only if GNAT style check mode is set (GNAT_Style_Check is True).

end Scn;
