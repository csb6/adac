------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              U N I X L I B                               --
--                                                                          --
--                                 B o d y                                  --
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

package body Unixlib is

   ----------------------
   -- Unix_File_Length --
   ----------------------

   function Unix_File_Length (FD : Unix_FD) return Int is

      function File_Length (FD : Unix_FD) return Int;
      pragma Interface (C, File_Length);
      pragma Interface_Name (File_Length, "file_length");

   begin
      return File_Length (FD);
   end Unix_File_Length;

   ---------------------------
   -- Unix_File_Time_Stamp  --
   ---------------------------

   function Unix_File_Time_Stamp (FD : Unix_FD) return Time_Stamp_Type is
      Stamp : Time_Stamp_Type;

      procedure File_Time (FD : Unix_FD; Stamp : in out Time_Stamp_Type);
      pragma Interface (C, File_Time);
      pragma Interface_Name (File_Time, "file_time");

   begin
      File_Time (FD, Stamp);
      return Stamp;
   end Unix_File_Time_Stamp;

end Unixlib;
