------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              U N I X L I B                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.11 $                             --
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

--  This package contains some interface functions to the Unix library

with System; use System;
with Types;  use Types;

package Unixlib is

   type Unix_FD is new Int;
   --  Unix file descriptor

   Unix_Standin  : constant Unix_FD := 0;
   Unix_Standout : constant Unix_FD := 1;
   Unix_Standerr : constant Unix_FD := 2;
   --  File descriptors for standard input output files

   function Unix_File_Length (FD : Unix_FD) return Int;
   --  Get length of unix file from descriptor

   function Unix_File_Time_Stamp (FD : Unix_FD) return Time_Stamp_Type;
   --  Get time stamp of file from descriptor

   procedure Unix_Write (FD : Unix_FD; A : Address; N : Int);
   pragma Interface (C, Unix_Write);
   pragma Interface_Name (Unix_Write, "write");
   --  Write N bytes from address A to file referenced by FD

   procedure Unix_Exit  (Status : Int);
   pragma Interface (C, Unix_Exit);
   pragma Interface_Name (Unix_Exit, "exit");
   --  Exit to Unix with given status code (program is terminated)

   procedure Unix_Abort;
   pragma Interface (C, Unix_Abort);
   pragma Interface_Name (Unix_Abort, "abort");
   --  Exit to Unix signalling an abort (traceback or other appropriate
   --  diagnostic information should be given if possible, or entry made
   --  to the debugger if that is possible).

   function  Unix_Read  (FD : Unix_FD; A : Address; N : Int) return Int;
   pragma Interface (C, Unix_Read);
   pragma Interface_Name (Unix_Read, "read");
   --  Read N bytes to address A from file referenced by FD. Returned value
   --  is count of bytes actually read, which can be less than N at EOF.

   function  Unix_Open_Read (Name : Address) return Unix_FD;
   pragma Interface (C, Unix_Open_Read);
   pragma Interface_Name (Unix_Open_Read, "open_read");
   --  Open file Name (NUL-terminated) for reading, returning file descriptor
   --  File descriptor returned is negative if file cannot be opened.

   function Unix_Create_File (Name : Address) return Unix_FD;
   pragma Interface (C, Unix_Create_File);
   pragma Interface_Name (Unix_Create_File, "open_create");
   --  Creates new file with given name (NUL-terminated) for writing, returning
   --  file descriptor for subsequent use in Unix_Write calls. File desctiptor
   --  returned is negative if file cannot be successfully created.

   procedure Unix_Close (FD : Unix_FD);
   pragma Interface (C, Unix_Close);
   pragma Interface_Name (Unix_Close, "close");
   --  Close file referenced by FD

end Unixlib;
