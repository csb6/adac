------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUNTIME COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . T R A C E B A C K                     --
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

--  This module provides the software traceback facility. An internal buffer
--  is maintained which stores the last N entries made (N is an internal
--  constant defined in the body).


pragma Ada_9X;
package System.Traceback is

   --  The following type is used to pass traceback name information

   subtype Line_Num is Natural;
   --  Line numbers in traceback buffer

   Tracebacks_Stored : Boolean := False;
   --  Set True if any entries made in traceback buffer

   procedure Store_TB (N : Line_Num; F : Address; S : Address);
   --  Store a traceback entry in the traceback buffer. N is the line number,
   --  F is the address of the null terminated file name, and S is the address
   --  of the null terminated subprogram name, with Null_Address indicating
   --  that no subprogram name is available.

   procedure Output_Traceback;
   --  Output a traceback dump. Does not affect the contents of the buffer.

end System.Traceback;
