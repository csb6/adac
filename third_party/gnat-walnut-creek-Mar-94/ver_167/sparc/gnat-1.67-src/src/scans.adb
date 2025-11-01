------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S C A N S                                 --
--                                                                          --
--                                 B o d y                                  --
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

package body Scans is

   ---------------------
   -- Save_Scan_State --
   ---------------------

   procedure Save_Scan_State (Saved_State : out Saved_Scan_State) is
   begin
      Saved_State.Save_Scan_Ptr           := Scan_Ptr;
      Saved_State.Save_Token              := Token;
      Saved_State.Save_Token_Ptr          := Token_Ptr;
      Saved_State.Save_Current_Line_Start := Current_Line_Start;
      Saved_State.Save_Start_Column       := Start_Column;
      Saved_State.Save_Token_Node         := Token_Node;
      Saved_State.Save_Token_Name         := Token_Name;
      Saved_State.Save_Prev_Token         := Prev_Token;
      Saved_State.Save_Prev_Token_Ptr     := Prev_Token_Ptr;
   end Save_Scan_State;

   ------------------------
   -- Restore_Scan_State --
   ------------------------

   procedure Restore_Scan_State (Saved_State : in Saved_Scan_State) is
   begin
      Scan_Ptr           := Saved_State.Save_Scan_Ptr;
      Token              := Saved_State.Save_Token;
      Token_Ptr          := Saved_State.Save_Token_Ptr;
      Current_Line_Start := Saved_State.Save_Current_Line_Start;
      Start_Column       := Saved_State.Save_Start_Column;
      Token_Node         := Saved_State.Save_Token_Node;
      Token_Name         := Saved_State.Save_Token_Name;
      Prev_Token         := Saved_State.Save_Prev_Token;
      Prev_Token_Ptr     := Saved_State.Save_Prev_Token_Ptr;
   end Restore_Scan_State;

end Scans;
