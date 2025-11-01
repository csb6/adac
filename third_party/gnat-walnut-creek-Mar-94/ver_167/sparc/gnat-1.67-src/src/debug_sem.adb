------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            D E B U G _ S E M                             --
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

with Atree;   use Atree;
with Debug;   use Debug;
with Sinfo;   use Sinfo;
with Sinput;  use Sinput;
with Output;  use Output;

package body Debug_Sem is

   Debug_A_Depth : Nat := 0;
   --  Output for the debug A flag is preceded by a sequence of vertical bar
   --  characters corresponding to the recursion depth of the actions being
   --  recorded (analysis, expansion, resolution and evaluation of nodes)
   --  This variable records the depth.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Debug_Output_Astring;
   --  Outputs Debug_A_Depth number of vertical bars, used to preface messages

   -------------------
   -- Debug_A_Entry --
   -------------------

   procedure Debug_A_Entry (S : Str; N : Node_Id) is
   begin
      if Debug_Flag_A then
         Debug_Output_Astring;
         Write_Str (S);
         Write_Str ("Node_Id = ");
         Write_Int (Int (N));
         Write_Location (Sloc (N));
         Write_String ("  ");
         Write_String (Node_Kind'Image (Nkind (N)));
         Write_Eol;
         Debug_A_Depth := Debug_A_Depth + 1;
      end if;
   end Debug_A_Entry;

   ------------------
   -- Debug_A_Exit --
   ------------------

   procedure Debug_A_Exit (S : Str; N : Node_Id; Comment : Str) is
   begin
      if Debug_Flag_A then
         Debug_A_Depth := Debug_A_Depth - 1;
         Debug_Output_Astring;
         Write_Str (S);
         Write_Str ("Node_Id = ");
         Write_Int (Int (N));
         Write_Str (Comment);
         Write_Eol;
      end if;
   end Debug_A_Exit;

   --------------------------                                           
   -- Debug_Output_Astring --                                           
   --------------------------                                           

   procedure Debug_Output_Astring is                                    
      Vbars : constant Str := "|||||||||||||||||||||||||";              

   begin                                                                
      if Debug_A_Depth > Vbars'Length then                              
         for I in Vbars'Length .. Debug_A_Depth loop                    
            Write_Char ('|');                                           
         end loop;                                                      

         Write_Str (Vbars);                                             

      else                                                              
         Write_Str (Vbars (1 .. Debug_A_Depth));                        
      end if;                                                           
   end Debug_Output_Astring;                                            

end Debug_Sem;
