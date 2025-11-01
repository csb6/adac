------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                D E B U G                                 --
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

with Output;  use Output;

package body Debug is

   ----------------------
   -- Debug Flag Usage --
   ----------------------

   --  Documentation of debug flags for compiler

   --  da   Generate messages tracking semantic analyzer progress
   --  db   Generate software traceback code
   --  dc   List names of units as they are compiled
   --  dd   Dynamic allocation of tables messages generated
   --  de   List the entity table
   --  df   Full tree/source print (includes withed units)
   --  dg   For source trees, print only generated (rewritten) tree
   --  dh   Force unit loads even in syntax only mode
   --  di   Generate messages for visibility linking/delinking
   --  dj   Generate continuous traceback output
   --  dk   Kill with traceback if compiler error (even if errors)
   --  dl   Generate unit load trace messages
   --  dm
   --  dn   Generate messages for node/list allocation
   --  do   For source trees, print only original tree
   --  dp   Generate messages for parser scope stack push/pops
   --  dq
   --  dr   Generate parser resynchronization messages
   --  ds   Print source from tree after last phase
   --  dt   Print full tree after last phase
   --  du   Print full tree after Par (before Sem)
   --  dv   Generate file name/version variables
   --  dw   Print source from tree after Par (before Sem)
   --  dx   Force expansion on, even if no code being generated
   --  dy   Print tree of package Standard
   --  dz   Print source of package Standard

   --  d1   Error msgs have node numbers where possible
   --  d2
   --  d3
   --  d4
   --  d5
   --  d6
   --  d7
   --  d8
   --  d9

   --  Documentation of debug flags for binder

   --  da
   --  db
   --  dc  List units as they are chosen
   --  dd
   --  de
   --  df
   --  dg
   --  dh
   --  di
   --  dj
   --  dk
   --  dl
   --  dm
   --  dn  List details of manipulation of Num_Pred values
   --  do
   --  dp
   --  dq
   --  dr
   --  ds
   --  dt
   --  du
   --  dv
   --  dw
   --  dx
   --  dy
   --  dz

   --  d1
   --  d2
   --  d3
   --  d4
   --  d5
   --  d6
   --  d7
   --  d8
   --  d9

   ----------------------
   -- Get_Debug_Flag_K --
   ----------------------

   function Get_Debug_Flag_K return Boolean is
   begin
      return Debug_Flag_K;
   end Get_Debug_Flag_K;

   --------------------
   -- Set_Debug_Flag --
   --------------------

   procedure Set_Debug_Flag (C : Char; Val : Boolean := True) is
      subtype Dig is Char range '1' .. '9';
      subtype Let is Char range 'a' .. 'z';

   begin
      if C in Dig then
         case Dig (C) is
            when '1' => Debug_Flag_1 := Val;
            when '2' => Debug_Flag_2 := Val;
            when '3' => Debug_Flag_3 := Val;
            when '4' => Debug_Flag_4 := Val;
            when '5' => Debug_Flag_5 := Val;
            when '6' => Debug_Flag_6 := Val;
            when '7' => Debug_Flag_7 := Val;
            when '8' => Debug_Flag_8 := Val;
            when '9' => Debug_Flag_9 := Val;
         end case;

      else
         case Let (C) is
            when 'a' => Debug_Flag_A := Val;
            when 'b' => Debug_Flag_B := Val;
            when 'c' => Debug_Flag_C := Val;
            when 'd' => Debug_Flag_D := Val;
            when 'e' => Debug_Flag_E := Val;
            when 'f' => Debug_Flag_F := Val;
            when 'g' => Debug_Flag_G := Val;
            when 'h' => Debug_Flag_H := Val;
            when 'i' => Debug_Flag_I := Val;
            when 'j' => Debug_Flag_J := Val;
            when 'k' => Debug_Flag_K := Val;
            when 'l' => Debug_Flag_L := Val;
            when 'm' => Debug_Flag_M := Val;
            when 'n' => Debug_Flag_N := Val;
            when 'o' => Debug_Flag_O := Val;
            when 'p' => Debug_Flag_P := Val;
            when 'q' => Debug_Flag_Q := Val;
            when 'r' => Debug_Flag_R := Val;
            when 's' => Debug_Flag_S := Val;
            when 't' => Debug_Flag_T := Val;
            when 'u' => Debug_Flag_U := Val;
            when 'v' => Debug_Flag_V := Val;
            when 'w' => Debug_Flag_W := Val;
            when 'x' => Debug_Flag_X := Val;
            when 'y' => Debug_Flag_Y := Val;
            when 'z' => Debug_Flag_Z := Val;
         end case;
      end if;
   end Set_Debug_Flag;

end Debug;
