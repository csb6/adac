------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUNTIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T R A C E B A C K                      --
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

with Debug; use Debug;

pragma Ada_9X;
package body System.Traceback is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Output_Traceback_Entry (L : Line_Num; F : Address; S : Address); 
   --  Outputs one traceback entry line, where L is the line number, F is     
   --  the file name address, and S is the subprogram name address.           

   --------------------------
   -- C Library procedures --
   --------------------------

   procedure puts (S : Address);
   pragma Interface (C, puts);

   procedure printf (F : Address; S : Address);
   pragma Interface (C, printf);

   procedure putchar (C : Character);
   pragma Interface (C, putchar);

   function strlen (S : Address) return Natural;
   pragma Interface (C, strlen);

   ---------------------
   -- Local Variables --
   ---------------------

   Bufsize : constant := 256;
   --  Number of entries in traceback buffer, must be a power of 2

   subtype Bufptr is Natural range 0 .. Bufsize - 1;

   Lines : array (Bufptr) of Line_Num := (others => 0);
   --  Lines entries in traceback buffer. Zero indicates an unused entry

   Fnams : array (Bufptr) of Address;
   --  File name entries in traceback buffer

   Snams : array (Bufptr) of Address;
   --  Subprogram name entries in traceback buffer

   Buffer_Ptr : Bufptr := 0;
   --  Traceback buffer pointer: next location to be set. The entries are used
   --  in a circular manner.

   P_String : constant String := "%s" & Ascii.NUL;
   --  Format for printing string with printf

   --------------
   -- Store_TB --
   --------------

   procedure Store_TB (N : Line_Num; F : Address; S : Address) is
   begin
      --  If no debug flag J, then just store the traceback table entry

      if not Debug_Flag_J then
         Lines (Buffer_Ptr) := N;
         Fnams (Buffer_Ptr) := F;
         Snams (Buffer_Ptr) := S;
         Buffer_Ptr := (Buffer_Ptr + 1) mod Bufsize;
         Tracebacks_Stored := True;

      --  If debug flag J is set, otuput the trace message now

      else
         Output_Traceback_Entry (N, F, S);
      end if;
   end Store_TB;

   ----------------------
   -- Output_Traceback --
   ----------------------

   procedure Output_Traceback is
      Header : constant String := "Abort: traceback follows" & Ascii.Nul;
      L      : Natural;
      I      : Bufptr;

   begin
      putchar (Ascii.LF);
      puts (Header'Address);
      putchar (Ascii.LF);
      I := Buffer_Ptr;

      loop
         if Lines (I) /= 0 then
            Output_Traceback_Entry (Lines (I), Fnams (I), Snams (I));
         end if;

         I := (I + 1) mod Bufsize;
         exit when I = Buffer_Ptr;
      end loop;

      putchar (Ascii.LF);
      putchar (Ascii.LF);
   end Output_Traceback;

   ----------------------------
   -- Output_Traceback_Entry --
   ----------------------------

   procedure Output_Traceback_Entry                                           
     (L : Line_Num; F : Address; S : Address)                                 
   is                                                                         
      D : Natural;                                                            

   begin                                                                      
      --  Output line number (5 digits, leading zero suppressed)              

      D := 10000;                                                             

      for J in 1 .. 5 loop                                                    
         if D > 1 and then L < D then                                         
            putchar (' ');                                                    
         else                                                                 
            putchar (Character'Val (48 + (L / D) mod 10));                    
         end if;                                                              

         D := D / 10;                                                         
      end loop;                                                               

      putchar ('.');                                                          
      putchar (' ');                                                          

      --  Output the file name                                                

      printf (P_String'Address, F);                                           

      --  Following test should really use Null_Address, but that wasn't      
      --  working in GNAT version 1.62, so zero was substituted ???.

      if S /= 0 then                                                  
         putchar (' ');                                                       

         for I in strlen (F) .. 18 loop
            putchar (' ');                                                    
         end loop;                                                            

         printf (P_String'Address, S);                                        
      end if;                                                                 

      putchar (Ascii.LF);                                                     
   end Output_Traceback_Entry;                                                

end System.Traceback;
