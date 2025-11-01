------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                F N A M E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.16 $                             --
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

with Csets;   use Csets;
with Comperr; use Comperr;
with Namet;   use Namet;
with Opt;     use Opt;
with Osint;   use Osint;

package body Fname is

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name (Uname : Unit_Name_Type) return File_Name_Type is
      Max_Namelen : Int;
      Uname_Ptr   : Int;
      Fname_Ptr   : Int;
      Unit_Char   : Char;    -- 's' or 'b' for spec or body

   begin
      Max_Namelen := System_Maximum_File_Name_Length;

      if Maximum_File_Name_Length < Max_Namelen then
         Max_Namelen := Maximum_File_Name_Length;
      end if;

      Get_Name_String (Uname);
      Uname_Ptr := 1;
      Fname_Ptr := 0;

      while Name_Buffer (Uname_Ptr) /= '%' loop
         pragma Assert (Uname_Ptr <= Name_Len, Compiler_Abort); -- missing %

         --  Special treatment for period

         if Name_Buffer (Uname_Ptr) = '.' then
            Fname_Ptr := Fname_Ptr + 1;
            Name_Buffer (Fname_Ptr) := '-';

         --  Otherwise just copy character, folding to lower case

         else
            Fname_Ptr := Fname_Ptr + 1;
            Name_Buffer (Fname_Ptr) := Fold_Lower (Name_Buffer (Uname_Ptr));
         end if;

         Uname_Ptr := Uname_Ptr + 1;
      end loop;

      Unit_Char := Name_Buffer (Uname_Ptr + 1);

      --  The file name (minus the extension) to be used is stored in
      --  Name_Buffer (1 .. Fname_Ptr). If it's too long then crunch it.

      if Fname_Ptr > Max_Namelen then

         --  First step in the crunch is to replace an initial System- by
         --  s- and an initial Ada- by a- to crunch runtime library file names

         if Fname_Ptr > 7 and then Name_Buffer (1 .. 7) = "system-" then
            Name_Buffer (1) := 's';
            Name_Buffer (2) := '-';

            for I in 8 .. Fname_Ptr loop
               Name_Buffer (I - 5) := Name_Buffer (I);
            end loop;

            Fname_Ptr := Fname_Ptr - 5;

         elsif Fname_Ptr > 4 and then Name_Buffer (1 .. 4) = "ada-" then
            Name_Buffer (1) := 'a';
            Name_Buffer (2) := '-';

            for I in 5 .. Fname_Ptr loop
               Name_Buffer (I - 2) := Name_Buffer (I);
            end loop;

            Fname_Ptr := Fname_Ptr - 2;
         end if;
      end if;

      --  If the file name is still too long, then next step is to remove
      --  all but the first and last components of a subunit or child name

      if Fname_Ptr > Max_Namelen then

         declare
            First_Minus : Int := Fname_Ptr;
            Last_Minus  : Int := 0;

         begin
            for I in 1 .. Fname_Ptr loop
               if Name_Buffer (I) = '-' then
                  First_Minus := I;
                  exit;
               end if;
            end loop;

            for I in reverse 1 .. Fname_Ptr loop
               if Name_Buffer (I) = '-' then
                  Last_Minus := I;
                  exit;
               end if;
            end loop;

            if First_Minus < Last_Minus then
               Name_Buffer
                 (First_Minus .. First_Minus + Fname_Ptr - Last_Minus) :=
                   Name_Buffer (Last_Minus .. Fname_Ptr);
               Fname_Ptr := Fname_Ptr - (Last_Minus - First_Minus);
            end if;
         end;
      end if;

      --  If that did not do the trick, we are still too long, so now
      --  is the time for the real crunching operation. The crunching
      --  algorithm is as follows:

      --    Divide the name into pieces delimited by underscores or minus signs
      --    Find the longest piece (leftmost if two are equally long)
      --    Remove last character of this piece
      --    Repeat until name is short enough

      while Fname_Ptr > Max_Namelen loop
         declare
            Long_Length : Int := 0;
            Long_Last   : Int := 0;
            Piece_Start : Int;
            Ptr         : Int;

         begin
            Ptr := 1;

            --  Loop through pieces to find longest piece

            while Ptr <= Fname_Ptr loop
               Piece_Start := Ptr;

               --  Loop through characters in one piece of name

               while Ptr <= Fname_Ptr
                 and then Name_Buffer (Ptr) /= '_'
                 and then Name_Buffer (Ptr) /= '-'
               loop
                  Ptr := Ptr + 1;
               end loop;

               if Ptr - Piece_Start > Long_Length then
                  Long_Length := Ptr - Piece_Start;
                  Long_Last := Ptr - 1;
               end if;

               Ptr := Ptr + 1;
            end loop;

            --  Remove last character of longest piece

            Name_Buffer (Long_Last .. Fname_Ptr - 1) :=
              Name_Buffer (Long_Last + 1 .. Fname_Ptr);
            Fname_Ptr := Fname_Ptr - 1;
         end;
      end loop;

      --  Here with the file name set and of OK length, add the extension

      Fname_Ptr := Fname_Ptr + 1;
      Name_Buffer (Fname_Ptr) := '.';
      Fname_Ptr := Fname_Ptr + 1;
      Name_Buffer (Fname_Ptr) := 'a';
      Fname_Ptr := Fname_Ptr + 1;
      Name_Buffer (Fname_Ptr) := 'd';
      Fname_Ptr := Fname_Ptr + 1;
      Name_Buffer (Fname_Ptr) := Unit_Char;

      Name_Len := Fname_Ptr;
      return File_Name_Type (Name_Find);

   end Get_File_Name;

end Fname;
