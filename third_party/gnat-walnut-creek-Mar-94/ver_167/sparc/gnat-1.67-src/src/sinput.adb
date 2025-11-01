------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S I N P U T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.24 $                             --
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

with Alloc;  use Alloc;
with Lib;    use Lib;
with Namet;  use Namet;
with Output; use Output;
with Scans;  use Scans;

package body Sinput is

   -----------------
   -- Backup_Line --
   -----------------

   procedure Backup_Line (P : in out Source_Ptr) is
      Src : constant Source_Buffer_Ptr := Get_Source_Buffer_Ptr (P);

   begin
      P := P - 1;
      if P = Src'First then return; end if;

      if Src (P) = CR then
         if Src (P - 1) = LF then
            P := P - 1;
         end if;

      else -- Src (P) = LF
         if Src (P - 1) = CR then
            P := P - 1;
         end if;
      end if;

      --  Now find first character of the previous line

      while P > Src'First
        and then Src (P - 1) /= LF
        and then Src (P - 1) /= CR
      loop
         P := P - 1;
      end loop;
   end Backup_Line;

   ----------------
   -- Line_Start --
   ----------------

   function Line_Start (P : Source_Ptr) return Source_Ptr is
      S   : Source_Ptr := P;
      Src : constant Source_Buffer_Ptr := Get_Source_Buffer_Ptr (P);

   begin
      while S > Src'First
        and then Src (S - 1) /= CR
        and then Src (S - 1) /= LF
      loop
         S := S - 1;
      end loop;

      return S;
   end Line_Start;

   --------------------
   -- Get_Col_Number --
   --------------------

   function Get_Col_Number (P : Source_Ptr) return Column_Number_Type is
      S     : Source_Ptr;
      C     : Column_Number_Type;
      Src : constant Source_Buffer_Ptr := Get_Source_Buffer_Ptr (P);

   begin
      S := Line_Start (P);
      C := 1;

      while S < P loop
         if Src (S) = HT then
            C := (C - 1) / 8 * 8 + (8 + 1);
         else
            C := C + 1;
         end if;

         S := S + 1;
      end loop;

      return C;
   end Get_Col_Number;

   ---------------------
   -- Get_Line_Number --
   ---------------------

   function Get_Line_Number (P : Source_Ptr) return Line_Number_Type is
      Sunit : Unit_Number_Type;
      Table : Lines_Table_Ptr;
      Lo    : Line_Number_Type;
      Hi    : Line_Number_Type;
      Mid   : Line_Number_Type;

   begin
      --  If the input source pointer is not a meaningful value then return
      --  at once with line number 1. This can happen for a file not found
      --  condition for a file loaded indirectly by RTE, and also perhaps on
      --  some unknown internal error conditions. In either case we certainly
      --  don't want to blow up.

      if P <  1 then
         return 1;

      --  Otherwise we can do the binary search

      else
         Sunit := Get_Sloc_Unit_Number (P);
         Table := File.Table (Sunit).Lines_Table;
         Lo    := 1;
         Hi    := File.Table (Sunit).Last_Line;

         loop
            Mid := (Lo + Hi) / 2;

            if P < Table (Mid) then
               Hi := Mid - 1;

            else -- P >= Table (Mid)

               if Mid = Hi or else
                  P < Table (Mid + 1)
               then
                  return Mid;
               else
                  Lo := Mid + 1;
               end if;

            end if;

         end loop;
      end if;
   end Get_Line_Number;

   ---------------------------
   -- Get_Source_Buffer_Ptr --
   ---------------------------

   function Get_Source_Buffer_Ptr (P : Source_Ptr) return Source_Buffer_Ptr is
      Sunit : Unit_Number_Type := Get_Sloc_Unit_Number (P);
      Table : Lines_Table_Ptr  := File.Table (Sunit).Lines_Table;

   begin
      if P not in Source_Cache'range then
         Source_Cache := File.Table (Get_Sloc_Unit_Number (P)).Source;
      end if;

      return Source_Cache;
   end Get_Source_Buffer_Ptr;

   ---------------
   -- Next_Line --
   ---------------

   procedure Next_Line is
      Lines_Table : Lines_Table_Ptr :=
        File.Table (Scan_Unit).Lines_Table;

      Last_Line : Line_Number_Type :=
        File.Table (Scan_Unit).Last_Line;

   begin
      Current_Line_Start := Scan_Ptr;

      --  Make entry in lines table if not already made (in some scan backup
      --  cases, we will be rescanning previously scanned source, so the entry
      --  may have already been made on the previous forward scan).

      if Source (Scan_Ptr) /= EOF
        and then Scan_Ptr > Lines_Table (Last_Line)
      then

         --  Reallocate the lines table if it has got too large. Note that we
         --  don't use the normal Table package mechanism because we have
         --  several of these tables, one for each source file.

         if Last_Line = Lines_Table'Last then

            declare
               New_Lines_Table : Lines_Table_Ptr :=
                  new Lines_Table_Type
                    (1 .. (Last_Line * (100 + Alloc_Lines_Increment) / 100));
            begin
               New_Lines_Table (1 .. Lines_Table'Last) :=
                 Lines_Table (1 .. Lines_Table'Last);
               Free_Lines (Lines_Table);
               Lines_Table := New_Lines_Table;
               File.Table (Scan_Unit).Lines_Table := Lines_Table;
            end;
         end if;

         Last_Line := Last_Line + 1;
         Lines_Table (Last_Line) := Current_Line_Start;
         File.Table (Scan_Unit).Last_Line := Last_Line;
      end if;
   end Next_Line;

   --------------------
   -- Write_Location --
   --------------------

   procedure Write_Location (P : Source_Ptr) is
      Scol : Int;

   begin
      if P = No_Location then
         Write_String ("  <no location>");
      elsif P = Standard_Location then
         Write_String ("  <standard location>");
      else
         Write_String ("  """);
         Write_Name (File.Table (Get_Sloc_Unit_Number (P)).File_Name);
         Write_String (""", line ");
         Scol := Column;
         Write_Int (Int (Get_Line_Number (P)));
         Write_Char ('(');
         Write_Int (Int (Get_Col_Number (P)));
         Write_Char (')');

         while Column < Scol + 7 loop
            Write_Char (' ');
         end loop;
      end if;
   end Write_Location;
end Sinput;
