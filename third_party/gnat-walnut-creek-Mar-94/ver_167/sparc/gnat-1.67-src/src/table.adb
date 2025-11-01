------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                T A B L E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.10 $                             --
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

with Debug;  use Debug;
with Output; use Output;
package body Table is

   Last_Val : Int;
   --  Current value of Last. Note that we declare this in the body because
   --  we don't want the client to modify Last except through one of the
   --  official interfaces (since a modification to Last may require a
   --  reallocation of the table).

   Min : Int;
   --  Subscript of the minimum entry in the currently allocated table

   Max : Int;
   --  Subscript of the maximum entry in the currently allocated table

   Length : Int := 0;
   --  Number of entries in currently allocated table. The value of zero
   --  ensures that we initially allocate the table.

   procedure Reallocate;
   --  Reallocate and extend the existing table

   ----------
   -- Init --
   ----------

   procedure Init is
      Old_Length : Int := Length;

   begin
      Min := Int (First);
      Last_Val := Min - 1;
      Max := Min + Initial - 1;
      Length := Max - Min + 1;

      --  If table is same size as before (happens when table is never
      --  expanded which is a common case), then simply reuse it, else free
      --  the old table and allocate a new one of the proper size.

      if Old_Length /= Length then
         Free (Table);
         Table := new Table_Type (Index_Type (Min) .. Index_Type (Max));
      end if;
   end Init;

   ----------
   -- Last --
   ----------

   function Last return Index_Type is
   begin
      return Index_Type (Last_Val);
   end Last;

   --------------
   -- Set_Last --
   --------------

   procedure Set_Last (New_Val : Index_Type) is
      Old_Last : Int;

   begin
      if Int (New_Val) < Last_Val then
         Last_Val := Int (New_Val);
      else
         Old_Last := Last_Val;
         Last_Val := Int (New_Val);

         if Last_Val > Max then
            Reallocate;
         end if;
      end if;
   end Set_Last;

   --------------------
   -- Increment_Last --
   --------------------

   procedure Increment_Last is
   begin
      Last_Val := Last_Val + 1;

      if Last_Val > Max then
         Reallocate;
      end if;
   end Increment_Last;

   --------------------
   -- Decrement_Last --
   --------------------

   procedure Decrement_Last is
   begin
      Last_Val := Last_Val - 1;
   end Decrement_Last;

   ----------
   -- Copy --
   ----------

   function Copy return Table_Ptr is
   begin
      return new Table_Type'(Table (First .. Last));
   end Copy;

   ----------------
   -- Reallocate --
   ----------------

   procedure Reallocate is
      Old_Table : Table_Ptr := Table;
      Old_Max   : Int := Max;

   begin
      while Max < Last_Val loop
         Length := Length * (100 + Increment) / 100;
         Max := Min + Length - 1;
      end loop;

      Table := new Table_Type (Index_Type (Min) .. Index_Type (Max));

      if Debug_Flag_D then
         Write_String ("--> Allocating new ");
         Write_Str (Table_Name);
         Write_String (" table, size = ");
         Write_Int (Max - Min + 1);
         Write_Eol;
      end if;

      for I in Min .. Old_Max loop
         Table (Index_Type (I)) := Old_Table (Index_Type (I));
      end loop;

      Free (Old_Table);
   end Reallocate;

begin
   Init;
end Table;
