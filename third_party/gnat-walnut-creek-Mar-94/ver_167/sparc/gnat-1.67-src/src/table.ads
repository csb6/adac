------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                T A B L E                                 --
--                                                                          --
--                                 S p e c                                  --
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

with Types; use Types;
with Unchecked_Deallocation;
generic
   type Component_Type is private;
   type Index_Type     is range <>;

   Low_Bound  : Index_Type;
   Initial    : Pos;
   Increment  : Pos;
   Table_Name : Str;

package Table is

--  This package provides an implementation of dynamically resizable one
--  dimensional arrays. The idea is to mimic the normal Ada semantics for
--  arrays as closely as possible with the one additional capability of
--  dynamically modifying the value of the Last attribute.

   --  Component_Type and Index_Type specify the type of the array, Low_Bound
   --  is the lower bound. Index_type must be an integer type. The effect is
   --  roughly to declare:

   --    Table : array (Low_Bound .. <>) of Component_Type;

   --  The Initial and Increment values control the allocation of the table.
   --  When the table is first allocated, the Initial value controls the actual
   --  size of the allocated table. The Increment value is a percentage value
   --  used to determine the increase in the table size (e.g. 100 = increase
   --  table size by 100%, i.e. double it). The Table_Name parameter is simply
   --  use in debug output messages it has no other usage, and is not
   --  referenced in non-debugging mode.

   --  The Last and Set_Last subprograms provide control over the current
   --  logical allocation. They are quite efficient, so they can be used freely
   --  freely (expensive reallocation occurs only at major granularity chunks,
   --  controlled by the allocation parameters.

   type Table_Type is array (Index_Type range <>) of Component_Type;
   --  Type for the table

   type Table_Ptr is access Table_Type;
   --  The table is actually represented as a pointer to allow reallocation

   Table : Table_Ptr := null;
   --  The table itself. The lower bound is the value of Low_Bound. Logically
   --  the upper bound is the current value of Last (although the actual size
   --  of the allocated table may be larger than this). The program may only
   --  access and modify Table entries in the range First .. Last.

   procedure Init;
   --  This procedure allocates a new table of size Initial (freeing any
   --  previously allocated larger table). It is not necessary to call
   --  Init when a table is first instantiated (since the instantiation does
   --  the same initialization steps). However, it is harmless to do so, and
   --  Init is convenient in reestablishing a table for new use.

   function Last return Index_Type;
   pragma Inline (Last);
   --  Returns the current value of the last used entry in the table, which
   --  can then be used as a subscript for Table. Note that the only way to
   --  modify Last is to call the Set_Last procedure. Last must always be
   --  used to determine the logically last entry.

   First : constant Index_Type := Low_Bound;
   --  Export First as synonym for Low_Bound (to be parallel with use of Last)

   procedure Set_Last (New_Val : Index_Type);
   pragma Inline (Set_Last);
   --  This procedure sets Last to the indicated value. If necessary the
   --  table is reallocated to accomodate the new value (i.e. on return
   --  the allocated table has an upper bound of at least Last). If Set_Last
   --  reduces the size of the table, then logically entries are removed from
   --  the table. If Set_Last increases the size of the table, then new entries
   --  are logically added to the table.

   procedure Increment_Last;
   pragma Inline (Increment_Last);
   --  Adds 1 to Last (sane as, but faster than, Set_Last (Last + 1).

   procedure Decrement_Last;
   pragma Inline (Decrement_Last);
   --  Subtracts 1 from Last (same as, but faster than, Set_Last (Last - 1).

   function Copy return Table_Ptr;
   --  This function returns a copy of the current table contents. The length
   --  of the table is determined by the current value of Last. The current
   --  contents of the table, and the setting of Last, are not modified.

   procedure Free is new Unchecked_Deallocation (Table_Type, Table_Ptr);
   --  Procedure used to free a table obtained by the Copy function

end Table;
