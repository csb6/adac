------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  L I B                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.25 $                             --
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

with Alloc;   use Alloc;
with Errout;  use Errout;
with Fname;   use Fname;
with Libfmt;  use Libfmt;
with Limits;  use Limits;
with Namet;   use Namet;
with Namet;   use Namet;
with Osint;   use Osint;
with Sinfo;   use Sinfo;
with Uname;   use Uname;

package body Lib is

   Lib_Version : constant Str (1 .. 16) := "GNAT Lib v 1.0  ";
   --  Library version. This value must be updated whenever any change to the
   --  compiler affects the library formats in such a way as to obsolete
   --  previously compiled library modules.

   ----------------
   -- Local Data --
   ----------------

   Load_Msg_Sloc : Source_Ptr;
   --  Location for placing error messages (a token in the main source text)
   --  This is set from Sloc (Enode) by Load only in the case where this Sloc
   --  is in the main source file. This ensures that not found messages and
   --  circular dependency messages reference the original with in this source.

   package Load_Stack is new Table (
      Component_Type => Unit_Number_Type,
      Index_Type     => Nat,
      Low_Bound      => 0,
      Initial        => 10,
      Increment      => 100,
      Table_Name     => "Lib.Load_Stack");
   --  The Load_Stack table contains a list of unit numbers (indexes into the
   --  File.Table) of units being loaded on a single dependency chain. The
   --  First entry is the main unit. The second entry, if present is a unit
   --  on which the first unit depends, etc. This stack is used to generate
   --  error messages showing the dependency chain if a file is not found.
   --  The Load function makes an entry in this table when it is called, and
   --  removes the entry just before it returns.

   type Unit_Ref_Table is array (Pos range <>) of Unit_Number_Type;
   --  Type to hold list of indirect references to unit number table

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Sort (Tbl : in out Unit_Ref_Table);
   --  This procedure sorts the given unit reference table in order of
   --  ascending unit names, where the ordering relation is as described
   --  by the comparison routines provided by package Uname.

   procedure Write_Dependency_Chain;
   --  This procedure is used to generate error message info lines that
   --  trace the current dependency chain when a load error occurs.

   --------------------------
   -- Get_Sloc_Unit_Number --
   --------------------------

   function Get_Sloc_Unit_Number (S : Source_Ptr) return Unit_Number_Type is
   begin
      for U in File.First .. File.Last loop
         if File.Table (U).Source /= null
           and then S in File.Table (U).Source'range
         then
            return U;
         end if;
      end loop;

      --  If not in the table, must be the main source unit, and we just
      --  have not got it put into the table yet.

      return Main_Unit;
   end Get_Sloc_Unit_Number;

   ---------------------------
   -- Get_Cunit_Unit_Number --
   ---------------------------

   function Get_Cunit_Unit_Number (N : Node_Id) return Unit_Number_Type is
   begin
      for U in File.First .. File.Last loop
         if File.Table (U).Cunit = N then
            return U;
         end if;
      end loop;

      --  If not in the table, must be the main source unit, and we just
      --  have not got it put into the table yet.

      return Main_Unit;
   end Get_Cunit_Unit_Number;

   --------------------
   -- Initialize_Lib --
   --------------------

   procedure Initialize_Lib is
   begin
      File.Init;
      Load_Stack.Init;
      Load_Stack.Increment_Last;
      Load_Stack.Table (Load_Stack.Last) := Main_Unit;

      --  Initialize File.Table entry for Main_Unit. Note that we don't know
      --  the unit name yet, that gets filled in when the parser parses the
      --  main unit, at which time a check is made that it matches the main
      --  file name, and then the Unit_Name field is set.

      File.Increment_Last;
      File.Table (Main_Unit).File_Name := Next_Main_Source;

      if File.Table (Main_Unit).File_Name /= No_File then
         File.Table (Main_Unit).Source :=
           Read_Source_File (File.Table (Main_Unit).File_Name, True);
         File.Table (Main_Unit).Full_File_Name    := Full_Source_Name;
         File.Table (Main_Unit).Time_Stamp        := Current_Source_File_Stamp;
         File.Table (Main_Unit).Loading           := True;
         File.Table (Main_Unit).Fatal_Error       := False;
         File.Table (Main_Unit).Generate_Code     := False;
         File.Table (Main_Unit).Identifier_Casing := Unknown;
         File.Table (Main_Unit).Keyword_Casing    := Unknown;
         File.Table (Main_Unit).Not_Found_Msg     := False;
         File.Table (Main_Unit).Body_Spec         := False;
         File.Table (Main_Unit).Version           := "      ";
         File.Table (Main_Unit).Last_Line         := 1;
         File.Table (Main_Unit).Lines_Table :=
           new Lines_Table_Type (1 .. Alloc_Lines_Initial);
         File.Table (Main_Unit).Lines_Table (1) :=
           File.Table (Main_Unit).Source'First;
      end if;
   end Initialize_Lib;

   ------------------
   -- Finalize_Lib --
   ------------------

   procedure Finalize_Lib is
   begin
      for I in File.First .. File.Last loop
         Free (File.Table (I).Source);
         Free_Lines (File.Table (I).Lines_Table);
      end loop;
   end Finalize_Lib;

   ----------
   -- List --
   ----------

   procedure List is separate;

   ---------------
   -- Is_Loaded --
   ---------------

   function Is_Loaded (Uname : Unit_Name_Type) return Boolean is
   begin
      for Unum in File.First .. File.Last loop
         if Uname = File.Table (Unum).Unit_Name then
            return True;
         end if;
      end loop;

      return False;
   end Is_Loaded;

   ----------
   -- Load --
   ----------

   function Load (Uname : Unit_Name_Type; Required : Boolean; Enode : Node_Id)
     return Unit_Number_Type is separate;

   ----------
   -- Sort --
   ----------

   procedure Sort (Tbl : in out Unit_Ref_Table) is separate;

   ----------
   -- Writ --
   ----------

   procedure Writ is separate;

   ----------------------------
   -- Write_Dependency_Chain --
   ----------------------------

   procedure Write_Dependency_Chain is
   begin
      for U in Load_Stack.First .. Load_Stack.Last - 1 loop
         Error_Msg_Unit_1 :=
           File.Table (Load_Stack.Table (U)).Unit_Name;
         Error_Msg_Unit_2 :=
           File.Table (Load_Stack.Table (U + 1)).Unit_Name;

         Error_Msg ("unit$ depends on unit$!", Load_Msg_Sloc);
      end loop;
   end Write_Dependency_Chain;
end Lib;
