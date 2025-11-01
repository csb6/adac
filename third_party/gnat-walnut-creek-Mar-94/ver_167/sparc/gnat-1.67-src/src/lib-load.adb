------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . L O A D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.19 $                             --
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

with Atree;  use Atree;
with Debug;  use Debug;
with Output; use Output;
with Par;
with Scn;    use Scn;

separate (Lib)
function Load (Uname : Unit_Name_Type; Required : Boolean; Enode : Node_Id)
  return Unit_Number_Type is

   Calling_Unit : Unit_Number_Type;
   --  Unit number of unit generating this call to Load (set only if
   --  Enode is present, otherwise undefined)

   Unum : Unit_Number_Type;
   --  Number of returned unit

   Fname : File_Name_Type := Get_File_Name (Uname);
   --  File name corresponding to unit name

begin
   if Debug_Flag_L then
      Write_String ("*** Load request for unit: ");
      Write_Unit_Name (Uname);

      if Required then
         Write_String (" (Required = True)");
      else
         Write_String (" (Required = False)");
      end if;

      Write_Eol;
   end if;

   --  Capture error location if it is for the main unit. The idea is to post
   --  errors on the main unit location, not on the most recent invocation

   if Get_Sloc_Unit_Number (Sloc (Enode)) = Main_Unit then
      Load_Msg_Sloc := Sloc (Enode);
   end if;

   --  If we are generating error messages, then capture calling unit

   if Present (Enode) then
      Calling_Unit := Get_Sloc_Unit_Number (Sloc (Enode));
   end if;

   --  See if we already have an entry for this unit

   Unum := File.First;

   while Unum <= File.Last loop
      exit when Uname = File.Table (Unum).Unit_Name;
      Unum := Unum + 1;
   end loop;

   --  Whether or not the entry was found, Unum is now the right value, since
   --  it is one more than File.Last (i.e. the index of the new entry we will
   --  create) in the not found case.

   --  A special check is necessary in the unit not found case. If the unit
   --  is not found, but the file in which it lives has already been loaded,
   --  then we have the problem that the file does not contain the unit that
   --  is needed. We simply treat this as a file not found condition.

   if Unum > File.Last then
      for I in File.First .. File.Last loop
         if Fname = File.Table (I).File_Name then
            if Present (Enode) then
               Error_Msg_Name_1 := Fname;
               Error_Msg_Unit_1 := Uname;
               Error_Msg ("~File{ does not contain unit$", Load_Msg_Sloc);
               Write_Dependency_Chain;
            end if;

            if Debug_Flag_L then
               Write_String ("*** File does not contain unit, Unit_Number = ");
               Write_Int (Int (Unum));
               Write_Eol;
            end if;

            --  Build a dummy entry for this unit so that we do not keep
            --  repeating the message if the same unit is loaded again.

            File.Increment_Last;
            File.Table (Unum).Unit_Name := Uname;
            File.Table (Unum).File_Name := Fname;
            File.Table (Unum).Loading   := False;
            File.Table (Unum).Source    := null;
         end if;
      end loop;
   end if;

   --  If we are proceeding with load, then make load stack entry

   Load_Stack.Increment_Last;
   Load_Stack.Table (Load_Stack.Last) := Unum;

   --  Case of entry already in table

   if Unum <= File.Last then

      --  Here is where we check for a circular dependency, which is
      --  an attempt to load a unit which is currently in the process
      --  of being loaded. We do *not* care about a circular chain that
      --  leads back to a body, because this kind of circular dependence
      --  legitimately occurs (e.g. two package bodies that contain
      --  inlined subprogram referenced by the other).

      if File.Table (Unum).Loading
        and then Is_Spec_Name (File.Table (Unum).Unit_Name)
      then
         if Present (Enode) then
            Error_Msg ("~Circular unit dependency", Load_Msg_Sloc);
            Write_Dependency_Chain;
         end if;

         if Debug_Flag_L then
            Write_String ("*** Circular dependency encountered");
            Write_Eol;
         end if;
      end if;

      if Debug_Flag_L then
         Write_String ("*** Unit already in file table, Unit_Number = ");
         Write_Int (Int (Unum));
         Write_Eol;
      end if;

   --  Case of constructing a new entry

   else
      if Debug_Flag_L then
         Write_String ("*** Building new file table entry, Unit_Number = ");
         Write_Int (Int (Unum));
         Write_Eol;
      end if;

      File.Increment_Last;
      File.Table (Unum).Unit_Name         := Uname;
      File.Table (Unum).File_Name         := Fname;
      File.Table (Unum).Loading           := True;
      File.Table (Unum).Not_Found_Msg     := False;
      File.Table (Unum).Body_Spec         := False;
      File.Table (Unum).Fatal_Error       := False;
      File.Table (Unum).Identifier_Casing := Unknown;
      File.Table (Unum).Generate_Code     := False;
      File.Table (Unum).Version           := "      ";
      File.Table (Unum).Keyword_Casing    := Unknown;
      File.Table (Unum).Last_Line         := 1;
      File.Table (Unum).Lines_Table       :=
        new Lines_Table_Type (1 .. Alloc_Lines_Initial);

      --  Open file, and if it opens OK, go ahead and parse it

      File.Table (Unum).Source :=
        Read_Source_File (File.Table (Unum).File_Name, False);

      if File.Table (Unum).Source /= null then
         File.Table (Unum).Lines_Table (1) := File.Table (Unum).Source'First;
         File.Table (Unum).Time_Stamp      := Current_Source_File_Stamp;
         File.Table (Unum).Full_File_Name  := Full_Source_Name;
         Initialize_Scanner (Unum);
         Par;
         File.Table (Unum).Loading := False;
      end if;
   end if;

   --  Case of file not found (Unum is always set, but if the Source field
   --  is set, it means that the file was not found and we have an error)

   if File.Table (Unum).Source = null then

      if Debug_Flag_L then
         Write_String ("*** File was not found, Unit_Number = ");
         Write_Int (Int (Unum));
         Write_Eol;
      end if;

      --  Generate message if unit required and no message previously given

      if Required then
         if not File.Table (Unum).Not_Found_Msg
           and then Present (Enode)
         then
            Error_Msg_Name_1 := File.Table (Unum).File_Name;
            Error_Msg ("~file{ not found", Load_Msg_Sloc);
            Write_Dependency_Chain;
            File.Table (Unum).Not_Found_Msg := True;
         end if;

         --  In any case, if unit is required and not available, this is a
         --  fatal error for the caller, set flag accordingly.

         if Present (Enode) then
            File.Table (Calling_Unit).Fatal_Error := True;
         end if;
      end if;

      --  We mark the entry as loaded even though the load didn't succeed,
      --  since the meaning of .Loading being True is that the load is in
      --  progress, which is certainly not true in this case!

      File.Table (Unum).Loading := False;

      --  And in all cases of file not found, return No_Unit. Note that the
      --  entry is left in the File.Table with its Source field set to null
      --  to indicate that the file was not found, as per spec.

      Load_Stack.Decrement_Last;
      return No_Unit;

   --  Case of load completed successfully

   else
      if Debug_Flag_L then
         Write_String ("*** Load completed successfully, Unit_Number = ");
         Write_Int (Int (Unum));
         Write_Eol;
      end if;


      File.Table (Unum).Loading := False;

      --  If loaded unit had a fatal error, then caller inherits it!

      if File.Table (Unum).Fatal_Error
        and then Present (Enode)
      then
         File.Table (Calling_Unit).Fatal_Error := True;
      end if;

      --  Return the entry in the file table

      Load_Stack.Decrement_Last;
      return Unum;
   end if;
end Load;
