------------------------------------------------------------------------------

--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               B C H E C K                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.6 $                              --
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

with ALI;     use ALI;
with Binderr; use Binderr;
with Gnatvsn; use Gnatvsn;
with Libfmt;  use Libfmt;
with Namet;   use Namet;
with Opt;     use Opt;

package body Bcheck is

   --------------------
   -- Check_Versions --
   --------------------

   procedure Check_Versions is
   begin
      for A in ALIs.First .. ALIs.Last loop

         if ALIs.Table (A).Ver /= Library_Version then
            Error_Msg_Name_1 := Unit.Table (ALIs.Table (A).First_Unit).Sfile;

            if Ignore_Time_Stamp_Errors then
               Error_Msg
                 ("?% should be recompiled (compiled with old version)");
            else
               Error_Msg ("% must be recompiled (compiled with old version)");
            end if;
         end if;

         if ALIs.Table (A).Std /= Standard_Version then
            Error_Msg_Name_1 := Unit.Table (ALIs.Table (A).First_Unit).Sfile;

            if Ignore_Time_Stamp_Errors then
               Error_Msg
                 ("?% should be recompiled (wrong version of Standard)");
            else
               Error_Msg ("% must be recompiled (wrong version of Standard)");
            end if;
         end if;
      end loop;
   end Check_Versions;

   -----------------------
   -- Check_Consistency --
   -----------------------

   procedure Check_Consistency is
      Src : Source_Id;
      --  Source file Id for this Sdep entry

   begin
      --  Loop through ALI files

      ALIs_Loop : for A in ALIs.First .. ALIs.Last loop

         --  Loop through Sdep entries in one ALI file

         Sdep_Loop : for D in
           ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep
         loop
            Src := Source_Id (Get_Name_Table_Info (Sdep.Table (D).Sfile));

            --  If stamp does not match, generate error message

            if Sdep.Table (D).Stamp /= Source.Table (Src).Stamp then
               Error_Msg_Name_1 := ALIs.Table (A).Sfile;
               Error_Msg_Name_2 := Sdep.Table (D).Sfile;

               --  Two styles of message, depending on whether or not
               --  the updated file is the one that must be recompiled

               if Error_Msg_Name_1 = Error_Msg_Name_2 then
                  if Ignore_Time_Stamp_Errors then
                     Error_Msg
                        ("?% has been modified and should be recompiled");
                  else
                     Error_Msg
                       ("% has been modified and must be recompiled");
                  end if;

               else
                  if Ignore_Time_Stamp_Errors then
                     Error_Msg
                       ("?% should be recompiled (% has been modified)");
                  else
                     Error_Msg
                       ("% must be recompiled (% has been modified)");
                  end if;
               end if;

               --  Exit from the loop through Sdep entries once we find one
               --  that does not match.

               exit Sdep_Loop;
            end if;

         end loop Sdep_Loop;
      end loop ALIs_Loop;
   end Check_Consistency;

end Bcheck;
