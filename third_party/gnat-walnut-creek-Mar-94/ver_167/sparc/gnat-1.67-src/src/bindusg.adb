------------------------------------------------------------------------------
--                                                                          --
--                        GBIND BINDER COMPONENTS                           --
--                                                                          --
--                             B I N D U S G                                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                            $Revision: 1.8 $                              --
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

with Osint;  use Osint;
with Output; use Output;
with Sysid;  use Sysid;
with Types;  use Types;
procedure Bindusg is

   procedure Write_Switch_Character is
   begin
      if MS_DOS then
         Write_Char ('/');
      else
         Write_Char ('-');
      end if;
   end Write_Switch_Character;

begin

   --  Usage line

   Write_String ("Usage: ");
   Write_Program_Name;
   Write_Char (' ');
   Write_String ("switches lfile");
   Write_Eol;
   Write_Eol;

   --  Line for -a switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("a      Require all source files to be");
   Write_String (" present");
   Write_Eol;

   --  Line for -b switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("b      Generate brief messages to std");
   Write_String ("err even if verbose mode set");
   Write_Eol;

   --  Line for -c switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("c      Check only, no generation of b");
   Write_String ("inder output file");
   Write_Eol;

   --  Line for -e switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("e      Output complete list of elabor");
   Write_String ("ation order dependencies");
   Write_Eol;

   --  Line for -i switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("ix     Identifier char set (x=1/2/3/4/p");
   Write_String ("/f/n) default = ");

   if MS_DOS then
      Write_String ("p (IBM/PC)");
   else
      Write_String ("1 (Latin-1)");
   end if;

   Write_Eol;

   --  Line for -l switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("l      Output chosen elaboration order");
   Write_Eol;

   --  Line for -m switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("mnnn   Limit number of detected error");
   Write_String ("s to nnn (1-999)");
   Write_Eol;

   --  Line for -n switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("n      No main program");
   Write_Eol;

   --  Line for -o switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("o file give the Output name (default is bind_xxx.c) ");
   Write_Eol;

   --  Line for -s switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("s      Check consistency of any source ");
   Write_String ("files that can be located");
   Write_Eol;

   --  Line for -t switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("t      Ignore time stamp errors");
   Write_Eol;

   --  Line for -v switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("v      Verbose mode. Error messages,");
   Write_String ("header, summary output to stdout");
   Write_Eol;

   --  Lines for -w switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("wx     Warning mode. (x=s/e for supp");
   Write_String ("ress/treat as error)");
   Write_Eol;

   --  Line for sfile

   Write_String ("  lfile   Library file names");

   if MS_DOS then
      Write_String (" (wild cards allowed for multiple files)");
   end if;

   Write_Eol;

end Bindusg;
