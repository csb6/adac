------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                U S A G E                                 --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                            $Revision: 1.27 $                             --
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

with Opt;    use Opt;
with Osint;  use Osint;
with Output; use Output;
with Sysid;  use Sysid;
with Types;  use Types;
procedure Usage is

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
   Write_String ("switches sfile");
   Write_Eol;
   Write_Eol;

   --  Line for -a switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("a      Assertions enabled. Pragma Ass");
   Write_String ("ert and pragma Debug to be activated");
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
   Write_String ("c      Check syntax and semantics onl");
   Write_String ("y (no code generation attempted)");
   Write_Eol;

   --  Line for -e switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("e      Error messages generated immed");
   Write_String ("iately, not saved up till end");
   Write_Eol;

   --  Line for -f switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("f      Full errors. Normally only fir");
   Write_String ("st error on each line is reported.");
   Write_Eol;

   --  Line for -g switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("g      GNAT style checks enabled");
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

   --  Line for -k switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("knnn   Limit file names to nnn characters (k = krunch)");
   Write_Eol;

   --  Line for -l switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("l      Output full source listing wi");
   Write_String ("th embedded error messages");
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
   Write_String ("n      No inlining of subprograms (");
   Write_String ("ignore pragma Inline)");
   Write_Eol;

   --  Line for -p switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("p      Suppress all checks");
   Write_Eol;

   --  Line for -r switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("r      Reference manual column layout");
   Write_String (" required");
   Write_Eol;

   --  Lines for -s switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("s      Syntax check only");
   Write_Eol;

   --  Lines for -t switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("t      Try semantics, even if parse errors");
   Write_Eol;

   --  Line for -u switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("u      List units for this compilation");
   Write_Eol;

   --  Line for -v switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("v      Verbose mode. Full error outp");
   Write_String ("ut with source lines to stdout");
   Write_Eol;

   --  Lines for -w switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("wx     Warning mode. (x=s/e for supp");
   Write_String ("ress/treat as error)");
   Write_Eol;

   --  Line for -83 switch

   Write_String ("  ");
   Write_Switch_Character;
   Write_String ("83     Enforce Ada 83 restrictions");
   Write_Eol;

   --  Line for sfile

   Write_String ("  sfile   Source file names");

   if MS_DOS then
      Write_String (" (wild cards allowed for multiple files)");
   end if;

   Write_Eol;

end Usage;
