------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             F R O N T E N D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $                              --
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

with Atree;    use Atree;
with Errout;   use Errout;
with Lib;      use Lib;
with Namet;    use Namet;
with Opt;      use Opt;
with Output;   use Output;
with Par;
with Rtsfind;  use Rtsfind;
with Sprint;   use Sprint;
with Scn;      use Scn;
with Sem;      use Sem;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Treepr;   use Treepr;
with Types;    use Types;
with Uintp;    use Uintp;
with Usage;

procedure Frontend is

begin
   --  Carry out package initializations. These are initializations which
   --  might logically be performed at elaboration time, were it not for
   --  the fact that we may be doing things more than once in the big loop
   --  over files. Like elaboration, the order in which these calls are
   --  made is in some cases important. For example, Lib cannot be
   --  initializd until Namet, since it uses names table entries.

   Initialize_Uint;
   Initialize_Errout;
   Initialize_Namet;
   Initialize_Snames;
   Initialize_Rtsfind;
   Initialize_Stringt;
   Initialize_Atree;
   Initialize_Lib;
   Initialize_Scanner (Main_Unit);

   if Verbose_Mode or Full_List then
      Write_Eol;

      if Operating_Mode = Generate_Code then
         Write_String ("Compiling: ");
      else
         Write_String ("Checking: ");
      end if;

      Write_Name (File.Table (Main_Unit).File_Name);
      Write_Eol;
   end if;

   Create_Standard;

   --  Here we call the parser to parse the compilation unit

   Par;

   --  If fatal error occurred, then reset mode to syntax check only

   if File.Table (Main_Unit).Fatal_Error then
      Operating_Mode := Check_Syntax;
   end if;

   --  Do dumps requested by appropriate debug flags. Note that these
   --  calls are done after resetting the operating mode, so that if
   --  we are in the last phase now, Operaing_Mode will indicate this.

   Tree_Dump ('P');
   Source_Dump ('P');

   --  Now run the semantics unless syntax only mode. Note that we
   --  alread set syntax only mode if a fatal error was detected.

   if Operating_Mode /= Check_Syntax then
      Semantics (File.Table (Main_Unit).Cunit);

      --  Do post semantics dumps

      Tree_Dump ('S');
      Source_Dump ('S');
   end if;

   --  List units if option set.

   if List_Units then
      Lib.List;
   end if;

end Frontend;
