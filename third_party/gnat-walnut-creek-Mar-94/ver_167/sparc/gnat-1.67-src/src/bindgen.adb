------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              B I N D G E N                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.15 $                              --
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

with ALI;    use ALI;
with Binde;  use Binde;
with Namet;  use Namet;
with Opt;    use Opt;
with Osint;  use Osint;
with Types;  use Types;

package body Bindgen is

   Statement_Buffer : Str (1 .. 1000);
   --  Buffer used for constructing output statements

   Withed_Text_IO : Boolean := False;
   --  Flag which indicates whether the program has a context clause for
   --  units Text_IO or Ada.Text_IO.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Gen_Elab_Calls;
   --  Generate sequence of elaboration calls

   procedure Gen_Main_Program_File;
   --  Generate lines for output file in main program case

   procedure Gen_Non_Main_Program_File;
   --  Generate lines for output file in non-main program case

   ---------------------
   -- Gen_Output_File --
   ---------------------

   procedure Gen_Output_File is
   begin
      Create_Binder_Output;

      if Bind_Main_Program then
         Gen_Main_Program_File;
      else
         Gen_Non_Main_Program_File;
      end if;

      Close_Binder_Output;
   end Gen_Output_File;

   --------------------
   -- Gen_Elab_Calls --
   --------------------

   procedure Gen_Elab_Calls is
      L : Nat;
      Col : Nat;

   begin
      for E in Elab_Order.First .. Elab_Order.Last loop
         Get_Name_String (Unit.Table (Elab_Order.Table (E)).Uname);
         --  Temporary check to see if the unit Text_IO has been withed. If it
         --  has been we need to set the flag "Withed_Text_IO" so that there
         --  is a call to "ada__text_io__aux__textio_finalization" inserted
         --  immediately after the main program completes. When "finalization"
         --  is implemented this code will be removed and be replaced by the
         --  appropriate finalization action in Text_IO.

         if Name_Buffer (1 .. Name_Len) = "ada.text_io%s" then
            Withed_Text_IO := True;
         end if;
         Statement_Buffer (1 .. 3) := "   ";

         --  Copy the unit name (and replace '.' into '__' for child unit) 
         L := 4;
         for I in 1 .. Name_Len - 2 loop
            if Name_Buffer (I) /= '.' then
               Statement_Buffer (L) := Name_Buffer (I);
               L := L + 1;
            else
               Statement_Buffer (L .. L + 1) := "__";
               L := L + 2;
            end if;
         end loop;

         Statement_Buffer (L .. L + 6) := "___elab";
         Statement_Buffer (L + 7) := Name_Buffer (Name_Len);
         Statement_Buffer (L + 8 .. L + 11) := " ();";

         L := L + 11;

         --  If not spec that has an associated body, then generate a
         --  comment giving the name of the corresponding ALI file

         if Unit.Table (Elab_Order.Table (E)).Utype /= Is_Spec then

            --  Tab to column 41 if not already past it

            Col := L;
            Statement_Buffer (L + 1) := HT;
            L := L + 1;

            Col := (Col + 7) / 8 * 8;

            while Col < 40 loop
               Statement_Buffer (L + 1) := HT;
               L := L + 1;
               Col := Col + 8;
            end loop;

            --  Now output the file name as a comment

            Get_Name_String
              (ALIs.Table (Unit.Table (Elab_Order.Table (E)).My_ALI).Afile);
            Statement_Buffer (L + 1 .. L + 5) := "  /* ";
            Statement_Buffer (L + 6 .. L + Name_Len + 5) :=
              Name_Buffer (1 .. Name_Len);
            L := L + Name_Len + 5;
            Statement_Buffer (L + 1 .. L + 3) := " */";
            L := L + 3;
         end if;

         Write_Binder_Info (Statement_Buffer (1 .. L));
      end loop;
   end Gen_Elab_Calls;

   ---------------------------
   -- Gen_Main_Program_File --
   ---------------------------

   procedure Gen_Main_Program_File is
   begin

      Write_Binder_Info ("#include <string.h> "                              );
      Write_Binder_Info ("   "                                               );
      Write_Binder_Info ("/* predefined exceptions */ "                      );
      Write_Binder_Info ("char constraint_error; "                           );
      Write_Binder_Info ("char numeric_error; "                              );
      Write_Binder_Info ("char program_error; "                              );
      Write_Binder_Info ("char storage_error; "                              );
      Write_Binder_Info ("char tasking_error; "                              );
      Write_Binder_Info ("  "                                                );
      Write_Binder_Info ("static int static_argc; "                          );
      Write_Binder_Info ("static char * *static_argv; "                      );
      Write_Binder_Info ("  "                                                );
      Write_Binder_Info ("int arg_count () { return static_argc; } "         );
      Write_Binder_Info ("  "                                                );
      Write_Binder_Info ("int len_arg (arg_num) "                            );
      Write_Binder_Info ("   int arg_num; "                                  );
      Write_Binder_Info ("   { return strlen(static_argv[arg_num]); } "      );
      Write_Binder_Info ("  "                                                );
      Write_Binder_Info ("int fill_arg (a, i) "                              );
      Write_Binder_Info ("   char * a; "                                     );
      Write_Binder_Info ("   int i; "                                        );
      Write_Binder_Info ("{ strncpy (a, static_argv[i], "                    );
      Write_Binder_Info ("     strlen(static_argv[i])); } "                  );
      Write_Binder_Info ("  "                                                );
      Write_Binder_Info ("void catch_except (ptr, i) "                       );
      Write_Binder_Info ("     void *ptr; "                                  );
      Write_Binder_Info ("     int i; "                                      );
      Write_Binder_Info ("{ "                                                );
      Write_Binder_Info ("  extern char *__gnat_exception; "                 );
      Write_Binder_Info (" "                                                 );
      Write_Binder_Info ("  if (!ptr)"                                       );
      Write_Binder_Info ("    {"                                             );
      Write_Binder_Info ("      if (__gnat_exception == &constraint_error)"  );
      Write_Binder_Info ("        puts (""\nraised Constraint_Error\n"");"   );
      Write_Binder_Info ("      else if (__gnat_exception == &numeric_error)");
      Write_Binder_Info ("        puts (""\nraised Numeric_Error\n"");"      );
      Write_Binder_Info ("      else if (__gnat_exception == &program_error)");
      Write_Binder_Info ("        puts (""\nraised Program_Error\n"");"      );
      Write_Binder_Info ("      else if (__gnat_exception == &storage_error)");
      Write_Binder_Info ("        puts (""\nraised Storage_Error\n"");"      );
      Write_Binder_Info ("      else"                                        );
      Write_Binder_Info ("        puts (""\nraised unhandled exception\n"");");
      Write_Binder_Info (" "                                                 );
      Write_Binder_Info ("      exit (1);"                                   );
      Write_Binder_Info ("   }"                                              );
      Write_Binder_Info ("  else"                                            );
      Write_Binder_Info ("    longjmp (ptr, i);"                             );
      Write_Binder_Info ("}"                                                 );
      Write_Binder_Info (" "                                                 );
      Write_Binder_Info ("void __gnat_raise_constraint ()"                   );
      Write_Binder_Info ("{"                                                 );
      Write_Binder_Info ("  extern char *__gnat_exception; "                 );
      Write_Binder_Info ("  extern void *__gnat_jmpbuf; "                    );
      Write_Binder_Info ("  __gnat_exception = &constraint_error;"           );
      Write_Binder_Info (""                                                  );
      Write_Binder_Info ("  catch_except (__gnat_jmpbuf, 1);"                );
      Write_Binder_Info ("}"                                                 );
      Write_Binder_Info (" "                                                 );

      if ALIs.Table (ALIs.First).Main_Program = Proc then
         Write_Binder_Info ("void main (argc, argv)"                         );
      else
         Write_Binder_Info ("int main (argc, argv)"                          );
      end if;

      Write_Binder_Info ("   int argc;"                                      );
      Write_Binder_Info ("   char * argv[];"                                 );
      Write_Binder_Info ("{ "                                                );
      Write_Binder_Info ("   static_argc = argc;"                            );
      Write_Binder_Info ("   static_argv = argv;"                            );
      Write_Binder_Info (" "                                                 );

      Gen_Elab_Calls;

      Write_Binder_Info (" ");
      Get_Name_String (Unit.Table (Unit.First).Uname);

      --  Main program is procedure case

      if ALIs.Table (ALIs.First).Main_Program = Proc then
         Statement_Buffer (1 .. 8) := "   _ada_";
         Statement_Buffer (9 .. Name_Len + 6) :=
           Name_Buffer (1 .. Name_Len - 2);
         Statement_Buffer (Name_Len + 7 .. Name_Len + 10) := " ();";
         Write_Binder_Info (Statement_Buffer (1 .. Name_Len + 10));

      --  Main program is function case

      else -- ALIs.Table (ALIs.First).Main_Program = Func
         Statement_Buffer (1 .. 16) := "   return (_ada_";
         Statement_Buffer (17 .. Name_Len + 14) :=
           Name_Buffer (1 .. Name_Len - 2);
         Statement_Buffer (Name_Len + 15 .. Name_Len + 19) := " ());";
         Write_Binder_Info (Statement_Buffer (1 .. Name_Len + 19));
      end if;

      --  Generate a call to the text_io procedure which performs cleanup
      --  operations on temporary files. See comment above at the beginning
      --  of Gen_Elab_Calls.
      if Withed_Text_IO then
         Write_Binder_Info ("   ada__text_io__aux__text_io_finalization ();");
      end if;
      Write_Binder_Info ("   exit (0);");
      Write_Binder_Info ("}");
   end Gen_Main_Program_File;

   -------------------------------
   -- Gen_Non_Main_Program_File --
   -------------------------------

   procedure Gen_Non_Main_Program_File is
   begin
      Write_Binder_Info ("void ada__bind ()" );
      Write_Binder_Info ("{ "                );
      Gen_Elab_Calls;
      Write_Binder_Info ("}"                 );
   end Gen_Non_Main_Program_File;
end Bindgen;
