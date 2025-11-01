------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               T R E E P R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.47 $                             --
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

with Atree;   use Atree;
with Csets;   use Csets;
with Debug;   use Debug;
with Einfo;   use Einfo;
with Lib;     use Lib;
with Namet;   use Namet;
with Opt;     use Opt;
with Output;  use Output;
with Sinfo;   use Sinfo;
with Sinput;  use Sinput;
with Stand;   use Stand;
with Stringt; use Stringt;
with Treeprs; use Treeprs;
with Uintp;   use Uintp;
with Uname;   use Uname;
with Unchecked_Deallocation;
package body Treepr is

   use Atree.Unchecked_Access;
   --  This module uses the unchecked access functions in package Atree
   --  since it does an untyped traversal of the tree (we do not want to
   --  count on the structure of the tree being correct in this routine!)

   ----------------------------------
   -- Approach Used for Tree Print --
   ----------------------------------

   --  When a complete subtree is being printed, a trace phase first marks
   --  the nodes and lists to be printed. This trace phase allocates logical
   --  numbers corresponding to the order in which the nodes and lists will
   --  be printed. These logical numbers are recorded in the following tables.
   --  Note that the numbers start at one, so that an initial value of zero
   --  indicates that a node or list has not yet been visited. The reason
   --  for using these numbers rather than the node id values is that we
   --  print the nodes and lists in a logical order corresponding to the
   --  pointer link structure, and thus the List_Id and Node_Id values
   --  would be out of order and hard to find.

   type Node_Numbers_Type is array (Node_Id range <>) of Nat;
   type Access_Node_Numbers_Type is access Node_Numbers_Type;
   Node_Numbers : Access_Node_Numbers_Type;

   type List_Numbers_Type is array (List_Id range <>) of Nat;
   type Access_List_Numbers_Type is access List_Numbers_Type;
   List_Numbers : Access_List_Numbers_Type;

   Printing_Descendants : Boolean;
   --  True if descendants are being printed, False if not. In the false case,
   --  only node Id's are printed. In the true case, node numbers as well as
   --  node Id's are printed, as described above.

   type Phase_Type is (Marking, Printing);
   --  Type for Phase variable

   Phase : Phase_Type;
   --  When an entire tree is being printed, the traversal operates in two
   --  phases. The first phase marks the nodes in use by installing node
   --  numbers in the node number table. The second phase prints the nodes.
   --  This variable indicates the current phase.

   Next_Sequence_Number : Int;
   --  Number of last visited node or list. Used during the marking phase to
   --  set proper node numbers in the node number tables, and during the
   --  printing phase to make sure that a given node is not printed more than
   --  once (nodes are printed in order during the printing phase, that's the
   --  point of numbering them in the first place!)

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Print_Init;
   --  Initialize for printing of tree with descendents

   procedure Print_Term;
   --  Clean up after printing of tree with descendents

   procedure Print_Char (C : Char);
   --  Print character C if currently in print phase, noop if in marking phase

   procedure Print_Entity_Kind (E : Entity_Id);
   --  Print entity kind name in mixed case if in print phase, noop if in
   --  marking phase.

   procedure Print_Name (N : Name_Id);
   --  Print name from names table if currently in print phase, noop if in
   --  marking phase. Note that the name is output in mixed case mode.

   procedure Print_Node_Kind (N : Node_Id);
   --  Print node kind name in mixed case if in print phase, noop if in
   --  marking phase.

   procedure Print_Str (S : Str);
   --  Print string S if currently in print phase, noop if in marking phase

   procedure Print_Int (I : Int);
   --  Print integer I if currently in print phase, noop if in marking phase

   procedure Print_Eol;
   --  Print end of line if currently in print phase, noop if in marking phase

   procedure Print_Node_Ref (N : Node_Id);
   --  Print "<empty>", "<error>" or "Node #nnn" with additional information
   --  in the latter case, including the Id and the Nkind of the node.

   procedure Print_List_Ref (L : List_Id);
   --  Print "<no list>", or "<empty node list>" or "Node list #nnn"

   procedure Print_Elist_Ref (E : Elist_Id);
   --  Print "<no elist>", or "<empty element list>" or "Element list #nnn"

   procedure Print_Field (Val : Int);
   --  Print representation of Field value (name, tree, string, uint, charcode)

   procedure Print_Flag (F : Boolean);
   --  Print True or False

   procedure Print_Node (N : Node_Id; Prefix_Str : Str; Prefix_Char : Char);
   --  This is the internal routine used to print a single node. Each line of
   --  output is preceded by Prefix_Str (which is used to set the indentation
   --  level and the bars used to link list elements). In addition, for lines
   --  other than the first, an additional character Prefix_Char is output.

   procedure Visit_Node (N : Node_Id; Prefix_Str : Str; Prefix_Char : Char);
   --  Called to process a single node in the case where descendents are to
   --  be printed before every line, and Prefix_Char added to all lines
   --  except the header line for the node.

   procedure Visit_List (L : List_Id; Prefix_Str : Str);
   --  Visit_List is called to process a list in the case where descendents
   --  are to be printed. Prefix_Str is to be added to all printed lines.

   procedure Visit_Elist (E : Elist_Id; Prefix_Str : Str);
   --  Visit_Elist is called to process an element list in the case where
   --  descendents are to be printed. Prefix_Str is to be added to all
   --  printed lines.

   ---------------
   -- Tree_Dump --
   ---------------

   procedure Tree_Dump (S : Char) is
      Max_Unit : Unit_Number_Type;

      procedure Underline;
      --  Put underline under string we just printed

      procedure Underline is
         Col : constant Int := Column;

      begin
         Write_Eol;

         while Col > Column loop
            Write_Char ('-');
         end loop;

         Write_Eol;
      end Underline;

   --  Start of processing for Tree_Dump

   begin
      if Debug_Flag_Y
        and then (S = 'S' or else Operating_Mode = Check_Syntax)
      then
         Write_Eol;
         Write_String ("Tree created for Standard (spec) ");

         if S = 'P' then
            Write_Str ("after parsing");
         else
            Write_Str ("after semantics");
         end if;

         Underline;
         Print_Node_Subtree (Standard_Package_Node);
         Write_Eol;
      end if;

      if (Debug_Flag_T and then
            (S = 'S' or else Operating_Mode = Check_Syntax))
        or else
         (Debug_Flag_U and then
            (S = 'P' and then Operating_Mode /= Check_Syntax))
      then
         if Debug_Flag_F then
            Max_Unit := File.Last;
         else
            Max_Unit := Main_Unit;
         end if;

         for U in Main_Unit .. Max_Unit loop
            if File.Table (U).Source /= null then
               Write_Eol;
               Write_String ("Tree created for ");
               Write_Unit_Name (File.Table (U).Unit_Name);

               if S = 'P' then
                  Write_Str (" after parsing ");
               else
                  Write_Str (" after semantics ");
               end if;

               Underline;
               Print_Node_Subtree (File.Table (U).Cunit);
               Write_Eol;
            end if;
         end loop;
      end if;
   end Tree_Dump;

   ---------------------
   -- Print_Tree_Node --
   ---------------------

   procedure Print_Tree_Node (N : Node_Id; Label : Str := "") is
   begin
      Printing_Descendants := False;
      Phase := Printing;
      Print_Node (N, Label, ' ');
   end Print_Tree_Node;

   ---------------------
   -- Print_Tree_List --
   ---------------------

   procedure Print_Tree_List (L : List_Id) is
      N : Node_Id;

   begin
      Printing_Descendants := False;
      Phase := Printing;

      Print_List_Ref (L);
      Print_Str (" List_Id = ");
      Print_Int (Int (L));
      Print_Eol;

      N := First (L);

      if N = Empty then
         Print_Str ("<empty node list>");
         Print_Eol;

      else
         loop
            Print_Char ('|');
            Print_Eol;
            exit when Next (N) = Empty;
            Print_Node (N, "", '|');
            N := Next (N);
         end loop;

         Print_Node (N, "", ' ');
         Print_Eol;
      end if;
   end Print_Tree_List;

   ---------------------
   -- Print_Tree_Elist --
   ---------------------

   procedure Print_Tree_Elist (E : Elist_Id) is
      M : Elmt_Id;

   begin
      Printing_Descendants := False;
      Phase := Printing;

      Print_Elist_Ref (E);
      Print_Eol;

      M := First_Elmt (E);

      if M = No_Elmt then
         Print_Str ("<empty element list>");
         Print_Eol;

      else
         loop
            Print_Char ('|');
            Print_Eol;
            exit when Next_Elmt (M) = No_Elmt;
            Print_Node (Id_Of (M), "", '|');
            M := Next_Elmt (M);
         end loop;

         Print_Node (Id_Of (M), "", ' ');
         Print_Eol;
      end if;
   end Print_Tree_Elist;

   ----------------
   -- Print_Init --
   ----------------

   procedure Print_Init is
   begin
      Printing_Descendants := True;
      Write_Eol;

      --  Clear node and list numbers to zero (= unmarked)

      Node_Numbers := new Node_Numbers_Type (First_Node_Id .. Last_Node_Id);
      List_Numbers := new List_Numbers_Type (First_List_Id .. Last_List_Id);

      for I in Node_Numbers'range loop
         Node_Numbers (I) := 0;
      end loop;

      for I in List_Numbers'range loop
         List_Numbers (I) := 0;
      end loop;
   end Print_Init;

   ----------------
   -- Print_Term --
   ----------------

   procedure Print_Term is
      procedure Free is new
         Unchecked_Deallocation (Node_Numbers_Type, Access_Node_Numbers_Type);

      procedure Free is new
         Unchecked_Deallocation (List_Numbers_Type, Access_List_Numbers_Type);

   begin
      Free (Node_Numbers);
      Free (List_Numbers);
   end Print_Term;

   ------------------------
   -- Print_Node_Subtree --
   ------------------------

   procedure Print_Node_Subtree (N : Node_Id) is
   begin
      Print_Init;

      Next_Sequence_Number := 1;
      Phase := Marking;
      Visit_Node (N, "", ' ');

      Next_Sequence_Number := 1;
      Phase := Printing;
      Visit_Node (N, "", ' ');

      Print_Term;
   end Print_Node_Subtree;

   ------------------------
   -- Print_List_Subtree --
   ------------------------

   procedure Print_List_Subtree (L : List_Id) is
   begin
      Print_Init;

      Next_Sequence_Number := 1;
      Phase := Marking;
      Visit_List (L, "");

      Next_Sequence_Number := 1;
      Phase := Printing;
      Visit_List (L, "");

      Print_Term;
   end Print_List_Subtree;

   -------------------------
   -- Print_Elist_Subtree --
   -------------------------

   procedure Print_Elist_Subtree (E : Elist_Id) is
   begin
      Print_Init;

      Next_Sequence_Number := 1;
      Phase := Marking;
      Visit_Elist (E, "");

      Next_Sequence_Number := 1;
      Phase := Printing;
      Visit_Elist (E, "");

      Print_Term;
   end Print_Elist_Subtree;

   ----------------
   -- Print_Char --
   ----------------

   procedure Print_Char (C : Char) is
   begin
      if Phase = Printing then
         Write_Char (C);
      end if;
   end Print_Char;

   -----------------------
   -- Print_Entity_Kind --
   -----------------------

   procedure Print_Entity_Kind (E : Entity_Id) is
      Ucase : Boolean;
      S     : constant String := Entity_Kind'Image (Ekind (E));

   begin
      if Phase = Printing then
         Ucase := True;

         --  Note: the call to Fold_Upper in this loop is to get past the GNAT
         --  bug of 'Image returning lower case instead of upper case.

         for I in S'range loop
            if Ucase then
               Write_Char (Fold_Upper (To_Char (S (I))));
            else
               Write_Char (Fold_Lower (To_Char (S (I))));
            end if;

            Ucase := (S (I) = '_');
         end loop;
      end if;
   end Print_Entity_Kind;

   ---------------
   -- Print_Int --
   ---------------

   procedure Print_Int (I : Int) is
   begin
      if Phase = Printing then
         Write_Int (I);
      end if;
   end Print_Int;

   ----------------
   -- Print_Name --
   ----------------

   procedure Print_Name (N : Name_Id) is
      Ucase : Boolean;

   begin
      if Phase = Printing then
         if N = No_Name then
            Print_Str ("<No_Name>");

         elsif N = Error_Name then
            Print_Str ("<Error_Name>");

         else
            Get_Name_String (N);

            if Name_Buffer (1) /= '"' then
               Print_Char ('"');
               Ucase := True;

               for I in 1 .. Name_Len loop
                  if Ucase then
                     Write_Char (Fold_Upper (Name_Buffer (I)));
                  else
                     Write_Char (Name_Buffer (I));
                  end if;

                  Ucase := (Name_Buffer (I) = '_');
               end loop;

               Print_Char ('"');

            --  Operator case, output as is (in quotes all lower case)

            else
               Write_Name (N);
            end if;
         end if;
      end if;
   end Print_Name;

   ---------------------
   -- Print_Node_Kind --
   ---------------------

   procedure Print_Node_Kind (N : Node_Id) is
      Ucase : Boolean;
      S     : constant String := Node_Kind'Image (Nkind (N));

   begin
      if Phase = Printing then
         Ucase := True;

         --  Note: the call to Fold_Upper in this loop is to get past the GNAT
         --  bug of 'Image returning lower case instead of upper case.

         for I in S'range loop
            if Ucase then
               Write_Char (Fold_Upper (To_Char (S (I))));
            else
               Write_Char (Fold_Lower (To_Char (S (I))));
            end if;

            Ucase := (S (I) = '_');
         end loop;
      end if;
   end Print_Node_Kind;

   ---------------
   -- Print_Str --
   ---------------

   procedure Print_Str (S : Str) is
   begin
      if Phase = Printing then
         Write_Str (S);
      end if;
   end Print_Str;

   ---------------
   -- Print_Eol --
   ---------------

   procedure Print_Eol is
   begin
      if Phase = Printing then
         Write_Eol;
      end if;
   end Print_Eol;

   --------------------
   -- Print_Node_Ref --
   --------------------

   procedure Print_Node_Ref (N : Node_Id) is
   begin
      if Phase /= Printing then
         return;
      end if;

      if N = Empty then
         Write_Str ("<empty>");

      elsif N = Error then
         Write_Str ("<error>");

      else
         if Printing_Descendants then
            if Node_Numbers (N) /= 0 then
               Write_String ("Node");
               Write_Str (" #");
               Write_Int (Node_Numbers (N));
               Write_Char (' ');
            end if;
         end if;

         Print_Node_Kind (N);

         if Nkind (N) in N_Has_Chars then
            Write_Char (' ');
            Print_Name (Chars (N));
         end if;

         if Nkind (N) in N_Entity then
            Write_Str (" (Entity_Id = ");
         else
            Write_Str (" (Node_Id = ");
         end if;

         Write_Int (Int (N));

         if Sloc (N) = Standard_Location then
            Write_Char ('s');
         end if;

         Write_Char (')');

      end if;
   end Print_Node_Ref;

   --------------------
   -- Print_List_Ref --
   --------------------

   procedure Print_List_Ref (L : List_Id) is
   begin
      if Phase /= Printing then
         return;
      end if;

      if L = No_List then
         Write_Str ("<no list>");

      elsif Is_Empty_List (L) then
         Write_Str ("<empty list> (List_Id = ");
         Write_Int (Int (L));
         Write_Char (')');

      else
         Write_Str ("List");

         if Printing_Descendants then
            Write_Str (" #");
            Write_Int (List_Numbers (L));
         end if;

         Write_Str (" (List_Id = ");
         Write_Int (Int (L));
         Write_Char (')');
      end if;
   end Print_List_Ref;

   ---------------------
   -- Print_Elist_Ref --
   ---------------------

   procedure Print_Elist_Ref (E : Elist_Id) is
   begin
      if Phase /= Printing then
         return;
      end if;

      if E = No_Elist then
         Write_Str ("<no elist>");

      elsif Is_Empty_Elmt_List (E) then
         Write_Str ("Empty elist, (Elist_Id = ");
         Write_Int (Int (E));
         Write_Char (')');

      else
         Write_Str ("(Elist_Id = ");
         Write_Int (Int (E));
         Write_Char (')');

         if Printing_Descendants then
            Write_Str (" #");
            Write_Int (List_Numbers (List_Id (Int (E) - Elist_Bias)));
         end if;
      end if;
   end Print_Elist_Ref;

   -----------------
   -- Print_Field --
   -----------------

   procedure Print_Field (Val : Int) is
   begin
      if Phase /= Printing then
         return;
      end if;

      if Val in Node_Range then
         Print_Node_Ref (Node_Id (Val));

      elsif Val in List_Range then
         Print_List_Ref (List_Id (Val));

      elsif Val in Elist_Range then
         Print_Elist_Ref (Elist_Id (Val));

      elsif Val in Names_Range then
         Print_Name (Name_Id (Val));
         Write_Str (" (Name_Id = ");
         Write_Int (Val);
         Write_Char (')');

      elsif Val in Strings_Range then
         Write_String_Table_Entry (String_Id (Val));
         Write_Str (" (String_Id = ");
         Write_Int (Val);
         Write_Char (')');

      elsif Val in Uint_Range then

         if Uint (Val) in Uint_Direct then
            Write_Int (Val - Int (Uint_Direct_Bias));

         else
            Get_Name_String (UI_Image (Uint (Val)));
            Print_Str (Name_Buffer (1 .. Name_Len));
            Write_Str (" (Uint = ");
            Write_Int (Val);
            Write_Char (')');
         end if;

      elsif Val in Char_Code_Range then
         Write_Str ("Character code = ");

         declare
            C : Int := Val - Char_Code_Bias;

         begin
            Write_Int (C);

            if C in 16#20# .. 16#7E# or else C in 16#A0# .. 16#FF# then
               Write_Str (" ('");
               Write_Char (Char'Val (C));
               Write_Str ("')");
            end if;
         end;

      else
         Print_Str ("****** Incorrect value = ");
         Print_Int (Val);
      end if;
   end Print_Field;

   ----------------
   -- Print_Flag --
   ----------------

   procedure Print_Flag (F : Boolean) is
   begin
      if F then
         Print_Str ("True");
      else
         Print_Str ("False");
      end if;
   end Print_Flag;

   ----------------
   -- Print_Node --
   ----------------

   procedure Print_Node (N : Node_Id; Prefix_Str : Str; Prefix_Char : Char) is
      F : Fchar;
      P : Int := Pchar_Pos (Nkind (N));

      Field_To_Be_Printed : Boolean;
      Prefix_Str_Char     : Str (Prefix_Str'First .. Prefix_Str'Last + 1);
      Nunit               : Unit_Number_Type;

   begin
      if Phase /= Printing then
         return;
      end if;

      Prefix_Str_Char (Prefix_Str'range)    := Prefix_Str;
      Prefix_Str_Char (Prefix_Str'Last + 1) := Prefix_Char;

      --  Print header line

      Print_Str (Prefix_Str);
      Print_Node_Ref (N);

      --  Print note if Analyzed flag set

      if Analyzed (N) then
         Print_Str (" (analyzed)");
      end if;

      Print_Eol;

      if N = Empty then
         return;
      end if;

      if not Is_List_Member (N) then
         Print_Str (Prefix_Str);
         Print_Str (" Parent = ");
         Print_Node_Ref (Parent (N));
         Print_Eol;
      end if;

      --  Print Sloc field if it is set

      if Sloc (N) /= No_Location then
         Print_Str (Prefix_Str_Char);
         Print_Str ("Sloc = ");

         if Sloc (N) = Standard_Location then
            Print_Str ("Standard_Location");
         else
            Nunit := Get_Sloc_Unit_Number (Sloc (N));
            Print_Int (Int (Sloc (N)) - Int (File.Table (Nunit).Source'First));
            Print_Str (" (Line ");
            Print_Int (Int (Get_Line_Number (Sloc (N))));
            Print_Str (", Col ");
            Print_Int (Int (Get_Col_Number (Sloc (N))));

            if Nunit /= Main_Unit then
               Print_Str (" in file ");
               Get_Name_String (File.Table (Nunit).File_Name);
               Print_Str (Name_Buffer (1 .. Name_Len));
            end if;

            Print_Char (')');
         end if;

         Print_Eol;
      end if;

      --  Print Chars field if present

      if Nkind (N) in N_Has_Chars and then Chars (N) /= No_Name then
         Print_Str (Prefix_Str_Char);
         Print_Str ("Chars = ");
         Print_Name (Chars (N));
         Write_Str (" (Name_Id = ");
         Write_Int (Int (Chars (N)));
         Write_Char (')');
         Print_Eol;
      end if;

      --  Print entity information for entities

      if Nkind (N) in N_Entity then
         Print_Str (Prefix_Str_Char);
         Print_Str ("Ekind = ");
         Print_Entity_Kind (N);
         Print_Eol;
      end if;

      --  Print Left_Opnd if present

      if Nkind (N) in N_Binary_Op then
         Print_Str (Prefix_Str_Char);
         Print_Str ("Left_Opnd = ");
         Print_Node_Ref (Left_Opnd (N));
         Print_Eol;
      end if;

      --   Print Right_Opnd if present

      if Nkind (N) in N_Op then
         Print_Str (Prefix_Str_Char);
         Print_Str ("Right_Opnd = ");
         Print_Node_Ref (Right_Opnd (N));
         Print_Eol;
      end if;

      --  Print Entity field if operator (other cases of Entity
      --  are in the table, so are handled in the normal circuit)

      if Nkind (N) in N_Op and then Present (Entity (N)) then
         Print_Str (Prefix_Str_Char);
         Print_Str ("Entity = ");
         Print_Node_Ref (Entity (N));
         Print_Eol;
      end if;

      --   Print Error_Posted flag

      if Error_Posted (N) then
         Print_Str (Prefix_Str_Char);
         Print_Str ("Error_Posted = True");
         Print_Eol;
      end if;

      --  Print special fields if we have a subexpression

      if Nkind (N) in N_Subexpr then
         if Parens (N) then
            Print_Str (Prefix_Str_Char);
            Print_Str ("Parens  = True");
            Print_Eol;
         end if;

         if Has_No_Side_Effects (N) then
            Print_Str (Prefix_Str_Char);
            Print_Str ("Has_No_Side_Effects = True");
            Print_Eol;
         end if;

         if Do_Range_Check (N) then
            Print_Str (Prefix_Str_Char);
            Print_Str ("Do_Range_Check = True");
            Print_Eol;
         end if;

         if Is_Overloaded (N) then
            Print_Str (Prefix_Str_Char);
            Print_Str ("Is_Overloaded = True");
            Print_Eol;
         end if;

         if Is_Evaluated (N) then
            Print_Str (Prefix_Str_Char);
            Print_Str ("Is_Evaluated = True");
            Print_Eol;
         end if;

         if Is_Static (N) then
            Print_Str (Prefix_Str_Char);
            Print_Str ("Is_Static = True");
            Print_Eol;
         end if;
      end if;

      --  Print Do_Overflow_Check field if present

      if Nkind (N) in N_Op and then Do_Overflow_Check (N) then
         Print_Str (Prefix_Str_Char);
         Print_Str ("Do_Overflow_Check = True");
         Print_Eol;
      end if;

      --  Print Etype field if present

      if Nkind (N) in N_Has_Etype 
        and then Present (Etype (N)) 
      then
         Print_Str (Prefix_Str_Char);
         Print_Str ("Etype = ");
         Print_Node_Ref (Etype (N));
         Print_Eol;
      end if;

      --  Loop to print other fields (those included in Pchars)

      while P < Pchar_Pos (Node_Kind'Succ (Nkind (N))) loop
         F := Pchars (P);
         P := P + 1;

         --  Check for case of False flag, which we never print, or
         --  an Empty field, which is also never printed

         case F is
            when F_Field1 => Field_To_Be_Printed := Field1 (N) /= Int (Empty);
            when F_Field2 => Field_To_Be_Printed := Field2 (N) /= Int (Empty);
            when F_Field3 => Field_To_Be_Printed := Field3 (N) /= Int (Empty);
            when F_Field4 => Field_To_Be_Printed := Field4 (N) /= Int (Empty);
            when F_Field5 => Field_To_Be_Printed := Field5 (N) /= Int (Empty);
            when F_Field6 => Field_To_Be_Printed := Field6 (N) /= Int (Empty);
            when F_Field7 => Field_To_Be_Printed := Field7 (N) /= Int (Empty);
            when F_Flag1  => Field_To_Be_Printed := Flag1 (N);
            when F_Flag2  => Field_To_Be_Printed := Flag2 (N);
            when F_Flag3  => Field_To_Be_Printed := Flag3 (N);
            when F_Flag4  => Field_To_Be_Printed := Flag4 (N);
            when F_Flag5  => Field_To_Be_Printed := Flag5 (N);
            when F_Flag6  => Field_To_Be_Printed := Flag6 (N);
            when F_Flag7  => Field_To_Be_Printed := Flag7 (N);
            when F_Flag8  => Field_To_Be_Printed := Flag8 (N);
            when F_Flag9  => Field_To_Be_Printed := Flag9 (N);
            when F_Flag10 => Field_To_Be_Printed := Flag10 (N);
            when F_Flag11 => Field_To_Be_Printed := Flag11 (N);
            when F_Flag12 => Field_To_Be_Printed := Flag12 (N);
            when F_Flag13 => Field_To_Be_Printed := Flag13 (N);
            when F_Flag14 => Field_To_Be_Printed := Flag14 (N);
            when F_Flag15 => Field_To_Be_Printed := Flag15 (N);
            when F_Flag16 => Field_To_Be_Printed := Flag16 (N);
         end case;

         --  Print field if it is to be printed

         if Field_To_Be_Printed then
            Print_Str (Prefix_Str_Char);

            while P < Pchar_Pos (Node_Kind'Succ (Nkind (N)))
              and then Pchars (P) not in Fchar
            loop
               Print_Char (Pchars (P));
               P := P + 1;
            end loop;

            Print_Str (" = ");

            case F is
               when F_Field1 => Print_Field (Field1 (N));
               when F_Field2 => Print_Field (Field2 (N));
               when F_Field3 => Print_Field (Field3 (N));
               when F_Field4 => Print_Field (Field4 (N));
               when F_Field5 => Print_Field (Field5 (N));
               when F_Field6 => Print_Field (Field6 (N));
               when F_Field7 => Print_Field (Field7 (N));

               when F_Flag1  => Print_Flag  (Flag1 (N));
               when F_Flag2  => Print_Flag  (Flag2 (N));
               when F_Flag3  => Print_Flag  (Flag3 (N));
               when F_Flag4  => Print_Flag  (Flag4 (N));
               when F_Flag5  => Print_Flag  (Flag5 (N));
               when F_Flag6  => Print_Flag  (Flag6 (N));
               when F_Flag7  => Print_Flag  (Flag7 (N));
               when F_Flag8  => Print_Flag  (Flag8 (N));
               when F_Flag9  => Print_Flag  (Flag9 (N));
               when F_Flag10 => Print_Flag  (Flag10 (N));
               when F_Flag11 => Print_Flag  (Flag11 (N));
               when F_Flag12 => Print_Flag  (Flag12 (N));
               when F_Flag13 => Print_Flag  (Flag13 (N));
               when F_Flag14 => Print_Flag  (Flag14 (N));
               when F_Flag15 => Print_Flag  (Flag15 (N));
               when F_Flag16 => Print_Flag  (Flag16 (N));
            end case;

            Print_Eol;

         --  Field is not to be printed (False flag field)

         else
            while P < Pchar_Pos (Node_Kind'Succ (Nkind (N)))
              and then Pchars (P) not in Fchar
            loop
               P := P + 1;
            end loop;
         end if;

      end loop;

   end Print_Node;

   ----------------
   -- Visit_Node --
   ----------------

   procedure Visit_Node (N : Node_Id; Prefix_Str : Str; Prefix_Char : Char) is

      New_Prefix : Str (Prefix_Str'First .. Prefix_Str'Last + 2);
      --  Prefix string for printing referenced fields

      procedure Visit_Descendent (D : Int);
      --  This procedure tests the given value of one of the Fields referenced
      --  by the current node to determine whether to visit it recursively.
      --  The recursive visit is appropriate if N is a non-empty Node_Id value
      --  whose parent is either Empty or is the current node (if there is some
      --  other non-empty parent, we prefer to trace it from that parent).

      procedure Visit_Descendent (D : Int) is
      begin

         --  Case of descendent is a node

         if D in Node_Range then

            --  Don't bother about a descendent in Standard unless we are
            --  already printing Standard nodes to start with (the latter
            --  happens only when Treepr is used to print out the tree of
            --  Standard itself)

            if Sloc (Node_Id (D)) = Standard_Location
              and then Sloc (N) /= Standard_Location
            then
               return;

            --  Don't bother about a descendent in a different unit than
            --  the node we came from. Note that the df debugging switch
            --  is handled at the top level by GNAT.

            elsif Sloc (Node_Id (D)) /= Standard_Location
              and then Sloc (N) /= Standard_Location
              and then Get_Sloc_Unit_Number (Sloc (Node_Id (D))) /=
                       Get_Sloc_Unit_Number (Sloc (N))
            then
               return;

            --  Don't bother about Empty or Error descendents, or nodes which
            --  have a parent, but the parent isn't the node that we came
            --  from (as explained above, we prefer to list such nodes near
            --  their real parents).

            elsif D = Int (Empty)
              or else D = Int (Error)
              or else (Parent (Node_Id (D)) /= Empty
                        and then Parent (Node_Id (D)) /= N)
            then
               return;

            --  Otherwise we can go ahead and visit the node

            else
               Visit_Node (Node_Id (D), New_Prefix, ' ');
            end if;

         --  Case of descendent is a list

         elsif D in List_Range then

            --  Don't bother with a missing list, or an empty list, or a list
            --  with a parent which is not the node we came from (see above)

            if D = Int (No_List)
              or else Is_Empty_List (List_Id (D))
              or else (List_Parent (List_Id (D)) /= Empty
                        and then List_Parent (List_Id (D)) /= N)
            then
               return;

            --  Otherwise, visit the referenced list

            else
               Visit_List (List_Id (D), New_Prefix);
            end if;

         --  Case of descendent is an element list

         elsif D in Elist_Range then

            --  Don't bother with a missing list, or an empty list, or a list
            --  with a parent which is not the node we came from (see above)

            if D = Int (No_Elist)
              or else Is_Empty_Elmt_List (Elist_Id (D))
              or else (Elist_Parent (Elist_Id (D)) /= Empty
                        and then Elist_Parent (Elist_Id (D)) /= N)
            then
               return;

            --  Otherwise, visit the referenced element list

            else
               Visit_Elist (Elist_Id (D), New_Prefix);
            end if;

         --  For all other kinds of descendents (strings, names, uints etc),
         --  there is nothing to visit (the contents of the field will be
         --  printed when we print the containing node, but what concerns
         --  us now is looking for descendents in the tree.

         else
            null;
         end if;
      end Visit_Descendent;

   --  Start of Visit_Node

   begin
      if N = Empty then
         return;
      end if;

      New_Prefix (Prefix_Str'range)    := Prefix_Str;
      New_Prefix (Prefix_Str'Last + 1) := Prefix_Char;
      New_Prefix (Prefix_Str'Last + 2) := ' ';

      if Phase = Marking then
         if Node_Numbers (N) /= 0 then
            return; -- already visited
         else
            Node_Numbers (N) := Next_Sequence_Number;
            Next_Sequence_Number := Next_Sequence_Number + 1;
         end if;

      else -- Phase = Printing
         if Node_Numbers (N) < Next_Sequence_Number then
            return; -- already printed
         else
            Print_Node (N, Prefix_Str, Prefix_Char);
            Print_Str (Prefix_Str);
            Print_Char (Prefix_Char);
            Print_Eol;
            Next_Sequence_Number := Next_Sequence_Number + 1;
         end if;
      end if;

      --  For defining entities, visit only the descendents that are
      --  sinfo knows about (i.e. Etype in Field5). Do not visit any
      --  other stuff referenced only through entity fields.

      if Nkind (N) in N_Entity then
         Visit_Descendent (Int (Etype (N)));

      --  For all other nodes, visit all their descendents (for all other
      --  nodes, the Sinfo structure knows about all possible descendents)

      else
         Visit_Descendent (Field1 (N));
         Visit_Descendent (Field2 (N));
         Visit_Descendent (Field3 (N));
         Visit_Descendent (Field4 (N));
         Visit_Descendent (Field5 (N));

         if Has_Extension (N) then
            Visit_Descendent (Field6 (N));
            Visit_Descendent (Field7 (N));
            Visit_Descendent (Field8 (N));
            Visit_Descendent (Field9 (N));
            Visit_Descendent (Field10 (N));
            Visit_Descendent (Field11 (N));
            Visit_Descendent (Field12 (N));
         end if;
      end if;
   end Visit_Node;

   ----------------
   -- Visit_List --
   ----------------

   procedure Visit_List (L : List_Id; Prefix_Str : Str) is
      N : Node_Id;

   begin
      if Phase = Marking then
         if List_Numbers (L) /= 0 then
            return; -- already visited
         end if;

      else -- Phase = Printing
         if List_Numbers (L) < Next_Sequence_Number then
            return; -- already printed
         end if;
      end if;

      Print_Str (Prefix_Str);
      Print_List_Ref (L);
      Print_Eol;

      Print_Str (Prefix_Str);
      Print_Str ("|List_Parent = ");
      Print_Node_Ref (List_Parent (L));
      Print_Eol;

      if Phase = Marking then
         List_Numbers (L) := Next_Sequence_Number;
      end if;

      Next_Sequence_Number := Next_Sequence_Number + 1;
      N := First (L);

      if N = Empty then
         Print_Str (Prefix_Str);
         Print_Str ("(Empty list)");
         Print_Eol;
         Print_Eol;

      else
         Print_Str (Prefix_Str);
         Print_Char ('|');
         Print_Eol;

         while Next (N) /= Empty loop
            Visit_Node (N, Prefix_Str, '|');
            N := Next (N);
         end loop;
      end if;

      Visit_Node (N, Prefix_Str, ' ');
   end Visit_List;

   -----------------
   -- Visit_Elist --
   -----------------

   procedure Visit_Elist (E : Elist_Id; Prefix_Str : Str) is
      M : Elmt_Id;
      L : List_Id;
      N : Node_Id;

   begin
      L := List_Id (Int (E) - Elist_Bias); -- equivalent List_Id
      if Phase = Marking then
         if List_Numbers (L) /= 0 then
            return; -- already visited
         end if;

      else -- Phase = Printing
         if List_Numbers (L) < Next_Sequence_Number then
            return; -- already printed
         end if;
      end if;

      Print_Str (Prefix_Str);
      Print_Elist_Ref (E);
      Print_Eol;

      Print_Str (Prefix_Str);
      Print_Str ("|Elist_Parent = ");
      Print_Node_Ref (Elist_Parent (E));
      Print_Eol;

      if Phase = Marking then
         List_Numbers (L) := Next_Sequence_Number;
      end if;

      Next_Sequence_Number := Next_Sequence_Number + 1;

      if Is_Empty_Elmt_List (E) then
         Print_Str (Prefix_Str);
         Print_Str ("(Empty element list)");
         Print_Eol;
         Print_Eol;

      else
         if Phase = Printing then
            M := First_Elmt (E);
            while M /= No_Elmt loop
               N := Id_Of (M);
               Print_Str (Prefix_Str);
               Print_Str (" ");
               Print_Node_Ref (N);
               Print_Eol;
               M := Next_Elmt (M);
            end loop;

            Print_Str (Prefix_Str);
            Print_Eol;
         end if;

         M := First_Elmt (E);
         while M /= No_Elmt loop
            Visit_Node (Id_Of (M), Prefix_Str, ' ');
            M := Next_Elmt (M);
         end loop;
      end if;
   end Visit_Elist;

   --------------------------
   -- Debugging procedures --
   --------------------------

   procedure PN (N : Node_Id) is
   begin
      Print_Tree_Node (N);
   end PN;

   procedure PT (N : Node_Id) is
   begin
      Print_Node_Subtree (N);
   end PT;

end Treepr;
