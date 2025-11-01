------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P A R . T C H K                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.12 $                             --
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

separate (Par)
package body Tchk is

   --  Error recovery: none of these routines raise Error_Resync

   -------------
   -- T_Abort --
   -------------

   procedure T_Abort is
   begin
      if Token = Tok_Abort then
         Scan;
      else
         Error_Msg_SC ("ABORT expected");
      end if;
   end T_Abort;

   -------------
   -- T_Arrow --
   -------------

   procedure T_Arrow is
   begin
      if Token = Tok_Arrow then
         Scan;

      --  A little recovery helper, accept then in place of =>

      elsif Token = Tok_Then then
         Error_Msg_BC ("""=>"" expected");
         Scan; -- past then used in place of =>

      else
         Error_Msg_AP ("""=>"" expected>");
      end if;
   end T_Arrow;

   ----------
   -- T_At --
   ----------

   procedure T_At is
   begin
      if Token = Tok_At then
         Scan;
      else
         Error_Msg_SC ("AT expected");
      end if;
   end T_At;

   -------------
   -- T_Begin --
   -------------

   procedure T_Begin is
   begin
      if Token = Tok_Begin then
         Scan;
      else
         Error_Msg_SC ("BEGIN expected");
      end if;
   end T_Begin;

   ------------
   -- T_Body --
   ------------

   procedure T_Body is
   begin
      if Token = Tok_Body then
         Scan;
      else
         Error_Msg_BC ("BODY expected");
      end if;
   end T_Body;

   -----------
   -- T_Box --
   -----------

   procedure T_Box is
   begin
      if Token = Tok_Box then
         Scan;
      else
         Error_Msg_AP ("""<>"" expected");
      end if;
   end T_Box;

   ------------
   -- T_Case --
   ------------

   procedure T_Case is
   begin
      if Token = Tok_Case then
         Scan;
      else
         Error_Msg_SC ("CASE expected");
      end if;
   end T_Case;

   -------------
   -- T_Colon --
   -------------

   procedure T_Colon is
   begin
      if Token = Tok_Colon then
         Scan;
      else
         Error_Msg_AP (""":"" expected");
      end if;
   end T_Colon;

   -------------------
   -- T_Colon_Equal --
   -------------------

   procedure T_Colon_Equal is
   begin
      if Token = Tok_Colon_Equal then
         Scan;

      elsif Token = Tok_Equal then
         Error_Msg_SC ("assignment symbol is :=");
         Scan;

      else
         Error_Msg_AP (""":="" expected");
      end if;
   end T_Colon_Equal;

   -------------
   -- T_Comma --
   -------------

   procedure T_Comma is
   begin
      if Token = Tok_Comma then
         Scan;

      else
         if Token = Tok_Pragma then
            P_Pragmas_Misplaced;
         end if;

         if Token = Tok_Comma then
            Scan;
         else
            Error_Msg_AP (""","" expected");
         end if;
      end if;

      if Token = Tok_Pragma then
         P_Pragmas_Misplaced;
      end if;
   end T_Comma;

   ---------------
   -- T_Dot_Dot --
   ---------------

   procedure T_Dot_Dot is
   begin
      if Token = Tok_Dot_Dot then
         Scan;
      else
         Error_Msg_AP (""".."" expected");
      end if;
   end T_Dot_Dot;

   -----------
   -- T_For --
   -----------

   procedure T_For is
   begin
      if Token = Tok_For then
         Scan;
      else
         Error_Msg_AP ("FOR expected");
      end if;
   end T_For;

   -----------------------
   -- T_Greater_Greater --
   -----------------------

   procedure T_Greater_Greater is
   begin
      if Token = Tok_Greater_Greater then
         Scan;
      else
         Error_Msg_AP (""">>"" expected");
      end if;
   end T_Greater_Greater;

   ------------------
   -- T_Identifier --
   ------------------

   procedure T_Identifier is
   begin
      if Token = Tok_Identifier then
         Scan;
      elsif Token in Token_Class_Literal then
         Error_Msg_SC ("identifier expected");
         Scan;
      else
         Error_Msg_AP ("identifier expected");
      end if;
   end T_Identifier;

   ----------
   -- T_In --
   ----------

   procedure T_In is
   begin
      if Token = Tok_In then
         Scan;
      else
         Error_Msg_AP ("IN expected");
      end if;
   end T_In;

   ----------
   -- T_Is --
   ----------

   procedure T_Is is
   begin
      if Token = Tok_Is then
         Scan;

      --  Allow OF, => or = to substitute for IS with complaint

      elsif Token = Tok_Arrow
        or else Token = Tok_Of
        or else Token = Tok_Equal
      then
         Error_Msg_SC ("IS expected");
         Scan; -- token used in place of IS
      else
         Error_Msg_AP ("IS expected");
      end if;

      while Token = Tok_Is loop
         Error_Msg_SC ("redundant IS ignored");
         Scan;
      end loop;
   end T_Is;

   ------------------
   -- T_Left_Paren --
   ------------------

   procedure T_Left_Paren is
   begin
      if Token = Tok_Left_Paren then
         Scan;
      else
         Error_Msg_AP ("missing ""(""");
      end if;
   end T_Left_Paren;

   ------------
   -- T_Loop --
   ------------

   procedure T_Loop is
   begin
      if Token = Tok_Loop then
         Scan;
      else
         Error_Msg_SC ("LOOP expected");

         if Token = Tok_Then then
            Scan;
         end if;
      end if;
   end T_Loop;

   -----------
   -- T_Mod --
   -----------

   procedure T_Mod is
   begin
      if Token = Tok_Mod then
         Scan;
      else
         Error_Msg_AP ("MOD expected");
      end if;
   end T_Mod;

   -----------
   -- T_New --
   -----------

   procedure T_New is
   begin
      if Token = Tok_New then
         Scan;
      else
         Error_Msg_AP ("NEW expected");
      end if;
   end T_New;

   ----------
   -- T_Of --
   ----------

   procedure T_Of is
   begin
      if Token = Tok_Of then
         Scan;
      else
         Error_Msg_AP ("missing OF");
      end if;
   end T_Of;

   ----------
   -- T_Or --
   ----------

   procedure T_Or is
   begin
      if Token = Tok_Or then
         Scan;
      else
         Error_Msg_AP ("OR expected");
      end if;
   end T_Or;

   ---------------
   -- T_Private --
   ---------------

   procedure T_Private is
   begin
      if Token = Tok_Private then
         Scan;
      else
         Error_Msg_SC ("PRIVATE expected");
      end if;
   end T_Private;

   -------------
   -- T_Range --
   -------------

   procedure T_Range is
   begin
      if Token = Tok_Range then
         Scan;
      else
         Error_Msg_AP ("RANGE expected");
      end if;
   end T_Range;

   -------------------
   -- T_Right_Paren --
   -------------------

   procedure T_Right_Paren is
   begin
      if Token = Tok_Right_Paren then
         Scan;
      else
         Error_Msg_AP ("missing "")""");
      end if;
   end T_Right_Paren;

   --------------
   -- T_Record --
   --------------

   procedure T_Record is
   begin
      if Token = Tok_Record then
         Scan;
      else
         Error_Msg_AP ("RECORD expected");
      end if;
   end T_Record;

   -----------------
   -- T_Semicolon --
   -----------------

   procedure T_Semicolon is
   begin

      if Token = Tok_Semicolon then
         Scan;

      --  An interesting little kludge here. If the previous token is a
      --  semicolon, then there is no way that we can legitimately need
      --  another semicolon. This could only arise in an error situation
      --  where an error has already been signalled. By simply ignoring
      --  the request for a semicolon in this case, we avoid some spurious
      --  missing semicolon messages.

      elsif Prev_Token = Tok_Semicolon then
         return;

      --  Otherwise we really do have a missing semicolon

      else
         Error_Msg_AP ("missing "";""");
      end if;
   end T_Semicolon;

   ------------
   -- T_Then --
   ------------

   procedure T_Then is
   begin
      if Token = Tok_Then then
         Scan;
      else
         Error_Msg_AP ("THEN expected");
      end if;
   end T_Then;

   ------------
   -- T_Type --
   ------------

   procedure T_Type is
   begin
      if Token = Tok_Type then
         Scan;
      else
         Error_Msg_AP ("TYPE expected");
      end if;
   end T_Type;

   -----------
   -- T_Use --
   -----------

   procedure T_Use is
   begin
      if Token = Tok_Use then
         Scan;
      else
         Error_Msg_SC ("USE expected");
      end if;
   end T_Use;

   ------------
   -- T_When --
   ------------

   procedure T_When is
   begin
      if Token = Tok_When then
         Scan;
      else
         Error_Msg_SC ("WHEN expected");
      end if;
   end T_When;

   --------------
   -- TF_Arrow --
   --------------

   procedure TF_Arrow is
      Scan_State : Saved_Scan_State;

   begin
      if Token = Tok_Arrow then
         Scan; -- skip arrow and we are done

      else
         T_Arrow; -- give missing arrow message
         Save_Scan_State (Scan_State); -- at start of junk tokens

         loop
            if Prev_Token_Ptr < Current_Line_Start
               or else Token = Tok_Semicolon
            then
               Restore_Scan_State (Scan_State); -- to where we were!
               return;
            end if;

            Scan; -- continue search!

            if Token = Tok_Arrow then
               Scan; -- past arrow
               return;
            end if;
         end loop;
      end if;
   end TF_Arrow;

   -----------
   -- TF_Is --
   -----------

   procedure TF_Is is
      Scan_State : Saved_Scan_State;

   begin
      if Token = Tok_Is then
         T_Is; -- past IS and we are done

      --  Allow OF or => or = in place of IS (with error message)

      elsif Token = Tok_Of
        or else Token = Tok_Arrow
        or else Token = Tok_Equal
      then
         T_Is; -- give missing IS message and skip bad token

      else
         T_Is; -- give missing IS message
         Save_Scan_State (Scan_State); -- at start of junk tokens

         loop
            if Prev_Token_Ptr < Current_Line_Start
               or else Token = Tok_Semicolon
            then
               Restore_Scan_State (Scan_State); -- to where we were!
               return;
            end if;

            Scan; -- continue search!

            if Token = Tok_Is
              or else Token = Tok_Of
              or else Token = Tok_Arrow
            then
               Scan; -- past IS or OF or =>
               return;
            end if;
         end loop;
      end if;
   end TF_Is;

   -------------
   -- TF_Loop --
   -------------

   procedure TF_Loop is
      Scan_State : Saved_Scan_State;

   begin
      if Token = Tok_Loop then
         Scan; -- past LOOP and we are done

      --  Allow THEN in place of LOOP

      elsif Token = Tok_Then then
         T_Loop; -- give missing LOOP message

      else
         T_Loop; -- give missing LOOP message
         Save_Scan_State (Scan_State); -- at start of junk tokens

         loop
            if Prev_Token_Ptr < Current_Line_Start
               or else Token = Tok_Semicolon
            then
               Restore_Scan_State (Scan_State); -- to where we were!
               return;
            end if;

            Scan; -- continue search!

            if Token = Tok_Loop or else Token = Tok_Then then
               Scan; -- past loop or then (message already generated)
               return;
            end if;
         end loop;
      end if;
   end TF_Loop;

   --------------
   -- TF_Return--
   --------------

   procedure TF_Return is
      Scan_State : Saved_Scan_State;

   begin
      if Token = Tok_Return then
         Scan; -- skip RETURN and we are done

      else
         Error_Msg_SC ("RETURN expected");
         Save_Scan_State (Scan_State); -- at start of junk tokens

         loop
            if Prev_Token_Ptr < Current_Line_Start
               or else Token = Tok_Semicolon
            then
               Restore_Scan_State (Scan_State); -- to where we were!
               return;
            end if;

            Scan; -- continue search!

            if Token = Tok_Return then
               Scan; -- past RETURN
               return;
            end if;
         end loop;
      end if;
   end TF_Return;

   ------------------
   -- TF_Semicolon --
   ------------------

   procedure TF_Semicolon is
      Scan_State : Saved_Scan_State;

   begin
      if Token = Tok_Semicolon then
         Scan; -- past ; and we are done
         return;

      --  An interesting little kludge here. If the previous token is a
      --  semicolon, then there is no way that we can legitimately need
      --  another semicolon. This could only arise in an error situation
      --  where an error has already been signalled. By simply ignoring
      --  the request for a semicolon in this case, we avoid some spurious
      --  missing semicolon messages.

      elsif Prev_Token = Tok_Semicolon then
         return;

      else
         if Token = Tok_Pragma then
            P_Pragmas_Misplaced;

            if Token = Tok_Semicolon then
               Scan; -- past semicolon
               return;
            end if;
         end if;

         T_Semicolon; -- give missing semicolon message
         Save_Scan_State (Scan_State); -- at start of junk tokens

         loop
            if Prev_Token_Ptr < Current_Line_Start then
               Restore_Scan_State (Scan_State); -- to where we were
               return;
            end if;

            Scan; -- continue search

            if Token = Tok_Semicolon then
               Scan; -- past semicolon
               return;

            elsif Token in Token_Class_After_SM then
               return;
            end if;
         end loop;
      end if;
   end TF_Semicolon;

   -------------
   -- TF_Then --
   -------------

   procedure TF_Then is
      Scan_State : Saved_Scan_State;

   begin
      if Token = Tok_Then then
         Scan; -- past THEN and we are done

      else
         T_Then; -- give missing THEN message
         Save_Scan_State (Scan_State); -- at start of junk tokens

         loop
            if Prev_Token_Ptr < Current_Line_Start
               or else Token = Tok_Semicolon
            then
               Restore_Scan_State (Scan_State); -- to where we were
               return;
            end if;

            Scan; -- continue search!

            if Token = Tok_Then then
               Scan; -- past THEN
               return;
            end if;
         end loop;
      end if;
   end TF_Then;

   ------------
   -- TF_Use --
   ------------

   procedure TF_Use is
      Scan_State : Saved_Scan_State;

   begin
      if Token = Tok_Use then
         Scan; -- past USE and we are done

      else
         T_Use; -- give USE expected message
         Save_Scan_State (Scan_State); -- at start of junk tokens

         loop
            if Prev_Token_Ptr < Current_Line_Start
               or else Token = Tok_Semicolon
            then
               Restore_Scan_State (Scan_State); -- to where we were
               return;
            end if;

            Scan; -- continue search!

            if Token = Tok_Use then
               Scan; -- past use
               return;
            end if;
         end loop;
      end if;
   end TF_Use;

end Tchk;
