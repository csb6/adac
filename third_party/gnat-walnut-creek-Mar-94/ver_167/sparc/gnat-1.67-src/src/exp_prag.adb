------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ P R A G                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.10 $                             --
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
with Comperr;  use Comperr;
with Exp_Util; use Exp_Util;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sinfo;    use Sinfo;
with Tbuild;   use Tbuild;
with Snames;   use Snames;

package body Exp_Prag is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Arg1 (N : Node_Id) return Node_Id;
   function Arg2 (N : Node_Id) return Node_Id;
   function Arg3 (N : Node_Id) return Node_Id;
   --  Obtain specified Pragma_Argument_Association

   procedure Expand_Pragma_Abort_Defer         (N : Node_Id);
   procedure Expand_Pragma_Interrupt_Priority  (N : Node_Id);

   --------------
   -- Arg1,2,3 --
   --------------

   function Arg1 (N : Node_Id) return Node_Id is
   begin
      return First (Pragma_Argument_Associations (N));
   end Arg1;

   function Arg2 (N : Node_Id) return Node_Id is
   begin
      return Next (Arg1 (N));
   end Arg2;

   function Arg3 (N : Node_Id) return Node_Id is
   begin
      return Next (Arg2 (N));
   end Arg3;

   ---------------------
   -- Expand_N_Pragma --
   ---------------------

   procedure Expand_N_Pragma (N : Node_Id) is
   begin
      case Get_Pragma_Id (Chars (Identifier (N))) is

         --  Pragmas requiring special expander action

         when Pragma_Abort_Defer =>
            Expand_Pragma_Abort_Defer (N);

         when Pragma_Interrupt_Priority =>
            Expand_Pragma_Interrupt_Priority (N);

         --  All other pragmas need no expander action

         when others => null;
      end case;

   end Expand_N_Pragma;

   -------------------------------
   -- Expand_Pragma_Abort_Defer --
   -------------------------------

   --  An Abort_Defer pragma appears as the first statement in a handled
   --  statement sequence (right after the begin). It defers aborts for
   --  the entire statement sequence, but not for any declarations or
   --  handlers (if any) associated with this statement sequence.

   --  With the current approach of explicit calls to Abort_Defer and
   --  Abort_Undefer, we accomplish this by inserting a call to Abort_Defer
   --  at the end of the associated declarations, and a call to Abort_Undefer
   --  at the end of the sequence of statements. In addition, if there are
   --  any exception handlers, a call to Abort_Undefer is placed at the start
   --  of the statements of each of the handlers.

   procedure Expand_Pragma_Abort_Defer (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      HSS : constant Node_Id := Parent (N);
      --  The N_Handled_Sequence_Of_Statements node

      P : constant Node_Id := Parent (HSS);
      --  The parent of the handled sequence has the declarations

      EH : Node_Id;
      --  An exception handler

   begin
      pragma Assert
        (Nkind (HSS) = N_Handled_Sequence_Of_Statements, Compiler_Abort (HSS));

      if Declarations (P) = No_List then
         Set_Declarations (P, New_List);
      end if;

      Append_To (Declarations (P), Build_Call (Loc, RTE (RE_Abort_Defer)));
      Append_To (Statements (HSS), Build_Call (Loc, RTE (RE_Abort_Undefer)));

      if List_Present (Exception_Handlers (HSS)) then
         EH := First (Exception_Handlers (HSS));

         while Present (EH) loop
            Prepend_To
              (Statements (EH), Build_Call (Loc, RTE (RE_Abort_Undefer)));
            EH := Next (EH);
         end loop;
      end if;
   end Expand_Pragma_Abort_Defer;

   --------------------------------------
   -- Expand_Pragma_Interrupt_Priority --
   --------------------------------------

   --  Supply default argument if none exists (System.Interrupt_Priority'Last)

   procedure Expand_Pragma_Interrupt_Priority (N : Node_Id) is
   begin
      if Pragma_Argument_Associations (N) = No_List then
         Set_Pragma_Argument_Associations (N, New_List_1 (
           Make_Pragma_Argument_Association (Sloc (N),
             Expression =>
               Make_Attribute_Reference (Sloc (N),
                 Prefix => RTE (RE_Interrupt_Priority),
                 Identifier => Make_Identifier (Sloc (N), Name_Last)))));
      end if;
   end Expand_Pragma_Interrupt_Priority;

end Exp_Prag;
