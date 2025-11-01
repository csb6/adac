------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 1                              --
--                                                                          --
--                                 B o d y                                  --
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

with Atree;    use Atree;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Opt;      use Opt;
with Sem_Ch5;  use Sem_Ch5;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Stand;    use Stand;

package body Sem_Ch11 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Exception_Handlers (L : List_Id);

   -----------------------------------
   -- Analyze_Exception_Declaration --
   -----------------------------------

   procedure Analyze_Exception_Declaration (N : Node_Id) is
      Id : constant Entity_Id := Defining_Identifier (N);

   begin
      Enter_Name (Id);
      Set_Ekind (Id, E_Exception);
      Set_Etype (Id, Standard_Exception_Type);
   end Analyze_Exception_Declaration;

   --------------------------------
   -- Analyze_Handled_Statements --
   --------------------------------

   procedure Analyze_Handled_Statements (N : Node_Id) is
      Handlers : constant List_Id := Exception_Handlers (N);

   begin
      Analyze_Statements (Statements (N));

      if List_Present (Handlers) then
         Analyze_Exception_Handlers (Handlers);
      end if;
   end Analyze_Handled_Statements;

   --------------------------------
   -- Analyze_Exception_Handlers --
   --------------------------------

   procedure Analyze_Exception_Handlers (L : List_Id) is
      Handler : Node_Id;
      Id      : Node_Id;

      procedure Check_Duplication (Id : Node_Id);
      --  Iterate through the identifiers in each handler to find duplicates

      procedure Check_Duplication (Id : Node_Id) is
         Handler : Node_Id;
         Id1     : Node_Id;

      begin
         Handler := First (L);

         while Present (Handler) loop
            Id1 := First (Exception_Choices (Handler));

            while Present (Id1) loop
               --  Only check against the exception choices which precede
               --  Id in the handler, since the ones that follow Id have not
               --  been analyzed yet and will be checked in a subsequent call.
               if Id = Id1 then
                  return;
               elsif Nkind (Id1) /= N_Others_Choice
                 and then Entity (Id) = Entity (Id1)
                 and then (Ada_83 or else Handler /= Parent (Id))
               then
                  Error_Msg_N
                    ("exception choice& duplicates earlier choice", Id);
                  return;
               end if;

               Id1 := Next (Id1);
            end loop;
            Handler := Next (Handler);
         end loop;
      end Check_Duplication;

   --  Start processing for Analyze_Exception_Handlers

   begin
      Handler := First (L);

      while Present (Handler) loop
         Id := First (Exception_Choices (Handler));

         while Present (Id) loop
            if Nkind (Id) = N_Others_Choice then
               if Present (Next (Id)) or else Present (Next (Handler))
               or else Present (Prev (Id)) then
                  Error_Msg_N ("OTHERS must appear alone and last", Id);
               end if;

            else
               Find_Name (Id);
               if Present (Renamed_Object (Entity (Id))) then
                  Set_Entity (Id, Renamed_Object (Entity (Id)));
               end if;

               Check_Duplication (Id);
            end if;

            Id := Next (Id);
         end loop;

         if Present (Choice_Parameter (Handler)) then
            Enter_Name (Choice_Parameter (Handler));

            --  The type and kind of a choice parameter should
            --  be defined in System.Exceptions.
         end if;

         Analyze_Statements (Statements (Handler));

         Handler := Next (Handler);
      end loop;
   end Analyze_Exception_Handlers;

   -----------------------------
   -- Analyze_Raise_Statement --
   -----------------------------

   procedure Analyze_Raise_Statement (N : Node_Id) is
      Exception_Id : Node_Id := Name (N);
      Exception_Name : Entity_Id;
      P : Node_Id;
      Nkind_P : Node_Kind;

   begin
      --  Reraise statement

      if No (Exception_Id) then

         P := Parent (N);
         Nkind_P := Nkind (P);         
         while Nkind_P /= N_Exception_Handler 
                 and then Nkind_P /= N_Subprogram_Body
                 and then Nkind_P /= N_Package_Body
                 and then Nkind_P /= N_Task_Body
                 and then Nkind_P /= N_Entry_Body
         loop 
            P := Parent (P);
            Nkind_P := Nkind (P);
         end loop;

         if Nkind (P) /= N_Exception_Handler then
            Error_Msg_N
              ("reraise statement must appear directly in a handler", N);
         end if;

      --  Normal case with exception id present

      else
         Find_Name (Exception_Id);
         Exception_Name := Entity (Exception_Id);
         if Present (Renamed_Object (Exception_Name)) then
            Set_Entity (Exception_Id, Renamed_Object (Exception_Name));
         end if;

         if Ekind (Exception_Name) /= E_Exception then
            Error_Msg_N ("exception name expected in raise statement", N);
         end if;
      end if;
   end Analyze_Raise_Statement;

end Sem_Ch11;
