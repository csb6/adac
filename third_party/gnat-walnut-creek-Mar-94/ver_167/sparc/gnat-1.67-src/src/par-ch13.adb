------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P A R . C H 1 3                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.21 $                             --
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
package body Ch13 is

   --  Local functions, used only in this chapter

   function P_Component_Clause return Node_Id;
   function P_Mod_Clause return Node_Id;

   ---------------------------------
   -- 13.1  Representation Clause --
   ---------------------------------

   --  REPRESENTATION_CLAUSE ::=
   --    ATTRIBUTE_DEFINITION_CLAUSE
   --  | ENUMERATION_REPRESENTATION_CLAUSE
   --  | RECORD_REPRESENTATION_CLAUSE
   --  | AT_CLAUSE

   --  ATTRIBUTE_DEFINITION_CLAUSE ::=
   --    for DIRECT_NAME'ATTRIBUTE_DESIGNATOR use EXPRESSION;
   --  | for DIRECT_NAME'ATTRIBUTE_DESIGNATOR use NAME;

   --  Note: in Ada 83, the expression must be a simple expression

   --  AT_CLAUSE ::= for DIRECT_NAME use at EXPRESSION;

   --  Note: in Ada 83, the expression must be a simple expression

   --  ENUMERATION_REPRESENTATION_CLAUSE ::=
   --    for first_subtype_DIRECT_NAME use ENUMERATION_AGGREGATE;

   --  ENUMERATION_AGGREGATE ::= ARRAY_AGGREGATE

   --  RECORD_REPRESENTATION_CLAUSE ::=
   --    for first_subtype_DIRECT_NAME use
   --      record [MOD_CLAUSE]
   --        {COMPONENT_CLAUSE}
   --      end record;

   --  The caller has checked that the initial token is FOR

   --  Error recovery: cannot raise Error_Resync, if an error occurs,
   --  the scan is repositioned past the next semicolon.

   function P_Representation_Clause return Node_Id is
      For_Loc         : Source_Ptr;
      Identifier_Node : Node_Id;
      Rep_Clause_Node : Node_Id;
      Expr_Node       : Node_Id;

   begin
      For_Loc := Token_Ptr;
      Scan; -- past FOR

      --  Note that the name in a representation clause is always a simple
      --  name, even in the attribute case, see AI-300 which made this so!

      Identifier_Node := P_Identifier;

      --  Check case of qualified name to give good error message

      if Token = Tok_Dot then
         Error_Msg_SC
            ("representation clause requires simple name!");

         loop
            exit when Token /= Tok_Dot;
            Scan; -- past dot
            Discard_Junk_Node (P_Identifier);
         end loop;
      end if;

      --  Attribute Definition Clause

      if Token = Tok_Apostrophe then
         Rep_Clause_Node := New_Node (N_Attribute_Definition_Clause, For_Loc);
         Set_Name (Rep_Clause_Node, Identifier_Node);
         Scan; -- past apostrophe
         Set_Identifier (Rep_Clause_Node, P_Identifier);
         T_Use;

         Expr_Node := P_Expression_No_Right_Paren;
         Check_Simple_Expression_In_Ada_83 (Expr_Node);
         Set_Expression (Rep_Clause_Node, Expr_Node);

      else
         TF_Use;

         --  AT follows USE (At Clause)

         if Token = Tok_At then
            if Ada_9X then
               Error_Msg_SC ("obsolescent feature: at clause?");
            end if;

            Scan; -- past AT
            Rep_Clause_Node := New_Node (N_At_Clause, For_Loc);
            Set_Identifier (Rep_Clause_Node, Identifier_Node);
            Expr_Node := P_Expression_No_Right_Paren;
            Check_Simple_Expression_In_Ada_83 (Expr_Node);
            Set_Expression (Rep_Clause_Node, Expr_Node);

         --  RECORD follows USE (Record Representation Clause)

         elsif Token = Tok_Record then
            Rep_Clause_Node :=
              New_Node (N_Record_Representation_Clause, For_Loc);
            Set_Identifier (Rep_Clause_Node, Identifier_Node);

            Push_Scope_Stack;
            Scope.Table (Scope.Last).Etyp := E_Record;
            Scope.Table (Scope.Last).Ecol := Start_Column;
            Scope.Table (Scope.Last).Sloc := Token_Ptr;
            Scan; -- past RECORD

            --  Possible Mod Clause

            if Token = Tok_At then
               Set_Mod_Clause (Rep_Clause_Node, P_Mod_Clause);
            end if;

            Set_Component_Clauses (Rep_Clause_Node, New_List);

            --  Loop through component clauses

            loop
               if Token not in Token_Class_Name then
                  exit when Check_End;
               end if;

               Append (P_Component_Clause,
                 Component_Clauses (Rep_Clause_Node));
            end loop;

         --  Left paren follows USE (Enumeration Representation Clause)

         elsif Token = Tok_Left_Paren then
            Rep_Clause_Node :=
              New_Node (N_Enumeration_Representation_Clause, For_Loc);
            Set_Identifier (Rep_Clause_Node, Identifier_Node);
            Set_Array_Aggregate (Rep_Clause_Node, P_Aggregate);

         --  Some other token follows FOR (invalid representation clause)

         else
            Error_Msg_SC ("invalid representation clause");
            raise Error_Resync;
         end if;
      end if;

      TF_Semicolon;
      return Rep_Clause_Node;

   exception
      when Error_Resync =>
         Resync_Past_Semicolon;
         return Error;

   end P_Representation_Clause;

   --------------------------------------
   -- 13.1  Type Representation Clause --
   --------------------------------------

   --  Parsed by P_Representation_Clause (13.1)

   ---------------------------------------
   -- 13.2  Attribute Definition Clause --
   ---------------------------------------

   --  Parsed by P_Representation_Clause (13.1)

   ---------------------------------------------
   -- 13.3  Enumeration Representation Clause --
   ---------------------------------------------

   --  Parsed by P_Representation_Clause (13.1)

   ----------------------------------------
   -- 13.4  Record Representation Clause --
   ----------------------------------------

   --  Parsed by P_Representation_Clause (13.1)

   ----------------------
   -- 13.4  Mod Clause --
   ----------------------

   --  MOD_CLAUSE ::= at mod static_EXPRESSION;

   --  Note: in Ada 83, the expression must be a simple expression

   --  The caller has checked that the initial Token is AT

   --  Error recovery: cannot raise Error_Resync

   function P_Mod_Clause return Node_Id is
      Mod_Node  : Node_Id;
      Expr_Node : Node_Id;

   begin
      if Ada_9X then
         Error_Msg_SC ("obsolescent feature: mod clause?");
      end if;

      Mod_Node := New_Node (N_Mod_Clause, Token_Ptr);
      Scan; -- past AT
      T_Mod;
      Expr_Node := P_Expression_No_Right_Paren;
      Check_Simple_Expression_In_Ada_83 (Expr_Node);
      Set_Expression (Mod_Node, Expr_Node);
      TF_Semicolon;
      return Mod_Node;
   end P_Mod_Clause;

   -----------------------------
   -- 13.4  Component Clauses --
   -----------------------------

   --  COMPONENT_CLAUSE ::=
   --    COMPONENT_CLAUSE_COMPONENT_NAME at POSITION
   --      range FIRST_BIT .. LAST_BIT;

   --  COMPONENT_CLAUSE_COMPONENT_NAME ::=
   --    component_DIRECT_NAME
   --  | component_DIRECT_NAME'ATTRIBUTE_DESIGNATOR
   --  | FIRST_SUBTYPE_DIRECT_NAME'ATTRIBUTE_DESIGNATOR

   --  POSITION ::= static_EXPRESSION

   --  Note: in Ada 83, the expression must be a simple expression

   --  FIRST_BIT ::= static_SIMPLE_EXPRESSION
   --  LAST_BIT ::= static_SIMPLE_EXPRESSION

   --  Note: the AARM V2.0 grammar has an error at this point, it uses
   --  EXPRESSION instead of SIMPLE_EXPRESSION for FIRST_BIT and LAST_BIT

   --  Error recovery: cannot raise Error_Resync

   function P_Component_Clause return Node_Id is
      Component_Node : Node_Id;
      Comp_Name      : Node_Id;
      Expr_Node      : Node_Id;

   begin
      Component_Node := New_Node (N_Component_Clause, Token_Ptr);
      Comp_Name := P_Name;

      if Nkind (Comp_Name) = N_Identifier
        or else Nkind (Comp_Name) = N_Attribute_Reference
      then
         Set_Component_Name (Component_Node, Comp_Name);
      else
         Error_Msg_N
           ("component name must be direct name or attribute", Comp_Name);
         Set_Component_Name (Component_Node, Error);
      end if;

      Set_Sloc (Component_Node, Token_Ptr);
      T_At;
      Expr_Node := P_Expression_No_Right_Paren;
      Check_Simple_Expression_In_Ada_83 (Expr_Node);
      Set_Position (Component_Node, Expr_Node);
      T_Range;
      Expr_Node := P_Expression_No_Right_Paren;
      Check_Simple_Expression_In_Ada_83 (Expr_Node);
      Set_First_Bit (Component_Node, Expr_Node);
      T_Dot_Dot;
      Expr_Node := P_Expression_No_Right_Paren;
      Check_Simple_Expression_In_Ada_83 (Expr_Node);
      Set_Last_Bit (Component_Node, Expr_Node);
      TF_Semicolon;
      return Component_Node;
   end P_Component_Clause;

   --------------------------
   -- 13.8  Code Statement --
   --------------------------

   --  CODE_STATEMENT ::= SUBTYPE_MARK'RECORD_AGGREGATE;

   --  On entry the caller has scanned the SUBTYPE_MARK (passed in as the
   --  single argument, and the scan points to the apostrophe.

   --  Error recovery: can raise Error_Resync

   function P_Code_Statement (Subtype_Mark : Node_Id) return Node_Id is
      Node1 : Node_Id;

   begin
      Scan; -- past apostrophe

      --  If left paren, then we have a possible code statement

      if Token = Tok_Left_Paren then
         Node1 := New_Node (N_Code_Statement, Sloc (Subtype_Mark));
         Set_Expression (Node1, P_Qualified_Expression (Subtype_Mark));
         TF_Semicolon;
         return Node1;

      --  Otherwise we have an illegal range attribute. Note that P_Call_Name
      --  ensures that Token = Tok_Range is the only possibility left here.

      else -- Token = Tok_Range
         Error_Msg_SC ("RANGE attribute illegal here!");
         raise Error_Resync;
      end if;

   end P_Code_Statement;

end Ch13;
