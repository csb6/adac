------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S C N . N L I T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.13 $                             --
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

with Uintp; use Uintp;
separate (Scn)
procedure Nlit is

   C : Char;
   --  Current source program character

   Base_Char : Char;
   --  Either # or : (character at start of based number)

   Base : Int;
   --  Value of base

   UI_Base : Uint;
   --  Value of base in Uint format

   UI_Int_Value : Uint;
   --  Value of integer scanned by Scan_Integer in Uint format

   UI_Num_Value : Uint;
   --  Value of integer in numeric value being scanned

   Scale : Int;
   --  Scale value for real literal

   UI_Scale : Uint;
   --  Scale in Uint format

   Scanp : Source_Ptr;
   --  Used to save scan pointer values

   Exponent_Is_Negative : Boolean;
   --  Set true for negative exponent

   Extended_Digit_Value : Int;
   --  Extended digit value

   Point_Scanned : Boolean;
   --  Flag for decimal point scanned in numeric literal

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Error_Digit_Expected;
   --  Signal error of bad digit, Scan_Ptr points to the location at which
   --  the digit was expected on input, and is unchanged on return.

   procedure Scan_Integer;
   --  Procedure to scan integer literal. On entry, Scan_Ptr points to a
   --  digit, on exit Scan_Ptr points past the last character of the integer.
   --  For each digit encountered, UI_Int_Value is multiplied by 10, and the
   --  value of the digit added to the result. In addition, the value in
   --  Scale is decremented by one for each actual digit scanned.

   --------------------------
   -- Error_Digit_Expected --
   --------------------------

   procedure Error_Digit_Expected is
   begin
      Error_Msg_S ("digit expected");
   end Error_Digit_Expected;

   -------------------
   --  Scan_Integer --
   -------------------

   procedure Scan_Integer is
      C : Char;
      --  Next character scanned

   begin
      C := Source (Scan_Ptr);

      --  Loop through digits (allowing underlines)

      loop
         UI_Int_Value := UI_Sum (UI_Product (UI_Int_Value, Uint_10),
                              UI_From_Int (Char'Pos (C) - Char'Pos ('0')));
         Scan_Ptr := Scan_Ptr + 1;
         Scale := Scale - 1;
         C := Source (Scan_Ptr);

         if C = '_' then

            loop
               Scan_Ptr := Scan_Ptr + 1;
               C := Source (Scan_Ptr);
               exit when C /= '_';
               Error_No_Double_Underline;
            end loop;

            if C not in '0' .. '9' then
               Error_Digit_Expected;
               exit;
            end if;

         else
            exit when C not in '0' .. '9';
         end if;
      end loop;

   end Scan_Integer;

----------------------------------
-- Start of Processing for Nlit --
----------------------------------

begin
   Base := 10;
   UI_Base := Uint_10;
   UI_Int_Value := Uint_0;
   Scale := 0;
   Scan_Integer;
   Scale := 0;
   Point_Scanned := False;
   UI_Num_Value := UI_Int_Value;

   --  Various possibilities now for continuing the literal are
   --  period, E/e (for exponent), or :/# (for based literal).

   Scale := 0;
   C := Source (Scan_Ptr);

   if C = '.' then

      while C = '.' and then Source (Scan_Ptr + 1) /= '.' loop
         if Point_Scanned then
            Error_Msg_S ("duplicate point ignored");
         end if;

         Point_Scanned := True;
         Scan_Ptr := Scan_Ptr + 1;
         C := Source (Scan_Ptr);

         if C not in '0' .. '9' then
            Error_Msg ("real literal cannot end with point", Scan_Ptr - 1);
         else
            Scan_Integer;
            UI_Num_Value := UI_Int_Value;
         end if;
      end loop;

   --  Based literal case. The base is the value we already scanned.
   --  Note also the check for := to catch the well known tricky
   --  bug otherwise arising from "x : integer range 1 .. 10:= 6;"

   elsif C = '#' or else
      (C = ':' and then Source (Scan_Ptr + 1) /= '=')
   then
      Base_Char := C;

      if Base_Char = ':' and then Ada_9X then
         Error_Msg_S ("obsolescent feature: use of colon for sharp?");
      end if;

      UI_Base := UI_Int_Value;

      if UI_Lt (UI_Base, Uint_2) or else UI_Gt (UI_Base, Uint_16) then
         Error_Msg_SC ("base not 2-16");
         UI_Base := Uint_16;
      end if;

      Base := UI_To_Int (UI_Base);
      Scan_Ptr := Scan_Ptr + 1;

      --  Scan out extended integer [. integer]

      C := Source (Scan_Ptr);
      UI_Int_Value := Uint_0;
      Scale := 0;

      loop
         if C in '0' .. '9' then
            Extended_Digit_Value := Int (Char'Pos (C) - Char'Pos ('0'));
         elsif C in 'A' .. 'F' then
            Extended_Digit_Value := Int (Char'Pos (C) - Char'Pos ('A') + 10);
         elsif C in 'a' .. 'f' then
            Extended_Digit_Value := Int (Char'Pos (C) - Char'Pos ('a') + 10);
         else
            Error_Msg_S ("extended digit expected");
            exit;
         end if;

         if Extended_Digit_Value >= Base then
            Error_Msg_S ("digit >= base");
         end if;

         UI_Int_Value :=
           UI_Sum (UI_Product (UI_Int_Value, UI_Base),
                               UI_From_Int (Extended_Digit_Value));
         Scale := Scale - 1;
         Scan_Ptr := Scan_Ptr + 1;
         C := Source (Scan_Ptr);

         if C = '_' then
            Scan_Ptr := Scan_Ptr + 1;
            C := Source (Scan_Ptr);

            if C = '_' then
               Error_No_Double_Underline;
               Scan_Ptr := Scan_Ptr + 1;
               C := Source (Scan_Ptr);
            end if;

         elsif C = '.' then
            if Point_Scanned then
               Error_Msg_S ("duplicate point ignored");
            end if;

            Scan_Ptr := Scan_Ptr + 1;
            C := Source (Scan_Ptr);
            Point_Scanned := True;
            Scale := 0;


         elsif C = Base_Char then
            Scan_Ptr := Scan_Ptr + 1;
            exit;

         elsif C = '#' or else C = ':' then
            Error_Msg_S ("based number delimiters must match");
            Scan_Ptr := Scan_Ptr + 1;
            exit;
         end if;

      end loop;
      UI_Num_Value := UI_Int_Value;

   end if;

   --  Scan out exponent

   if not Point_Scanned then
      Scale := 0;
      UI_Scale := Uint_0;
   else
      UI_Scale := UI_From_Int (Scale);
   end if;

   if Source (Scan_Ptr) = 'e' or else Source (Scan_Ptr) = 'E' then
      Scan_Ptr := Scan_Ptr + 1;
      Exponent_Is_Negative := False;

      if Source (Scan_Ptr) = '+' then
         Scan_Ptr := Scan_Ptr + 1;

      elsif Source (Scan_Ptr) = '-' then

         if not Point_Scanned then
            Error_Msg_S ("negative exponent not allowed for integer literal");
         else
            Exponent_Is_Negative := True;
         end if;

         Scan_Ptr := Scan_Ptr + 1;
      end if;

      UI_Int_Value := Uint_0;

      if Source (Scan_Ptr) in '0' .. '9' then
         Scan_Integer;
      else
         Error_Digit_Expected;
      end if;

      if Exponent_Is_Negative then
         UI_Scale := UI_Difference (UI_Scale, UI_Int_Value);
      else
         UI_Scale := UI_Sum (UI_Scale, UI_Int_Value);
      end if;
   end if;

   --  Case of real literal to be returned

   if Point_Scanned then
      Token := Tok_Real_Literal;
      Token_Node := New_Node (N_Real_Literal, Token_Ptr);

      if Base = 10 then
         Set_Decimal (Token_Node, True);
         Set_Denominator (Token_Node, UI_Negate (UI_Scale));

      elsif UI_Is_Negative (UI_Scale) then
         Set_Denominator (Token_Node,
           UI_Exponentiate (UI_Base, UI_Negate (UI_Scale)));

      else
         UI_Num_Value := UI_Product (UI_Num_Value,
           UI_Exponentiate (UI_Base, UI_Scale));
         Set_Denominator (Token_Node, Uint_0);
         Set_Decimal (Token_Node, True);
      end if;

      Set_Numerator (Token_Node, UI_Num_Value);

   --  Case of integer literal to be returned

   else
      Token := Tok_Integer_Literal;
      Token_Node := New_Node (N_Integer_Literal, Token_Ptr);

      if UI_Ne (UI_Scale, Uint_0) then
         Set_Intval (Token_Node,
           UI_Product (UI_Num_Value, UI_Exponentiate (UI_Base, UI_Scale)));
      else
         Set_Intval (Token_Node, UI_Num_Value);
      end if;

   end if;

   return;

end Nlit;
