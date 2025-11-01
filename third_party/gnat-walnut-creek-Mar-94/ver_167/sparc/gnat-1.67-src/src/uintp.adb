------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                U I N T P                                 --
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

with Comperr; use Comperr;
with Limits;  use Limits;
with Namet;   use Namet;

package body Uintp is

   use Uintp_Private_Part;
   --  We are allowed to see our own private data!

   function N_Digits (Input : Uint) return Int;
   pragma Inline (N_Digits);
   --  Returns number of "digits" in a Uint.

   procedure Init_Operand (UI : Uint; Vec : out UI_Vector);
   pragma Inline (Init_Operand);
   --  This procedure copies the digits from UI or the table into the
   --  vector parameter. The parameter should be of the correct
   --  size as determined by a previous call to N_Digits with UI.

   function Vector_To_Uint
     (In_Vec : UI_Vector; Negative : boolean) return Uint;
   --  Functions that calculate values in UI_Vectors, call this function
   --  to create and return the Uint value.

   Uint_Int_First, Uint_Int_Last : Uint;
   --  Uint values containing Int'First and Int'Last. Initialized in
   --  package initialization and used in UI_Is_In_Int_Range.

   Int1 : constant Int := 1;
   --  Needed to avoid some ambiguities

   ---------------
   -- Last_Uint --
   ---------------

   function Last_Uint return Uint is
   begin
      return Uints.Last;
   end Last_Uint;

   ---------------------
   -- Initialize_Uint --
   ---------------------

   procedure Initialize_Uint is
   begin
      Uints.Init;
      Udigits.Init;

      UI_Power (0) := Uint_1;

      for I in 1 .. UI_Power'Last loop
         UI_Power (I) := UI_Product (UI_Power (I - 1), Uint_2);
      end loop;

   end Initialize_Uint;

   ---------------
   -- N_Digits --
   ---------------

   function N_Digits (Input : Uint) return Int is
   begin
      if Input <= Uint_Direct_Last then
         return 1;
      else
         return Uints.Table (Input).Length;
      end if;
   end N_Digits;

   -------------------
   -- Init_Operand --
   -------------------

   procedure Init_Operand (UI : Uint; Vec : out UI_Vector) is
      Loc : Int;
   begin
      if UI <= Uint_Direct_Last then
         Vec (1) := Int (UI - Uint_Direct_Bias);
      else
         Loc := Uints.Table (UI).Loc;

         for I in 1 .. N_Digits (UI) loop
            Vec (I) := Udigits.Table (Loc + I - 1);
         end loop;
      end if;
   end Init_Operand;

   ---------------------
   -- Vector_To_Uint --
   ---------------------

   function Vector_To_Uint (In_Vec : UI_Vector; Negative : Boolean)
                                return Uint is
      Size : Int;

   begin

      --  The vector can contain leading zeros. These are not stored in the
      --  table, so loop through the vector looking for the first non-zero
      --  digit.

      for I in In_Vec'range loop
         if In_Vec (I) /= 0 then

            --  The length of the value is the length of the rest of the vector

            Size := In_Vec'Last - I + 1;

            if Size = 1 then
               if Negative then
                  return Uint (Int (Uint_Direct_Bias) - In_Vec (I));
               else
                  return Uint (Int (Uint_Direct_Bias) + In_Vec (I));
               end if;
            end if;

            --  The value takes more than one digit, so it is stored in the
            --  table. Expand the table to contain the count and digits.
            --  the index of the first new location is the return value.

            Uints.Increment_Last;
            Uints.Table (Uints.Last).Length := Size;
            Uints.Table (Uints.Last).Loc    := Udigits.Last + 1;

            Udigits.Increment_Last;

            if Negative then
               Udigits.Table (Udigits.Last) := - In_Vec (I);
            else
               Udigits.Table (Udigits.Last) := + In_Vec (I);
            end if;

            for J in 2 .. Size loop
               Udigits.Increment_Last;
               Udigits.Table (Udigits.Last) := In_Vec (I + J - 1);
            end loop;

            return Uints.Last;
         end if;
      end loop;

      --  Dropped through loop only if vector contained all zeros

      return Uint_0;

   end Vector_To_Uint;

   ------------------
   -- UI_From_Int --
   ------------------

   function UI_From_Int (Input : Int) return Uint is
   begin

      --  The case -Base < Input < Base is the usual and simple case.

      if -Base < Input and then Input < Base then
         return Uint (Int (Uint_Direct_Bias) + Input);

      --  For values of larger magnitude, compute digits into a vector and
      --  call Vector_To_Uint.

      else
         declare
            Max_For_Int : constant := 4;
            --  Base is defined so that 4 Uint digits is sufficient to hold
            --  the largest Int.

            V : UI_Vector (1 .. Max_For_Int);
            Temp_Integer : Int;
            Ret_Value : Uint;

         begin
            for I in V'range loop
               V (I) := 0;
            end loop;

            Temp_Integer := Input;

            for I in reverse V'range loop
               V (I) := abs (Temp_Integer rem Base);
               Temp_Integer := Temp_Integer / Base;
            end loop;

            return Vector_To_Uint (V, Input < 0);
         end;
      end if;
   end UI_From_Int;

   ----------------
   -- UI_To_Int --
   ----------------

   function UI_To_Int (Input : Uint) return Int is
   begin
      if Input <= Uint_Direct_Last then
         return Int (Input - Uint_Direct_Bias);

      --  Case of input is more than one digit

      else
         declare
            In_Length : Int := N_Digits (Input);
            In_Vec : UI_Vector (1 .. In_Length);
            Ret_Int : Int;

         begin

            --  Uints of more than one digit could be outside the range for
            --  Ints. Caller should have checked for this if not certain.
            --  Fatal error to attempt to convert from value outside Int'range.

            pragma Assert (UI_Is_In_Int_Range (Input));

            --  Otherwise, proceed ahead, we are OK

            Init_Operand (Input, In_Vec);
            Ret_Int := 0;

            --  Calculate -|Input| and then negates if value is positive.
            --  This handles our current definition of Int (based on
            --  2s complement). Is it secure enough?

            for Idx in In_Vec'range loop
               Ret_Int := Ret_Int * Base - abs In_Vec (Idx);
            end loop;

            if In_Vec (1) < 0 then
               return Ret_Int;
            else
               return -Ret_Int;
            end if;
         end;
      end if;
   end UI_To_Int;

   ---------------
   -- UI_Image --
   ---------------

   function UI_Image (Input : Uint) return Name_Id is

      procedure Set_Digits (U : Uint);
      --  Recursive procedure to set digits of non-negative Uint value

      procedure Set_Digits (U : Uint) is
      begin
         if UI_Ge (U, Uint_10) then
            Set_Digits (UI_Quotient (U, Uint_10));
         end if;

         if Name_Len = Max_Name_Length - 6 then
            Name_Buffer (Name_Len + 1) := '.';
            Name_Buffer (Name_Len + 2) := '.';
            Name_Buffer (Name_Len + 3) := '.';
            Name_Len := Name_Len + 3;

         elsif Name_Len < Max_Name_Length - 6 then
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) :=
              Char'Val (Char'Pos ('0') + UI_To_Int (UI_Rem (U, Uint_10)));
         end if;
      end Set_Digits;

   begin
      if UI_Is_Negative (Input) then
         Name_Len := 1;
         Name_Buffer (1) := '-';
         Set_Digits (UI_Negate (Input));

      else
         Name_Len := 0;
         Set_Digits (Input);
      end if;

      return Name_Find;
   end UI_Image;

   -------------------------
   -- UI_Is_In_Int_Range --
   -------------------------

   function UI_Is_In_Int_Range (Input : Uint) return Boolean is
   begin
      return UI_Ge (Input, Uint_Int_First)
        and then UI_Le (Input, Uint_Int_Last);
   end UI_Is_In_Int_Range;

   -----------------
   -- UI_Is_Zero --
   -----------------

   function UI_Is_Zero (Input : Uint) return Boolean is
   begin
      return Input = Uint_0;
   end UI_Is_Zero;

   ---------------------
   -- UI_Is_Negative --
   ---------------------

   function UI_Is_Negative (Input : Uint) return Boolean is
   begin
      return Input < Uint_Direct_Bias
        or else (Input >= Uint_First_Entry
                   and then Udigits.Table (Uints.Table (Input).Loc) < 0);
   end UI_Is_Negative;

   ---------------------
   -- UI_Is_Positive --
   ---------------------

   function UI_Is_Positive (Input : Uint) return Boolean is
   begin
      if Input <= Uint_Direct_Bias then
         return False;
      elsif Input <= Uint_Direct_Last then
         return True;
      else
         return Udigits.Table (Uints.Table (Input).Loc) > 0;
      end if;
   end UI_Is_Positive;

   ------------
   -- UI_Eq --
   ------------

   function UI_Eq (Left, Right : Uint) return Boolean is
   begin
      return not UI_Ne (Left, Right);
   end UI_Eq;

   ------------
   -- UI_Ne --
   ------------

   function UI_Ne (Left, Right : Uint) return Boolean is
      Left_Loc  : Int;
      Right_Loc : Int;
      Size      : Int;

   begin
      Size := N_Digits (Left);

      if Size /= N_Digits (Right) then
         return True;

      elsif Size = 1 then
         return Left /= Right;

      else
         Left_Loc  := Uints.Table (Left).Loc;
         Right_Loc := Uints.Table (Right).Loc;

         for I in 0 .. Size -  1 loop
            if Udigits.Table (Left_Loc + I) /=
               Udigits.Table (Right_Loc + I)
            then
               return True;
            end if;
         end loop;

         return False;
      end if;
   end UI_Ne;

   ------------
   -- UI_Lt --
   ------------

   function UI_Lt (Left, Right : Uint) return Boolean is
      L_Length : constant Int := N_Digits (Left);
      R_Length : constant Int := N_Digits (Right);

   begin

      --  Quick processing for both arguments one digit long

      if L_Length = 1 and then R_Length = 1 then
         return Left < Right;

      --  At least one argument is more than one digit long

      else
         declare
            L_Vec : UI_Vector (1 .. L_Length);
            R_Vec : UI_Vector (1 .. R_Length);

         begin
            Init_Operand (Left, L_Vec);
            Init_Operand (Right, R_Vec);

            if L_Vec (1) < 0 then

               --  First argument negative, second argument non-negative

               if R_Vec (1) >= 0 then
                  return True;

               --  Both arguments negative

               else
                  if L_Length /= R_Length then
                     return L_Length > R_Length;

                  elsif L_Vec (1) /= R_Vec (1) then
                     return L_Vec (1) < R_Vec (1);

                  else
                     for I in 2 .. L_Vec'Last loop
                        if L_Vec (I) /= R_Vec (I) then
                           return L_Vec (I) < R_Vec (I);
                        end if;
                     end loop;

                     return False;
                  end if;
               end if;

            else

               --  First argument non-negative, second argument negative

               if R_Vec (1) < 0 then
                  return False;

               --  Both arguments non-negative

               else
                  if L_Length /= R_Length then
                     return L_Length < R_Length;
                  else
                     for I in L_Vec'range loop
                        if L_Vec (I) /= R_Vec (I) then
                           return L_Vec (I) < R_Vec (I);
                        end if;
                     end loop;

                     return False;
                  end if;
               end if;
            end if;
         end;
      end if;
   end UI_Lt;

   ------------
   -- UI_Gt --
   ------------

   function UI_Gt (Left, Right : Uint) return Boolean is
   begin
      return UI_Lt (Right, Left);
   end UI_Gt;

   ------------
   -- UI_Le --
   ------------

   function UI_Le (Left, Right : Uint) return Boolean is
   begin
      return not UI_Lt (Right, Left);
   end UI_Le;

   ------------
   -- UI_Ge --
   ------------

   function UI_Ge (Left, Right : Uint) return Boolean is
   begin
      return not UI_Lt (Left, Right);
   end UI_Ge;

   -------------
   -- UI_Abs --
   -------------

   function UI_Abs (Right : Uint) return Uint is
   begin
      if UI_Is_Negative (Right) then
         return UI_Negate (Right);
      else
         return Right;
      end if;
   end UI_Abs;

   -------------
   -- UI_Rem --
   -------------

   function UI_Rem (Left, Right : Uint) return Uint is
   begin
      if N_Digits (Left) = 1 and then N_Digits (Right) = 1 then
         return UI_From_Int (UI_To_Int (Left) rem UI_To_Int (Right));
      else
         return UI_Difference
               (Left, UI_Product (UI_Quotient (Left, Right), Right));
      end if;
   end UI_Rem;

   -------------
   -- UI_Mod --
   -------------

   function UI_Mod (Left, Right : Uint) return Uint is
      Urem : Uint := UI_Rem (Left, Right);

   begin
      if UI_Is_Negative (Left) = UI_Is_Negative (Right)
         or else UI_Is_Zero (Urem)
      then
         return Urem;
      else
         return UI_Sum (Right, Urem);
      end if;
   end UI_Mod;

   ----------------
   -- UI_Negate --
   ----------------

   function UI_Negate (Right : Uint) return Uint is
   begin

      --  Quick processing for single digit case

      if Right <= Uint_Direct_Last then
         return Uint_Direct_Bias - (Right  - Uint_Direct_Bias);

      --  Else copy the value to the end of the table, negating 1st digit

      else
         declare
            Length : Int := Uints.Table (Right).Length;
            Loc    : Int := Uints.Table (Right).Loc;

         begin
            Uints.Increment_Last;
            Uints.Table (Uints.Last).Length := Length;
            Uints.Table (Uints.Last).Loc := Udigits.Last + 1;

            Udigits.Increment_Last;
            Udigits.Table (Udigits.Last) := - Udigits.Table (Loc);

            for Idx in 2 .. Length loop
               Udigits.Increment_Last;
               Udigits.Table (Udigits.Last) := Udigits.Table (Loc + Idx - 1);
            end loop;

            return Uints.Last;
         end;
      end if;
   end UI_Negate;

   --------------------
   -- UI_Difference --
   --------------------

   function UI_Difference (Left, Right : Uint) return Uint is
   begin
      return UI_Sum (Left, UI_Negate (Right));
   end UI_Difference;

   -------------
   -- UI_Sum --
   -------------

   function UI_Sum (Left, Right : Uint) return Uint is
   begin
      --  First try simple case where Int "+" can be used;

      if Left <= Uint_Direct_Last and then Right <= Uint_Direct_Last then
         return (Left - Uint_Direct_Bias) + Right;

      --  Otherwise full circuit is needed

      else
         declare
            L_Length   : Int := N_Digits (Left);
            R_Length   : Int := N_Digits (Right);
            L_Vec      : UI_Vector (1 .. L_Length);
            R_Vec      : UI_Vector (1 .. R_Length);
            Sum_Length : Int;
            Tmp_Int, Carry, Borrow : Int;
            X_Bigger, Y_Bigger, Result_Negative : Boolean := False;

         begin
            Init_Operand (Left, L_Vec);
            Init_Operand (Right, R_Vec);
            --  Copy operands to vectors.

            --  At least one of them is more than 1 digit - so calculation is
            --  needed.

            --  Calculate number of digits sufficient to hold result.

            if L_Length > R_Length then
               Sum_Length := L_Length + 1;
               X_Bigger := True;
            else
               Sum_Length := R_Length + 1;
               if R_Length > L_Length then Y_Bigger := True; end if;
            end if;

            declare

               --  Make copies of the absolute values of L_Vec and R_Vec into
               --  X and Y both with lengths equal to the maximum possibly
               --  needed. This makes looping over the digits much simpler.

               X      : UI_Vector (1 .. Sum_Length);
               Y      : UI_Vector (1 .. Sum_Length);
               Tmp_UI : UI_Vector (1 .. Sum_Length);

            begin
               for I in 1 .. Sum_Length - L_Length loop
                  X (I) := 0;
               end loop;

               X (Sum_Length - L_Length + 1) := abs L_Vec (1);

               for I in 2 .. L_Length loop
                  X (I + (Sum_Length - L_Length)) := L_Vec (I);
               end loop;

               for I in 1 .. Sum_Length - R_Length loop
                  Y (I) := 0;
               end loop;

               Y (Sum_Length - R_Length + 1) := abs R_Vec (1);

               for I in 2 .. R_Length loop
                  Y (I + (Sum_Length - R_Length)) := R_Vec (I);
               end loop;

               if (L_Vec (1) < 0) = (R_Vec (1) < 0) then

                  --  Same sign so just add

                  Carry := 0;
                  for I in reverse 1 .. Sum_Length loop
                     Tmp_Int := X (I) + Y (I) + Carry;
                     if Tmp_Int > Base then
                        Tmp_Int := Tmp_Int - Base;
                        Carry := 1;
                     else
                        Carry := 0;
                     end if;
                     X (I) := Tmp_Int;
                  end loop;

                  return Vector_To_Uint (X, L_Vec (1) < 0);

               else

                  --  Find which one has bigger magnitude

                  if not (X_Bigger or Y_Bigger) then
                     for I in L_Vec'range loop
                        if abs L_Vec (I) > abs R_Vec (I) then
                           X_Bigger := True;
                           exit;
                        elsif abs R_Vec (I) > abs L_Vec (I) then
                           Y_Bigger := True;
                           exit;
                        end if;
                     end loop;
                  end if;

                  --  If they have identical magnitude, just return 0, else
                  --  swap if necessary so that X had the bigger magnitude.
                  --  Determine if result is negative at this time.

                  Result_Negative := False;

                  if not (X_Bigger or Y_Bigger) then
                     return Uint_0;

                  elsif Y_Bigger then
                     if R_Vec (1) < 0 then
                        Result_Negative := True;
                     end if;

                     Tmp_UI := X;
                     X := Y;
                     Y := Tmp_UI;

                  else
                     if L_Vec (1) < 0 then
                        Result_Negative := True;
                     end if;
                  end if;

                  --  Subtract Y from the bigger X

                  Borrow := 0;

                  for I in reverse 1 .. Sum_Length loop
                     Tmp_Int := X (I) - Y (I) + Borrow;

                     if Tmp_Int < 0 then
                        Tmp_Int := Tmp_Int + Base;
                        Borrow := -1;
                     else
                        Borrow := 0;
                     end if;

                     X (I) := Tmp_Int;
                  end loop;

                  return Vector_To_Uint (X, Result_Negative);

               end if;
            end;
         end;
      end if;
   end UI_Sum;

   ------------------
   -- UI_Quotient --
   ------------------

   function UI_Quotient (Left, Right : Uint) return Uint is
      L_Length    : Int := N_Digits (Left);
      R_Length    : Int := N_Digits (Right);
      Q_Length    : Int := L_Length - R_Length + 1;
      L_Vec       : UI_Vector (1 .. L_Length);
      R_Vec       : UI_Vector (1 .. R_Length);
      D           : Int;
      Remainder   : Int;
      Tmp_Divisor : Int;
      Carry       : Int;
      Tmp_Int     : Int;
      Tmp_Dig     : Int;

   begin

      --  0 divisors should be checked for outside of calls to this
      --  function - so this is considered an error in the compiler code.

      pragma Assert (not UI_Is_Zero (Right));

      --  Some special cases that are simpler to compute than the general
      --  case are treated first.

      if L_Length = 1 and then R_Length = 1 then
         return UI_From_Int (UI_To_Int (Left) / UI_To_Int (Right));
      elsif  L_Length < R_Length then
         return Uint_0;
      end if;

      --  Copy operands into vectors

      Init_Operand (Left, L_Vec);
      Init_Operand (Right, R_Vec);


      if R_Length = 1 then

         --  In this case, each digit (from most to least significant) can
         --  simply be divided by the divisor, with the remainder carried
         --  to the next digit.

         Remainder := 0;
         Tmp_Divisor := abs R_Vec (1);

         declare
            Quotient : UI_Vector (1 .. L_Length);
         begin

            for J in L_Vec'range loop
               Tmp_Int     := Remainder * Base + abs L_Vec (J);
               Quotient (J) := Tmp_Int / Tmp_Divisor;
               Remainder   := Tmp_Int rem Tmp_Divisor;
            end loop;

            return
              Vector_To_Uint (Quotient, (L_Vec (1) < 0 xor R_Vec (1) < 0));

         end;

      end if;

      --  The possible simple cases have been exhausted. Now turn to the
      --  algorithm D from the section of Knuth mentioned at the top of
      --  this package.

      declare
         Dividend : UI_Vector (1 .. L_Length + 1);
         Divisor  : UI_Vector (1 .. R_Length);
         Quotient : UI_Vector (1 .. Q_Length);
         Divisor_Dig1, Divisor_Dig2 : Int;
         Q_Guess : Int;
      begin

         --  [ NORMALIZE ] (step D1 in the algorithm). First calculate the
         --  scale d, and then multiply Left and Right (u and v in the book)
         --  by d to get the dividend and divisor to work with.

         D := Base / (abs R_Vec (1) + 1);

         Dividend (1) := 0;
         Dividend (2) := abs L_Vec (1);

         for I in 3 .. L_Length + 1 loop
            Dividend (I) := L_Vec (I - 1);
         end loop;

         Divisor (1) := abs R_Vec (1);

         for I in 2 .. R_Length loop
            Divisor (I) := R_Vec (I);
         end loop;

         if D > 1 then

            --  Multiply Dividend by D

            Carry := 0;
            for J in reverse Dividend'range loop
               Tmp_Int      := Dividend (J) * D + Carry;
               Dividend (J) := Tmp_Int rem Base;
               Carry        := Tmp_Int / Base;
            end loop;

            --  Multiply Divisor by d.

            Carry := 0;
            for J in reverse Divisor'range loop
               Tmp_Int      := Divisor (J) * D + Carry;
               Divisor (J)  := Tmp_Int rem Base;
               Carry        := Tmp_Int / Base;
            end loop;

         end if;

         --  Main loop of long division algorithm.

         Divisor_Dig1 := Divisor (1);
         Divisor_Dig2 := Divisor (2);

         for J in Quotient'range loop

            --  [ CALCULATE Q (hat) ] (step D3 in the algorithm).

            Tmp_Int := Dividend (J) * Base + Dividend (J + 1);
            --  Used in couple of places below.

            if Dividend (J) = Divisor_Dig1 then
               Q_Guess := Base - 1;
            else
               Q_Guess := Tmp_Int / Divisor_Dig1;
            end if;
            --  Initial guess.

            while Divisor_Dig2 * Q_Guess >
                  (Tmp_Int - Q_Guess * Divisor_Dig1) * Base + Dividend (J + 2)
            loop
               Q_Guess := Q_Guess - 1;
            end loop;
            --  Refine the guess.

            --  [ MULTIPLY & SUBTRACT] (step D4). Q_Guess * Divisor is
            --  subtracted from the remaining dividend.

            Carry := 0;
            for K in reverse Divisor'range loop
               Tmp_Int := Dividend (J + K) - Q_Guess * Divisor (K) + Carry;
               Tmp_Dig := Tmp_Int rem Base;
               Carry   := Tmp_Int / Base;
               if Tmp_Dig < 0 then
                  Tmp_Dig := Tmp_Dig + Base;
                  Carry   := Carry - 1;
               end if;
               Dividend (J + K) := Tmp_Dig;
            end loop;

            Dividend (J) := Dividend (J) + Carry;

            --  [ TEST REMAINDER ] & [ ADD BACK ] (steps D5 and D6)
            --  Here there is a slight difference from the book: the last
            --  carry is always added in above and below (cancelling each
            --  other). In fact the dividend going negative is used as
            --  the test.

            --  If the Dividend went negative, then Q_Guess was off by
            --  one, so it is decremented, and the divisor is added back
            --  into the relevant portion of the dividend.

            if Dividend (J) < 0 then
               Q_Guess := Q_Guess - 1;
               Carry := 0;
               for K in reverse Divisor'range loop
                  Tmp_Int := Dividend (J + K) + Divisor (K) + Carry;
                  if Tmp_Int > Base then
                     Tmp_Int := Tmp_Int - Base;
                     Carry := 1;
                  else
                     Carry := 0;
                  end if;
                  Dividend (J + K) := Tmp_Int;
               end loop;
               Dividend (J) := Dividend (J) + Carry;
            end if;

            Quotient (J) := Q_Guess;
            --  Got the quotient digit;

         end loop;

         return Vector_To_Uint (Quotient, (L_Vec (1) < 0 xor R_Vec (1) < 0));

      end;   -- End of declare block

   end UI_Quotient;

   -----------------
   -- UI_Product --
   -----------------

   function UI_Product (Left, Right : Uint) return Uint is

      L_Length : Int := N_Digits (Left);
      R_Length : Int := N_Digits (Right);
      L_Vec : UI_Vector (1 .. L_Length);
      R_Vec : UI_Vector (1 .. R_Length);

   begin

      if L_Length = 1 and then R_Length = 1 then
         return UI_From_Int
           (Int (Left - Uint_Direct_Bias) * Int (Right - Uint_Direct_Bias));
      end if;
      --  Do the simple case.

      Init_Operand (Left, L_Vec);
      Init_Operand (Right, R_Vec);
      --  Copy operands into vectors.

      --  General case (Algorithm M in Knuth).

      declare
         Product : UI_Vector (1 .. L_Length + R_Length);
         Tmp_Sum, Carry : Int;

      begin
         for I in Product'range loop
            Product (I) := 0;
         end loop;

         for J in reverse R_Vec'range loop
            Carry := 0;
            for K in reverse L_Vec'range loop
               Tmp_Sum :=
                 abs (L_Vec (K) * R_Vec (J)) + Product (J + K) + Carry;
               Product (J + K) := Tmp_Sum rem Base;
               Carry := Tmp_Sum / Base;
            end loop;
            Product (J) := Carry;
         end loop;

         return Vector_To_Uint (Product, (L_Vec (1) < 0 xor R_Vec (1) < 0));
      end;
   end UI_Product;

   ----------------------
   -- UI_Exponentiate --
   ----------------------

   --  The exponentiation function is from 4.6.3 of the same volume of Knuth.

   function UI_Exponentiate (Left, Right : Uint) return Uint is

      N : Uint := Right;
      Squares : Uint := Left;
      Result : Uint := Uint_1;
      Uint_2 : Uint;

   begin
      --  Any value raised to power of 0 is 1

      if UI_Is_Zero (Right) then
         return Uint_1;
      end if;

      --  Negative exponents not allowed for Integers

      pragma Assert (not UI_Is_Negative (Right));

      --  0 to any positive power is 0.

      if UI_Is_Zero (Left) then
         return Uint_0;
      end if;

      --  Any value raised to power of 1 is that value

      if Right = 1 then
         return Left;
      end if;

      --  Now do general case.

      Uint_2 := UI_From_Int (2);

      loop
         if UI_Eq (UI_Mod (N, Uint_2), Uint_1) then
            Result := UI_Product (Result, Squares);
         end if;

         N := UI_Quotient (N, Uint_2);

         if UI_Is_Zero (N) then return Result; end if;

         Squares := UI_Product (Squares, Squares);
      end loop;
   end UI_Exponentiate;

begin
   Uint_Int_First := UI_From_Int (Int'First);
   Uint_Int_Last  := UI_From_Int (Int'Last);

end Uintp;
