------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                U I N T P                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.14 $                             --
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

with Alloc; use Alloc;
with Types; use Types;
with Table;
package Uintp is

--  Support for universal integer arithmetic

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header file sinfo.h

   --------------------
   -- Uint Constants --
   --------------------

   --  Note: these are all in the directly represented range, so it is
   --  legitimate to use the = and /= operators directly on these values,
   --  rather than using the UI_Eq and UI_Ne functions

   Uint_0  : constant Uint := Uint_Direct_Bias;
   Uint_1  : constant Uint := Uint_Direct_Bias + 1;
   Uint_2  : constant Uint := Uint_Direct_Bias + 2;
   Uint_3  : constant Uint := Uint_Direct_Bias + 3;
   Uint_4  : constant Uint := Uint_Direct_Bias + 4;
   Uint_5  : constant Uint := Uint_Direct_Bias + 5;
   Uint_6  : constant Uint := Uint_Direct_Bias + 6;
   Uint_7  : constant Uint := Uint_Direct_Bias + 7;
   Uint_8  : constant Uint := Uint_Direct_Bias + 8;
   Uint_9  : constant Uint := Uint_Direct_Bias + 9;
   Uint_10 : constant Uint := Uint_Direct_Bias + 10;
   Uint_16 : constant Uint := Uint_Direct_Bias + 16;
   Uint_32 : constant Uint := Uint_Direct_Bias + 32;

   Uint_Minus_1 : constant Uint := Uint_Direct_Bias - 1;
   Uint_Minus_2 : constant Uint := Uint_Direct_Bias - 2;
   Uint_Minus_3 : constant Uint := Uint_Direct_Bias - 3;
   Uint_Minus_4 : constant Uint := Uint_Direct_Bias - 4;
   Uint_Minus_5 : constant Uint := Uint_Direct_Bias - 5;
   Uint_Minus_6 : constant Uint := Uint_Direct_Bias - 6;
   Uint_Minus_7 : constant Uint := Uint_Direct_Bias - 7;
   Uint_Minus_8 : constant Uint := Uint_Direct_Bias - 8;
   Uint_Minus_9 : constant Uint := Uint_Direct_Bias - 9;

   --  The constants in this table, initialized by UI_Initialize, represent
   --  the powers of 2 (i.e. UI_Power (N) contains the Uint value 2 ** N).
   --  These values are generally NOT in the directly represented range,
   --  so equality comparisons should use UI_Eq and UI_Ne, rather than
   --  using the = and /= operators directly.

   UI_Power : array (Int range 0 .. 64) of Uint;

   -----------------
   -- Subprograms --
   -----------------

   function Last_Uint return Uint;
   pragma Inline (Last_Uint);
   --  Returns Uint value for last currently allocated entry in Uint table

   procedure Initialize_Uint;
   --  Initialize Uint tables

   function UI_Sum (Left, Right : Uint) return Uint;
   --  Returns sum of two universal integers.

   function UI_Difference (Left, Right : Uint) return Uint;
   pragma Inline (UI_Difference);
   --  Returns difference of two universal integers.

   function UI_Quotient (Left, Right : Uint) return Uint;
   --  Returns quotient of two universal integers. Fatal error if
   --  Right is zero.

   function UI_Product (Left, Right : Uint) return Uint;
   --  Returns product of two universal integers.

   function UI_Exponentiate (Left, Right : Uint) return Uint;
   --  Returns result of exponentiating two universal integers.
   --  Fatal error if Right is negative.

   function UI_Abs (Right : Uint) return Uint;
   pragma Inline (UI_Abs);
   --  Returns abs function of universal integer.

   function UI_Mod (Left, Right : Uint) return Uint;
   pragma Inline (UI_Mod);
   --  Returns mod function of two universal integers.

   function UI_Rem (Left, Right : Uint) return Uint;
   pragma Inline (UI_Rem);
   --  Returns rem function of two universal integers.

   function UI_Negate (Right : Uint) return Uint;
   pragma Inline (UI_Negate);
   --  Returns negative of universal integer.

   function UI_Eq (Left, Right : Uint) return Boolean;
   pragma Inline (UI_Eq);
   --  Compares universal integers for equality.

   function UI_Ne (Left, Right : Uint) return Boolean;
   pragma Inline (UI_Ne);
   --  Compares universal integers for inequality.

   function UI_Lt (Left, Right : Uint) return Boolean;
   --  Compares universal integers for less than.

   function UI_Le (Left, Right : Uint) return Boolean;
   pragma Inline (UI_Le);
   --  Compares universal integers for less than or equal.

   function UI_Gt (Left, Right : Uint) return Boolean;
   pragma Inline (UI_Gt);
   --  Compares universal integers for greater than.

   function UI_Ge (Left, Right : Uint) return Boolean;
   pragma Inline (UI_Ge);
   --  Compares universal integers for greater than or equal.

   function UI_Image (Input : Uint) return Name_Id;
   --  Returns Image of Universal Integer as a string in the names table

   function UI_From_Int (Input : Int) return Uint;
   --  Converts Int value to universal integer form.

   function UI_Is_In_Int_Range (Input : Uint) return Boolean;
   pragma Inline (UI_Is_In_Int_Range);
   --  Determines if universal integer is in Int range.

   function UI_To_Int (Input : Uint) return Int;
   --  Converts universal integer value to Int. Fatal error
   --  if value is not in appropriate range.

   function UI_Is_Zero (Input : Uint) return Boolean;
   pragma Inline (UI_Is_Zero);
   --  Determines if universal integer value is zero.

   function UI_Is_Negative (Input : Uint) return Boolean;
   pragma Inline (UI_Is_Negative);
   --  Determines if universal integer value is less than zero.

   function UI_Is_Positive (Input : Uint) return Boolean;
   pragma Inline (UI_Is_Positive);
   --  Determines if universal integer value is greater than zero.

   -----------------------------
   -- Private Part Subpackage --
   -----------------------------

   --  The following package contains the definition of the data structure
   --  used by the implementation of the Uint package. Logically it really
   --  corresponds to the private part, hence the name. The reason that it
   --  is defined as a sub-package is to allow special access from clients
   --  that need to see the internals of the data structures.

   package Uintp_Private_Part is

   --  As described in Types, universal integer values are represented as
   --  multi-digit numbers using Base (=2**15, defined in Types) as the
   --  base for the representation, with the first digit (only) carrying
   --  the sign. One digit numbers are represented directly by adding the
   --  value Uint_Direct_Bias, so that they lie in the range:

   --     Uint_Direct_Bias -  (Base - 1) .. Uint_Direct_Bias + (Base - 1)

   --  The definitions Uint_Direct_First and Uint_Direct_Last (defined in
   --  Types) define the limits of this range.

   --  Values outside this range (which have more than one digit) are stored
   --  using two tables. The secondary table Udigits contains sequences of
   --  Int values consisting of the digits of the number in a radix Base
   --  system. The digits are stored from most significant to least significant
   --  with the first digit only carrying the sign.

   --  There is one entry in the primary Uints table for each distinct Uint
   --  value. This table entry contains the length (number of digits) and
   --  a starting offset of the value in the Udigits table.

   --  Base is defined to allow the primitive operations (a0, b0, c0)
   --  defined in the section "The Classical Algorithms" (sec. 4.3.1)
   --  of Knuth's "The Art of Computer Programming", Vol. 2. It is these
   --  algorithms that are used in this package.

   --  Some subprograms defined in this package manipulate the Udigits
   --  table directly, while for others it is more convenient to work with
   --  locally defined arrays of the digits of the the Universal Integers.
   --  The type UI_Vector is defined for this purpose and some internal
   --  subprograms used for converting from one to the other are defined.

      type UI_Vector is array (Pos range <>) of Int;
      --  Vector containing the integer values of a Uint value

   --  Note: An earlier version of this package used pointers of arrays
   --  of Ints (dynamically allocated) for the Uint type. The change
   --  leads to a few less natural idioms used throughout this code, but
   --  eliminates all uses of the heap except for the table package itself.
   --  For example, Uint parameters are often converted to UI_Vectors for
   --  internal manipulation. This is done by creating the local UI_Vector
   --  using the function N_Digits on the Uint to find the size needed for
   --  the vector, and then calling Init_Operand to copy the values out
   --  of the table into the vector.

      type Uint_Entry is record
         Length : Pos;
         --  Length of entry in Udigits table in digits (i.e. in words)

         Loc : Int;
         --  Starting location in Udigits table of this Uint value
      end record;

      package Uints is new Table (
         Component_Type => Uint_Entry,
         Index_Type     => Uint,
         Low_Bound      => Uint_First_Entry,
         Initial        => Alloc_Uints_Initial,
         Increment      => Alloc_Uints_Increment,
         Table_Name     => "Uints");

      package Udigits is new Table (
         Component_Type => Int,
         Index_Type     => Int,
         Low_Bound      => 0,
         Initial        => Alloc_Udigits_Initial,
         Increment      => Alloc_Udigits_Increment,
         Table_Name     => "Udigits");

   end Uintp_Private_Part;

end Uintp;
