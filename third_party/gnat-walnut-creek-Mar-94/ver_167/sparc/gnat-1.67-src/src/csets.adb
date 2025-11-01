------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                C S E T S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.8 $                              --
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

with Opt; use Opt;
package body Csets is

   -----------------------------
   -- Definitions for Latin-1 --
   -----------------------------

   Fold_Latin_1 : Translate_Table := Translate_Table'(

      'a' => 'A',  X_E0 => X_C0,  X_F0 => X_D0,
      'b' => 'B',  X_E1 => X_C1,  X_F1 => X_D1,
      'c' => 'C',  X_E2 => X_C2,  X_F2 => X_D2,
      'd' => 'D',  X_E3 => X_C3,  X_F3 => X_D3,
      'e' => 'E',  X_E4 => X_C4,  X_F4 => X_D4,
      'f' => 'F',  X_E5 => X_C5,  X_F5 => X_D5,
      'g' => 'G',  X_E6 => X_C6,  X_F6 => X_D6,
      'h' => 'H',  X_E7 => X_C7,
      'i' => 'I',  X_E8 => X_C8,  X_F8 => X_D8,
      'j' => 'J',  X_E9 => X_C9,  X_F9 => X_D9,
      'k' => 'K',  X_EA => X_CA,  X_FA => X_DA,
      'l' => 'L',  X_EB => X_CB,  X_FB => X_DB,
      'm' => 'M',  X_EC => X_CC,  X_FC => X_DC,
      'n' => 'N',  X_ED => X_CD,  X_FD => X_DD,
      'o' => 'O',  X_EE => X_CE,  X_FE => X_DE,
      'p' => 'P',  X_EF => X_CF,
      'q' => 'Q',
      'r' => 'R',
      's' => 'S',
      't' => 'T',
      'u' => 'U',
      'v' => 'V',
      'w' => 'W',
      'x' => 'X',
      'y' => 'Y',
      'z' => 'Z',

      'A' => 'A',  X_C0 => X_C0,  X_D0 => X_D0,
      'B' => 'B',  X_C1 => X_C1,  X_D1 => X_D1,
      'C' => 'C',  X_C2 => X_C2,  X_D2 => X_D2,
      'D' => 'D',  X_C3 => X_C3,  X_D3 => X_D3,
      'E' => 'E',  X_C4 => X_C4,  X_D4 => X_D4,
      'F' => 'F',  X_C5 => X_C5,  X_D5 => X_D5,
      'G' => 'G',  X_C6 => X_C6,  X_D6 => X_D6,
      'H' => 'H',  X_C7 => X_C7,
      'I' => 'I',  X_C8 => X_C8,  X_D8 => X_D8,
      'J' => 'J',  X_C9 => X_C9,  X_D9 => X_D9,
      'K' => 'K',  X_CA => X_CA,  X_DA => X_DA,
      'L' => 'L',  X_CB => X_CB,  X_DB => X_DB,
      'M' => 'M',  X_CC => X_CC,  X_DC => X_DC,
      'N' => 'N',  X_CD => X_CD,  X_DD => X_DD,
      'O' => 'O',  X_CE => X_CE,  X_DE => X_DE,
      'P' => 'P',  X_CF => X_CF,  X_DF => X_DF,  X_FF => X_FF,
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',

      '0' => '0',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',

      '_' => '_',

      others => ' ');

   -----------------------------
   -- Definitions for Latin-2 --
   -----------------------------

   Fold_Latin_2 : Translate_Table := Translate_Table'(

      'a' => 'A',  X_E0 => X_C0,  X_F0 => X_D0,
      'b' => 'B',  X_E1 => X_C1,  X_F1 => X_D1,  X_B1 => X_A1,
      'c' => 'C',  X_E2 => X_C2,  X_F2 => X_D2,
      'd' => 'D',  X_E3 => X_C3,  X_F3 => X_D3,  X_B3 => X_A3,
      'e' => 'E',  X_E4 => X_C4,  X_F4 => X_D4,
      'f' => 'F',  X_E5 => X_C5,  X_F5 => X_D5,  X_B5 => X_A5,
      'g' => 'G',  X_E6 => X_C6,  X_F6 => X_D6,  X_B6 => X_A6,
      'h' => 'H',  X_E7 => X_C7,
      'i' => 'I',  X_E8 => X_C8,  X_F8 => X_D8,
      'j' => 'J',  X_E9 => X_C9,  X_F9 => X_D9,  X_B9 => X_A9,
      'k' => 'K',  X_EA => X_CA,  X_FA => X_DA,  X_BA => X_AA,
      'l' => 'L',  X_EB => X_CB,  X_FB => X_DB,  X_BB => X_AB,
      'm' => 'M',  X_EC => X_CC,  X_FC => X_DC,  X_BC => X_AC,
      'n' => 'N',  X_ED => X_CD,  X_FD => X_DD,
      'o' => 'O',  X_EE => X_CE,  X_FE => X_DE,  X_BE => X_AE,
      'p' => 'P',  X_EF => X_CF,  X_FF => X_DF,  X_BF => X_AF,
      'q' => 'Q',
      'r' => 'R',
      's' => 'S',
      't' => 'T',
      'u' => 'U',
      'v' => 'V',
      'w' => 'W',
      'x' => 'X',
      'y' => 'Y',
      'z' => 'Z',

      'A' => 'A',  X_C0 => X_C0,  X_D0 => X_D0,
      'B' => 'B',  X_C1 => X_C1,  X_D1 => X_D1,  X_A1 => X_A1,
      'C' => 'C',  X_C2 => X_C2,  X_D2 => X_D2,
      'D' => 'D',  X_C3 => X_C3,  X_D3 => X_D3,  X_A3 => X_A3,
      'E' => 'E',  X_C4 => X_C4,  X_D4 => X_D4,
      'F' => 'F',  X_C5 => X_C5,  X_D5 => X_D5,  X_A5 => X_A5,
      'G' => 'G',  X_C6 => X_C6,  X_D6 => X_D6,  X_A6 => X_A6,
      'H' => 'H',  X_C7 => X_C7,
      'I' => 'I',  X_C8 => X_C8,  X_D8 => X_D8,
      'J' => 'J',  X_C9 => X_C9,  X_D9 => X_D9,  X_A9 => X_A9,
      'K' => 'K',  X_CA => X_CA,  X_DA => X_DA,  X_AA => X_AA,
      'L' => 'L',  X_CB => X_CB,  X_DB => X_DB,  X_AB => X_AB,
      'M' => 'M',  X_CC => X_CC,  X_DC => X_DC,  X_AC => X_AC,
      'N' => 'N',  X_CD => X_CD,  X_DD => X_DD,
      'O' => 'O',  X_CE => X_CE,  X_DE => X_DE,  X_AE => X_AE,
      'P' => 'P',  X_CF => X_CF,  X_DF => X_DF,  X_AF => X_AF,
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',

      '0' => '0',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',

      '_' => '_',

      others => ' ');

   -----------------------------
   -- Definitions for Latin-3 --
   -----------------------------

   Fold_Latin_3 : Translate_Table := Translate_Table'(

      'a' => 'A',  X_E0 => X_C0,
      'b' => 'B',  X_E1 => X_C1,  X_F1 => X_D1,  X_B1 => X_A1,
      'c' => 'C',  X_E2 => X_C2,  X_F2 => X_D2,
      'd' => 'D',                 X_F3 => X_D3,
      'e' => 'E',  X_E4 => X_C4,  X_F4 => X_D4,
      'f' => 'F',  X_E5 => X_C5,  X_F5 => X_D5,  X_B5 => X_A5,
      'g' => 'G',  X_E6 => X_C6,  X_F6 => X_D6,  X_B6 => X_A6,
      'h' => 'H',  X_E7 => X_C7,
      'i' => 'I',  X_E8 => X_C8,  X_F8 => X_D8,
      'j' => 'J',  X_E9 => X_C9,  X_F9 => X_D9,  X_B9 => X_A9,
      'k' => 'K',  X_EA => X_CA,  X_FA => X_DA,  X_BA => X_AA,
      'l' => 'L',  X_EB => X_CB,  X_FB => X_DB,  X_BB => X_AB,
      'm' => 'M',  X_EC => X_CC,  X_FC => X_DC,  X_BC => X_AC,
      'n' => 'N',  X_ED => X_CD,  X_FD => X_DD,
      'o' => 'O',  X_EE => X_CE,  X_FE => X_DE,
      'p' => 'P',  X_EF => X_CF,                 X_BF => X_AF,
      'q' => 'Q',
      'r' => 'R',
      's' => 'S',
      't' => 'T',
      'u' => 'U',
      'v' => 'V',
      'w' => 'W',
      'x' => 'X',
      'y' => 'Y',
      'z' => 'Z',

      'A' => 'A',  X_C0 => X_C0,
      'B' => 'B',  X_C1 => X_C1,  X_D1 => X_D1,  X_A1 => X_A1,
      'C' => 'C',  X_C2 => X_C2,  X_D2 => X_D2,
      'D' => 'D',                 X_D3 => X_D3,
      'E' => 'E',  X_C4 => X_C4,  X_D4 => X_D4,
      'F' => 'F',  X_C5 => X_C5,  X_D5 => X_D5,  X_A5 => X_A5,
      'G' => 'G',  X_C6 => X_C6,  X_D6 => X_D6,  X_A6 => X_A6,
      'H' => 'H',  X_C7 => X_C7,
      'I' => 'I',  X_C8 => X_C8,  X_D8 => X_D8,
      'J' => 'J',  X_C9 => X_C9,  X_D9 => X_D9,  X_A9 => X_A9,
      'K' => 'K',  X_CA => X_CA,  X_DA => X_DA,  X_AA => X_AA,
      'L' => 'L',  X_CB => X_CB,  X_DB => X_DB,  X_AB => X_AB,
      'M' => 'M',  X_CC => X_CC,  X_DC => X_DC,  X_AC => X_AC,
      'N' => 'N',  X_CD => X_CD,  X_DD => X_DD,
      'O' => 'O',  X_CE => X_CE,  X_DE => X_DE,
      'P' => 'P',  X_CF => X_CF,                 X_AF => X_AF,
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',

      '0' => '0',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',

      '_' => '_',

      others => ' ');

   -----------------------------
   -- Definitions for Latin-4 --
   -----------------------------

   Fold_Latin_4 : Translate_Table := Translate_Table'(

      'a' => 'A',  X_E0 => X_C0,  X_F0 => X_D0,
      'b' => 'B',  X_E1 => X_C1,  X_F1 => X_D1,  X_B1 => X_A1,
      'c' => 'C',  X_E2 => X_C2,  X_F2 => X_D2,
      'd' => 'D',  X_E3 => X_C3,  X_F3 => X_D3,  X_B3 => X_A3,
      'e' => 'E',  X_E4 => X_C4,  X_F4 => X_D4,
      'f' => 'F',  X_E5 => X_C5,  X_F5 => X_D5,  X_B5 => X_A5,
      'g' => 'G',  X_E6 => X_C6,  X_F6 => X_D6,  X_B6 => X_A6,
      'h' => 'H',  X_E7 => X_C7,
      'i' => 'I',  X_E8 => X_C8,  X_F8 => X_D8,
      'j' => 'J',  X_E9 => X_C9,  X_F9 => X_D9,  X_B9 => X_A9,
      'k' => 'K',  X_EA => X_CA,  X_FA => X_DA,  X_BA => X_AA,
      'l' => 'L',  X_EB => X_CB,  X_FB => X_DB,  X_BB => X_AB,
      'm' => 'M',  X_EC => X_CC,  X_FC => X_DC,  X_BC => X_AC,
      'n' => 'N',  X_ED => X_CD,  X_FD => X_DD,
      'o' => 'O',  X_EE => X_CE,  X_FE => X_DE,  X_BE => X_AE,
      'p' => 'P',  X_EF => X_CF,
      'q' => 'Q',
      'r' => 'R',
      's' => 'S',
      't' => 'T',
      'u' => 'U',
      'v' => 'V',
      'w' => 'W',
      'x' => 'X',
      'y' => 'Y',
      'z' => 'Z',

      'A' => 'A',  X_C0 => X_C0,  X_D0 => X_D0,
      'B' => 'B',  X_C1 => X_C1,  X_D1 => X_D1,  X_A1 => X_A1,
      'C' => 'C',  X_C2 => X_C2,  X_D2 => X_D2,
      'D' => 'D',  X_C3 => X_C3,  X_D3 => X_D3,  X_A3 => X_A3,
      'E' => 'E',  X_C4 => X_C4,  X_D4 => X_D4,
      'F' => 'F',  X_C5 => X_C5,  X_D5 => X_D5,  X_A5 => X_A5,
      'G' => 'G',  X_C6 => X_C6,  X_D6 => X_D6,  X_A6 => X_A6,
      'H' => 'H',  X_C7 => X_C7,
      'I' => 'I',  X_C8 => X_C8,  X_D8 => X_D8,
      'J' => 'J',  X_C9 => X_C9,  X_D9 => X_D9,  X_A9 => X_A9,
      'K' => 'K',  X_CA => X_CA,  X_DA => X_DA,  X_AA => X_AA,
      'L' => 'L',  X_CB => X_CB,  X_DB => X_DB,  X_AB => X_AB,
      'M' => 'M',  X_CC => X_CC,  X_DC => X_DC,  X_AC => X_AC,
      'N' => 'N',  X_CD => X_CD,  X_DD => X_DD,
      'O' => 'O',  X_CE => X_CE,  X_DE => X_DE,  X_AE => X_AE,
      'P' => 'P',  X_CF => X_CF,
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',

      '0' => '0',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',

      '_' => '_',

      others => ' ');

   ----------------------------
   -- Definitions for IBM PC --
   ----------------------------

   Fold_IBM_PC : Translate_Table := Translate_Table'(

      'a' => 'A',
      'b' => 'B',
      'c' => 'C',
      'd' => 'D',
      'e' => 'E',
      'f' => 'F',
      'g' => 'G',
      'h' => 'H',
      'i' => 'I',
      'j' => 'J',
      'k' => 'K',
      'l' => 'L',
      'm' => 'M',
      'n' => 'N',
      'o' => 'O',
      'p' => 'P',
      'q' => 'Q',
      'r' => 'R',
      's' => 'S',
      't' => 'T',
      'u' => 'U',
      'v' => 'V',
      'w' => 'W',
      'x' => 'X',
      'y' => 'Y',
      'z' => 'Z',

      'A' => 'A',
      'B' => 'B',
      'C' => 'C',
      'D' => 'D',
      'E' => 'E',
      'F' => 'F',
      'G' => 'G',
      'H' => 'H',
      'I' => 'I',
      'J' => 'J',
      'K' => 'K',
      'L' => 'L',
      'M' => 'M',
      'N' => 'N',
      'O' => 'O',
      'P' => 'P',
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',

      X_80 => X_80,  -- C cedilla
      X_81 => X_9A,  -- u umlaut
      X_82 => X_90,  -- e acute
      X_83 => X_83,  -- a circumflex
      X_84 => X_8E,  -- a umlaut
      X_85 => X_85,  -- a grave
      X_86 => X_8F,  -- a ring
      X_87 => X_80,  -- c cedilla
      X_88 => X_88,  -- e circumflex
      X_89 => X_89,  -- e umlaut
      X_8A => X_8A,  -- e grave
      X_8B => X_8B,  -- i umlaut
      X_8C => X_8C,  -- i circumflex
      X_8D => X_8D,  -- i grave
      X_8E => X_8E,  -- A umlaut
      X_8F => X_8F,  -- A ring

      X_90 => X_90,  -- E acute
      X_91 => X_92,  -- ae
      X_92 => X_92,  -- AE
      X_93 => X_93,  -- o circumflex
      X_94 => X_99,  -- o umlaut
      X_95 => X_95,  -- o grave
      X_96 => X_96,  -- u circumflex
      X_97 => X_97,  -- u grave
      X_98 => X_98,  -- y umlaut
      X_99 => X_99,  -- O umlaut
      X_9A => X_9A,  -- U umlaut

      X_A0 => X_A0,  -- a acute
      X_A1 => X_A1,  -- i acute
      X_A2 => X_A2,  -- o acute
      X_A3 => X_A3,  -- u acute
      X_A4 => X_A5,  -- n tilde
      X_A5 => X_A5,  -- N tilde
      X_A6 => X_A6,  -- a underline
      X_A7 => X_A7,  -- o underline

      X_E0 => X_E0,  -- lower case alpha
      X_E1 => X_E1,  -- lower case beta
      X_E2 => X_E2,  -- upper case gamma
      X_E3 => X_E3,  -- lower case pi
      X_E4 => X_E4,  -- upper case sigma (lower/upper sigma not equivalent)
      X_E5 => X_E5,  -- lower case sigma (lower/upper sigma not equivalent)
      X_E6 => X_E6,  -- lower case mu
      X_E7 => X_E7,  -- lower case tau
      X_E8 => X_E8,  -- upper case phi   (lower/upper phi not equivalent)
      X_E9 => X_E9,  -- lower case theta
      X_EA => X_EA,  -- upper case omega
      X_EB => X_EB,  -- lower case delta
      X_ED => X_ED,  -- lower case phi   (lower/upper phi not equivalent)
      X_EE => X_EE,  -- lower case epsilon

      X_FC => X_FC,  -- lower case eta

      '0' => '0',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',

      '_' => '_',

      others => ' ');

   -----------------------------------------
   -- Definitions for Full Upper Half Set --
   -----------------------------------------

   --  The full upper half set allows all upper half characters as letters,
   --  and does not recognize any upper/lower case equivalences in this half.

   Fold_Full_Upper_Half : Translate_Table := Translate_Table'(

      'a' => 'A',
      'b' => 'B',
      'c' => 'C',
      'd' => 'D',
      'e' => 'E',
      'f' => 'F',
      'g' => 'G',
      'h' => 'H',
      'i' => 'I',
      'j' => 'J',
      'k' => 'K',
      'l' => 'L',
      'm' => 'M',
      'n' => 'N',
      'o' => 'O',
      'p' => 'P',
      'q' => 'Q',
      'r' => 'R',
      's' => 'S',
      't' => 'T',
      'u' => 'U',
      'v' => 'V',
      'w' => 'W',
      'x' => 'X',
      'y' => 'Y',
      'z' => 'Z',

      'A' => 'A',
      'B' => 'B',
      'C' => 'C',
      'D' => 'D',
      'E' => 'E',
      'F' => 'F',
      'G' => 'G',
      'H' => 'H',
      'I' => 'I',
      'J' => 'J',
      'K' => 'K',
      'L' => 'L',
      'M' => 'M',
      'N' => 'N',
      'O' => 'O',
      'P' => 'P',
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',

      X_80 => X_80,  X_90 => X_90,  X_A0 => X_A0,  X_B0 => X_B0,
      X_81 => X_81,  X_91 => X_91,  X_A1 => X_A1,  X_B1 => X_B1,
      X_82 => X_82,  X_92 => X_92,  X_A2 => X_A2,  X_B2 => X_B2,
      X_83 => X_83,  X_93 => X_93,  X_A3 => X_A3,  X_B3 => X_B3,
      X_84 => X_84,  X_94 => X_94,  X_A4 => X_A4,  X_B4 => X_B4,
      X_85 => X_85,  X_95 => X_95,  X_A5 => X_A5,  X_B5 => X_B5,
      X_86 => X_86,  X_96 => X_96,  X_A6 => X_A6,  X_B6 => X_B6,
      X_87 => X_87,  X_97 => X_97,  X_A7 => X_A7,  X_B7 => X_B7,
      X_88 => X_88,  X_98 => X_98,  X_A8 => X_A8,  X_B8 => X_B8,
      X_89 => X_89,  X_99 => X_99,  X_A9 => X_A9,  X_B9 => X_B9,
      X_8A => X_8A,  X_9A => X_9A,  X_AA => X_AA,  X_BA => X_BA,
      X_8B => X_8B,  X_9B => X_9B,  X_AB => X_AB,  X_BB => X_BB,
      X_8C => X_8C,  X_9C => X_9C,  X_AC => X_AC,  X_BC => X_BC,
      X_8D => X_8D,  X_9D => X_9D,  X_AD => X_AD,  X_BD => X_BD,
      X_8E => X_8E,  X_9E => X_9E,  X_AE => X_AE,  X_BE => X_BE,
      X_8F => X_8F,  X_9F => X_9F,  X_AF => X_AF,  X_BF => X_BF,

      X_C0 => X_C0,  X_D0 => X_D0,  X_E0 => X_E0,  X_F0 => X_F0,
      X_C1 => X_C1,  X_D1 => X_D1,  X_E1 => X_E1,  X_F1 => X_F1,
      X_C2 => X_C2,  X_D2 => X_D2,  X_E2 => X_E2,  X_F2 => X_F2,
      X_C3 => X_C3,  X_D3 => X_D3,  X_E3 => X_E3,  X_F3 => X_F3,
      X_C4 => X_C4,  X_D4 => X_D4,  X_E4 => X_E4,  X_F4 => X_F4,
      X_C5 => X_C5,  X_D5 => X_D5,  X_E5 => X_E5,  X_F5 => X_F5,
      X_C6 => X_C6,  X_D6 => X_D6,  X_E6 => X_E6,  X_F6 => X_F6,
      X_C7 => X_C7,  X_D7 => X_D7,  X_E7 => X_E7,  X_F7 => X_F7,
      X_C8 => X_C8,  X_D8 => X_D8,  X_E8 => X_E8,  X_F8 => X_F8,
      X_C9 => X_C9,  X_D9 => X_D9,  X_E9 => X_E9,  X_F9 => X_F9,
      X_CA => X_CA,  X_DA => X_DA,  X_EA => X_EA,  X_FA => X_FA,
      X_CB => X_CB,  X_DB => X_DB,  X_EB => X_EB,  X_FB => X_FB,
      X_CC => X_CC,  X_DC => X_DC,  X_EC => X_EC,  X_FC => X_FC,
      X_CD => X_CD,  X_DD => X_DD,  X_ED => X_ED,  X_FD => X_FD,
      X_CE => X_CE,  X_DE => X_DE,  X_EE => X_EE,  X_FE => X_FE,
      X_CF => X_CF,  X_DF => X_DF,  X_EF => X_EF,  X_FF => X_FF,

      '0' => '0',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',

      '_' => '_',

      others => ' ');

   ---------------------------------------
   -- Definitions for No Upper Half Set --
   ---------------------------------------

   --  The no upper half set allows no upper half characters as letters, and
   --  thus there are no upper/lower case equivalences in this half. This set
   --  corresponds to the Ada 83 rules.

   Fold_No_Upper_Half : Translate_Table := Translate_Table'(

      'a' => 'A',
      'b' => 'B',
      'c' => 'C',
      'd' => 'D',
      'e' => 'E',
      'f' => 'F',
      'g' => 'G',
      'h' => 'H',
      'i' => 'I',
      'j' => 'J',
      'k' => 'K',
      'l' => 'L',
      'm' => 'M',
      'n' => 'N',
      'o' => 'O',
      'p' => 'P',
      'q' => 'Q',
      'r' => 'R',
      's' => 'S',
      't' => 'T',
      'u' => 'U',
      'v' => 'V',
      'w' => 'W',
      'x' => 'X',
      'y' => 'Y',
      'z' => 'Z',

      'A' => 'A',
      'B' => 'B',
      'C' => 'C',
      'D' => 'D',
      'E' => 'E',
      'F' => 'F',
      'G' => 'G',
      'H' => 'H',
      'I' => 'I',
      'J' => 'J',
      'K' => 'K',
      'L' => 'L',
      'M' => 'M',
      'N' => 'N',
      'O' => 'O',
      'P' => 'P',
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',

      '0' => '0',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',

      '_' => '_',

      others => ' ');

   ----------------------------
   -- Initialize_Char_Tables --
   ----------------------------

   procedure Initialize_Char_Tables is
   begin

      --  Set Fold_Upper table from source code indication

      if Identifier_Character_Set = '1' then
         Fold_Upper      := Fold_Latin_1;

      elsif Identifier_Character_Set = '2' then
         Fold_Upper      := Fold_Latin_2;

      elsif Identifier_Character_Set = '3' then
         Fold_Upper      := Fold_Latin_3;

      elsif Identifier_Character_Set = '4' then
         Fold_Upper      := Fold_Latin_4;

      elsif Identifier_Character_Set = 'p' then
         Fold_Upper      := Fold_IBM_PC;

      elsif Identifier_Character_Set = 'f' then
         Fold_Upper      := Fold_Full_Upper_Half;

      else -- Identifier_Character_Set = 'n'
         Fold_Upper      := Fold_No_Upper_Half;
      end if;

      --  Use Fold_Upper table to compute Fold_Lower table

      Fold_Lower := Fold_Upper;

      for I in Char loop
         if I /= Fold_Upper (I) then
            Fold_Lower (Fold_Upper (I)) := I;
            Fold_Lower (I) := I;
         end if;
      end loop;

      Fold_Lower (' ') := ' ';

      --  Build Identifier_Char table from used entries of Fold_Upper

      for I in Char loop
         Identifier_Char (I) := Fold_Upper (I) /= ' ';
      end loop;

      --  Letter_Or_Digit differs only in the entry for underline

      Letter_Or_Digit := Identifier_Char;
      Letter_Or_Digit ('_') := False;
      return;
   end Initialize_Char_Tables;

   --------------------------
   -- Is_Upper_Case_Letter --
   --------------------------

   function Is_Upper_Case_Letter (C : Char) return Boolean is
   begin
      return C /= Fold_Lower (C);
   end Is_Upper_Case_Letter;

   --------------------------
   -- Is_Lower_Case_Letter --
   --------------------------

   function Is_Lower_Case_Letter (C : Char) return Boolean is
   begin
      return C /= Fold_Upper (C);
   end Is_lower_Case_Letter;

end Csets;
