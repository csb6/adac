------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ A T T R                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.3 $                              --
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

--  Attribute handling is isolated in a separate package to ease the addition
--  of implementation defined attributes. Logically this processing belongs
--  in chapter 4. See Sem_Ch4 for a description of the relation of the
--  Analyze and Resolve routines for expression components.

with Types; use Types;
package Sem_Attr is
   procedure Analyze_Attribute (N : Node_Id);
   --  Performs bottom up semantic analysis of an attribute. Note that the
   --  parser has already checked that type returning attributes appear only
   --  in appropriate contexts (i.e. in subtype marks, or as prefixes for
   --  other attributes).

   procedure Resolve_Attribute (N : Node_Id; Typ : Entity_Id);
   --  Performs type resolution of attribute. If the attribute yields
   --  a universal value, mark its type as that of the context. On
   --  the other hand, if the context itself is universal (as in
   --  T'Val (T'Pos (X)), mark the type as being the largest type of
   --  that class that can be used at run-time. This is correct since
   --  either the value gets folded (in which case it doesn't matter
   --  what type of the class we give if, since the folding uses universal
   --  arithmetic anyway) or it doesn't get folded (in which case it is
   --  going to be dealt with at runtime, and the largest type is right).

end Sem_Attr;
