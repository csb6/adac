------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                            S Y S T E M . X P                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $                              --
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

pragma Ada_9X;
package body System.Xp is

   --------------------------
   -- Exponentiate_Integer --
   --------------------------

   --  Note that negative exponents get a constraint error because the
   --  subtype of the Right argument (the exponent) is Natural.

   function Exponentiate_Integer
     (Left : Type_Of_Base; Right : Natural) return Type_Of_Base
   is
      Result : Type_Of_Base := 1;
      Factor : Type_Of_Base := Left;
      Exp    : Natural := Right;

   begin
      --  We use the standard logarithmic approach, Exp gets shifted right
      --  testing successive low order bits and Factor is the value of the
      --  base raised to the next power of 2.

      --  Note: it is not worth special casing the cases of base values -1,0,+1
      --  since the expander does this when the base is a literal, and other
      --  cases will be extremely rare.

      while Exp /= 0 loop
         if Exp rem 2 /= 0 then
            Result := Result * Factor;
         end if;

         Factor := Factor * Factor;
         Exp := Exp / 2;
      end loop;

      return Result;
   end Exponentiate_Integer;

   ------------------------
   -- Exponentiate_Float --
   ------------------------

   function Exponentiate_Float
     (Left : Type_Of_Base; Right : Integer) return Type_Of_Base
   is
      Result : Type_Of_Base := 1.0;
      Factor : Type_Of_Base := Left;
      Exp    : Natural := Right;

   begin
      --  We use the standard logarithmic approach, Exp gets shifted right
      --  testing successive low order bits and Factor is the value of the
      --  base raised to the next power of 2. For positive exponents we
      --  multiply the result by this factor, for negative exponents, we
      --  divide by this factor.

      if Exp >= 0 then

         --  For a positive exponent, if we get a constraint error during
         --  this loop, it is an overflow, and the constraint error will
         --  simply be passed on to the caller.

         while Exp /= 0 loop
            if Exp rem 2 /= 0 then
               Result := Result * Factor;
            end if;

            Factor := Factor * Factor;
            Exp := Exp / 2;
         end loop;

         return Result;

      else -- Exp < 0 then

         --  For the negative exponent case, a constraint error during this
         --  calculation happens if Factor gets too large, and the proper
         --  response is to return 0.0, since what we essenmtially have is
         --  1.0 / infinity, and the closest model number will be zero.

         begin

            while Exp /= 0 loop
               if Exp rem 2 /= 0 then
                  Result := Result * Factor;
               end if;

               Factor := Factor * Factor;
               Exp := Exp / 2;
            end loop;

            return 1.0 / Result;

         exception

            when Constraint_Error =>
               return 0.0;
         end;
      end if;
   end Exponentiate_Float;

end System.Xp;
