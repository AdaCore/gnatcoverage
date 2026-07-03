------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2009-2024, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

package body Traces_Lines is

   ---------
   -- "*" --
   ---------

   function "*" (L, R : Line_State) return Line_State is
   begin
      --  In order to reduce code duplication and ensure that this function is
      --  commutative, ensure that L <= R.

      if L > R then
         return R * L;
      end if;

      --  We don't want non instrumented code/not coverable code/exempted code
      --  to mask any confirmed violations, but we still want them to show up
      --  if gnatcov does not detect any violation.

      case L is
         when Not_Covered                                    =>
            return
              (if R in Partially_Covered | Covered
               then Partially_Covered
               else L);

         when Partially_Covered                              =>
            return Partially_Covered;

         when Covered                                        =>
            return (if R in Covered | No_Code then L else R);

         when No_Code                                        =>
            return R;

         when Not_Coverable                                  =>
            return (if R = Covered then L else R);

         when Undetermined_Coverage .. Exempted_No_Violation =>
            return L;
      end case;
   end "*";

end Traces_Lines;
