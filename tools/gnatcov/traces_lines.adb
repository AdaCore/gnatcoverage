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
      case L is
         when No_Code                               =>
            return R;

         --  We don't want non instrumented code / Not_Coverable code to mask
         --  any confirmed violations, but we still want them to show up if
         --  gnatcov does not detect any violation.

         when Not_Coverable | Undetermined_Coverage =>
            if R in No_Code | Covered then
               return L;
            else
               return R;
            end if;

         when others                                =>
            case R is
               when No_Code                               =>
                  return L;

               when Undetermined_Coverage | Not_Coverable =>
                  if L = Covered then
                     return R;
                  else
                     return L;
                  end if;

               when Not_Covered                           =>
                  return Line_State'Min (L, Partially_Covered);

               when Partially_Covered | Disabled_Coverage =>
                  return R;

               when Covered                               =>
                  return Line_State'Max (L, Partially_Covered);
            end case;
      end case;
   end "*";

end Traces_Lines;
