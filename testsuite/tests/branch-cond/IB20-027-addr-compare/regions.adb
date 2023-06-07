with System; use System;

package body Regions is
   procedure Check (R : Region) is
      pragma Precondition
        (R'Address /= System.Null_Address
         and then R.Data'Address /= System.Null_Address);
   begin
      null;
   end;
end;
