with System; use System;
package body Check is

   function Valid (S : T_Str; X, Y : Integer) return Boolean is
   begin
      return (X > 10
                and then
                (Y = 0
                   or else
                   (Y = 1
                      and then S.Value'Address /= System.Null_address)));
   end;
end;
