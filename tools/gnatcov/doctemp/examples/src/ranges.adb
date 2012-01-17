------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

package body Ranges is

   function Between (X1, X2, X : Integer) return Boolean is
   begin
      if X1 < X2 then
         return X >= X1 and then X <= X2;
      else
         return X >= X2 and then X <= X1;
      end if;
   end Between;

end Ranges;
