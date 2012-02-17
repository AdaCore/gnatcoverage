------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

package body Ranges is

   function Between (X1, X2, V : Integer) return Boolean is
   begin
      if X1 < X2 then
         return V >= X1 and then V <= X2;
      else
         return V >= X2 and then V <= X1;
      end if;
   end Between;

end Ranges;
