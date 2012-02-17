------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

package body Intops is
   procedure Inc (X : in out Integer) is
   begin
      X := X + 1;
   end Inc;
end Intops;
