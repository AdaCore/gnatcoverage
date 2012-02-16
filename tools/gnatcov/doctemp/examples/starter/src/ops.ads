------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

package Ops is
   type Op_Kind is (Increment, Decrement);
   procedure Apply (Op : Op_Kind; X : in out Integer);
end Ops;
