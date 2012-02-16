------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

package Intops is
   procedure Inc (X : in out Integer);
   pragma Inline (Inc);
end Intops;
