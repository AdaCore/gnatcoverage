------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Support, Ranges; use Support, Ranges;

procedure Test_X1VX2 is
begin
   Assert (Between (X1 => 2, X2 => 5, V => 3)); -- X1 < V < X2
end Test_X1VX2;
