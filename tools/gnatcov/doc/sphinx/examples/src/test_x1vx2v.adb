------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Support, Ranges; use Support, Ranges;

procedure Test_X1VX2V is
begin
   Assert (Between (X1 => 2, X2 => 5, V => 3)); -- X1 < V < X2
   Assert (not Between (X1 => 2, X2 => 5, V => 8)); -- X1 < X2 < V
end Test_X1VX2V;
