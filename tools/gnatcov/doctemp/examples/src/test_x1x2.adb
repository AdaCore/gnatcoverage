------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Support, Ranges; use Support, Ranges;

procedure Test_X1X2 is
begin
   Assert (Between (X1 => 2, X2 => 5, X => 3)); -- X1 < X < X2
   Assert (not Between (X1 => 2, X2 => 5, X => 8)); -- X1 < X2 < X
end Test_X1X2;
