------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Support, Ops; use Support, Ops;

procedure Test_Ops1  is
begin
   Assert (Divides (2, 4));
   Assert (not Divides (2, 5));
end Test_Ops1;
