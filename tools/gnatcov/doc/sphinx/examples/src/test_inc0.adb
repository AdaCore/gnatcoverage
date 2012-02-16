------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Support, Intops; use Support, Intops;

procedure Test_Inc0  is
   X : Integer := 0;
begin
   Inc (X);
   Assert (X = 1);
end Test_Inc0;
