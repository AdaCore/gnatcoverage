------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Ops;

procedure Test_Inc is
   X : Integer := 4;
begin
   Ops.Apply (Ops.Increment, X);
   pragma Assert (X = 5);
end Test_Inc;
