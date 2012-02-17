------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Support, Div_With_Check; use Support;

procedure Test_Div1  is
   X : constant Integer := 4;
begin
   Assert (Div_With_Check (X, 1) = X);
end Test_Div1;
