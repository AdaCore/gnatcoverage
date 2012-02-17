------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Support, POSI; use Support, POSI;

procedure Test_Genpos is
begin
   Pos_T1.Count (X => 1);
   Assert (Pos_T1.N_Positive = 1);

   Pos_T2.Count (X => -1);
   Assert (Pos_T2.N_Positive = 0);
end Test_Genpos;
