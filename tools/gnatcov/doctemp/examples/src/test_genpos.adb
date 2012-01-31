------------------------------------------------------------------------------
--                              GNATcoverage                                --
--                        Copyright (C) 2012, AdaCore                       --
------------------------------------------------------------------------------

with Support, Genpos; use Support;

procedure Test_Genpos is
   type T1 is new Integer;
   package Pos_T1 is new Genpos (Num_T => T1);

   type T2 is new Integer;
   package Pos_T2 is new Genpos (Num_T => T2);
begin
   Pos_T1.Count (X => 1);
   Assert (Pos_T1.N_Positive = 1);

   Pos_T2.Count (X => -1);
   Assert (Pos_T2.N_Positive = 0);
end Test_Genpos;
