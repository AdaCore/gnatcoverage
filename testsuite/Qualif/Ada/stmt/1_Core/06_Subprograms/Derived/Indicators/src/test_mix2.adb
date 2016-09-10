with Support; use Support;
with Values; use Values;

-- Call subprograms with indicators with different set of
-- values than those without indicators.

procedure Test_Mix2 is
   IV0 : T_Int := (Value => 0);
   IVneg : T_Int := (Value => -1);
   IVpos : T_Int := (Value => 1);
begin
   Characterize1 (IV0);
   Characterize1 (IVpos);
   
   Characterize2 (IV0);
   Characterize2 (IVneg);
   
   T_Int_Characterize1 (IVpos);
   T_Int_Characterize1 (IVneg);
   
   T_Int_Characterize2 (IV0);
   T_Int_Characterize2 (IVpos);
   T_Int_Characterize2 (IVneg);
   
   Assert (N_Positives = 3);
   Assert (N_Negatives = 3);
   Assert (N_Zeroes = 3);
end;

--# values.adb
--  /test_pos_c1/ l+ ## 0
--  /pos_c1/      l+ ## 0
--  /test_neg_c1/ l+ ## 0
--  /neg_c1/      l- ## s-
--  /zero_c1/     l+ ## 0

--  /test_pos_c2/ l+ ## 0
--  /pos_c2/      l- ## s-
--  /test_neg_c2/ l+ ## 0
--  /neg_c2/      l+ ## 0
--  /zero_c2/     l+ ## 0

--  /test_pos_i1/ l+ ## 0
--  /pos_i1/      l+ ## 0
--  /test_neg_i1/ l+ ## 0
--  /neg_i1/      l+ ## 0
--  /zero_i1/     l- ## s-

--  /test_pos_i2/ l+ ## 0
--  /pos_i2/      l+ ## 0
--  /test_neg_i2/ l+ ## 0
--  /neg_i2/      l+ ## 0
--  /zero_i2/     l+ ## 0

