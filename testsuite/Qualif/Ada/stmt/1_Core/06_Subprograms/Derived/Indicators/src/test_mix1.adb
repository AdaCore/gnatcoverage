with Support; use Support;
with Values; use Values;

-- Call once subprograms with indicators with different
-- values than those without indicators

procedure Test_Mix1 is
   IV0 : T_Int := (Value => 0);
   IVneg : T_Int := (Value => -1);
   IVpos : T_Int := (Value => 1);
begin
   Characterize1 (IV0);
   Characterize2 (IVpos);
   
   T_Int_Characterize1 (IVpos);
   T_Int_Characterize2 (IVneg);
   
   Assert (N_Positives = 2);
   Assert (N_Negatives = 1);
   Assert (N_Zeroes = 1);
end;

--# values.adb
--  /test_pos_c1/ l+ ## 0
--  /pos_c1/      l- ## s-
--  /test_neg_c1/ l+ ## 0
--  /neg_c1/      l- ## s-
--  /zero_c1/     l+ ## 0

--  /test_pos_c2/ l+ ## 0
--  /pos_c2/      l+ ## 0
--  /test_neg_c2/ l- ## s-
--  /neg_c2/      l- ## s-
--  /zero_c2/     l- ## s-

--  /test_pos_i1/ l+ ## 0
--  /pos_i1/      l+ ## 0
--  /test_neg_i1/ l- ## s-
--  /neg_i1/      l- ## s-
--  /zero_i1/     l- ## s-

--  /test_pos_i2/ l+ ## 0
--  /pos_i2/      l- ## s-
--  /test_neg_i2/ l+ ## 0
--  /neg_i2/      l+ ## 0
--  /zero_i2/     l- ## s-

