with Support; use Support;
with Values; use Values;

procedure Test_Allzero is
   IV : T_Int := (Value => 0);
begin
   Characterize1 (IV);
   Characterize2 (IV);
   T_Int_Characterize1 (IV);
   T_Int_Characterize2 (IV);
   
   Assert (N_Positives = 0);
   Assert (N_Negatives = 0);
   Assert (N_Zeroes = 4);
end;

--# values.adb
--  /test_pos/ l+ ## 0
--  /pos/      l- ## s-
--  /test_neg/ l+ ## 0
--  /neg/      l- ## s-
--  /zero/     l+ ## 0

