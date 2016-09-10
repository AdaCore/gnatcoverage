with Support; use Support;
with Values; use Values;

procedure Test_Allpos is
   IV : T_Int := (Value => 5);
begin
   Characterize1 (IV);
   Characterize2 (IV);
   T_Int_Characterize1 (IV);
   T_Int_Characterize2 (IV);
   
   Assert (N_Positives = 4);
   Assert (N_Negatives = 0);
   Assert (N_Zeroes = 0);
end;

--# values.adb
--  /test_pos/ l+ ## 0
--  /pos/      l+ ## 0
--  /test_neg/ l- ## s-
--  /neg/      l- ## s-
--  /zero/     l- ## s-

