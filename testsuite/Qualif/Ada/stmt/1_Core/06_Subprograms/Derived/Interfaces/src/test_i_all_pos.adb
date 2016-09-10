with Support; use Support;
with I_Objects; use I_Objects;

-- Check behavior with characterization of a positive Int
-- and all cases on Float.

procedure Test_I_All_Pos is
   IO : T_Int;
   FO : T_Float;
begin
   IO.Value := 1;
   Characterize (IO);
   
   FO.Value := -11.0;
   Characterize (FO);
   
   FO.Value := 117.0;
   Characterize (FO);
   
   FO.Value := 0.0;
   Characterize (FO);
   
   Assert (N_Positives = 2);
   Assert (N_Negatives = 1);
   Assert (N_Zeroes = 1);
end;

--# i_objects.adb
--  /char_test_pos_int/ l+ ## 0
--  /char_pos_int/  l+ ## 0
--  /char_test_neg_int/ l- ## s-
--  /char_neg_int/  l- ## s-
--  /char_zero_int/  l- ## s-

--  /char_test_pos_float/ l+ ## 0
--  /char_pos_float/  l+ ## 0
--  /char_test_neg_float/ l+ ## 0
--  /char_neg_float/  l+ ## 0
--  /char_zero_float/  l+ ## 0

