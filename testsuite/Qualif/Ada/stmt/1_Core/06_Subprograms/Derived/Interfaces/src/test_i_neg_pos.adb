with Support; use Support;
with I_Objects; use I_Objects;

-- Check behavior with characterization of a negative Int,
-- and of a positive Float.

procedure Test_I_Neg_Pos is
   IO : T_Int;
   FO : T_Float;
begin
   IO.Value := -1;
   Characterize (IO);
   
   FO.Value := 11.0;
   Characterize (FO);
   
   Assert (N_Positives = 1);
   Assert (N_Negatives = 1);
   Assert (N_Zeroes = 0);
end;

--# i_objects.adb
--  /char_test_pos_int/ l+ ## 0
--  /char_pos_int/  l- ## s-
--  /char_test_neg_int/ l+ ## 0
--  /char_neg_int/  l+ ## 0
--  /char_zero_int/  l- ## s-

--  /char_test_pos_float/ l+ ## 0
--  /char_pos_float/  l+ ## 0
--  /char_test_neg_float/ l- ## s-
--  /char_neg_float/  l- ## s-
--  /char_zero_float/  l- ## s-

