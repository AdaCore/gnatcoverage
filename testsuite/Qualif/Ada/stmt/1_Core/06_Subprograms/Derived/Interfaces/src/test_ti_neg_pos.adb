with Support; use Support;
with TI_Objects; use TI_Objects;

-- Check behavior with characterization of a negative Int,
-- and of a positive Float.

procedure Test_TI_Neg_Pos is
   IO : T_Int;
   FO : T_Float;
begin
   Assign_Id_To (IO);
   Assert (IO.Id >= 0);
   
   IO.Value := -1;
   Characterize (IO);
   
   Assign_Id_To (FO);
   Assert (FO.Id >= 0);
   
   FO.Value := 11.0;
   Characterize (FO);
   
   Assert (N_Positives = 1);
   Assert (N_Negatives = 1);
   Assert (N_Zeroes = 0);
end;

--# ti_objects.adb
--  /id_test/   l+ ## 0
--  /id_assign/ l+ ## 0
--  /id_recall/ l- ## s-

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

