with Support; use Support;
with TI_Objects; use TI_Objects;

-- Check behavior with characterization of all cases on Int,
-- and all cases on Float.

procedure Test_TI_All_All is
   IO : T_Int;
   FO : T_Float;
begin
   Assign_Id_To (IO);
   Assert (IO.Id >= 0);
   
   IO.Value := -1;
   Characterize (IO);
   
   IO.Value := 1;
   Characterize (IO);
   
   IO.Value := 0;
   Characterize (IO);
   
   Assign_Id_To (FO);
   Assert (FO.Id >= 0);
   
   FO.Value := -11.0;
   Characterize (FO);
   
   FO.Value := 7.0;
   Characterize (FO);
   
   FO.Value := 0.0;
   Characterize (FO);
   
   Assert (N_Positives = 2);
   Assert (N_Negatives = 2);
   Assert (N_Zeroes = 2);
end;

--# ti_objects.adb
--  /id_test/   l+ ## 0
--  /id_assign/ l+ ## 0
--  /id_recall/ l- ## s-

--  /char_test_pos_int/ l+ ## 0
--  /char_pos_int/  l+ ## 0
--  /char_test_neg_int/ l+ ## 0
--  /char_neg_int/  l+ ## 0
--  /char_zero_int/  l+ ## 0

--  /char_test_pos_float/ l+ ## 0
--  /char_pos_float/  l+ ## 0
--  /char_test_neg_float/ l+ ## 0
--  /char_neg_float/  l+ ## 0
--  /char_zero_float/  l+ ## 0

