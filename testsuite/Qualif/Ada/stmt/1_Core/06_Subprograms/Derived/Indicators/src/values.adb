package body Values is
   
   procedure Characterize1 (IV : T_Int) is
   begin
      if IV.Value > 0 then                -- # test_pos_c1
         N_Positives := N_Positives + 1;  -- # pos_c1
      elsif IV.Value < 0 then             -- # test_neg_c1
         N_Negatives := N_Negatives + 1;  -- # neg_c1
      else
         N_Zeroes := N_Zeroes + 1;        -- # zero_c1
      end if;
   end;
   
   overriding
   procedure Characterize2 (IV : T_Int) is
   begin
      if IV.Value > 0 then                -- # test_pos_c2
         N_Positives := N_Positives + 1;  -- # pos_c2
      elsif IV.Value < 0 then             -- # test_neg_c2
         N_Negatives := N_Negatives + 1;  -- # neg_c2
      else
         N_Zeroes := N_Zeroes + 1;        -- # zero_c2
      end if;
   end;
   
   procedure T_Int_Characterize1 (IV : T_Int) is
   begin
      if IV.Value > 0 then                -- # test_pos_i1
         N_Positives := N_Positives + 1;  -- # pos_i1
      elsif IV.Value < 0 then             -- # test_neg_i1
         N_Negatives := N_Negatives + 1;  -- # neg_i1
      else
         N_Zeroes := N_Zeroes + 1;        -- # zero_i1
      end if;         
   end;
   
   not overriding
   procedure T_Int_Characterize2 (IV : T_Int) is
   begin
      if IV.Value > 0 then                -- # test_pos_i2
         N_Positives := N_Positives + 1;  -- # pos_i2
      elsif IV.Value < 0 then             -- # test_neg_i2
         N_Negatives := N_Negatives + 1;  -- # neg_i2
      else
         N_Zeroes := N_Zeroes + 1;        -- # zero_i2
      end if;         
   end;
end;
