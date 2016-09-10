package body TI_Objects is
   
   N_Id_Recall : Integer := 0;
   Next_Id : Integer := 0;
   
   procedure Assign_Id_To (O : in out Identifiable) is
   begin
      if O.Id > 0 then           -- # id_test
         N_Id_Recall := N_Id_Recall + 1; -- # id_recall
      else
         O.Id := Next_Id;        -- # id_assign
         Next_Id := Next_Id + 1; -- # id_assign
      end if;
   end;
   
   procedure Characterize (IV : T_Int) is
   begin
      if IV.Value > 0 then        -- # char_test_pos_int
         N_Positives := N_Positives + 1;   -- # char_pos_int
      elsif IV.Value < 0 then     -- # char_test_neg_int
         N_Negatives := N_Negatives + 1;  -- # char_neg_int
      else
         N_Zeroes := N_Zeroes + 1; -- # char_zero_int
      end if;
   end;
   
   procedure Characterize (FV : T_Float) is
   begin
      if FV.Value > 0.0 then        -- # char_test_pos_float
         N_Positives := N_Positives + 1;   -- # char_pos_float
      elsif FV.Value < 0.0 then     -- # char_test_neg_float
         N_Negatives := N_Negatives + 1;  -- # char_neg_float
      else
         N_Zeroes := N_Zeroes + 1; -- # char_zero_float
      end if;
   end;
           
end;
