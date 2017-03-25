package body Check_G is
   procedure Test (Value : T) is
   begin
      if Low > Value or else High < Value then         -- # test-out
         Out_Of_Range_Count := Out_Of_Range_Count + 1; -- # out-range
      else
         In_Range_Count := In_Range_Count + 1;         -- # in-range
      end if;
   end Test;
end Check_G;
