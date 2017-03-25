package body Sensors is

   procedure Test (Value : Float; Unit : Temp_Unit) is
   begin
      if Unit = C then -- # check-test-c
         N_Tests_C := N_Tests_C + 1; -- # check-do-c
         RC.Test (Temp_C (Value));   -- # check-do-c
      else
         N_Tests_F := N_Tests_F + 1; -- # check-do-f
         RF.Test (Temp_F (Value));   -- # check-do-f
      end if;
   end Test;
end Sensors;
