package body Div is
   procedure Divide (X, Y: Integer; Res: out Integer) is
   begin
      if Y = 0 then           -- # explicit_check
         raise My_Constraint_Error;  -- # explicit_raise
      else
         Res := X/Y;        -- # computation
      end if;
   exception
      when My_Constraint_Error =>
         N_In_MyCE_Handler := N_In_MyCE_Handler + 1;   -- # explicit_handler
      when Constraint_Error =>
         N_In_Wrong_Handler := N_In_Wrong_Handler + 1; -- # wrong_handler
      when others =>
         N_In_Wrong_Handler := N_In_Wrong_Handler + 1; -- # wrong_handler
    end Divide;
end Div;
