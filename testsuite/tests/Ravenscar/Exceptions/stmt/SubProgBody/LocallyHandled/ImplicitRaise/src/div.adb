pragma Unsuppress (All_Checks);

package body Div is
   procedure Divide (X, Y: Integer; Res: out Integer) is
   begin
      Res := X/Y;      -- # division
   exception
      when Constraint_Error =>
        N_In_Handler := N_In_Handler + 1;  -- # in_handler
   end Divide;
end Div;
