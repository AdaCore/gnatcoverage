package body Div is
   function Unhandled_Divide (X, Y: Integer) return Integer is
   begin
      if Y = 0 then           -- # explicit_check
         raise Constraint_Error;  -- # explicit_raise
      else
         return X/Y;        -- # computation
      end if;
   end Unhandled_Divide;

   procedure Divide (X, Y: Integer; Res: out Integer) is
   begin
      Res := Unhandled_Divide (X,Y);  -- # division
      N_Comp_Success := N_Comp_Success + 1;  -- # no_exception
   exception
      when Constraint_Error =>
         N_Excpt_Prop := N_Excpt_Prop + 1; -- # propagated_up
      when others =>
         N_Wrong_Excpt_Prop := N_Wrong_Excpt_Prop + 1; -- # wrong_excpt
   end;
end Div;
