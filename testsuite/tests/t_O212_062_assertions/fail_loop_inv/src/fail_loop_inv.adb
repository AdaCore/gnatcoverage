pragma Assertion_Policy (Check);

with Ada.Assertions;

procedure Fail_Loop_Inv is
begin
   declare
      High : Integer := 3;                                  -- # high_decl
   begin
      for J in 1 .. High loop                               -- # loop_cond
         pragma Loop_Invariant (High < 0);                  -- # loop_inv
      end loop;
   exception
      when Ada.Assertions.Assertion_Error => null;          -- # catch
   end;
end Fail_Loop_Inv;
