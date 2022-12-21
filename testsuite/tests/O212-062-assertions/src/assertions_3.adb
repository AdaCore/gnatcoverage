pragma Assertion_Policy (Check);

with Ada.Assertions;

procedure Assertions_3 is
begin
   declare
      High : Integer := 3;                                  -- # high_decl
   begin
      --  Failed loop invariant

      for J in 1 .. High loop                               -- # loop_3
         pragma Loop_Invariant (High < 0);                  -- # loop_inv
      end loop;
   exception
      when Ada.Assertions.Assertion_Error => null;          -- # catch_3
   end;
end Assertions_3;
