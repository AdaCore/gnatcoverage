pragma Assertion_Policy (Check);

with Ada.Assertions;
with Functions; use Functions;

procedure Fail_But_Ok_Cond
is
   X : Integer := 1;
begin
   begin
      --  This assertion fails, all conditions are evaluated

      pragma Assert (Baz (X) /= 1 or else X = 0);            -- # assertion
   exception
      when Ada.Assertions.Assertion_Error => null;           -- # catch
   end;
end Fail_But_Ok_Cond;
