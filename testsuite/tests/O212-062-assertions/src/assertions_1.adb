pragma Assertion_Policy (Check);

with Ada.Assertions;
with Assertions; use Assertions;

procedure Assertions_1
is
   X : Integer := 1;
begin
   begin
      --  This assertion fails, all conditions are evaluated

      pragma Assert (Baz (X) = 1 or else X /= 0);            -- # assert_3
      null;                                                  -- # null
   exception
      when Ada.Assertions.Assertion_Error => null;           -- # catch_1
   end;
end Assertions_1;
