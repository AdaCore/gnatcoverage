with Ada.Assertions;
with Assertions_2;

procedure Catch_Assertions_2 is
begin
   begin
      Assertions_2;
   exception
      when Ada.Assertions.Assertion_Error => null;
   end;
end Catch_Assertions_2;
