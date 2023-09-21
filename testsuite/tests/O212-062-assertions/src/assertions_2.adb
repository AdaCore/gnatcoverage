with Ada.Assertions;
with Assertions; use Assertions;

procedure Assertions_2
is
   Y : Integer := 2;
begin
   begin
      --  Call to Foo with a parameter that does not satisfy the
      --  precondition.

      Y := Foo (-2);                                        -- # foo_2
   exception
      when Ada.Assertions.Assertion_Error => null;          -- # catch_2
   end;
end Assertions_2;
