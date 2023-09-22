with Assertions; use Assertions;
with Silent_Last_Chance;

procedure Assertions_2
is
   Y : Integer := 2;
begin
   --  Call to Foo with a parameter that does not satisfy the
   --  precondition.

   Y := Foo (-2);  -- # foo_2
end Assertions_2;
