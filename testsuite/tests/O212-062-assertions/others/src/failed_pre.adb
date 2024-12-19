with Functions; use Functions;

procedure Failed_Pre
is
   Y : Integer := 2;
begin
   --  Call to Foo with a parameter that does not satisfy the
   --  precondition.

   Y := Foo (-2);  -- # foo_call
end Failed_Pre;
