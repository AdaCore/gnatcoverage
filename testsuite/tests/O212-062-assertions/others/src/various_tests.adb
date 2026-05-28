pragma Assertion_Policy (Check);

with Functions; use Functions;
with Run_Assert;

procedure Various_Tests
is
   X : Integer := 0;
   Y : Integer := X;
   A : Boolean := True;
   B : Boolean := False;
begin
   --  Only the first condition is evaluated

   pragma Assert ((X = 0 or else A) or else X = 8, "msg");  -- # assert

   --  Call a function. Evaluate all conditions of the precondition;
   --  evaluate only the first condition of the postcondition. The first
   --  loop invariant is entirely covered while the last condition of the
   --  second loop invariant is not evaluated.

   Y := Foo (0);                                            -- # foo

   --  Call an expression function with a precondition of which the last
   --  condition is not evaluated.

   Y := Bar (0);                                            -- # bar

   --  Call to function with an aspect defined with the function body.

   A := Same (A, B);

end Various_Tests;
