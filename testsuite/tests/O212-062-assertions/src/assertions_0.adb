pragma Assertion_Policy (Check);

with Assertions; use Assertions;

procedure Assertions_0
is
   X : Integer := 0;
   Y : Integer := X;
   A : Boolean := True;
   B : Boolean := False;
begin
   --  The last condition is not evaluated

   pragma Assert ((X = 0 or else A) or else X = 0);         -- # assert_1

   --  Call a function. Evaluate all conditions of the precondition;
   --  evaluate only the first condition of the postcondition. The first
   --  loop invariant is entirely covered while the last condition of the
   --  second loop invariant is not evaluated.

   Y := Foo (0);                                            -- # foo_0

   --  The if's decision is False, so the statement contained is not executed

   if X /= 0 then                                           -- # if_false
      pragma Assert (X /= 0 or else False);                 -- # assert_2
      null;                                                 -- # null
   end if;

   --  Call an expression function with a precondition of which the last
   --  condition is not evaluated.

   Y := Bar (0);                                            -- # bar_0

   --  Call to function with an aspect defined with the function body.

   A := Same (A, B);

end Assertions_0;
