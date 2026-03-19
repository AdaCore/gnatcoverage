package Twisters is

   function Identity (B : Boolean) return Boolean;

   -- We really care about inlining in the middle of decisions
   -- in this series of tests, so:
   pragma Inline_Always (Identity);

end;
