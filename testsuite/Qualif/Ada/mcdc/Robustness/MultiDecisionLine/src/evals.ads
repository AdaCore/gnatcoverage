package Evals is

   -- Provide support for multiple actual expressions on calls.

   -- Make sure all the arguments are used to prevent dead code at
   -- call sites when the calls happens to be inlined.

   procedure Eval (A, B : Boolean);
end;

