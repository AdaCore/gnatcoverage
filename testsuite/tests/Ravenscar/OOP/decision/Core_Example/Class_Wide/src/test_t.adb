--  Test driver for checking DC in case when a class-wide operation is
--  involved. The class-wide operation to check is not a condition in any
--  decision, it is just called on its own.
--
--  The driver calls a class-wide operation in such a way that a top-level
--  decision in its body is evaluated to True. DC for the called dispatching
--  operations (here we have dynamic dispatching) is also checked.

with Check_Class_Wide;

with Support; use Support;
procedure Test_T is
   Result : Integer;
begin
   Check_Class_Wide
     (Check_Var => 1,
      Result    => Result);

   Assert (Result = 1);
end Test_T;

--# parent.adb
-- /compute_c_eval/  l! ## dF-
-- /compute_c_true/  l+ ## 0
-- /compute_c_false/ l- ## s-
-- /valid_c/         l- ## s-

--# parent-child1.adb
-- /compute_eval/      l! ## dF-
-- /compute_true/      l+ ## 0
-- /compute_false/     l- ## s-
-- /valid_if_eval/     l! ## dF-
-- /valid_if_true/     l+ ## 0
-- /valid_elsif_eval/  l- ## s-
-- /valid_elsif_ true/ l- ## s-
-- /valid_false/       l- ## s-

--# parent-child2.adb
-- /compute_eval/      l- ## s-
-- /compute_true/      l- ## s-
-- /compute_false/     l- ## s-
-- /valid_if_eval/     l- ## s-
-- /valid_if_true/     l- ## s-
-- /valid_elsif_eval/  l- ## s-
-- /valid_elsif_ true/ l- ## s-
-- /valid_false/       l- ## s-
