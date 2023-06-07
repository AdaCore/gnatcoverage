--  Test driver for checking DC in case when a dispatching operations are
--  involved. The top-level dispatching operation to check is not a condition
--  in any  decision, it is just called on its own.
--
--  The driver calls a top dispatching operation in such a way that
--  * operations of T2 are executed;
--  * evaluation of the decision in its body of the primitive raises an
--    exception;
--  * IF and ELSIF decisions in the primitive called from the first primitive
--    are both evaluated to False

 with Check_Dynamic_Binding;

with Support; use Support;
procedure Test_2_X is
   Result : Integer := 1;
begin
   Check_Dynamic_Binding
     (Check_Var => 4,
      Result    => Result);

   Assert (False);
exception
   when Constraint_Error =>
      Assert (Result = 1);
end Test_2_X;

--# parent-child1.adb
-- /compute_eval/      l- ## s-
-- /compute_true/      l- ## s-
-- /compute_false/     l- ## s-
-- /valid_if_eval/     l- ## s-
-- /valid_if_true/     l- ## s-
-- /valid_elsif_eval/  l- ## s-
-- /valid_elsif_ true/ l- ## s-
-- /valid_false/       l- ## s-

--# parent-child2.adb
-- /compute_eval/      l! ## d-
-- /compute_true/      l- ## s-
-- /compute_false/     l- ## s-
-- /valid_if_eval/     l! ## dT-
-- /valid_if_true/     l- ## s-
-- /valid_elsif_eval/  l! ## dT-
-- /valid_elsif_ true/ l- ## s-
-- /valid_false/       l+ ## 0
