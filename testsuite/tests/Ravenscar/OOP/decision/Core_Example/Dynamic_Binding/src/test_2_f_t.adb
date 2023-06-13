--  Test driver for checking DC in case when a dispatching operations are
--  involved. The top-level dispatching operation to check is not a condition
--  in any  decision, it is just called on its own.
--
--  The driver calls a top dispatching operation in such a way that
--  * operations of T2 are executed
--  * the decision in its body of the primitive that is called first is
--    evaluated to False,
--  * the IF decision in the primitive called from the first primitive is
--    evaluated. to True.

 with Check_Dynamic_Binding;

with Support; use Support;
procedure Test_2_F_T is
   Result : Integer;
begin
   Check_Dynamic_Binding
     (Check_Var => 2,
      Result    => Result);

   Assert (Result = 999);
end Test_2_F_T;

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
-- /compute_eval/      l! ## dT-
-- /compute_true/      l- ## s-
-- /compute_false/     l+ ## 0
-- /valid_if_eval/     l! ## dF-
-- /valid_if_true/     l+ ## 0
-- /valid_elsif_eval/  l- ## s-
-- /valid_elsif_ true/ l- ## s-
-- /valid_false/       l- ## s-
