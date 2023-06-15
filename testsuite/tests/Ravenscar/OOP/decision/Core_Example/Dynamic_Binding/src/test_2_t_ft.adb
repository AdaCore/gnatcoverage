--  Test driver for checking DC in case when a dispatching operations are
--  involved. The top-level dispatching operation to check is not a condition
--  in any  decision, it is just called on its own.
--
--  The driver calls a top dispatching operation in such a way that
--  * operations of T2 are executed;
--  * the decision in its body of the primitive that is called first is
--    evaluated to True;
--  * the IF decision in the primitive called from the first primitive is
--    evaluated to False, and ELSIF decision - to True.

 with Check_Dynamic_Binding;

with Support; use Support;
procedure Test_2_T_FT is
   Result : Integer;
begin
   Check_Dynamic_Binding
     (Check_Var => 3,
      Result    => Result);

   Assert (Result = -20);
end Test_2_T_FT;

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
-- /compute_eval/      l! ## dF-
-- /compute_true/      l+ ## 0
-- /compute_false/     l- ## s-
-- /valid_if_eval/     l! ## dT-
-- /valid_if_true/     l- ## s-
-- /valid_elsif_eval/  l! ## dF-
-- /valid_elsif_ true/ l+ ## 0
-- /valid_false/       l- ## s-
