--  Test driver for checking DC in case when a dispatching operations are
--  involved. The top-level dispatching operation to check is not a condition
--  in any  decision, it is just called on its own.
--
--  The driver calls a top dispatching operation three times in such a way that
--  * operations of T2 are executed;
--  * the decision in its body of the primitive that is called first is
--    evaluated to False, to True and raises and exception;
--  * the IF decision in the primitive called from the first primitive is
--    evaluated to False and to True, and ELSIF decision - to True.

 with Check_Dynamic_Binding;

with Support; use Support;
procedure Test_2_FTX is
   Result : Integer;
begin
   Check_Dynamic_Binding
     (Check_Var => 2,
      Result    => Result);

   Assert (Result = 999);

   Check_Dynamic_Binding
     (Check_Var => 3,
      Result    => Result);

   Assert (Result = -20);

   Result := 13;

   Check_Dynamic_Binding
     (Check_Var => 4,
      Result    => Result);

   Assert (False);
exception
   when Constraint_Error =>
      Assert (Result = 13);
end Test_2_FTX;

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
-- /compute_eval/      l+ ## 0
-- /compute_true/      l+ ## 0
-- /compute_false/     l+ ## 0
-- /valid_if_eval/     l+ ## 0
-- /valid_if_true/     l+ ## 0
-- /valid_elsif_eval/  l+ ## 0
-- /valid_elsif_ true/ l+ ## 0
-- /valid_false/       l+ ## 0
