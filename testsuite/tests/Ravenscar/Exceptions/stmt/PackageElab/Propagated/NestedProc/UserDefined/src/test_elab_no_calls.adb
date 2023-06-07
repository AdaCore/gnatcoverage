-- Driver to test exception propagation across subprograms.
-- Do not call the functional code. No exception raised, everything
-- is uncovered.

with Support; use Support;
with Elab;
with Global; use Global;

procedure test_elab_no_calls is
begin
   Assert (Correct_Exception_Raised = False);
   Assert (Wrong_Exception_Raised = False);
end;

--# elab.adb
-- /exception_decl/      l- ## s-
-- /test/                l- ## s-
-- /explicit_violation/  l- ## s-
-- /no_exp_violation/    l- ## s-
-- /decl/                l- ## s-
-- /implicit_violation/  l- ## s-
-- /no_imp_violation/    l- ## s-
-- /wrong_handler/       l- ## s-
-- /properly_handled/    l- ## s-
-- /wrong_exception/     l- ## s-

-- 7.0.3 misses dominance markers that would
-- allow marking our bare decls accurately:

--%tags: 7.0.3
-- =/exception_decl/ ~l- ## ~s-
-- =/decl/           ~l- ## ~s-
