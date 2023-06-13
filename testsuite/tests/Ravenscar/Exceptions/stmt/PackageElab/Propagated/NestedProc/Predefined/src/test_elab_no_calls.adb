-- Driver to test exception propagation across subprograms.
-- Do not call the functional code. Everything should be uncovered.

with Support; use Support;
with Elab;
with Global; use Global;

procedure test_elab_no_calls is
begin
   Assert (Correct_Exception_Raised = False);
   Assert (Wrong_Exception_Raised = False);
end;

--# elab.adb
-- /decl/                ~l- ## ~s-
-- /test/                l- ## s-
-- /explicit_violation/  l- ## s-
-- /no_exp_violation/    l- ## s-
-- /implicit_violation/  l- ## s-
-- /no_imp_violation/    l- ## s-
-- /wrong_handler/       l- ## s-
-- /properly_handled/    l- ## s-
-- /wrong_exception/     l- ## s-
