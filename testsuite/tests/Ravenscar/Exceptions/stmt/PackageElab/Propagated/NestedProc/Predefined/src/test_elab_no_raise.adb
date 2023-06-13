-- Driver to test exception propagation across subprograms.
-- Call a procedure containing a package spec, so that Constraint_Error
-- is not raised at all.

with Support; use Support;
with Elab;
with Global; use Global;

procedure test_elab_no_raise is
begin
   Elab (2);

   Assert (Correct_Exception_Raised = False);
   Assert (Wrong_Exception_Raised = False);
end;

--# elab.adb
-- /test/                l+ ## 0
-- /explicit_violation/  l- ## s-
-- /no_exp_violation/    l+ ## 0
-- /implicit_violation/  l+ ## 0
-- /no_imp_violation/    l+ ## 0
-- /wrong_handler/       l- ## s-
-- /properly_handled/    l- ## s-
-- /wrong_exception/     l- ## s-
