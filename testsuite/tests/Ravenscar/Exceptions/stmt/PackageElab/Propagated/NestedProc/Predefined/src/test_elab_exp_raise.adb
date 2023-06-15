-- Driver to test exception propagation across subprograms.
-- Call a procedure containing a package spec, so that Constraint_Error
-- is explicitly raised and propagated to the calling subprogram.
with Support; use Support;
with Elab;
with Global; use Global;

procedure test_elab_exp_raise is
begin
   Elab (20);

   Assert (Correct_Exception_Raised = True);
   Assert (Wrong_Exception_Raised = False);
end;

--# elab.adb
-- /test/                l+ ## 0
-- /explicit_violation/  l+ ## 0
-- /no_exp_violation/    l- ## s-
-- /implicit_violation/  l+ ## 0
-- /no_imp_violation/    l- ## s-
-- /wrong_handler/       l- ## s-
-- /properly_handled/    l+ ## 0
-- /wrong_exception/     l- ## s-
