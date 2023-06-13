-- Driver to test exception propagation across subprograms.
-- Call a procedure containing a package spec, so that Constraint_Error
-- is implicitly raised.
-- Note: in this case, the exception is actually not propagated to
-- another subprogram, but handled by the same subprogram.

with Support; use Support;
with Elab;
with Global; use Global;

procedure test_elab_imp_raise is
begin
   Elab (1);

   Assert (Correct_Exception_Raised = True);
   Assert (Wrong_Exception_Raised = False);
end;

--# subp.adb
-- /test/                l+ ## 0
-- /explicit_violation/  l- ## s-
--# elab.adb
-- /no_exp_violation/    l+ ## 0
-- /implicit_violation/  l+ ## 0
-- /no_imp_violation/    l- ## s-
-- /wrong_handler/       l- ## s-
-- /properly_handled/    l+ ## 0
-- /wrong_exception/     l- ## s-
