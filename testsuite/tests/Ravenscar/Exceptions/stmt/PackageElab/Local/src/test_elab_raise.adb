-- Driver to test intrasubprogram package elaboration.
-- Call procedure Elab so that the package elaboration nested within
--  it raises an exception. Check that the exception is handled
--  properly, and that the statements of the block in which the
--  exception are uncovered -- i.e. not executed.

with Support; use Support;
with Elab;
with Global; use Global;

procedure test_elab_raise is
begin
   Elab (20);

   Assert (Correct_Exception_Raised = True);
   Assert (Wrong_Exception_Raised = False);
end;

--# elab.adb
-- /decl/             l+ ## 0
-- /violation/        l+ ## 0
-- /stmt/             l- ## s-
-- /wrong_handler/    l- ## s-
-- /properly_handled/ l+ ## 0
-- /wrong_exception/  l- ## s-

-- 7.0.3 misses dominance markers that would
-- allow marking our bare decls accurately:

--%tags: 7.0.3
-- =/decl/           ~l+ ## 0
