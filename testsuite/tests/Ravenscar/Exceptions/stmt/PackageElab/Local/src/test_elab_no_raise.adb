-- Driver to test intrasubprogram package elaboration.
-- Call procedure Elab so that the package elaboration nested within
--  it does not raise an exception. Check that the entire block is
--  executed and that the code of the exception handlers are
--  uncovered.

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
-- /decl/             l+ ## 0
-- /violation/        l+ ## 0
-- /stmt/             l+ ## 0
-- /wrong_handler/    l- ## s-
-- /properly_handled/ l- ## s-
-- /wrong_exception/  l- ## s-

-- 7.0.3 misses dominance markers that would
-- allow marking our bare decls accurately:

--%tags: 7.0.3
-- =/decl/           ~l+ ## 0
