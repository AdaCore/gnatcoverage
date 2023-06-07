-- Driver to test intrasubprogram package elaboration.
-- Do not call the procedure containing the package elaboration.
-- All of the code should be uncovered.

with Support; use Support;
with Elab;
with Global; use Global;

procedure test_elab_no_calls is
begin
   Assert (Correct_Exception_Raised = False);
   Assert (Wrong_Exception_Raised = False);
end;

--# elab.adb
-- /decl/             l- ## s-
-- /violation/        l- ## s-
-- /stmt/             l- ## s-
-- /wrong_handler/    l- ## s-
-- /properly_handled/ l- ## s-
-- /wrong_exception/  l- ## s-

-- 7.0.3 misses dominance markers that would
-- allow marking our bare decls accurately:

--%tags: 7.0.3
-- =/decl/           ~l- ## ~s-
