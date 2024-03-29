with Stacks, Support; use Stacks, Support;

--  Push only, no overflow. Underflow & overflow both uncovered/exempted.

procedure Test_Push_0 is
   S : Stack (Size => 5);

begin
   Push (S, 3);
   Assert (Errcount (S) = 0);
end;

--# stacks.adb
-- /op_push/     l+ ## 0
-- /op_pop/      l- ## s-
-- /op_oflow/    l* ## x+
-- /op_oflow_v2/ l= ## Xs-
-- /op_uflow/    l* ## x+
-- /op_uflow_v1/ l= ## Xs-
-- /op_uflow_v2/ l= ## Xs-
-- /op_handler/  l- ## s-

-- /push_decl/   l+ ## 0
-- /push_body/   l+ ## 0
-- /pop_decl/    l- ## s-
-- /pop_body/    l- ## s-
-- /err_body/    l+ ## 0
