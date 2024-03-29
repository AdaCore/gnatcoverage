with Stacks, Support; use Stacks, Support;

--  Push only, no overflow, no underflow. Handler exempted.

procedure Test_Push_0 is
   S : Stack (Size => 5);

begin
   Push (S, 3);
   Assert (Errcount (S) = 0);
end;

--# stacks.adb
-- /op_push/         l+ ## 0
-- /op_pop/          l- ## s-
-- /test_oflow/      l+ ## 0
-- /op_oflow/        l- ## s-
-- /test_uflow/      l- ## s-
-- /op_uflow/        l- ## s-
-- /op_handler/      l* ## x+
-- /op_handler_stmt/ l= ## Xs-

-- /push_decl/       l+ ## 0
-- /push_body/       l+ ## 0
-- /pop_decl/        l- ## s-
-- /pop_body/        l- ## s-
-- /err_body/        l+ ## 0
