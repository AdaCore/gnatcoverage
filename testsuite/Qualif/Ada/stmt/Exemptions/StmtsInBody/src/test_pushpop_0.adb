with Stacks, Support; use Stacks, Support;

-- Push and Pop, no overflow, no underflow - Both exempted.

procedure Test_PushPop_0 is
   S : Stack (Size => 2);
   V : Integer;
begin
   Push (S, 1);   
   Push (S, 2);   
   
   Pop (S, V);
   Assert (V = 2);
   
   Pop (S, V);
   Assert (V = 1);
   
   Assert (Errcount (S) = 0);
end;

--# stacks.adb
-- /op_push/     l+ ## 0
-- /op_pop/      l+ ## 0
-- /op_oflow/    l* ## x+
-- /op_oflow_v2/ l= ## Xs-
-- /op_uflow/    l* ## x+
-- /op_uflow_v2/ l= ## Xs-
-- /op_handler/  l- ## s-

-- /push_decl/   l+ ## 0
-- /push_body/   l+ ## 0
-- /pop_decl/    l+ ## 0
-- /pop_body/    l+ ## 0
-- /err_body/    l+ ## 0
