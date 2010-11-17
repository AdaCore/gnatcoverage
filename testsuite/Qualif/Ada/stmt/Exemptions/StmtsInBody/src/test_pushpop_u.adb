with Stacks, Support; use Stacks, Support;

-- Push, then Pop until underflow. Overflow uncovered/exempted only.

procedure Test_PushPop_U is
   S : Stack (Size => 2);
   V : Integer;
begin
   Push (S, 1);   
   Pop (S, V);
   Assert (V = 1);
   Assert (Errcount (S) = 0);
   
   Pop (S, V);
   Assert (Errcount (S) = 1);
end;

--# stacks.adb
-- /op_push/    l+ 0
-- /op_pop/     l+ 0
-- /op_oflow/   l* x+
-- /op_uflow/   l# x0
-- /op_handler/ l+ 0

-- /push_decl/ l+ 0
-- /push_body/ l+ 0
-- /pop_decl/  l+ 0
-- /pop_body/  l+ 0
-- /err_body/  l+ 0
