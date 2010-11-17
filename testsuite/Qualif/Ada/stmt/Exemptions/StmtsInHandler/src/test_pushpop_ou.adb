with Stacks, Support; use Stacks, Support;

-- Push until overflow, then Pop until underflow. Handler covered.

procedure Test_PushPop_OU is
   S : Stack (Size => 2);
   V : Integer;
begin
   Push (S, 1);
   Push (S, 2);
   Assert (Errcount (S) = 0);

   Push (S, 3);
   Assert (Errcount (S) = 1);

   Pop (S, V);
   Assert (V = 2);

   Pop (S, V);
   Assert (V = 1);

   Pop (S, V);
   Assert (Errcount (S) = 2);
end;

--# stacks.adb
-- /op_push/    l+ 0
-- /op_pop/     l+ 0
-- /test_oflow/ l+ 0
-- /op_oflow/   l+ 0
-- /test_uflow/ l+ 0
-- /op_uflow/   l+ 0
-- /op_handler/ l# x0

-- /push_decl/ l+ 0
-- /push_body/ l+ 0
-- /pop_decl/  l+ 0
-- /pop_body/  l+ 0
-- /err_body/  l+ 0
