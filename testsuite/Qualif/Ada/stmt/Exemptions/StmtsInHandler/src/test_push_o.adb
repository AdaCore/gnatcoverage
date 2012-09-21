with Stacks, Support; use Stacks, Support;

-- Push only, until overflow. Handler covered.

procedure Test_Push_O is
   S : Stack (Size => 2);

begin
   Push (S, 1);
   Push (S, 3);
   Assert (Errcount (S) = 0);

   Push (S, 4);
   Assert (Errcount (S) = 1);
end;

--# stacks.adb
-- /op_push/    l+ ## 0
-- /op_pop/     l- ## s-
-- /test_oflow/ l+ ## 0
-- /op_oflow/   l+ ## 0
-- /test_uflow/ l- ## s-
-- /op_uflow/   l- ## s-
-- /op_handler/ l# ## x0

-- /push_decl/ l+ ## 0
-- /push_body/ l+ ## 0
-- /pop_decl/  l- ## s-
-- /pop_body/  l- ## s-
-- /err_body/  l+ ## 0
