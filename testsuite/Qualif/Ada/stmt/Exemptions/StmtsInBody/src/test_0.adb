with Stacks, Support; use Stacks, Support;

-- Call nothing, no overflow, no underflow - Both exempted.

procedure Test_0 is
begin
   null;
end;

--# stacks.adb
-- /op_case/    l- s-
-- /op_push/    l- s-
-- /op_pop/     l- s-
-- /op_oflow/   l* x+
-- /op_uflow/   l* x+
-- /op_handler/ l- s-

-- /push_decl/ l- s-
-- /push_body/ l- s-
-- /pop_decl/  l- s-
-- /pop_body/  l- s-
-- /err_body/  l- s-
