with Stacks, Support; use Stacks, Support;

-- Call nothing, no overflow, no underflow - region exempted.

procedure Test_0 is
begin
   null;
end;

--# stacks.adb
-- /xregion/   l* x+

-- /push_decl/ l- s-
-- /push_body/ l- s-
-- /pop_decl/  l- s-
-- /pop_body/  l- s-
-- /err_body/  l- s-
