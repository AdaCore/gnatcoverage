with Stacks, Support; use Stacks, Support;

procedure Test_Push_Pop is
   S : Stack (Size => 2);
   V : Integer;
begin
   Push (S, 2);
   Pop (S, V);
end;

--# stacks.adb
-- /disabled/  lD ## 0
-- /push_decl/ l+ ## 0
-- /push_body/ l+ ## 0
-- /pop_decl/  l+ ## 0
-- /pop_body/  l+ ## 0
-- /err_body/  l+ ## 0
