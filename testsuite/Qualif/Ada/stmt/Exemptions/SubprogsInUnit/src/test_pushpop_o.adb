with Stacks, Support; use Stacks, Support;

-- Push until overflow, then Pop. Region exempted.

procedure Test_PushPop_O is
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
end;

--# stacks.adb
-- /xregion/   l* ## x+

-- /push_decl/ l+ ## 0
-- /push_body/ l+ ## 0
-- /pop_decl/  l+ ## 0
-- /pop_body/  l+ ## 0
-- /err_body/  l+ ## 0
