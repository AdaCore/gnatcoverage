with Stacks, Support; use Stacks, Support;

-- Push, then Pop until underflow. Region exempted.

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
-- /xregion/   l* x+

-- /push_decl/ l+ 0
-- /push_body/ l+ 0
-- /pop_decl/  l+ 0
-- /pop_body/  l+ 0
-- /err_body/  l+ 0
