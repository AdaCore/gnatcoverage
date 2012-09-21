with Stacks, Support; use Stacks, Support;

-- Push only, until overflow. Region exempted.

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
-- /xregion/   l* ## x+

-- /push_decl/ l+ ## 0
-- /push_body/ l+ ## 0
-- /pop_decl/  l- ## s-
-- /pop_body/  l- ## s-
-- /err_body/  l+ ## 0
