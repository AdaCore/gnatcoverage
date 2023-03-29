with Stacks, Support; use Stacks, Support;

-- Pop only, immediate underflow. Region exempted.

procedure Test_Pop_U is
   S : Stack (Size => 2);
   V : Integer;
begin
   Pop (S, V);
   Assert (Errcount (S) = 1);
end;

--# stacks.adb
-- /xregion/      l* ## x+
-- /xregion_2/    l= ## Xs-
-- /xregion_3/    l= ## Xs-
-- /xregion_4/    l= ## Xs-
-- /xregion_5/    l= ## Xs-
-- /xregion_8/    l= ## Xs-
-- /xregion_9/    l= ## Xs-

-- /push_decl/ l- ## s-
-- /push_body/ l- ## s-
-- /pop_decl/  l+ ## 0
-- /pop_body/  l+ ## 0
-- /err_body/  l+ ## 0
