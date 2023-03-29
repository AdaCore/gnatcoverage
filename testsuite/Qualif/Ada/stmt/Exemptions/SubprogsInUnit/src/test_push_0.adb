with Stacks, Support; use Stacks, Support;

--  Push only, no overflow, no underflow. Region exempted.

procedure Test_Push_0 is
   S : Stack (Size => 5);

begin
   Push (S, 3);
   Assert (Errcount (S) = 0);
end;

--# stacks.adb
-- /xregion/      l* ## x+
-- /xregion_3/    l= ## Xs-
-- /xregion_6/    l= ## Xs-
-- /xregion_7/    l= ## Xs-
-- /xregion_8/    l= ## Xs-
-- /xregion_9/    l= ## Xs-
-- /xregion_10/    l= ## Xs-

-- /push_decl/ l+ ## 0
-- /push_body/ l+ ## 0
-- /pop_decl/  l- ## s-
-- /pop_body/  l- ## s-
-- /err_body/  l+ ## 0
