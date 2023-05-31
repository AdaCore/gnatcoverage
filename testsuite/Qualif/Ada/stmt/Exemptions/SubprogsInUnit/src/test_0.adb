with Stacks, Support; use Stacks, Support;

-- Call nothing, no overflow, no underflow - region exempted.

procedure Test_0 is
begin
   null;
end;

--# stacks.adb
-- /xregion/    l* ## x+
-- /xregion_01/ l= ## Xs-
-- /xregion_02/ l= ## Xs-
-- /xregion_03/ l= ## Xs-
-- /xregion_04/ l= ## Xs-
-- /xregion_05/ l= ## Xs-
-- /xregion_06/ l= ## Xs-
-- /xregion_07/ l= ## Xs-
-- /xregion_08/ l= ## Xs-
-- /xregion_09/ l= ## Xs-
-- /xregion_10/ l= ## Xs-
-- /xregion_11/ l= ## Xs-

-- /push_decl/ l- ## s-
-- /push_body/ l- ## s-
-- /pop_decl/  l- ## s-
-- /pop_body/  l- ## s-
-- /err_body/  l- ## s-
