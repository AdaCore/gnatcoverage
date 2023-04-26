with Stacks, Support; use Stacks, Support;

-- Call nothing, no overflow, no underflow - region exempted.

procedure Test_0 is
begin
   null;
end;

--# stacks.adb
-- /xregion/      l* ## x+
-- /xregion_1/    l= ## Xs-
-- /xregion_2/    l= ## Xs-
-- /xregion_3/    l= ## Xs-
-- /xregion_4/    l= ## Xs-
-- /xregion_5/    l= ## Xs-
-- /xregion_6/    l= ## Xs-
-- /xregion_7/    l= ## Xs-
-- /xregion_8/    l= ## Xs-
-- /xregion_9/    l= ## Xs-
-- /xregion_10/   l= ## Xs-

-- /push_decl/ l- ## s-
-- /push_body/ l- ## s-
-- /pop_decl/  l- ## s-
-- /pop_body/  l- ## s-
-- /err_body/  l- ## s-
