with Exemptions;

procedure Test_1 is
   X : Positive := 1;
begin
   Exemptions (X);
   Exemptions (X);
end Test_1;

--# exemptions.adb
-- /if_1/            l+ ## 0
-- /inc_1/           l+ ## 0
-- /if_2/            l! ## dT-
-- /inc_2/           l- ## s-
-- /exempt1/         l* ## x+
-- /exempt1_if1/     l= ## XoT-
-- /exempt1_inc1/    l= ## Xs-
-- /exempt1_inc2/    l= ## Xs-
-- /exempt2/         l* ## x+
-- /exempt2_if2/     l= ## XoT-
-- /exempt2_inc3/    l= ## Xs-
-- /exempt2_inc4/    l= ## Xs-
-- /exempt2_inc5/    l= ## Xs-
-- /exempt2_inc6/    l= ## Xs-
