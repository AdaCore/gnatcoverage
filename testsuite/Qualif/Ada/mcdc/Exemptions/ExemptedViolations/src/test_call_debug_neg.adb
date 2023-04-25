with Exemptions;

-- Test driver for exemptions. Exercise only part of the exempted code,
-- evaluating only part of the decisions.

procedure Test_Call_Debug_Neg is
begin
   Exemptions (True, -42);
end Test_Call_Debug_Neg;

--# exemptions.adb
-- /ex/          l* ## x+
-- /ex_if_neg/   l= ## XoF-
-- /ex_error/    l= ## X0
-- /ex_if_debug/ l= ## Xs-
-- /ex_no_error/ l= ## Xs-
