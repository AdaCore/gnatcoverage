with Exemptions;

-- Test driver for exemptions. Exercise only part of the exempted code,
-- evaluating all decisions but only part of the conditions.

procedure Test_Call_No_Debug is
begin
   Exemptions (False, 42);
end Test_Call_No_Debug;

--# exemptions.adb
-- /ex/          l* ## x+
-- /ex_if_neg/   l= ## XoT-
-- /ex_error/    l= ## Xs-
-- /ex_if_debug/ l= ## s=>X0, dmu=>XoT-
-- /ex_no_error/ l= ## Xs-
