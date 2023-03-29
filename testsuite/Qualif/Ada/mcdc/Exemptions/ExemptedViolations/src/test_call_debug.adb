with Exemptions;

--  Test driver for exemptions. Call the subprogram and execute only part of
--  the exempted code, evaluating all conditions of all decisions.

procedure Test_Call_Debug is
begin
   Exemptions (True, 42);
end Test_Call_Debug;

--# exemptions.adb
-- /ex/          l* ## x+
-- /ex_if_neg/   l= ## s=>X0, dmu=>XoT-
-- /ex_error/    l= ## Xs-
-- /ex_if_debug/ l= ## XoF-
-- /ex_no_error/ l= ## X0
