with Exemptions;

--  Test driver for exemptions. It only "withes" the functional code, so the
--  only constructs that are expected to be reported as covered are those that
--  are executed/elaborated when the spec and body of the package Exemptions
--  are elaborated, of which there are none.

procedure Test_No_Call is
begin
   null;
end;

--# exemptions.adb
-- /ex/          l* ## x+
-- /ex_if_neg/   l= ## Xs-
-- /ex_error/    l= ## Xs-
-- /ex_if_debug/ l= ## Xs-
-- /ex_no_error/ l= ## Xs-
