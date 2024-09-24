with Pragmas;

--  Test the correct reporting of coverage for all supported pragmas.
--  All assertion are executed, some have conditions that are not.

procedure Test_Pragmas is
begin
   Pragmas;
end Test_Pragmas;

--# pragmas.adb
-- /id/       l+ ## 0
-- /assume/   l+ ## 0
-- /c_pre/    l+ ## 0
-- /c_post/   l+ ## 0
-- /c_ti/     a=>l+, c=>l! ## a=>0, c=>ac!
-- /c_inv/    l+ ## 0
-- /c_assert/ a=>l+, c=>l! ## a=>0, c=>ac!
-- /cut_1/    a=>l+, c=>l! ## a=>0, c=>0
-- /cut_2/    a=>l+, c=>l! ## a=>0, c=>ac!
-- /cut_3/    a=>l+, c=>l! ## a=>0, c=>ac!
-- /fail/     l! ## aT-
-- /catch/    l+ ## 0
