with Pragmas;

procedure Test_Pragmas is
begin
   Pragmas;
end Test_Pragmas;

--# pragmas.adb
-- /id/       l+ ## 0
-- /assume/   l+ ## 0
-- /c_pre/    l+ ## 0
-- /c_post/   l+ ## 0
-- /c_ti/     a=>l+, c=>l! ## a=>0, a=>ac!
-- /c_inv/    l+ ## 0
-- /c_assert/ a=>l+, c=>l! ## a=>0, c=>ac!
-- /cut/      a=>l+, c=>l! ## a=>0, c=>ac!
-- /fail/     l! ## aT-
-- /catch/    l+ ## 0
