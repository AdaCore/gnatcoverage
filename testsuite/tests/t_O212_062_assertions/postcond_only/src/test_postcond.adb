with Postcond;
with Postcond2012;

--  Test that the only coverage information that is reported for assertions
--  is that concerning the postcondition.

procedure Test_Postcond is
begin
   Postcond;
   Postcond2012;
end Test_Postcond;

--# postcond.adb
-- /post_asp/  l- ## a-
-- /post_prag/ a=>l+, c=>l! ## a=>0, c=>ac!:"I = 0"
-- /pre_prag/  l. ## 0
-- /b_ret/     l+ ## 0
-- /assert/    l. ## 0
-- /ko/        l- ## s-
-- /stmt/      l+ ## 0

--# postcond2012.adb
-- /post_a/  a=>l+, c=>l? ## a=>0, c=>a?
-- /a_body/  l+ ## 0
-- /stmt/    l+ ## 0
