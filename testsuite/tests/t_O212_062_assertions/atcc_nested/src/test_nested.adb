with Nested; use Nested;

--  Test the behavior of gnatcov when presented with assertions with nested
--  decisions. No violations are expected in nested decisions as gnatcov should
--  not look inside them. (FIXME: for now)

procedure Test_Nested is
   Dummy : Integer := One;
begin
   Dummy := Two;
   Dummy := Three;
end Test_Nested;

--# nested.ads
-- /success/         l+ ## 0
-- /fail_part/       a=>l+, c=>l! ## 0
-- /fail_ac/         a=>l+, c=>l! ## a=>0, c=>ac!
-- /fail_2_ac/       a=>l+, c=>l! ## a=>0, c=>ac!,ac!
--# nested.adb
-- /success/         l+ ## 0
-- /fail/            a=>l+, c=>l! ## a=>0, c=>ac!
