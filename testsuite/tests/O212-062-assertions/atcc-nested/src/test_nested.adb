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
-- /fail_part/       l! ## 0
-- /fail_ac/         l! ## ac!
-- /fail_2_ac/       l! ## ac!,ac!
--# nested.adb
-- /success/         l+ ## 0
-- /fail/            l! ## ac!
