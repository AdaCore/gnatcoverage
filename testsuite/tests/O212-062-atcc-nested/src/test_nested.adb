with Nested; use Nested;

procedure Test_Nested is
   Dummy : Integer := One;
begin
   Dummy := Two;
   Dummy := Three;
end Test_Nested;

--# nested.ads
-- /success/         l+ ## 0
-- /fail/            l! ## ac!
--# nested.adb
-- /success/         l+ ## 0
-- /fail/            l! ## ac!
