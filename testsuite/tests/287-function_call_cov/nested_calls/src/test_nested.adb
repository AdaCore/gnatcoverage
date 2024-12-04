with Nested;

--  Check that nested calls are all instrumented and that the correct call
--  violations are reported. Calls made in a function spec as a parameter's
--  default value should never be instrumented.

procedure Test_Nested is
begin
    Nested;
end Test_Nested;

--# nested.adb
-- /stmt/   l+ ## 0
-- /fun/    l+ ## 0
-- /call/   l+ ## 0
-- /if/     l! ## dT-
-- /v_stmt/ l- ## s-
-- /v_call/ l- ## s=>s-, f=>c-
