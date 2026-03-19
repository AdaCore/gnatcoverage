with Enum;

--  Test that the use of enum values, which are calls, are not instrumented as
--  calls. This is checked with enum values used in case statements that need
--  them to be static values. Instrumenting them as calls would result in a
--  compilation error as they would no longer be static.

procedure Test_Enum is
begin
    Enum;
end Test_Enum;

--# enum.adb
-- /decl/     l+ ## 0
-- /if/       l! ## dT-
-- /stmt_no/  l- ## s-
-- /stmt_yes/ l+ ## 0
