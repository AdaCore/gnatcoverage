with Discriminant;

--  Test that the use of enum values, which are calls, are not instrumented as
--  calls. This is checked with an enum value used in a record discriminant
--  that needs it to be a static value. Instrumenting it as a call would
--  result in a compilation error as they would no longer be static.

procedure Test_Discriminant is
begin
   Discriminant;
end Test_Discriminant;

--# discriminant.adb
-- /a/ l+ ## 0
