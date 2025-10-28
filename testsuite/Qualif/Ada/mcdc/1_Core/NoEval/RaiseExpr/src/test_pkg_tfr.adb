with Silent_Last_Chance;
with Support;

with Pkg; use Pkg;

procedure Test_Pkg_TFR is
begin
   Support.Assert (Identity (A => True, Do_Raise => False));
   Support.Assert (not Identity (A => False, Do_Raise => False));
   Support.Assert (not Identity (A => True, Do_Raise => True));
exception
   when Custom_Error =>
      null;
end Test_Pkg_TFR;

--  This specific test shows that, despite having a T/F pair for Do_Raise, the
--  raise expression evaluation prevents the last evaluation of the decision
--  from completing and thus we can't find a pair to show the independent
--  influence of Do_Raise.

--# pkg.adb
--
-- /stmt/      l+ ## 0
-- /expr/      l! ## c!:"Do_Raise", c!:"raise Custom_Error"
-- /ret_false/ l+ ## 0
-- /ret_true/  l+ ## 0
