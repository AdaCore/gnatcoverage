with Support;

with Pkg; use Pkg;

procedure Test_Pkg_TF is
begin
   Support.Assert (Identity (A => True, Do_Raise => False));
   Support.Assert (not Identity (A => False, Do_Raise => False));
end Test_Pkg_TF;

--# pkg.adb
--
-- /stmt/      l+ ## 0
-- /expr/      l! ## c!:"Do_Raise", c!:"raise Custom_Error"
-- /ret_false/ l+ ## 0
-- /ret_true/  l+ ## 0
