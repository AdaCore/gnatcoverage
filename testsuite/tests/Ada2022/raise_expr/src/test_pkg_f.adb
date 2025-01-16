with Support;

with Pkg; use Pkg;

procedure Test_Pkg_F is
begin
   Support.Assert (not Identity (A => False, Do_Raise => False));
end Test_Pkg_F;

--# pkg.adb
--
-- /stmt/      l+ ## 0
-- /expr/      l! ## oT-
-- /ret_false/ l+ ## 0
-- /ret_true/  l- ## s-
