with Support;

with Pkg; use Pkg;

procedure Test_Pkg_T is
begin
   Support.Assert (Identity (A => True, Do_Raise => False));
end Test_Pkg_T;

--# pkg.adb
--
-- /stmt/      l+ ## 0
-- /expr/      l! ## oF-
-- /ret_false/ l- ## s-
-- /ret_true/  l+ ## 0
