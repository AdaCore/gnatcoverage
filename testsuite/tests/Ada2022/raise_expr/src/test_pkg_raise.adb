with Support;

with Pkg; use Pkg;

procedure Test_Pkg_Raise is
begin
   Support.Assert (Identity (A => True, Do_Raise => True));
exception
   when Custom_Error =>
      null;
end Test_Pkg_Raise;

--# pkg.adb
--
-- /stmt/      l+ ## 0
-- /expr/      l! ## o-
-- /ret_false/ l- ## s-
-- /ret_true/  l- ## s-
