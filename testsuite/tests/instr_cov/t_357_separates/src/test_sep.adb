with Pkg;

with Support;

procedure Test_Sep is
begin
   Support.Assert (Pkg.Qux);
end Test_Sep;

--# pkg.adb pkg-sep.adb
--
-- /eval/ l+ ## 0

--%opts: --trace-mode=src
--
-- /decl/ l+ ## 0
