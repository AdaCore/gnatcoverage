with Pkg;

procedure Test_Full is
begin
   Pkg.Print_If (False, "some message");
   Pkg.Print_If (True, "some message");
end Test_Full;

--# pkg.adb
--  /condition/ l# ## 0
--  /put_line/  l+ ## 0
