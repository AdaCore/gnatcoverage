with Pkg;

procedure Test_Full is
begin
   Pkg.Print_If (True, False, "some message");
   Pkg.Print_If (True, True, "some message");
end Test_Full;

--# pkg.adb
--  /put_line_0/  l+ ## 0
--  /put_line_x/  l+ ## 0
--  /condition/   l# ## 0
