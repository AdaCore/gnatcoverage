with Pkg;

procedure Test_Full is
begin
   Pkg.Print_If (True, False, "some message");
   Pkg.Print_If (True, True, "some message");
end Test_Full;

--# pkg.adb
--  /expr_cond/ l+ ## 0
--  /if_cond/   l+ ## 0
--  /put_line/  l+ ## 0
