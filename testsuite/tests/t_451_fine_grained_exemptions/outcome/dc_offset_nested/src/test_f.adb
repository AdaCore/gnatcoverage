with Pkg;

procedure Test_F is
begin
   Pkg.Print_If (True, False, "some message");
end Test_F;

--# pkg.adb
--  /put_line_0/  l+ ## 0
--  /put_line_x/  l+ ## 0
--  /put_line_d/  l+ ## 0
--  /condition/   l! ## dT-
