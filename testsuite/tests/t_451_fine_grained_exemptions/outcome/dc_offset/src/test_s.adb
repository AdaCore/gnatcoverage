with Pkg;

procedure Test_S is
begin
   Pkg.Print_If (False, False, "some message");
end Test_S;

--# pkg.adb
--  /put_line_0/  l+ ## 0
--  /put_line_x/  l+ ## 0
--  /condition_0/ l! ## dT-
--  /condition_1/ l! ## d-
