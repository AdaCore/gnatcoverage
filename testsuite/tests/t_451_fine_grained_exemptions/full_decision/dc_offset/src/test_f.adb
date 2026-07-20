with Pkg;

procedure Test_F is
begin
   Pkg.Print_If (False, False, False, "some message");
end Test_F;

--# pkg.adb
--  /put_line_0/  l+ ## 0
--  /put_line_x/  l+ ## 0
--  /condition_0/ l! ## dT-
--  /condition_1/ l* ## Xo-, Xc!:"C2", Xc!:"C3"
