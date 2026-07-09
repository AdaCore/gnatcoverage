with Pkg;

procedure Test_TF is
begin
   Pkg.Print_If (False, False, False, "some message");
   Pkg.Print_If (True, False, True, "some message");
   Pkg.Print_If (True, True, True, "some message");
end Test_TF;

--# pkg.adb
--  /put_line_0/  l+ ## 0
--  /put_line_x/  l+ ## 0
--  /condition_0/ l+ ## 0
--  /condition_1/ l* ## Xc!:"C3"
