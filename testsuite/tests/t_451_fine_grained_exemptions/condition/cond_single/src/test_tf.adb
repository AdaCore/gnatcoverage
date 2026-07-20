with Pkg;

procedure Test_TF is
begin
   Pkg.Print_If (True, False, "some message");
   Pkg.Print_If (True, True, "some message");
end Test_TF;

--# pkg.adb
--  /condition/ l* ## Xc!:"C1"
--  /put_line/  l+ ## 0
