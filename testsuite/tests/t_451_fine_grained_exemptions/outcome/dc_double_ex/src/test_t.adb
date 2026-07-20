with Pkg;

procedure Test_T is
begin
   Pkg.Print_If (True, True, "some message");
end Test_T;

--# pkg.adb
--  /put_line_0/  l+ ## 0
--  /put_line_x/  l+ ## 0
--  /condition/   l* ## XoF-
