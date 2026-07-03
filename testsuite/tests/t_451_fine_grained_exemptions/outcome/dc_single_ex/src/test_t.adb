with Pkg;

procedure Test_T is
begin
   Pkg.Print_If (True, "some message");
end Test_T;

--# pkg.adb
--  /condition/ l* ## XoF-
--  /put_line/  l+ ## 0
