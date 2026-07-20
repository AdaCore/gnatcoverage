with Pkg;

procedure Test_T is
begin
   Pkg.Print_If (True, True, "some message");
end Test_T;

--# pkg.adb
--  /condition/ l! ## XoF-, c!:"C1", c!:"C2"
--  /put_line/  l+ ## 0
