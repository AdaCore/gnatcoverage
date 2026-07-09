with Pkg;

procedure Test_T is
begin
   Pkg.Print_If (True, True, "some message");
end Test_T;

--# pkg.adb
--  /condition/ l! ## c!:"C2", mDcEval:"F - -> FALSE (justification: J1)"
--  /put_line/  l+ ## 0
