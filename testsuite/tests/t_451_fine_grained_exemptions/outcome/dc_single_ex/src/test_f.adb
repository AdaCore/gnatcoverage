with Pkg;

procedure Test_F is
begin
   Pkg.Print_If (False, "some message");
end Test_F;

--# pkg.adb
--  /condition/ l! ## dT-
--  /put_line/  l- ## s-
