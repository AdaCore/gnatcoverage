with Pkg;

procedure Test_Full is
begin
   Pkg.Print_If (False, True, "some message");
end Test_Full;

--# pkg.ads
--  /precondition/ l? ## a?
--# pkg.adb
--  /condition/    l! ## dT-
--  /put_line/     l- ## s-
