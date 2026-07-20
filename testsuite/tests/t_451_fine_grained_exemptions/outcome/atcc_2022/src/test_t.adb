with Pkg;

procedure Test_T is
begin
   Pkg.Print_If (True, False, "some message");
end Test_T;

--# pkg.ads
--  /precondition/ l! ## ac!:"C2"
--# pkg.adb
--  /condition/    l! ## dT-
--  /put_line/     l- ## s-
