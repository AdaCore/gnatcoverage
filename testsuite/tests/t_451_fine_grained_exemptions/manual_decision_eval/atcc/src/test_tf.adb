with Pkg;

procedure Test_TF is
begin
   Pkg.Print_If (True, False, "some message");
end Test_TF;

--# pkg.ads
--  /precondition/ l! ## ac!:"C2", mDcEval:"T - -> TRUE (justification: J)"
--# pkg.adb
--  /condition/    l! ## dT-
--  /put_line/     l- ## s-
