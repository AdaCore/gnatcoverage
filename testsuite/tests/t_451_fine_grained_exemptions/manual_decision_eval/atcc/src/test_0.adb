with Pkg;

procedure Test_0 is
begin
   null;
end Test_0;

--# pkg.ads
--  /precondition/ l! ## ac!:"C2", mDcEval:"T - -> TRUE (justification: J)"
--# pkg.adb
--  /condition/    l- ## s-
--  /put_line/     l- ## s-
