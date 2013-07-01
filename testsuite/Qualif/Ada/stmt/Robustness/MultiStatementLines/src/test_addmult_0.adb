with Support, Addmult; use Support;

--  Call nothing. Verify that all the statements are reported uncovered.

procedure Test_Addmult_0 is
begin
   Assert (True);
end;

--# addmult.adb
--  /compute/ l- ## s-:"S .=", s-:"P .="
--  /decl/   ~l- ## ~s-
