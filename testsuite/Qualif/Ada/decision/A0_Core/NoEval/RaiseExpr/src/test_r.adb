with Silent_Last_Chance;
with Support;

with Pkg;

procedure Test_R is
begin
   Support.Assert(Pkg.To_Pos (3, True) = 3);
exception
   when Program_Error => null;
end Test_R;


--# pkg.adb

--  /eval/  l! ## d-
--  /true/  l- ## s-
--  /false/ l- ## s-
