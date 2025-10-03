with Silent_Last_Chance;
with Support;

with Pkg;

procedure Test_FR is
begin
   Support.Assert(Pkg.To_Pos (-3, False) = 3);
   Support.Assert(Pkg.To_Pos (-3, True) = 3);
exception
   when Program_Error => null;
end Test_FR;


--# pkg.adb

--  /eval/  l! ## dT-
--  /true/  l- ## s-
--  /false/ l+ ## 0
