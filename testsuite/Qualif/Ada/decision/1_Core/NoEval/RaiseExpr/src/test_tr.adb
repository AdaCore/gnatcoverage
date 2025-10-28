with Silent_Last_Chance;
with Support;

with Pkg;

procedure Test_TR is
begin
   Support.Assert(Pkg.To_Pos (3, False) = 3);
   Support.Assert(Pkg.To_Pos (3, True) = 3);
exception
   when Program_Error => null;
end Test_TR;


--# pkg.adb

--  /eval/  l! ## dF-
--  /true/  l+ ## 0
--  /false/ l- ## s-
