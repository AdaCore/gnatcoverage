with Support;

with Pkg;

procedure Test_F is
begin
   Support.Assert(Pkg.To_Pos (-3, False) = 3);
end Test_F;


--# pkg.adb

--  /eval/  l! ## dT-
--  /true/  l- ## s-
--  /false/ l+ ## 0
