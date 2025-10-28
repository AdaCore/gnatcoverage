with Support;

with Pkg;

procedure Test_T is
begin
   Support.Assert(Pkg.To_Pos (3, False) = 3);
end Test_T;


--# pkg.adb

--  /eval/  l! ## dF-
--  /true/  l+ ## 0
--  /false/ l- ## s-
