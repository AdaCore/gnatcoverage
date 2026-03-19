with Support;

with Pkg;

procedure Test_FT is
begin
   Support.Assert(Pkg.To_Pos (-3, False) = 3);
   Support.Assert(Pkg.To_Pos (3, False) = 3);
end Test_FT;


--# pkg.adb

--  /eval/  l+ ## 0
--  /true/  l+ ## 0
--  /false/ l+ ## 0
