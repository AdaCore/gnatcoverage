with Support, Levels.NOF; use Support, Levels, Levels.NOF;

--P Don't call anything, verify that everything is reported uncovered.

procedure Test_Levels_NOF_0 is
begin
   null;
end;

--# levels-nof.adb
-- /(decl|loop|inc|ret)/ l- s-

