with Global_Refs.Inrange;

procedure Test_Global_Inrange is
begin
   null;
end;

--# global_refs-inrange.ads
--  /decl/   l+ ## 0
--  /clause/ l. ## 0

--# data.adb
--  /test_index/ l+ ## 0
--  /in_range/   l+ ## 0
--  /out_range/  l- ## s-
