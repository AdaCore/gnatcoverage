with Global_Refs.Outrange;

procedure Test_Global_Outrange is
begin
   null;
end;

--# global_refs-outrange.ads
--  /decl/   l+ ## 0
--  /clause/ l. ## 0

--# data.adb
--  /test_index/ l+ ## 0
--  /in_range/   l- ## s-
--  /out_range/  l+ ## 0
