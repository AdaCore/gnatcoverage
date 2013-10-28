with Global_Refs.Full;

procedure Test_Global_full is
begin
   null;
end;

--# global_refs-full.ads
--  /decl/   l+ ## 0
--  /clause/ l. ## 0

--# data.adb
--  /test_index/ l+ ## 0
--  /in_range/   l+ ## 0
--  /out_range/  l+ ## 0
