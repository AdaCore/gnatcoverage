with Process;

procedure Test_Local_Outrange is
begin
   Process.Check_Outrange;
end;

--# process.adb
--  /in_decl/   l- ## s-
--  /in_stmt/   l- ## s-
--  /in_clause/ l. ## 0

--  /out_decl/   l+ ## 0
--  /out_stmt/   l+ ## 0
--  /out_clause/ l. ## 0

--# data.adb
--  /test_index/ l+ ## 0
--  /in_range/   l- ## s-
--  /out_range/  l+ ## 0
