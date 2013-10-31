with Process;

procedure Test_0 is
begin
   null;
end;

--# process.adb
--  /in_decl/   l- ## s-
--  /in_stmt/   l- ## s-
--  /in_clause/ l. ## 0

--  /out_decl/   l- ## s-
--  /out_stmt/   l- ## s-
--  /out_clause/ l. ## 0

--# data.adb
--  /test_index/ l- ## s-
--  /in_range/   l- ## s-
--  /out_range/  l- ## s-
