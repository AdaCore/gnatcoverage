with Show_Whether_Local;
procedure Test_Glob is
begin
   Show_Whether_Local ("_Global");
end Test_Glob;

--# show_whether_local.adb
--  /evalA/  l! ## dT-
--  /evalD/  l! ## d!
--  /true/   l- ## s-
--  /false/  l+ ## 0
--  /ret/    l+ ## 0
--  /exc/    l- ## s-
--
-- %opts: --trace-mode=src
-- =/evalD/  l! ## dT-
