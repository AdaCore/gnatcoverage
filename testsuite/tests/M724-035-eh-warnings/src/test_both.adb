with Show_Whether_Local;
procedure Test_Both is
begin
   Show_Whether_Local ("_Local");
   Show_Whether_Local ("Global");
end Test_Both;

--# show_whether_local.adb
--  /eval/  l+ ## 0
--  /true/  l+ ## 0
--  /false/ l+ ## 0
--  /ret/   l+ ## 0
--  /exc/   l- ## s-
