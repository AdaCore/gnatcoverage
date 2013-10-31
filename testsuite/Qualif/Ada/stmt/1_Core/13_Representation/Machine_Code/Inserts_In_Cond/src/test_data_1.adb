with Data, Support; use Data, Support;

procedure Test_Data_1 is
begin
   Assert (Id (5, Pathno => 1) = 5);
end;

--# data.adb
--  /asm_1/     l+ ## 0
--  /extra_1/   l+ ## 0c
--  /asm_n/     l- ## s-
--  /extra_n/   l- ## 0c
