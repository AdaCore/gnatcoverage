with Data, Support; use Data, Support;

procedure Test_Data_N is
begin
   Assert (Id (12, Pathno => 2) = 12);
end;

--# data.adb
--  /asm_1/     l- ## s-
--  /extra_1/   l- ## 0c
--  /asm_n/     l+ ## 0
--  /extra_n/   l+ ## 0c
