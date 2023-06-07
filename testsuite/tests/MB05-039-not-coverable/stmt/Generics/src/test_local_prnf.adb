with Check_Local_PRNF;

procedure Test_Local_PRNF is
begin
   Check_Local_PRNF;
end;

--# values.adb
--  /stmt/        l+ ## 0
--  /test_x/      l+ ## 0
--  /xpos/        l+ ## 0
--  /test_factor/ l+ ## 0
--  /fpos/        l0 ## s0
--  /fneg/        l+ ## 0

--  /xneg/        l- ## s-

--  %cargs: -O1
--  =/xneg/       l0 ## s0

