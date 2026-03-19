with Check_Local_PRPF;

procedure Test_Local_PRPF is
begin
   Check_Local_PRPF;
end;

--# values.adb
--  /stmt/        l+ ## 0
--  /test_x/      l+ ## 0
--  /xpos/        l+ ## 0
--  /test_factor/ l+ ## 0
--  /fpos/        l+ ## 0
--  /fneg/        l0 ## s0

--  /xneg/        l- ## s-

--  %cargs: -O1
--  =/xneg/       l0 ## s0
