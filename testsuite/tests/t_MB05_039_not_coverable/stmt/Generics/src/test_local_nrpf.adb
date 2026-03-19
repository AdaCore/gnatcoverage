with Check_Local_NRPF;

procedure Test_Local_NRPF is
begin
   Check_Local_NRPF;
end;

--# values.adb
--  /stmt/        l+ ## 0
--  /test_x/      l+ ## 0
--  /xneg/        l+ ## 0
--  /test_factor/ l+ ## 0
--  /fpos/        l+ ## 0
--  /fneg/        l0 ## s0

--  /xpos/        l- ## s-

-- %cargs: -O1
--  =/xpos/       l0 ## s0
