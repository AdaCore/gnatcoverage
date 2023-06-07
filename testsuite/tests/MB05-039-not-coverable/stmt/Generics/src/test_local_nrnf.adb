with Check_Local_NRNF;

procedure Test_Local_NRNF is
begin
   Check_Local_NRNF;
end;

--# values.adb
--  /stmt/        l+ ## 0
--  /test_x/      l+ ## 0
--  /xneg/        l+ ## 0
--  /test_factor/ l+ ## 0
--  /fpos/        l0 ## s0
--  /fneg/        l+ ## 0

--  /xpos/        l- ## s-

-- %cargs: -O1
--  =/xpos/        l0 ## s0
