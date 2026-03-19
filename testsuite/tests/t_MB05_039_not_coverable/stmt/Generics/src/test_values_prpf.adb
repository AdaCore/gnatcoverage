with Values_PRPF;

procedure Test_Values_PRPF is
begin
   Values_PRPF.Check;
end;

--# values.adb
--  /stmt/        l+ ## 0
--  /test_x/      l+ ## 0
--  /xpos/        l+ ## 0
--  /xneg/        l- ## s-
--  /test_factor/ l+ ## 0
--  /fpos/        l+ ## 0
--  /fneg/        l0 ## s0

-- %cargs: -O1
-- =/xneg/        l0 ## s0
