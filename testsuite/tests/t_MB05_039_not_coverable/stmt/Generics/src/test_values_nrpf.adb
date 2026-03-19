with Values_NRPF;

procedure Test_Values_NRPF is
begin
   Values_NRPF.Check;
end;

--# values.adb
--  /stmt/        l+ ## 0
--  /test_x/      l+ ## 0
--  /xpos/        l- ## s-
--  /xneg/        l+ ## 0
--  /test_factor/ l+ ## 0
--  /fpos/        l+ ## 0
--  /fneg/        l0 ## s0

-- %cargs: -O1
-- =/xpos/        l0 ## s0
