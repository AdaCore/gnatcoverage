with Values_NRNF;

procedure Test_Values_NRNF is
begin
   Values_NRNF.Check;
end;

--# values.adb
--  /stmt/        l+ ## 0
--  /test_x/      l+ ## 0
--  /xpos/        l- ## s-
--  /xneg/        l+ ## 0
--  /test_factor/ l+ ## 0
--  /fpos/        l0 ## s0
--  /fneg/        l+ ## 0

-- %cargs: -O1
-- =/xpos/        l0 ## s0
