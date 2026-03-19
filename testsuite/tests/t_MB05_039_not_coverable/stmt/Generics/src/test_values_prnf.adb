with Values_PRNF;

procedure Test_Values_PRNF is
begin
   Values_PRNF.Check;
end;

--# values.adb
--  /stmt/        l+ ## 0
--  /test_x/      l+ ## 0
--  /xpos/        l+ ## 0
--  /xneg/        l- ## s-
--  /test_factor/ l+ ## 0
--  /fpos/        l0 ## s0
--  /fneg/        l+ ## 0

-- %cargs: -O1
-- =/xneg/        l0 ## s0
