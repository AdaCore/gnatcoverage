with Run; use Run;

-- Topology: _ and then _

procedure Test_Computing_V02 is
begin
   Run_FF_F;
   Run_TF_F;
end;

-- eval-x, eval-y if evaluators are on different lines
-- opX, opY if they are on the same line

--# computing.adb|computing.c

--  /on-true/        l- ## s-
--  /on-false/       l+ ## 0

--%cov: --level=stmt
--  =/eval-X1 :d:/   l+ ## 0
--  =/eval-X1 :e:/   l+ ## 0

--  =/eval-X2 :d:/   l+ ## 0
--  =/eval-X2 :e:/   l+ ## 0

--  =/eval-all :d:/  l+ ## 0
--  =/eval-all :e:/  l+ ## 0

--%cov: --level=stmt\+decision

--  =/eval-X1 :d:/   l! ## dT-
--  =/eval-X1 :e:/   l+ ## eT-

--  =/eval-X2 :d:/   l! ## 0
--  =/eval-X2 :e:/   l+ ## 0

--  =/eval-all :d:/  l! ## dT-
--  =/eval-all :e:/  l+ ## eT-

--%cov: --level=stmt\+(uc_)?mcdc

--  =/eval-X1 :d:/   l! ## dT-
--  =/eval-X1 :e:/   l! ## eT-

--  =/eval-X2 :d:/   l! ## 0
--  =/eval-X2 :e:/   l! ## 0

--  =/eval-all :d:/  l! ## dT-
--  =/eval-all :e:/  l! ## eT-
