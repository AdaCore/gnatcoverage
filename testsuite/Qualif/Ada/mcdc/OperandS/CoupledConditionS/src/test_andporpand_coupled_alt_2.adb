with Support, AndPorPand_Coupled_Alt;
use Support, AndPorPand_Coupled_Alt;

--  Covering all conditions but one
--  the coupled condition is never covered

procedure Test_AndPorPand_Coupled_Alt_2 is
begin
   --  Not covering B
   --  F1 : A is coupled in
   --  A and then (B or else C) and then A'
   --  A : {1, 2}
   --  C : {1, 3}
   Assert (F1 (True, False, True) = True); -- 1
   Assert (F1 (False, True, False) = False); -- 2
   Assert (F1 (True, False, False) = False); -- 3

   --  Not covering B
   --  F2 : B is coupled in
   --  A and then (B or else B') and then C
   --  A : {1, 2}
   --  C : {1, 3}
   Assert (F2 (True, True, True) = True); -- 1
   Assert (F2 (False, True, True) = False); -- 2
   Assert (F2 (True, True, False) = False); -- 3

   --  Not covering B
   --  F3 : A is coupled in
   --  A and then (A' or else B) and then C
   --  A : {2, 3}
   --  C : {1, 2} with short-circuit masking
   Assert (F3 (True, False, False) = False); -- 1
   Assert (F3 (True, True, True) = True); -- 2
   Assert (F3 (False, True, True) = False); -- 3


end Test_AndPorPand_Coupled_Alt_2;

--# andporpand_coupled_alt.adb
-- /F1_evaluate/        l! c!:"B"
-- /coupF1_evaluate/    l! c!:"A"
-- /F1_decisionTrue/    l+ 0
-- /F1_decisionFalse/   l+ 0
-- /F2_evaluate/        l! c!:"B"
-- /coupF2_evaluate/    l! c!:"B"
-- /F2_decisionTrue/    l+ 0
-- /F2_decisionFalse/   l+ 0
-- /F3_evaluate/        l! 0
-- /coupF3_evaluate/    l! c!:"A"
-- /coupF3_evaluate/    l! c!:"B"
-- /F3_decisionTrue/    l+ 0
-- /F3_decisionFalse/   l+ 0
-- /decl/               ~l+ 0
-- /returnValue/        l+ 0
-- /returnTrue/         l+ 0
-- /returnFalse/        l+ 0
-- /returnTrue/         l+ 0
-- /returnFalse/        l+ 0

