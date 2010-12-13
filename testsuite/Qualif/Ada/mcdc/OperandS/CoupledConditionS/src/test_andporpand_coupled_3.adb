with Support, AndPorPand_Coupled;
use Support, AndPorPand_Coupled;

--  Covering all conditions but one
--  the coupled condition is never covered

procedure Test_AndPorPand_Coupled_3 is
begin
   --  Not covering C
   --  F1 : A == D  in
   --  A and then (B or else C) and then D
   --  B : {1, 2}
   --  A : {2, 3}
   Assert (F1 (True, False, False) = False); -- 1
   Assert (F1 (True, True, False) = True); -- 2
   Assert (F1 (False, True, False) = False); -- 3

   --  Not covering C
   --  F2 : B == D  in
   --  A and then (B or else D) and then C
   --  A : {1, 2}
   --  B : {1, 3}
   Assert (F2 (True, True, True) = True); -- 1
   Assert (F2 (False, True, True) = False); -- 2
   Assert (F2 (True, False, True) = False); -- 3

   --  Not covering B and C
   --  F3 : A == D in
   --  A and then (D or else B) and then C
   --  A : {1, 2}
   Assert (F3 (True, False, True) = True); -- 1
   Assert (F3 (False, False, True) = False); -- 2
   Assert (F3 (False, True, True) = False); -- 3


end Test_AndPorPand_Coupled_3;

--# andporpand_coupled.adb
-- /F1_evaluate/        l! c!:"C"
-- /F1_evaluate/        l! c!:"D"
-- /F1_decisionTrue/    l+ 0
-- /F1_decisionFalse/   l+ 0
-- /F2_evaluate/        l! c!:"C"
-- /F2_evaluate/        l! c!:"D"
-- /F2_decisionTrue/    l+ 0
-- /F2_decisionFalse/   l+ 0
-- /F3_evaluate/        l! c!:"B"
-- /F3_evaluate/        l! c!:"C"
-- /F3_evaluate/        l! c!:"D"
-- /F3_decisionTrue/    l+ 0
-- /F3_decisionFalse/   l+ 0
-- /decl/               ~l+ 0
-- /returnValue/        l+ 0
-- /returnTrue/         l+ 0
-- /returnFalse/        l+ 0

