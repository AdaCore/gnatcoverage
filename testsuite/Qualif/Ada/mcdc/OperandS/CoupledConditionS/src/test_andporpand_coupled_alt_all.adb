with Support, AndPorPand_Coupled_Alt;
use Support, AndPorPand_Coupled_Alt;

--  Try to cover all conditions
--  the coupled condition is never covered

procedure Test_AndPorPand_Coupled_Alt_All is
begin

   --  F1 : A coupled in
   --  A and then (B or else C) and then A'
   Assert (F1 (True, True, True) = True);
   Assert (F1 (True, True, False) = True);
   Assert (F1 (True, False, True) = True);
   Assert (F1 (True, False, False) = False);
   Assert (F1 (False, True, True) = False);
   Assert (F1 (False, True, False) = False);
   Assert (F1 (False, False, True) = False);
   Assert (F1 (False, False, False) = False);

   --  F2 : B coupled in
   --  A and then (B or else B') and then C
   Assert (F2 (True, True, True) = True);
   Assert (F2 (True, True, False) = False);
   Assert (F2 (True, False, True) = False);
   Assert (F2 (True, False, False) = False);
   Assert (F2 (False, True, True) = False);
   Assert (F2 (False, True, False) = False);
   Assert (F2 (False, False, True) = False);
   Assert (F2 (False, False, False) = False);

   --  Not covering B
   --  F3 : A coupled
   --  A and then (A' or else B) and then C
   Assert (F3 (True, True, True) = True);
   Assert (F3 (True, True, False) = False);
   Assert (F3 (True, False, True) = True);
   Assert (F3 (True, False, False) = False);
   Assert (F3 (False, True, True) = False);
   Assert (F3 (False, True, False) = False);
   Assert (F3 (False, False, True) = False);
   Assert (F3 (False, False, False) = False);


end Test_AndPorPand_Coupled_Alt_All;

--# andporpand_coupled_alt.adb
-- /F1_evaluate/        l! 0
-- /coupF1_evaluate/    l! c!:"A"
-- /F1_decisionTrue/    l+ 0
-- /F1_decisionFalse/   l+ 0
-- /F2_evaluate/        l! 0
-- /coupF2_evaluate/    l! c!:"B"
-- /F2_decisionTrue/    l+ 0
-- /F2_decisionFalse/   l+ 0
-- /F3_evaluate/        l! 0
-- /coupF3_evaluate/    l! c!:"B"
-- /coupF3_evaluate/    l! c!:"A"
-- /F3_decisionTrue/    l+ 0
-- /F3_decisionFalse/   l+ 0
-- /decl/               ~l+ 0
-- /returnValue/        l+ 0
-- /returnTrue/         l+ 0
-- /returnFalse/        l+ 0

