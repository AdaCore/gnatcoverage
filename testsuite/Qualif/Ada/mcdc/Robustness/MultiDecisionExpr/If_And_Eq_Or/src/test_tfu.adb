with Support, And_Eq_Or;
use Support;

procedure Test_TFU is
begin
   Assert (And_Eq_Or (True, True, False, False) = False);
   Assert (And_Eq_Or (True, True, False, True) = True);
   Assert (And_Eq_Or (True, True, True, False) = True);
end;

--# and_eq_or.adb
--  /eval/  l! ## eF-:"A"
--  /true/  l+ ## 0
--  /false/ l+ ## 0

