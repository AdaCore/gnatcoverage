with Support, And_Eq_Or;
use Support;

procedure Test_FC is
begin
   Assert (And_Eq_Or (True, False, False, False) = True);
   Assert (And_Eq_Or (True, False, True, False) = False);
end;

--# and_eq_or.adb
--  /eval/  l! ## eT-:"A", c!:"D"
--  /true/  l+ ## 0
--  /false/ l+ ## 0

