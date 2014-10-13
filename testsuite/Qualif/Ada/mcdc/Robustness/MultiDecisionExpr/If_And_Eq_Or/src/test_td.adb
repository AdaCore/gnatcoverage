with Support, And_Eq_Or;
use Support;

procedure Test_TD is
begin
   Assert (And_Eq_Or (True, True, False, False) = False);
   Assert (And_Eq_Or (True, True, False, True) = True);
end;

--# and_eq_or.adb
--  /eval/  l! ## eF-:"A", c!:"C"
--  /true/  l+ ## 0
--  /false/ l+ ## 0

