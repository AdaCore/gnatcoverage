with Support, And_Eq_Or;
use Support;

procedure Test_BT is
begin
   Assert (And_Eq_Or (True, True, True, True) = True);
   Assert (And_Eq_Or (True, False, True, False) = False);
end;

--# and_eq_or.adb
--  /eval/  l! ## c!:"A", eF-:"C"
--  /true/  l+ ## 0
--  /false/ l+ ## 0

