with Support, And_Eq_Or;
use Support;

procedure Test_BF is
begin
   Assert (And_Eq_Or (True, True, False, False) = False);
   Assert (And_Eq_Or (True, False, False, False) = True);
end;

--# and_eq_or.adb
--  /eval/  l! ## c!:"A", eT-:"C"
--  /true/  l+ ## 0
--  /false/ l+ ## 0

