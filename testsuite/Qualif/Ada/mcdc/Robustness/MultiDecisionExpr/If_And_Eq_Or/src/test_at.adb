with Support, And_Eq_Or;
use Support;

procedure Test_AT is
begin
   Assert (And_Eq_Or (True, True, True, True) = True);
   Assert (And_Eq_Or (False, True, True, False) = False);
end;

--# and_eq_or.adb
--  /eval/  l! ## c!:"B", eF-:"C"
--  /true/  l+ ## 0
--  /false/ l+ ## 0
