with Support, And_Eq_Or;
use Support;

procedure Test_BD_F is
begin
   Assert (And_Eq_Or (True, True, False, False) = False);
   Assert (And_Eq_Or (True, False, False, True) = False);
end;

--# and_eq_or.adb
--  /eval/  l! ## c!:"A", c!:"C", dT-:"A"
--  /true/  l- ## s-
--  /false/ l+ ## 0

