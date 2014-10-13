with Support, And_Eq_Or;
use Support;

procedure Test_TT is
begin
   Assert (And_Eq_Or (True, True, True, True) = True);
end;

--# and_eq_or.adb
--  /eval/  l! ## eF-:"A", eF-:"C", dF-:"A"
--  /true/  l+ ## 0
--  /false/ l- ## s-

