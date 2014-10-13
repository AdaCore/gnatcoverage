with Support, And_Eq_Or;
use Support;

procedure Test_FF is
begin
   Assert (And_Eq_Or (True, False, False, False) = True);
end;

--# and_eq_or.adb
--  /eval/  l! ## eT-:"A", eT-:"C", dF-:"A"
--  /true/  l+ ## 0
--  /false/ l- ## s-

