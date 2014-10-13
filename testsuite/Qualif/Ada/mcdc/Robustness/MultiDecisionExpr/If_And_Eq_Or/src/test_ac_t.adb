with Support, And_Eq_Or;
use Support;

procedure Test_AC_T is
begin
   Assert (And_Eq_Or (True, True, True, False) = True);
   Assert (And_Eq_Or (False, True, False, False) = True);
end;

--# and_eq_or.adb
--  /eval/  l! ## c!:"B", c!:"D", dF-:"A"
--  /true/  l+ ## 0
--  /false/ l- ## s-

