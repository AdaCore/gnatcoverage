with Support, And_Eq_Or;
use Support;

procedure Test_FUFU_T is
begin
   Assert -- and 1, or 2
     (And_Eq_Or (True, True, True, True) = True);
   
   Assert -- and 2, or 1
     (And_Eq_Or (False, True, False, False) = True);
   
   Assert -- and 3, or 1
     (And_Eq_Or (True, False, False, False) = True);
   
   Assert -- and 1, or 3
     (And_Eq_Or (True, True, False, True) = True);
end;

--# and_eq_or.adb
--  /eval/  l! ## dF-:"A"
--  /true/  l+ ## 0
--  /false/ l- ## s-
