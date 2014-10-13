with Support, Or_Eq_And_Not; use Support;

procedure Test_TT is
begin
   Assert (Or_Eq_And_Not (True, False, True, False) = True);
end;

--# or_eq_and_not.adb
--  /eval/ l! ## eF-:"A", eF-:"C"

 
