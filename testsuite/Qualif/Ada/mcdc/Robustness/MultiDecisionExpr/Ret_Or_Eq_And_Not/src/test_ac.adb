with Support, Or_Eq_And_Not; use Support;

procedure Test_AC is
begin
   Assert (Or_Eq_And_Not (False, False, True, False) = False);
   Assert (Or_Eq_And_Not (True, False, False, False) = False);
end;

--# or_eq_and_not.adb
--  /eval/ l! ## c!:"B", c!:"not D"

 
