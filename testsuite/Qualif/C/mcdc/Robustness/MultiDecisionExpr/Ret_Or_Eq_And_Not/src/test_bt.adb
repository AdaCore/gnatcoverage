with Or_Eq_And_Not, Support; use Support;

procedure Test_BT is
begin
   Assert (Or_Eq_And_Not (False, False, True, False) = False);
   Assert (Or_Eq_And_Not (False, True, True, False) = True);
end;

--# oreqandnot.c
--   /eval/ l! ## c!:"A", eF-:"C"
