with Or_Eq_And_Not, Support; use Support;

procedure Test_AT is
begin
   Assert (Or_Eq_And_Not (False, False, True, False) = False);
   Assert (Or_Eq_And_Not (True, False, True, False) = True);
end;

--# oreqandnot.c
--   /eval/ l! ## c!:"B", eF-:"C"
