with Or_Eq_And_Not, Support; use Support;

procedure Test_BF is
begin
   Assert (Or_Eq_And_Not (False, False, False, False) = True);
   Assert (Or_Eq_And_Not (False, True, False, False) = False);
end;

--# oreqandnot.c
--   /eval/ l! ## c!:"A", eT-:"C"
