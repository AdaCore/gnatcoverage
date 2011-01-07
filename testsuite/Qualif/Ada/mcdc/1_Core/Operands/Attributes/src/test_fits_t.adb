with Support, Fits; use Support, Fits;

procedure Test_Fits_T is
begin
   Assert (Fits1 ("blob", "bl") = True);
   Assert (Fits2 ("blob", "bl") = True);
end;

--# fits.adb
-- /single/      l! d!
-- /orelse/      l! dF-
-- /returnTrue/  l+ 0
-- /returnFalse/ l- s-

