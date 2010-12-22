with Support, Fits; use Support, Fits;

procedure Test_Fits_F is
begin
   Assert (Fits1 ("blob", "toolong") = False);
   Assert (Fits2 ("blob", "toolong") = False);
end;

--# fits.adb
-- /single/      l! d!
-- /orelse/      l! dT-
-- /returnTrue/  l- s-
-- /returnFalse/ l+ 0

