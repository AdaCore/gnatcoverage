with Support, Fits; use Support, Fits;

procedure Test_Fits_TF is
begin
   Assert (Fits1 ("blob", "bl") = True);
   Assert (Fits1 ("blob", "toolong") = False);
   Assert (Fits2 ("blob", "bl") = True);
   Assert (Fits2 ("blob", "toolong") = False);
end;

--# fits.adb
-- /single/      l+ 0
-- /orelse/      l+ 0
-- /returnTrue/  l+ 0
-- /returnFalse/ l+ 0

