with Support, Fits; use Support, Fits;

procedure Test_Fits_LT is
begin
   Assert (Fits1 ("blob", "toolong") = False);
   Assert (Fits1 ("blob", "bl") = True);

   Assert (Fits2 ("blob", "toolong") = False);
   Assert (Fits2 ("blob", "bl") = True);
end;

--# fits.adb
-- /single/      l+ 0
-- /orelse/      l! m!:"Key.Length = S.Length"
-- /returnTrue/  l+ 0
-- /returnFalse/ l+ 0

