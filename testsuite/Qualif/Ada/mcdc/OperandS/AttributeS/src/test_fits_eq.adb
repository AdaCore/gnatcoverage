with Support, Fits; use Support, Fits;

procedure Test_Fits_EQ is
begin
   Assert (Fits1 ("blob", "toolong") = False);
   Assert (Fits1 ("blob", "blob") = True);
   
   Assert (Fits2 ("blob", "toolong") = False);
   Assert (Fits2 ("blob", "blob") = True);
end;

--# fits.adb
-- /single/      l+ 0
-- /orelse/      l! c!:"Key'Length < S'Length"
-- /returnTrue/  l+ 0
-- /returnFalse/ l+ 0

