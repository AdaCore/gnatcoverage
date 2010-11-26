with Support, Flip; use Support, Flip;

procedure Test_Flip_T is
begin
   Assert (F (True) = False);
end;

--# flip.adb
-- /evaluate/    l! d!
-- /returnTrue/  l- s-
-- /returnFalse/ l+ 0
