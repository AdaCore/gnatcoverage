with Support, Flip; use Support, Flip;

procedure Test_Flip_T is
begin
   Assert (F (True) = False);
end;

--# flip.adb
-- /eval/    l! oT-
-- /returnTrue/  l- s-
-- /returnFalse/ l+ 0
-- /returnVal/   l+ 0
