with Support, Flip; use Support, Flip;

procedure Test_Flip_TF is
begin
   Assert (F (False) = True);
   Assert (F (True) = False);
end;

--# flip.adb
-- /eval/    l+ 0
-- /returnTrue/  l+ 0
-- /returnFalse/ l+ 0
-- /returnVal/   l+ 0
