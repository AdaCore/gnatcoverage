with Support, Flip; use Support, Flip;

procedure Test_Flip_F is
begin
   Assert (F (False) = True);
end;

--# flip.adb
-- /eval/    l! ## oF-
-- /returnTrue/  l+ ## 0
-- /returnFalse/ l- ## s-
-- /returnVal/   l+ ## 0
