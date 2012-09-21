with Support, Andthen; use Support;

--  Exercize the function with a (False, True) input and verify that the
--  statement isn't reported uncovered despite the evaluation shortcut.

procedure Test_AndThen_FT is
begin
   Assert (AndThen (False, True) = False);
end;

--# andthen.adb
-- /andthen/ l+ ## 0
