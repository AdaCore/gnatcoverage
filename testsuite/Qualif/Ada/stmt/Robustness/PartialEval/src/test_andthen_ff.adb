with Support, Andthen; use Support;

--  Exercize the function with a (False, False) input and verify that the
--  statement isn't reported uncovered despite the evaluation shortcut.

procedure Test_AndThen_FF is
begin
   Assert (Andthen (False, False) = False);
end;

--# andthen.adb
-- /andthen/ l+ ## 0

