with Support, Andthen; use Support;

--  Exercize the function with a (True, False) input and verify that the
--  statement isn't reported uncovered.

procedure Test_AndThen_TF is
begin
   Assert (Andthen (True, False) = False);
end;

--# andthen.adb
-- /andthen/ l+ 0
