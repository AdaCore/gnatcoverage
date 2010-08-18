with Support, Andthen; use Support;

--  Exercize the function with a (True, True) input and verify that the
--  statement isn't reported uncovered.

procedure Test_AndThen_TT is
begin
   Assert (Andthen (True, True));
end;

--# andthen.adb
--  /andthen/ l+ 0
