with Support, Andthen; use Support;

-- Don't call the function and verify that the body is reported uncovered.

procedure Test_AndThen_0 is
begin
   Assert (True);
end;

--# andthen.adb
-- /andthen/ l- s-
