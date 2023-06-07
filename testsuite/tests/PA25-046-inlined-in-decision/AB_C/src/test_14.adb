with Support, P1; use Support, P1;

procedure Test_14 is
begin
   Combine (False, False, False);
   Assert (R = False);
   Combine (False, False, True);
   Assert (R = True);
end;


--# p1.adb
--  /and/ l! ## eT-
--  /or/  l! ## c!:"F1"
