with Ops, Support; use Ops, Support;

procedure Test_Ops_Internal is
   Opd : Opdata;
begin
   Call_Ops (Pop => False, Iop => True, Opd => Opd);
   Assert (Opd.Np = 0 and then Opd.Ni = 1);
end;

--# ops.adb
--  /ops/      l+ 0
--  /internal/ l+ 0
--  /private/  l- s-
