with Ops, Support; use Ops, Support;

procedure Test_Ops_Private is
   Opd : Opdata;
begin
   Call_Ops (Pop => True, Iop => False, Opd => Opd);
   Assert (Opd.Np = 1 and then Opd.Ni = 0);
end;

--# ops.adb
--  /ops/      l+ 0
--  /internal/ l- s-
--  /private/  l+ 0
