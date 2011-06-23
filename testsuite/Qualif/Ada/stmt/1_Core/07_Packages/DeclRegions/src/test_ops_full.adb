with Ops, Support; use Ops, Support;

procedure Test_Ops_Full is
   Opd : Opdata;
begin
   Call_Ops (Pop => True, Iop => True, Opd => Opd);
   Assert (Opd.Np = 1 and then Opd.Ni = 1);
end;

--# ops.adb
--  /ops/      l+ 0
--  /internal/ l+ 0
--  /private/  l+ 0
