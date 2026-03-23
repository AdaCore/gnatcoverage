with Ops, Support; use Ops, Support;

procedure Test_Ops_0 is
   Opd : Opdata;
begin
   Assert (Opd.Np = 0 and then Opd.Ni = 0);
end;

--# ops.adb
--  /ops/      l- ## s-
--  /internal/ l- ## s-
--  /private/  l- ## s-
