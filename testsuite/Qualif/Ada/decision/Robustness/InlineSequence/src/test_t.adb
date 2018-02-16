with Ops; use Ops;
with Support; use Support;

procedure Test_T is
begin
   Process (X => Int64'First, Y => Int64'First);
   Assert (Rx = 2**63);
   Assert (Ry = 2**63);
end;

--# ops.adb
--  /eval/ l! ## d!:"X ="
