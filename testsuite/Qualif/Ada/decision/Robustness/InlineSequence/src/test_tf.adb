with Ops; use Ops;
with Support; use Support;

procedure Test_TF is
begin
   Process (X => Int64'First, Y => -8);
   Assert (Rx = 2**63);
   Assert (Ry = 8);
end;

--# ops.adb
--  /eval/ l+ ## 0
