with Ops; use Ops;
with Support; use Support;

procedure Test_FT is
begin
   Process (X => 1, Y => Int64'First);
   Assert (Rx = 1);
   Assert (Ry = 2**63);
end;

--# ops.adb
--  /eval/ l+ ## 0
