with Ops; use Ops;
with Support; use Support;

procedure Test_F is
begin
   Process (X => 1, Y => -2);
   Assert (Rx = 1);
   Assert (Ry = 2);
end;

--# ops.adb
--  /eval/ l! ## d!:"X ="
