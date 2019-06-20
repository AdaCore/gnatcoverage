with Ops; use Ops;
with Support; use Support;

procedure Test_T is
begin
   Process (X => Int64'First, Y => Int64'First);
   Assert (Rx = 2**63);
   Assert (Ry = 2**63);
end;

--  We're testing a simple decision for which binary
--  traces provide no means to infer which outcome was
--  taken but which instrumentation can sort out.

--# ops.adb
--  /eval/ l! ## d!:"X ="

-- %opts: --trace-mode=src
-- =/eval/ l! ## dF-:"X ="
