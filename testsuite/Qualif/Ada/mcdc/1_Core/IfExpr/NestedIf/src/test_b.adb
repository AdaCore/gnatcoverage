with Support; use Support;
with Sys; use Sys;

procedure Test_B is
begin
   Assert (Andthen (True, True) = True);
   Assert (Andthen (True, False) = False);
EnD;

--# sys.adb

--%opts: --trace-mode=bin
--  /eval/ l! ## d!:"A"

--%opts: --trace-mode=src
--  /eval/ l! ## dF-:"A"
