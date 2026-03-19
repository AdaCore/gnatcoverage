with Support; use Support;
with Sys; use Sys;

procedure Test_F is
begin
   Assert (Andthen (True, False) = False);
   Assert (Andthen (False, True) = False);
EnD;

--# sys.adb

--%opts: --trace-mode=bin
--  /eval/ l! ## d!:"B"

--%opts: --trace-mode=src
--  /eval/ l! ## dT-:"B"
