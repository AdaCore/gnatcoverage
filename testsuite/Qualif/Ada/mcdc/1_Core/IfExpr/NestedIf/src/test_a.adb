with Support; use Support;
with Sys; use Sys;

procedure Test_A is
begin
   Assert (Andthen (True, True) = True);
   Assert (Andthen (False, True) = False);
EnD;

--# sys.adb

--%opts: --trace-mode=bin
--  /eval/ l! ## d!:"B"

--%opts: --trace-mode=src
--  /eval/ l! ## dF-:"B"
