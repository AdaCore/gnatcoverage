with Support; use Support;
with Sys; use Sys;

procedure Test_T is
begin
   Assert (Andthen (True, True) = True);
EnD;

--# sys.adb

--%opts: --trace-mode=bin
--  /eval/ l! ## d!:"A", d!:"B"

--%opts: --trace-mode=src
--  /eval/ l! ## dF-:"A",dF-:"B"
