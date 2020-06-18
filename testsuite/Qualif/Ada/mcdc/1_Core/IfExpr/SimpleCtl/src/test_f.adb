with Support; use Support;
with Sys; use Sys;

procedure Test_F is
   X : Boolean := False;
begin
   Assert (If0 (False, X, True) = True);
   Assert (If1 (False, X, False) = False);
   Assert (Ifnot0 (True, X, False) = False);
   Assert (Ifnot1 (True, X, True) = True);
end;

--# sys.adb
--  /ifa/ l! ## d!
--  /ifnota/ l! ## d!

--%opts: --trace-mode=src
--  =/ifa/ l! ## dT-
--  =/ifnota/ l! ## dT-

