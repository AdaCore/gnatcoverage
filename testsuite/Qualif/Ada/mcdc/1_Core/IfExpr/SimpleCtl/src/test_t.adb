with Support; use Support;
with Sys; use Sys;

procedure Test_T is
   X : Boolean := False;
begin
   Assert (If0 (True, False, X) = False);
   Assert (If1 (True, True, X) = True);
   Assert (Ifnot0 (False, True, X) = True);
   Assert (Ifnot1 (False, False, X) = False);
end;

--# sys.adb
--  /ifa/ l! ## d!
--  /ifnota/ l! ## d!

--%opts: --trace-mode=src
--  =/ifa/ l! ## dF-
--  =/ifnota/ l! ## dF-

