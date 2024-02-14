with Support; use Support;
with Sys; use Sys;

procedure Test_TF is
   X : Boolean := False;
begin
   Assert (If0 (False, X, True) = True);
   Assert (If0 (True, True, X) = True);
   Assert (If1 (False, X, True) = True);
   Assert (If1 (True, True, X) = True);
   Assert (Ifnot0 (True, X, False) = False);
   Assert (Ifnot0 (False, False, X) = False);
   Assert (Ifnot1 (True, X, False) = False);
   Assert (Ifnot1 (False, False, X) = False);
end;

--# sys.adb
--  /ifa/ l+ ## 0
--  /ifnota/ l+ ## 0
