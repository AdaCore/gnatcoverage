with Fuand;

procedure Test_FUAND_A is
begin
   Fuand.Test_A;
end;

--# fuand.ads
--
--%opts: --trace-mode=bin
--  /eval/ l! ## c!:"Line(A"
--
--%opts: --trace-mode=src
--  /eval/ l! ## c!:"(Line(A"
