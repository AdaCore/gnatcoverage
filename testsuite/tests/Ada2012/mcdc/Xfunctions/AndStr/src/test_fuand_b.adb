with Fuand;

procedure Test_FUAND_B is
begin
   Fuand.Test_B;
end;

--# fuand.ads
--
--%opts: --trace-mode=bin
--  /eval/ l! ## c!:"Color(A"
--
--%opts: --trace-mode=src
--  /eval/ l! ## c!:"(Color(A"
