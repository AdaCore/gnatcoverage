with Pkg; use Pkg;

--  Check the correct function coverage analysis of different kinds of null
--  procedures when never called.

procedure Test_Not_Called is
begin
    null;
end Test_Not_Called;

--# pkg.ads
-- /p/ l- ## s=>s-, f=>s-,f-

--# pkg.adb
-- /p/ l- ## s=>s-, f=>s-,f-
