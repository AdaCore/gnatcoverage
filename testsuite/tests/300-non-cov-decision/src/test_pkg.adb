with Pkg;

procedure Test_Pkg is
begin
   Pkg.Foo(True);
end Test_Pkg;

--# pkg.adb
-- /st/ l+ ## 0
-- /dc/ l+ ## 0

-- %opts: --trace-mode=src
-- =/dc/ l! ## dF-

-- %cov: --non-coverable
-- =/dc/ l0 ## d0
