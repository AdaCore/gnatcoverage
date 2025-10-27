with Make_Calls;

--  Check the coverage of a generic procedure, generic null procedure, generic
--  function and a generic expression function declared in a regular procedure
--  when called. We expect the generic subprograms to be covered.
--
--  LIMITATION
--  The coverage of instances of generic subprograms is not computed.

procedure Test_Call is
begin
   Make_Calls;
end Test_Call;

--# make_calls.adb
-- /test/     l+ ## 0
-- /stmt/     l+ ## 0
-- /call/     l+ ## 0

--# pkg.adb
-- /fun/      l+ ## 0
-- /stmt/     l+ ## 0
-- /null_fun/ l? ## s=>s?, f=>s?,f?
-- /exp_fun/  l? ## s=>s?, f=>s?,f?

--# pkg.ads
-- /decl/     l+ ## 0
