with Make_Calls;

--  Check the coverage of a generic procedure, generic null, procedure, generic
--  function and a generic expression function declared in a regular procedure
--  when not called. We expect the generic subprograms to not be covered.
--
--  LIMITATION
--  The coverage of instances of generic subprograms is not computed.

procedure Test_No_Call is
begin
   null;
end Test_No_Call;

--# make_calls.adb
-- /test/     l- ## f-
-- /stmt/     l- ## s-
-- /call/     l- ## s-,c-

--# pkg.adb
-- /fun/      l- ## f-
-- /stmt/     l- ## s-
-- /null_fun/ l? ## s=>s?, f=>s?,f?
-- /exp_fun/  l? ## s=>s?, f=>s?,f?

--# pkg.ads
-- /decl/     l+ ## 0
