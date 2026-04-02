with Types_A; use Types_A;

procedure Test_T is
   SIO : SI (UB => 5);
   CIO : CI (UB => 2);
   STIO : STI (UB => 10);
   CTIO : CTI (UB => 4);

begin
   null;
end;

--# types_a.ads
--  /base/  l+ ## 0
--
--%opts: --trace-mode=bin
--  /si/   l! ## dF-
--  /ci/   l! ## dF-
--  /sti/  l! ## dF-
--  /cti/  l! ## dF-
--
--  Even though we don't instrument Invariant aspects' decisions is source-trace
--  mode, binary-trace mode still detects them as decision violations.
