with tcheck; use tCheck;

procedure Test_tCheck_T is
   SIO : SI (UB => 5);
   CIO : CI (UB => 2);
   STIO : STI (UB => 10);
   CTIO : CTI (UB => 4);

begin
   null;
end;

--# tcheck.ads
--
--%opts: --trace-mode=bin
--  /si/  l! ## dF-
--  /ci/  l! ## dF-
--
--  In source-traces mode, we do not emit coverage obligations for the
--  pragmas by default, as we don't know what the assertion policy is.
