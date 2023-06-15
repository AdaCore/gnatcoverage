with tNoCheck; use tNoCheck;

procedure Test_tNocheck_T is
   SIO : SI (UB => 5);
   STIO : STI (UB => 10);
begin
   null;
end;

--# tnocheck.ads

-- We expect only l. or l+ on type declarations in
-- this case (invariants disabled), which depends on
-- whether or not code gets generated, in turn varying
-- with the kind of runtime configuration.

-- Not worth tracking the exact different scenarii, so
-- we don't state any particular expectation so l+ or
-- l. will go by regardless.

-- If code gets generated and turns out uncovered, we
-- will get a failure out of an unexpected violation.
