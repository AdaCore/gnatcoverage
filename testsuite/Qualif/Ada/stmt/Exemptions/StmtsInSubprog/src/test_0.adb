with MX;

--  Do nothing, so statement gets covered at all, and all the exemption
--  regions do exempt their statements.

procedure Test_0 is
begin
   null;
end Test_0;

--# multiple_exemptions.adb
-- /dcl/               l- s-
-- /1_if/              l- s-
-- /1_flag/            l- s-
-- /1_exem/            l* x+
-- /stmt/              l- s-
-- /2_if/              l- s-
-- /2_flag/            l- s-
-- /2_exem/            l* x+
-- /3_if/              l- s-
-- /3_flag/            l- s-
-- /3_exem/            l* x+
-- /h_flag/            l- s-
-- /h_exem/            l* x+

