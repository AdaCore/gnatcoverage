--  Call into the functional code, arranging not to execute any exempted
--  region, so all the regions do exempt statements.

with MX;  use MX;

procedure Test_Exempt_All is
begin
   Trigger_0XR;
end;

--# multiple_exemptions.adb
-- /dcl/               l+ ## 0
-- /1_if/              l+ ## 0
-- /1_flag/            l- ## s-
-- /1_exem/            l* ## x+
-- /1_exem_v1/         l= ## Xs-
-- /stmt/              l+ ## 0
-- /2_if/              l+ ## 0
-- /2_flag/            l- ## s-
-- /2_exem/            l* ## x+
-- /2_exem_v1/         l= ## Xs-
-- /3_if/              l- ## s-
-- /3_flag/            l- ## s-
-- /3_exem/            l* ## x+
-- /3_exem_v1/         l= ## Xs-
-- /h_flag/            l- ## s-
-- /h_exem/            l* ## x+
-- /h_exem_v1/         l= ## Xs-
-- /h_exem_v2/         l= ## Xs-
-- /h_exem_v3/         l= ## Xs-
-- /h_exem_v4/         l= ## Xs-
