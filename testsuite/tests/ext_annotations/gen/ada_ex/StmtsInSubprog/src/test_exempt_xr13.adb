with MX; use MX;

--  Call functional code arranging to get into exempted region 2 only,
--  hence exempt violations from regions 1 and 3.

procedure Test_Exempt_XR13 is
begin
   Trigger_XR2;
end;

--# multiple_exemptions.adb
-- /dcl/               l+ ## 0
-- /1_if/              l+ ## 0
-- /1_flag/            l- ## s-
-- /1_exem/            l* ## x+
-- /1_exem_v1/         l= ## Xs-
-- /stmt/              l+ ## 0
-- /2_if/              l+ ## 0
-- /2_flag/            l+ ## 0
-- /2_exem/            l# ## x0
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
