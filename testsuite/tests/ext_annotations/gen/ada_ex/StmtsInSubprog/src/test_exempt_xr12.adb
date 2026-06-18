with MX; use MX;

--  Call functional code arranging to get into exempted region 3 only,
--  hence exempt violations from regions 1 and 2.

procedure Test_Exempt_XR12 is
begin
   Trigger_XR3;
end;

--# multiple_exemptions.adb
-- /dcl/               l+ ## 0
-- /1_if/              l+ ## 0
-- /1_flag/            l- ## s-
-- /1_exem/            l* ## x+:"exemption section #1"
-- /1_exem_v1/         l= ## Xs-
-- /stmt/              l+ ## 0
-- /2_if/              l+ ## 0
-- /2_flag/            l- ## s-
-- /2_exem/            l* ## x+:"exemption section #2"
-- /2_exem_v1/         l= ## Xs-
-- /3_if/              l+ ## 0
-- /3_flag/            l+ ## 0
-- /3_exem/            l# ## x0:"exemption section #3"
-- /h_flag/            l- ## s-
-- /h_exem/            l* ## x+:"exemption section #4"
-- /h_exem_v1/         l= ## Xs-
-- /h_exem_v2/         l= ## Xs-
-- /h_exem_v3/         l= ## Xs-
-- /h_exem_v4/         l= ## Xs-
