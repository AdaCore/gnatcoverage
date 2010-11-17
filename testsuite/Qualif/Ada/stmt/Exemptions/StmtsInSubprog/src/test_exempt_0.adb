--  Call into the functional code, arranging to execute all the code in all
--  the exempted sections, so no violation gets exempted.

with MX, Support;  use MX, Support;

procedure Test_Exempt_0 is
begin
   Trigger_XR1;
   Trigger_XR2;
   Trigger_XR3;
end;

--# multiple_exemptions.adb
-- /dcl/               l+ 0
-- /1_if/              l+ 0
-- /1_flag/            l+ 0
-- /1_exem/            l# x0
-- /stmt/              l+ 0
-- /2_if/              l+ 0
-- /2_flag/            l+ 0
-- /2_exem/            l# x0
-- /3_if/              l+ 0
-- /3_flag/            l+ 0
-- /3_exem/            l# x0
-- /h_flag/            l+ 0
-- /h_exem/            l# x0












