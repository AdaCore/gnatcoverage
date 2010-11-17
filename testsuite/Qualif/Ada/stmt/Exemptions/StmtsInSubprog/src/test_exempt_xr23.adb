with MX; use MX;

--  Call functional code arranging to get into exempted region 1 only,
--  hence exempt violations from regions 2 and 3.

procedure Test_Exempt_XR23 is
begin
   Trigger_XR1;
   
   -- Region 1 raises an exception, force running the other blocks
   -- without the exempted regions.
   
   Trigger_0XR;
end;

--# multiple_exemptions.adb
-- /dcl/               l+ 0
-- /1_if/              l+ 0
-- /1_flag/            l+ 0
-- /1_exem/            l# x0
-- /stmt/              l+ 0
-- /2_if/              l+ 0
-- /2_flag/            l- s-
-- /2_exem/            l* x+
-- /3_if/              l- s-
-- /3_flag/            l- s-
-- /3_exem/            l* x+
-- /h_flag/            l+ 0
-- /h_exem/            l# x0
