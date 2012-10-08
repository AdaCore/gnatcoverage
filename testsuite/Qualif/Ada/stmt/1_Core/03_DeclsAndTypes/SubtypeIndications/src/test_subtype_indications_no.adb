--  Test driver for subtype indications. It only 'with's the functional code
--  but does not call anything from it, so only the library-level declaration
--  is expected to be reported as covered.

with Subtype_Indications;
with Support; use Support;

procedure Test_Subtype_Indications_No is
begin
   Assert (True);
end Test_Subtype_Indications_No;

--# subtype_indications.ads
-- /dcl/ l+ ## 0

--# subtype_indications.adb
-- /1_local_dcl/ l- ## s-
-- /1_stmt/      l- ## s-
-- /2_local_dcl/ l- ## s-
-- /2_stmt/      l- ## s-
-- /3_local_dcl/ l- ## ~s-
-- /3_stmt/      l- ## s-





