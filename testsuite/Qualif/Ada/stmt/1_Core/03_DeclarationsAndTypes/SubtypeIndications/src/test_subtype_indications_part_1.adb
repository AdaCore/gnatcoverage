--  Test driver for subtype indications. It calls a subprogram from the
--  functional code (another driver - Test_Subtype_Indications_Part_2 calls
--  other routines), so the library-level declaration and some of the local
--  declarations are expected to be reported as covered, and other local
--  declarations are expected to be reported as uncovered.

with Decls_Support;       use Decls_Support;
with Subtype_Indications; use Subtype_Indications;
with Support;             use Support;

procedure Test_Subtype_Indications_Part_1 is
   V : Var_String := Get_Var_String (1, "a");
begin
   Assert (V.Data = "a");
end Test_Subtype_Indications_Part_1;

--# subtype_indications.ads
-- /dcl/ l+ 0

--# subtype_indications.adb
-- /1_local_dcl/ l+ 0
-- /1_stmt/      l+ 0
-- /2_local_dcl/ l- s-
-- /2_stmt/      l- s-
-- /3_local_dcl/ l- ~s-
-- /3_stmt/      l- s-





