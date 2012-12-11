--  Test driver for subtype indications. It calls two subprograms from the
--  functional code (another driver - Test_Subtype_Indications_Part_1 calls
--  another routin), so the library-level declaration and some of the local
--  declarations are expected to be reported as covered, and other local
--  declarations are expected to be reported as uncovered.

with Decls_Support;       use Decls_Support;
with Subtype_Indications; use Subtype_Indications;
with Support;             use Support;

procedure Test_Subtype_Indications_Part_2 is
   V1 : Vector (1 .. 3) := (1, 2, 3);
   V2 : Vector (1 .. 3);

begin
   V2 := Simple_Sort (V1);
   Assert (V2 (1) = 3 and then V2 (2) = 2 and then V2 (3) = 1);

   Assert (Some_Fun (2, "ab") = 2);
end Test_Subtype_Indications_Part_2;

--# subtype_indications.ads
-- /dcl/ l+ ## 0

--# subtype_indications.adb
-- /1_local_dcl/ l- ## s-
-- /1_stmt/      l- ## s-
-- /2_local_dcl/ l+ ## 0
-- /2_bare_dcl/ ~l+ ## 0
-- /2_stmt/      l+ ## 0
-- /3_local_dcl/ l+ ## 0
-- /3_stmt/      l+ ## 0





