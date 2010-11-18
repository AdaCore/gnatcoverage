--  Test driver for subtype indications. It calls all the subprograms from the
--  functional code, so the library-level declaration and all the local
--  declarations are expected to be reported as covered.

with Decls_Support;       use Decls_Support;
with Subtype_Indications; use Subtype_Indications;
with Support;             use Support;

procedure Test_Subtype_Indications_Full is
   V  : Var_String := Get_Var_String (1, "a");
   V1 : Vector (1 .. 3) := (1, 2, 3);
   V2 : Vector (1 .. 3);

begin
   Assert (V.Data = "a");

   V2 := Simple_Sort (V1);
   Assert (V2 (1) = 3 and then V2 (2) = 2 and then V2 (3) = 1);

   Assert (Some_Fun (2, "ab") = 2);
end Test_Subtype_Indications_Full;

--# subtype_indications.ads
-- /dcl/ l+ 0

--# subtype_indications.adb
-- /1_local_dcl/ l+ 0
-- /1_stmt/      l+ 0
-- /2_local_dcl/ l+ 0
-- /2_stmt/      l+ 0
-- /3_local_dcl/ l+ 0
-- /3_stmt/      l+ 0





