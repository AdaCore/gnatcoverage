--  This package contains declaration sequences in different contexts:
--     library level sequence;
--     local sequence
--     part of generic unit

with Decls_Support; use Decls_Support;
with Support;       use Support;
package Decls_Pack is

   subtype Small_Vector is Vector (1 .. Identity (2)); -- # lib_level_dcl
   Day           : Week_Day := Mon;                    -- # lib_level_dcl
   My_War_String : Var_String;                         -- # lib_level_dcl

   generic
      Par : Integer;
   package Inner_G is
      subtype Another_Small_Vector is Vector (Par .. Par + 10); -- # gen_dcl
      Int : Integer := Par;                                     -- # gen_dcl
      Zero : Coordinate;                                        -- # gen_dcl
   end Inner_G;

   function Simple_Sort (Arg : Vector) return Vector;

end Decls_Pack;
