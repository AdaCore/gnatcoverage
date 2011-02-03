--  This package contains library-level scalar object declaration with explicit
--  initialization expression and subprograms containing local scalar object
--  declarations with explicit initialization that are expected to be covered
--  only when subprograms are called.
--  Also contains generic packages that do the same for other kinds of types -
--  array and record

with Decls_Support; use Decls_Support;
with Support; use Support;
package Decls_Pack_1 is

   Count : Natural := Identity (0);                -- # dcls

   procedure Local_Swap (I, J : in out Integer);

   function Local_Fun (Arg : Week_Day) return Week_Day;
   --  Returns the next week day. For Sunday returns Monday

   generic
      Par1 : Integer;
      Par2 : Integer;
   package Decls_Pack_Matrix_G is

      Matrix_V : Matrix (1 .. 2, 1 .. 2) := ((Par1, Par2), (3, Identity (4))); -- # g1_dcls

      procedure Local_Swap (M1, M2 : in out Matrix);

      function Local_Fun (Arg : Matrix) return Matrix;
      --  Changes the sign of each component of the argument.
   end Decls_Pack_Matrix_G;

   generic
   package Decls_Pack_Records_G is

      My_String : Var_String := (Len => 3, Data => "Ada");    -- # g2_dcls

      procedure Local_Swap (C1, C2 : in out Coordinate);

      function Local_Fun (Arg : Var_String) return Var_String;
      --  For the Data field of the argument, replace each character with the
      --  next one.
   end Decls_Pack_Records_G;

end Decls_Pack_1;
