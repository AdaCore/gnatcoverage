package body Decls_Pack is

   procedure Local_1 (Res : in out Boolean) is
      Matrix_C : constant Matrix (1 ..2 , 1 .. 2) := ((1, 2), (3, 4)); -- # decl1
   begin
      Res := not Res;                                         -- # code1
   end Local_1;

   function Local_2 (Arg : Boolean) return Boolean is
      Vector_C : constant Vector := (1 .. 3 => Identity (4)); -- # decl2
   begin
      return not Arg;                                                -- # code2
   end Local_2;

end Decls_Pack;
