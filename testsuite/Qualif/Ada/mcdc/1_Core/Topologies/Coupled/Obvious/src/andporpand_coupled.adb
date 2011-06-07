package body AndPorPand_Coupled is

   function F1 (A, B, C : Boolean) return Boolean is
      D : Boolean := A;  -- # decl
   begin
      return A and then (B or else C) and then D; -- # F1_evaluate
   end F1;

   function F2 (A, B, C : Boolean) return Boolean is
      D : Boolean := B;  -- # decl
      pragma Volatile (D); -- preserve eval of B or else D
   begin
      return A and then (B or else D) and then C; -- # F2_evaluate
   end F2;

   function F3 (A, B, C : Boolean) return Boolean is
      D : Boolean := A;  -- # decl
      pragma Volatile (D); -- preserve eval
   begin
      return A and then (D or else B) and then C; -- # F3_evaluate
   end F3;

end AndPorPand_Coupled;

