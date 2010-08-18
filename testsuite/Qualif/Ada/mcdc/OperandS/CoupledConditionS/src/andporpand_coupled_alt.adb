package body AndPorPand_Coupled_Alt is

   function F1 (A, B, C : Boolean) return Boolean is
   begin
      return A and then (B or else C)  -- # F1_evaluate
        and then A; -- # coupF1_evaluate
   end F1;

   function F2 (A, B, C : Boolean) return Boolean is
   begin
      return A and then (B  -- # F2_evaluate
                         or else B) and then C; -- # coupF2_evaluate
   end F2;

   function F3 (A, B, C : Boolean) return Boolean is
      D : Boolean := A;  -- # decl
   begin
      return A and then  -- # F3_evaluate
        (A or else B) and then C;  -- # coupF3_evaluate
   end F3;

end AndPorPand_Coupled_Alt;

