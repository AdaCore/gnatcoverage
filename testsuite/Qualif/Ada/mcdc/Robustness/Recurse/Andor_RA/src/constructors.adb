with Simple_Pools;

package body Constructors is

   package Bandor_Pool is new Simple_Pools.Basic_Pool
     (Data_Type => Bandor, Capacity => 20);
   
   function Expr_Andor (A, B, C : Expr_Ref) return Expr_Ref is
      Eptr : constant Bandor_Pool.Data_Access := Bandor_Pool.Allocate;
   begin
      Eptr.all := (A => A, B => B, C => C);
      return Expr_Ref (Eptr);
   end;

   function Expr_Andor (A, B, C : Boolean) return Expr_Ref is
   begin
      return Expr_Andor (Expr_Val (A), Expr_Val (B), Expr_Val (C));
   end;

   function Expr_Andor_Andor (A, B, C, D, E : Boolean) return Expr_Ref is
   begin
      return Expr_Andor (Expr_Andor (A, B, C), Expr_Val (D), Expr_Val (E));
   end;

end;

