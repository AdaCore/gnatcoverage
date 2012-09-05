with Simple_Pools;

package body Constructors is
   
   package Band_Pool is new Simple_Pools.Basic_Pool
     (Data_Type => Band, Capacity => 20);
   
   function Expr_And (A, B : Expr_Ref) return Expr_Ref is
      Eptr : constant Band_Pool.Data_Access := Band_Pool.Allocate;
   begin
      Eptr.all := (A => A, B => B);
      return Expr_Ref (Eptr);
   end;

   function Expr_And (A, B : Boolean) return Expr_Ref is
   begin
      return Expr_And (Expr_Val (A), Expr_Val (B));
   end;

   function Expr_And_And (A, B, C : Boolean) return Expr_Ref is
   begin
      return Expr_And (Expr_Val (A), Expr_And (B, C));
   end;

end;

