with Simple_Pools;

package body Exprs.E_Val is
   
   package Bval_Pool is new Simple_Pools.Basic_Pool
     (Data_Type => Bval, Capacity => 64);
   
   function Eval (E : Bval) return Boolean is
   begin
      return E.V;
   end;

   function Expr_Val (V : Boolean) return Expr_Ref is
      Eptr : constant Bval_Pool.Data_Access := Bval_Pool.Allocate;
   begin
      Eptr.all := (V => V);
      return Expr_Ref (Eptr);
   end;

end;

