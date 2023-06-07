with Support; use Support;

package body Expr is
   
   procedure As_RHS (A, B : Boolean) is
   begin
      GB := A and then B; -- # eval
   end;
   
   procedure As_CASE (A, B : Boolean) is
   begin
      case A and then B is  -- # eval
         when True => GX := GX + 1;  -- # true
         when False => GX := GX - 1; -- # false
      end case;      
   end;
   
   procedure Set_GB (Value : Boolean) is
   begin
      GB := Value;
   end;
   
   procedure As_ACTUAL (A, B : Boolean) is
   begin
      Set_GB (A and then B); -- # eval
   end;
   
   function As_RETURN (A, B : Boolean) return Boolean is
   begin
      return A and then B; -- # eval
   end;
   
   procedure As_DECLINIT (A, B : Boolean) is
      X : Boolean := A and then B;  -- # eval
      pragma Volatile (X);
   begin
      GB := X;
   end;
   
   type My_Bool (Value : Boolean) is null record;
   
   procedure As_DISCINIT (A, B : Boolean) is
      X : My_Bool (Value => A and then B); -- # eval
      pragma Volatile (X);
   begin
      GB := X.Value;
   end;
   
   type My_Block is array (1 .. 1) of Boolean;
   
   procedure As_AGGREGATE (A, B : Boolean) is
      X : My_Block := (others => A and then B); -- # eval
      pragma Volatile (X);
   begin
      GB := X(X'First);
   end;
   
   pragma Check_Policy (Precondition, On);
   pragma Check_Policy (Postcondition, On);
   
   procedure As_BODYPRECOND (A, B : Boolean) is
      pragma Precondition (A and then B); -- # freestanding-expr
   begin
      GX := GX + 1;
   end;
   
   procedure As_BODYPOSTCOND (A, B : Boolean) is
      pragma Postcondition (A and then B); -- # freestanding-expr
   begin
      GX := GX + 1;
   end;
      
   procedure As_SPECPRECOND (A, B : Boolean) is
   begin
      GX := GX + 1;
   end;
   
   procedure As_SPECPOSTCOND (A, B : Boolean) is
   begin
      GX := GX + 1;
   end;
   
   procedure As_DEBUG_ASSERT (A, B : Boolean) is
      pragma Debug_Policy (Check);
   begin
      pragma Debug (Assert (A and then B)); -- # eval
      GX := GX + 1;
   end;
end;
