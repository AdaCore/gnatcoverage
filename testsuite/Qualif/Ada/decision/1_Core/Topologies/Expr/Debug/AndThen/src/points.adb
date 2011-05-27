pragma Debug_Policy (Check);

package body Points is

   procedure Assert (X : Boolean) is
   begin
      pragma Annotate (Xcov, Exempt_On, "test verifies coverage at call sites");  -- # exempt
      if not X then                                                               -- # exempt
         raise Program_Error;                                                     -- # exempt
      end if;                                                                     -- # exempt
      pragma Annotate (Xcov, Exempt_Off, "test verifies coverage at call sites"); -- # exempt
   end Assert;

   procedure Set (P : out Point; X, Y : Integer) is
   begin
      P := (X => X, Y => Y, Set => True); -- # bodySet
   end;

   function Same_X (P1, P2 : Point) return Boolean is
   begin
      pragma Debug (Assert (P1.Set and then P2.Set)); -- # preSX
      return P1.X = P2.X; -- # bodySX
   end;

   function Same_Y (P1, P2 : Point) return Boolean is
   begin
      pragma Debug (Assert (P1.Set and then P2.Set)); -- # preSY
      return P1.Y = P2.Y; -- # bodySY
   end;

   function Same_XY (P1, P2 : Point) return Boolean is
   begin
      pragma Debug (Assert (P1.Set and then P2.Set)); -- # preSXY
      return P1.X = P2.X and then P1.Y = P2.Y; -- # retSXY
   end;
end;


