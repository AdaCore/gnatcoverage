pragma Check_Policy (Precondition, On);

package body Points is

   procedure Set (P : out Point; X, Y : Integer) is
   begin
      P := (X => X, Y => Y, Set => True); -- # bodySet
   end;

   function Same_X (P1, P2 : Point) return Boolean is
   begin
      return P1.X = P2.X; -- # bodySX
   end;

   function Same_Y (P1, P2 : Point) return Boolean is
   begin
      return P1.Y = P2.Y; -- # bodySY
   end;

   function Same_XY (P1, P2 : Point) return Boolean is
      pragma Precondition (P1.Set and then P2.Set); -- # evalSXY
   begin
      return P1.X = P2.X and then P1.Y = P2.Y; -- # retSXY
   end;
end;


