pragma Check_Policy (Precondition, On);

package body Points is

   procedure Set (P : out Point; X, Y : Integer) is
      pragma Precondition (not P.Set); -- # preSet
   begin
      P := (X => X, Y => Y, Set => True); -- # bodySet
   end;

   function X (P : Point) return Integer is
   begin
      return P.X; -- # bodyX
   end;

   function Y (P : Point) return Integer is
   begin
      return P.Y; -- # bodyY
   end;
end;


