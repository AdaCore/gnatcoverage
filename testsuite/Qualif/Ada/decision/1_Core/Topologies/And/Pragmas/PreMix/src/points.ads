pragma Check_Policy (Precondition, On);

package Points is
   type Point is record
      X, Y : Integer;
      Set  : Boolean := False;
   end record;

   procedure Set (P : out Point; X, Y : Integer);

   function Same_X (P1, P2 : Point) return Boolean;
   pragma Precondition (P1.Set and then P2.Set); -- # preSX

   function Same_Y (P1, P2 : Point) return Boolean;
   pragma Precondition (P1.Set and then P2.Set); -- # preSY

   function Same_XY (P1, P2 : Point) return Boolean;
end;
