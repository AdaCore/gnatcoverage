pragma Check_Policy (Precondition, On);

package body Points is

   procedure Set (P : out Point; X, Y : Integer) is
      pragma Precondition (P.Set = False); -- # preSet
   begin
      P := (X => X, Y => Y, Set => True); -- # bodySet
   end;
end;


