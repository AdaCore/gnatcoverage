package Points is
   type Point is record
      X, Y : Integer;
      Set  : Boolean := False;
   end record;

   procedure Set (P : out Point; X, Y : Integer);

   function X (P : Point) return Integer;
   pragma Precondition (P.Set); -- # preX

   function Y (P : Point) return Integer;
   pragma Precondition (P.Set); -- # preY

end;
