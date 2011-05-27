package Points is
   type Point is record
      X, Y : Integer;
      Set  : Boolean := False;
   end record;

   procedure Set (P : out Point; X, Y : Integer);

   function Same_X (P1, P2 : Point) return Boolean;
   function Same_Y (P1, P2 : Point) return Boolean;
   function Same_XY (P1, P2 : Point) return Boolean;
end;
