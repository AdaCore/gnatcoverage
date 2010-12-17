package Points is
   type Point is record
      X, Y : Integer;
      Set  : Boolean := False;
   end record;

   procedure Set (P : out Point; X, Y : Integer);
end;
