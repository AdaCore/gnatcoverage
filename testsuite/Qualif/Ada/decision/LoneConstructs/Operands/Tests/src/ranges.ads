package Ranges is

   type XYrange is private;

   procedure Set (R : out XYrange; X, Y : Integer);

   function Overlap (R1, R2 : XYrange) return Boolean;

private
   type XYrange is record
      X, Y  : Integer;
      Valid : Boolean;
   end record;
end;
