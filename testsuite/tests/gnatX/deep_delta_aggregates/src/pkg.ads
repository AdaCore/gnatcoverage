pragma Extensions_Allowed (On);

package Pkg is

   type Point is record
      X, Y : Integer;
   end record;

   type Segment is array (Boolean) of Point;

   S : Segment := (others => (0, 0));

   procedure Set_X
     (A, B : Boolean; Value : Integer; Negate : Boolean);

end Pkg;
