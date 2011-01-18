package Values is

   type Int is record
      Val : Integer := 0;
      Set : Boolean := False;
   end record;

   procedure Set (V : out Int; Val : Integer);
   function Value (V : Int) return Integer;

   function Same (V1, V2 : Int) return Boolean;
   --  Whether V1.Val = V2.Val, checking that V1.Set and V2.Set

end;
