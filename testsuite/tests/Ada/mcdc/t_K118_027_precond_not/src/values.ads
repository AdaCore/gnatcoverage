package Values is

   type Int is record
      Val : Integer := 0;
      Set : Boolean := False;
   end record;

   procedure Set (V : in out Int; Val : Integer; Reset_Ok : Boolean);
   pragma Precondition ((not V.Set) or else Reset_Ok);
end;
