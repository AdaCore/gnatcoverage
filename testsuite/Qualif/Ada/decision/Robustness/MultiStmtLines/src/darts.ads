package Darts is
   
   type Game is record
      Score, Hits, Fancy_Hits : Natural := 0;
   end record;
   
   procedure Reset (G : in out Game);
   procedure Register
     (Hit : Natural; Double, Triple : Boolean; G : in out Game);
end;
