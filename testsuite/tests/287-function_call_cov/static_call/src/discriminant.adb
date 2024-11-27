pragma Ada_2012;

procedure Discriminant is
   type Color is (Red, Green, Blue, Not_On);          -- # a

   type Disc_Rec (Disc : Color) is                    -- # a
      record
         null;                                        -- # a
   end record;

   My_Rec : constant Disc_Rec := (Disc => Red);       -- # a
begin
   null;
end Discriminant;
