pragma Ada_2022;

package body Pkg is

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out Point; Adjuster : Point_Adjuster) is
   begin
      Adjuster.all (Self);  -- # stmt
   end Adjust;

   -----------
   -- Div_2 --
   -----------

   procedure Div_2 (Self : in out Point) is
   begin
      Self.X := @ / 2; -- # div_2
      Self.Y := @ / 2; -- # div_2
   end Div_2;

   ----------
   -- Test --
   ----------

   procedure Test (Self : in out Point) is
   begin
      Adjust (Self, Div_2'Access);  -- # stmt
   end Test;

end Pkg;
