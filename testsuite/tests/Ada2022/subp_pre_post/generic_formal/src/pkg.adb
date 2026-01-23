pragma Ada_2022;
pragma Assertion_Policy (Check);

with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out Point) is
   begin
      Adjuster (Self);  -- # stmt
   end Adjust;

   package body Adjust_Pkg is

      ------------
      -- Adjust --
      ------------

      procedure Adjust (Self : in out Point) is
      begin
         Adjuster (Self);  -- # stmt
      end Adjust;

   end Adjust_Pkg;

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
      procedure Adjust_Inst is new Adjust (Div_2);          -- # stmt
      package Adjust_Pkg_Inst is new Adjust_Pkg (Div_2);    -- # stmt

      For_Proc : Point := Self;                             -- # stmt
      For_Pkg  : Point := Self;                             -- # stmt
   begin
      Adjust_Inst (For_Proc);                               -- # stmt
      Adjust_Pkg_Inst.Adjust (For_Pkg);                     -- # stmt

      if For_Proc /= For_Pkg then                           -- # check
         raise Program_Error;                               -- # raise
      end if;

      Self := For_Proc;                                     -- # stmt
   end Test;

end Pkg;
