pragma Ada_2012;

package body Pkg is

   --  Generic procedure
   procedure P1 (X : A) is                -- # fun
   begin
      Dummy := 1;                         -- # stmt
   end P1;

   --  Generci null procedure
   procedure P2 (X : D) is null;          -- # null_fun

   --  Generic function
   function F1 (X : B) return B           -- # fun
   is
      Dummy : B := X;                     -- # stmt
   begin
      return Dummy;                       -- # stmt
   end F1;

   --  Generic expression function
   function F2 (X : C) return C is (X);   -- # exp_fun

end Pkg;
