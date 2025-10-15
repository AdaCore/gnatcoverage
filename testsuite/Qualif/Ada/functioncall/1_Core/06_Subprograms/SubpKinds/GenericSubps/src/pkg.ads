pragma Ada_2012;

package Pkg is

   Dummy : Integer := 0;                    -- # decl

   --  Generic procedure
   generic
      Type A is private;
   procedure P1 (X : A);

   --  Generic null procedure
   generic
      Type D is private;
   procedure P2 (X : D);

  --  Generic function
   generic
      Type B is private;
   function F1 (X : B) return B;

   --  Generic expression function
   generic
      Type C is private;
   function F2 (X : C) return C;

end Pkg;
