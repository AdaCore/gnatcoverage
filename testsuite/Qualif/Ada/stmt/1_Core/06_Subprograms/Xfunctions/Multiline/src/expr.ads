pragma Ada_2012;

with Values;

package Expr is
   function Plus (X, Y : Integer) return Integer is 
      (X + Y); -- # stmt
      
   --  Stick some decl with elab code to make sure that the
   --  coverage here in absence of call doesn't back-propagate.
      
   X : Integer := Values.One; -- # decl
end;
