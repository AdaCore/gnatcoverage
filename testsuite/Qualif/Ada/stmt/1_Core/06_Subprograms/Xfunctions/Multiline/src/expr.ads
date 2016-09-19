pragma Ada_2012;

with Values;

package Expr is
   -- The statement SCO corresponding to an expression function
   -- spans the entire declaration, so we need continuation anchors
   -- to allow multiline expectations
   
   function Plus (X, Y : Integer) return Integer is -- # fnstmt
      (X + Y); -- # cont
      
   --  Stick some decl with elab code to make sure that the
   --  coverage here in absence of call doesn't back-propagate.
      
   X : Integer := Values.One; -- # decl
end;
