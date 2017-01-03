pragma Ada_2012;
package FUAND is
 
   -- We will be checking if X in (2*Y | Y+1)
 
   type Control is record
      X, Y : Integer;
   end record;
 
   function Plus1 (Y : Integer) return Integer is (Y + 1);
 
   function Twice (Y : Integer) return Integer is (Y * 2);
 
   FF : constant Control := (X => 1, Y => 1);
   TT1 : constant Control := (X => 3, Y => 2); -- X = Y + 1
   TT2 : constant Control := (X => 4, Y => 2); -- X = Y * 2
 
   type Operands is record
      A, B : Control;
   end record;
 
   function Andthen (Ops : Operands) return Boolean;
end;
