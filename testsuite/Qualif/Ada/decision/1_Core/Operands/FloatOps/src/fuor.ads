package FUOR is
   
   type My_Float is digits 8 range -5.0 .. 5.0; 
   
   type Capsum is record
      X, Y, Cap : My_Float;
   end record;
   
   FF : constant Capsum := (X => 1.0, Y => 1.0, Cap => 1.0);
   TT : constant Capsum := (X => 1.0, Y => 1.0, Cap => 3.0);
   
   type Operands is record
      A, B : Capsum;
   end record;
   
   function Orelse (Ops : Operands) return Boolean;
end;
