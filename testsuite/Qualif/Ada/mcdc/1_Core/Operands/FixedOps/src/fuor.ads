package FUOR is
   
   type Volt is delta 0.125 range 0.0 .. 255.0;
      
   type Capsum is record
      X, Y, Cap : Volt;
   end record;
   
   FF : constant Capsum := (X => 1.0, Y => 1.0, Cap => 1.0);
   TT : constant Capsum := (X => 1.0, Y => 1.0, Cap => 3.0);
   
   type Operands is record
      A, B : Capsum;
   end record;
   
   function Orelse (Ops : Operands) return Boolean;
end;
