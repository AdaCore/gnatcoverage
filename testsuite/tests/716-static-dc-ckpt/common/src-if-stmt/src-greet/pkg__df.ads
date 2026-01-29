package Pkg is

   function Difficult_True (X : Integer) return Boolean;

   Val : Boolean := not Difficult_True (30);

end Pkg;
