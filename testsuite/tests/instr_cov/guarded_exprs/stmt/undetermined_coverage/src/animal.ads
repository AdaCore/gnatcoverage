package Animal is
   type Animal is (Dog, Cat, Cow);

   function Scream (A : Animal) return String;
end Animal;
