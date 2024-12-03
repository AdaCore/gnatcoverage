package Life is
   type Animal is (Dog, Cat, Cow);

   function Scream(A : Animal; Silent : Boolean) return String;
end Life;
