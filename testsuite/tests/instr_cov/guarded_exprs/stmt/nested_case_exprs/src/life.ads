package Life is
   type Mammal_Kind is (Dog, Cat, Cow);
   type Reptile_Kind is (Snake, Turtle);
   type Animal_Kind is (Mammal, Reptile);

   type Animal (T : Animal_Kind) is record
      case T is
         when Mammal =>
            M_Kind : Mammal_Kind;
         when Reptile =>
            R_Kind : Reptile_Kind;
      end case;
   end record;

   function Scream (A : Animal) return String;
end Life;
