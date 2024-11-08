pragma Ada_2022;

package body Animal is
   function Scream (A : Animal) return String is
      Sound : constant String := -- # decl
        (case A is               -- # case_root
         when Dog => "Woof",     -- # alt_dog
         when Cat => "Meow",     -- # alt_cat
         when Cow => "Mooo");    -- # alt_cow
   begin
      return Sound;              -- # return
   end Scream;
end Animal;
