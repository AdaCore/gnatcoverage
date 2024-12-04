pragma Ada_2012;

package body Animal is
   function Scream (A : Animal) return String is
   begin
      return                    -- # return
        (case A is              -- # case_root
         when Dog => "Woof",    -- # alt_dog
         when Cat => "Meow",    -- # alt_cat
         when Cow => "Mooo");   -- # alt_cow
   end Scream;
end Animal;
