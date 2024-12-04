pragma Ada_2022;

package body Life is
   function Animal_Is_Meowing (A : Animal) return Boolean is
   begin
      return                           -- # return
        (declare                       -- # decl
            Sound: constant String :=  -- # sound
              (case A is               -- # case
               when Dog => "Woof",     -- # dog
               when Cat => "Meow",     -- # cat
               when Cow => "Mooo");    -- # cow
         begin                         -- # begin
            Sound = "Meow");           -- # cmp
   end Animal_Is_Meowing;
end Life;
