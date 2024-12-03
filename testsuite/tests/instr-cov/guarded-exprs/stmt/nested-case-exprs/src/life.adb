pragma Ada_2022;

package body Life is
   function Scream (A : Animal) return String is
   begin
      return                                -- # return
        (case A.T is                        -- # case_animal
            when Mammal  =>                 -- # when_mammal
              (case A.M_Kind is             -- # case_mammal
                  when Dog => "Woof",       -- # when_dog
                  when Cat => "Meow",       -- # when_cat
                  when Cow => "Mooo"),      -- # when_cow
            when Reptile =>                 -- # when_reptile
              (case A.R_Kind is             -- # case_reptile
                  when Snake  => "Ssss",    -- # when_snake
                  when Turtle => "...."));  -- # when_turtle
   end Scream;
end Life;
