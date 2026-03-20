pragma Ada_2022;

package body Pkg is
   function Foo(A : Animal) return String is
   begin
      if                                     -- # if
        (case A is                           -- # case
            when Dog | Cat => True,          -- # when_dog_cat
            when Cow       => False) then    -- # when_cow
         return "pet";                       -- # return_pet
      else
         return "cattle";                    -- # return_cattle
      end if;
   end Foo;
end Pkg;
