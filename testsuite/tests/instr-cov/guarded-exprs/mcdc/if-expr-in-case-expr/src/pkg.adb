pragma Ada_2022;

package body Pkg is
   function Foo (A : Animal; B, C : Boolean) return String is
   begin
      return                  -- # return
        (case A is            -- # case
         when Dog => "",      -- # dog
         when Cat => "Meow",  -- # cat
         when Cow =>          -- # cow
           (if                -- # if
               B and then C   -- # cond
            then
               "Moooh"        -- # true
            else
               "No Moo :(")); -- # false
   end Foo;
end Pkg;
