pragma Ada_2022;

package body Life is
   function Scream(A : Animal; Silent : Boolean) return String is
   begin
      return                    -- # return
        (if not Silent then     -- # if_cond
           (case A is           -- # case
            when Dog => "Woof", -- # dog
            when Cat => "Meow", -- # cat
            when Cow => "Mooo") -- # cow
         else "....");          -- # else
   end Scream;
end Life;
