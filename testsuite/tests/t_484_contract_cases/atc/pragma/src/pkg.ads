pragma Ada_2012;
pragma Assertion_Policy (Check);

package Pkg is

   type T is record
      X, Y : Integer;
   end record;

   function Cartesian_Product (Self : T) return Integer;
   pragma Contract_Cases
     ((Self.X >= 0 and then Self.Y >= 0 => -- # guard-pp
         Cartesian_Product'Result >= 0,    -- # conseq-pp
       Self.X >= 0 and then Self.Y < 0 =>  -- # guard-pn
         Cartesian_Product'Result < 0,     -- # conseq-pn
       Self.X < 0 and then Self.Y >= 0 =>  -- # guard-np
         Cartesian_Product'Result < 0,     -- # conseq-np
       others =>                           -- # guard-others
         Cartesian_Product'Result > 0));   -- # conseq-others

end Pkg;
