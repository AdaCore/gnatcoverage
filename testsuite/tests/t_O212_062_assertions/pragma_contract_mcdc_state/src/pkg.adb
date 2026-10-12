pragma Ada_2022;
pragma Assertion_Policy (Check);

package body Pkg is

   function F (A, B : Boolean) return Boolean is
      pragma Precondition (A or else not B);        -- # pre
      pragma Assert (not B or else A);              -- # assert
   begin
      return A and then B;                          -- # ret
   end F;

end Pkg;
