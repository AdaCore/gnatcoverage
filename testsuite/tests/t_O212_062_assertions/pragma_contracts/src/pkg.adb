pragma Ada_2022;
pragma Assertion_Policy (Check);

package body Pkg is

   function Foo (A, B : Boolean) return Boolean is
   begin
      return A;                                     -- # foo_ret
   end Foo;

   function Bar (A, B : Boolean) return Boolean is

      --  Contract pragma in the declarative part of a subprogram body

      pragma Precondition (A or else not B);        -- # bar_pre
   begin
      return B;                                     -- # bar_ret
   end Bar;

end Pkg;
