pragma Assertion_Policy (Check);
pragma Ada_2012;

package body Functions is

   ---------
   -- Foo --
   ---------

   function Foo (I : Integer) return Integer
   is
      High : Integer := 3;                                     -- # foo_hi_decl
   begin
     for J in 1 .. High loop                                   -- # foo_loop_1
       pragma Loop_Invariant (J = J + 1 - 1 and then J < 4);   -- # foo_inv_1
     end loop;

     for J in 1 .. High loop                                   -- # foo_loop_2
       pragma Loop_Invariant (J = J + 1 - 1 or else J < 4);    -- # foo_inv_2
     end loop;
     return I;                                                 -- # foo_return
   end Foo;

   ----------
   -- Same --
   ----------

   function Same (A, B : Boolean) return Boolean
   is
      --  Subprogram body with aspect without prior declaration

      function Id (C : Boolean) return Boolean
        with Pre  => C or else (C or else not C),              -- # id_pre
             Post => Id'Result = C                             -- # id_post
      is
      begin
         return C;                                             -- # id_ret
      end Id;

   begin
      --  Decision nested as parameter of a function call within an assertion.
      --  The function parameter should not be seen as a condition for the
      --  assertion decision.

      --  Last two conditions are not evaluated

      pragma Assert (Id (A or else B) or else A or else B);    -- # nested_1

      --  Last condition is not evaluated

      pragma Assert (B or else A or else Id (A or else B));    -- # nested_2

      --  All conditions are evaluated

      pragma Assert (B or else Id (A or else B));              -- # nested_3
      return B;                                                -- # same_ret
   end Same;

end Functions;
