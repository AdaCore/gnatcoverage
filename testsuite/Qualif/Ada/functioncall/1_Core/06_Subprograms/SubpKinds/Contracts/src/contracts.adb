pragma Ada_2012;
pragma Assertion_Policy (Check);

package body Contracts is

   procedure Foo (B : Boolean)             -- # foo
   is
      function T return Boolean is (True); -- # t
   begin
      pragma Assert (T or else T);         -- # assert
      null;                                -- # null
   end Foo;
end;
