pragma Ada_2012;
pragma Assertion_Policy (Check);

package Foo is

   function Bar (L, R : Integer) return Integer
      with Pre =>
         L > 100           -- # decision-1
         and then R < 100; -- # decision-2

end Foo;
