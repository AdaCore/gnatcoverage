pragma Ada_2012;
pragma Assertion_Policy (Check);

package Contracts is
   procedure Foo (B : Boolean)
     with Pre => B or else B;    -- # pre
end;
