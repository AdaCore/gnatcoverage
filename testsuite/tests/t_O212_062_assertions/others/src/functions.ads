pragma Ada_2022;
pragma Assertion_Policy (Check);

package Functions is

   function Foo (I : Integer) return Integer
     with Pre  => (I >= 0 and then False) or else I >= 0,      -- # foo_pre
          Post => (I = Foo'Result or else Foo'Result /= 42);   -- # foo_post

   function Bar (I : Integer) return Integer is (I)            -- # bar_expr
     with Pre => (I < 0 or else I >= 0) or else (I = 42);      -- # bar_pre

   function Baz (I : Integer) return Integer is (I);           -- # baz_expr

   function Same (A, B : Boolean) return Boolean;

end Functions;
