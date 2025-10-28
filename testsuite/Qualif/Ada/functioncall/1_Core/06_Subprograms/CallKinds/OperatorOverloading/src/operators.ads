pragma Ada_2012;

package Operators is
   type T is new Integer;                                -- # decl

   One : T := 1;                                         -- # decl
   Two : T := 2;                                         -- # decl

   function "+" (Left, Right : Boolean) return Boolean   -- # fun
   is (Left and Right);                                  -- # stmt

   function "+" (Arg : T) return T                       -- # fun
   is (0 - Arg);                                         -- # stmt

   function "&" (L,R : Float) return Float               -- # fun
   is (42.0);                                            -- # stmt

   function "&" (L : Integer; R : String) return Integer -- # fun
   is (42);                                              -- # stmt

   function "&" (L : String; R : Integer) return Integer -- # fun
   is (42);                                              -- # stmt

   function "<" (L, R : T) return Boolean                -- # fun
   is (L /= R);                                          -- # stmt

   package Reversed is
      function "<" (L, R : T) return Boolean             -- # fun
      is (L > R);                                        -- # stmt

      package Inner is
         function "+" (Arg : T) return T                 -- # fun
         is (0 - Arg);                                   -- # stmt
      end Inner;
   end Reversed;
end Operators;
