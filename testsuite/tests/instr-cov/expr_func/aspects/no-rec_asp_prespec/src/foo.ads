pragma Ada_2012;

package Foo is

   function Fact (N : Integer) return Integer
   with Pre  => N > 0,
        Post => Fact'Result >= N;

   function Fact (N : Integer) return integer is
      (if N > 1 then N * (N -1) else N);  -- # expr_func

end Foo;