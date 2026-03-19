package body Pkg.Child is

   function Foo (X : Integer) return Integer is
   begin
      return X; -- # stmt
   end Foo;

end Pkg.Child;
