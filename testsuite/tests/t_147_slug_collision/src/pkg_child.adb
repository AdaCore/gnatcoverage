package body Pkg_Child is

   function Bar (X : Integer) return Integer is
   begin
      return X; -- # stmt
   end Bar;

end Pkg_Child;
