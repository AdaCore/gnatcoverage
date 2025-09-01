package Pkg is

   --  Extra feature to ensure the file is not instrumented or compiled: we use
   --  an expression function, which is Ada2012.

   function Bar (X : Integer) return Integer is (if X > 0 then X else X);

end Pkg;
