package Constructs is
   --  We place the code of interest in the library package to make it looking
   --  like a typical fucnctional code in a typical Ada program (a subprogram
   --  declared in a package)

   function In_Range (X, L, R : Integer) return Boolean;
   --  This function checks whether X is in range L .. R, but its real goal is
   --  to put Ada constructs of interest in defferent execution paths.

end Constructs;
