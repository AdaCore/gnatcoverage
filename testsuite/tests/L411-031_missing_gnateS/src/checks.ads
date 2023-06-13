package Checks is
   
   --  Weak assertion checking service that just counts assertion
   --  failures.
   
   --  The point of this code is to offer something to build and run to check
   --  for warnings about missing compilation options. We don't care about the
   --  actual coverage assessments here and need to minimize dependencies
   --  against runtime services such as an exception last chance handler.
   
   Assert_Failures : Natural := 0;
   procedure Assert (Cond : Boolean);

end;
