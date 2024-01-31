package body System.Task_Primitives.Operations is

   package Specific is
      procedure Dummy;
   end Specific;

   package body Specific is separate;

   -----------
   -- Dummy --
   -----------

   procedure Dummy is
   begin
      Specific.Dummy;
   end Dummy;

   function Register_Foreign_Thread return Integer is separate;

end System.Task_Primitives.Operations;
