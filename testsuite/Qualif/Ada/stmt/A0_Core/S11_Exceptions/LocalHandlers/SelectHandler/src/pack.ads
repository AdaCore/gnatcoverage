package Pack is
   type My_Int is range -100 .. 100;

   My_Exception : exception;

   --  The routines below contain different forms of exceptional handlers.
   --  There is no exception propagation

   procedure Proc1 (I : in out My_Int);
   function Fun1 (I : My_Int) return My_Int;
   --  only OTHERS handler

   procedure Proc2 (I : in out My_Int);
   function Fun2 (I : My_Int) return My_Int;
   --  Handlers for specific exceptions, no OTHERS handler

   procedure Proc3 (I : in out My_Int);
   function Fun3 (I : My_Int) return My_Int;
   --  Handler for a predefined Constraint_Error exception and OITHERS handler

   procedure Proc4 (I : in out My_Int);
   function Fun4 (I : My_Int) return My_Int;
   --  Handler for a user-defined My_Exception exception and OITHERS handler

end Pack;
