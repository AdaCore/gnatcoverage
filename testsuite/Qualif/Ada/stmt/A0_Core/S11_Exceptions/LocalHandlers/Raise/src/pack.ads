package Pack is
   type My_Int is range -100 .. 100;

   My_Exception : exception;

   --  The routines below contain different cases of exception raise. All
   --  the handlers are trivial. There is no exception propagation

   procedure Proc1 (I : in out My_Int);
   function Fun1 (I : My_Int) return My_Int;
   --  Explicit raise of user-defined exception in a subprogram body

   procedure Proc2 (I : in out My_Int);
   function Fun2 (I : My_Int) return My_Int;
   --  Explicit raise of predefined Constraint_Error

   procedure Proc3 (I : in out My_Int);
   function Fun3 (I : My_Int) return My_Int;
   --  Implicit raise of predefined exception

   procedure Proc4 (I : in out My_Int);
   function Fun4 (I : My_Int) return My_Int;
   --  Exception raise in a block statement

end Pack;
