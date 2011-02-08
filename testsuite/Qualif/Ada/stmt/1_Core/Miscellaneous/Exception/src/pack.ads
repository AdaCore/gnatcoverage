package Pack is
   type My_Int is range -100 .. 100;

   My_Exception : exception;

   procedure Proc1 (I : in out My_Int);
   function Fun1 (I : My_Int) return My_Int;
   --  Explicit raise of user-defined exception in a subprogram body

   procedure Proc2 (I : in out My_Int);
   function Fun2 (I : My_Int) return My_Int;
   --  Explicit raise of predefined Constraint_Error

   procedure Proc3 (I : in out My_Int);
   function Fun3 (I : My_Int) return My_Int;
   --  Exception raise in a block statement

end Pack;
