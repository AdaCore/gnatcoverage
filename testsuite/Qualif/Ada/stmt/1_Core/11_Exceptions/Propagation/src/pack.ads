package Pack is
   type My_Int is range -100 .. 100;

   Exception_1 : exception;
   Exception_2 : exception;

   procedure Proc_With_Blocks (I, J, K : in out My_Int);
   --  Contains nested blocks in the body
end Pack;
