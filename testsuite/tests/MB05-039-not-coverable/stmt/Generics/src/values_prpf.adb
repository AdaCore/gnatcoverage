with Support, Values; use Support;

package body Values_PRPF is

   type My_Range is range 1 .. 100;
   My_Factor : constant := 3;

   package My_Processor is new
     Values (Value_T => My_Range, Factor => My_Factor);

   procedure Check is
      X : My_Range := 12;
   begin
      Assert (My_Processor.F(X) = Integer(X+1) + 2*My_Factor);
   end;

end;
