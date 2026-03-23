with Support, Values; use Support;

package body Values_NRNF is

   type My_Range is range -100 .. -1;
   My_Factor : constant := -3;

   package My_Processor is new
     Values (Value_T => My_Range, Factor => My_Factor);

   procedure Check is
      X : My_Range := -12;
   begin
      Assert (My_Processor.F(X) = Integer(X-4) + My_Factor);
   end;

end;
