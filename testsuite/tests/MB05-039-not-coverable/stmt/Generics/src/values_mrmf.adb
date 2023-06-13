with Support, Values; use Support;

package body Values_MRMF is
   
   type My_P_Range is range 1 .. 100;
   My_P_Factor : constant := 3;
   
   package My_Processor_P is new
     Values (Value_T => My_P_Range, Factor => My_P_Factor);
      
   type My_N_Range is range -100 .. -1;
   My_N_Factor : constant := -3;
   
   package My_Processor_N is new
     Values (Value_T => My_N_Range, Factor => My_N_Factor);
      
   procedure Check is
      XP : My_P_Range := 12;
      XN : My_N_Range := -12;
   begin
      Assert (My_Processor_P.F(XP) = Integer(XP+1) + 2*My_P_Factor);
      Assert (My_Processor_N.F(XN) = Integer(XN-4) + My_N_Factor);
   end;
   
end;

