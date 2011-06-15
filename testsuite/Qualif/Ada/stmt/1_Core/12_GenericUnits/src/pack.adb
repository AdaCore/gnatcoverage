package body Pack is
   function New_Value_G (I : Int) return Int is
   begin
      return I + 1;                              -- # new_value_g
   end New_Value_G;

   package body Pack_G is
      procedure Swap (A, B : in out T) is
         Tmp : T := A;                           -- # swap
      begin
         A := B;                                 -- # swap
         B := Tmp;                               -- # swap
      end Swap;
   end Pack_G;

   procedure Proc (I, J : in out Integer) is
      package Pack_I is new Pack_G (Integer, 1);
   begin
      Pack_I.Swap (I, J);                        -- # proc
   end Proc;

end Pack;
