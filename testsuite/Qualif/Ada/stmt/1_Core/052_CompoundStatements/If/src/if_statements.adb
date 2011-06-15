package body IF_Statements is

   function In_Range (X, Min, Max : Integer) return Boolean is
   begin
      if X < Min then     -- # XcmpMin
         return False;    -- # XoutMin
      elsif X > Max then  -- # XcmpMax
         return False;    -- # XoutMax
      else
         return True;     -- # Xin
      end if;
   end In_Range;

   procedure Set_Max
     (Res        : out T;
      Arg1, Arg2 :     T)
   is
   begin
      Res := Arg1;         -- # setmax

      if Arg2 > Res then   -- # setmax
         Res := Arg2;      -- # inifsetmax
      end if;
   end Set_Max;

end IF_Statements;

