package body Add is
   type Uint_64 is mod 2 ** 64;

   function "+" (Left : Time; Right : Time_Span) return Stamp is
   begin
      if Right >= 0                                                 -- # tover0
        and then Uint_64(Time'Last)-Uint_64(Left) >= Uint_64(Right) -- # tover1
      then
         return (Valid => True,                               -- # retp0
           Value => Time (Uint_64 (Left) + Uint_64 (Right))); -- # retp1

      elsif Right < 0                         -- # tunder0
        and then Left >= Time (abs (Right))   -- # tunder1
      then
         return (Valid => True,                                     -- # retm0
           Value => Time (Uint_64 (Left) - Uint_64 (abs (Right)))); -- # retm1
      else
         return (Valid => False, Value => 0); -- # fault
      end if;
   end "+";
end Add;
