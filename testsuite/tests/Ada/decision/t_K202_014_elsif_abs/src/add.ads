package Add is
   type Time is mod 2 ** 64;
   for Time'Size use 64;

   type Time_Span is range -2 ** 63 .. 2 ** 63 - 1;
   for Time_Span'Size use 64;

   type Stamp is record
      Valid : Boolean;
      Value : Time;
   end record;

   function "+" (Left : Time; Right : Time_Span) return Stamp;
end Add;
