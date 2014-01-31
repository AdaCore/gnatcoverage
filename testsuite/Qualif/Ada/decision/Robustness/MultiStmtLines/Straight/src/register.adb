procedure Register
  (Hit : Natural; Double, Triple : Boolean; G : in out Game)
is
   function Times (X : Natural; Factor : Natural)  return Natural;
   pragma Noinline (Times);
   
   function Times (X : Natural; Factor : Natural)  return Natural
   is
   begin
      return X * Factor; -- # times
   End;
   
   This_Score : Natural;
begin
   
   --  Here, we aim at the simplest possible code for the inner sequence
   --  of statements in IFs, so are careful not to have checks or boolean
   --  expressions there.
   
   -- if/else; simple decision
   if Hit > 0 then This_Score := Hit; else This_Score := 0; End if; -- # init
      
   -- if/no-else; simple decision. No check nor expr eval 
   if Double then This_Score := Times (This_Score, 2); end if; -- # double
   
   -- if/no-else; stmt; simple decision
   if Triple then This_Score := Times (This_Score, 3); end if; G.Score := G.Score + This_Score; -- # triple
   
   -- stmt; if/no-else; complex decision
   G.Hits := G.Hits + 1; if Double or else Triple then G.Fancy_Hits := G.Fancy_Hits + 1; end if; -- # hits
end;
