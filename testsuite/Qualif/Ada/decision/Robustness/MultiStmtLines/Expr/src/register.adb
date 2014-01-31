procedure Register
  (Hit : Natural; Double, Triple : Boolean; G : in out Game)
is
   function Times (X, Factor : Integer; Cond : Boolean) return Natural;
   pragma No_Inline (Times);
   
   function Times (X, Factor : Integer; Cond : Boolean) return Natural is
      Cond_Latch : Boolean; -- # times
      pragma Volatile (Cond_Latch);  -- # times
   begin
      Cond_Latch := Cond; -- # times
      return X * Factor;  -- # times
   end;

   This_Score : Natural;
begin
   
   --  Here, the inner computations within if statements involve a complex
   --  decision as a subprogram actual, forcing code with column numbers. The
   --  expressions use here play no functional role.
  
   -- if/else; simple decision
   if Hit > 0 then This_Score := Hit; else This_Score := 0; End if; -- # init
   
   -- if/no-else; simple decision
   if Double then This_Score := Times(This_Score, 2, G.Score > 0 and then Hit > 0); end if; -- # double
   
   -- if/no-else; stmt; simple decision
   if Triple then This_Score := Times(This_Score, 3, G.Score > 0 or else Hit > 2); end if; G.Score := G.Score + This_Score; -- # triple
   
   -- stmt; if/no-else; complex decision
   G.Hits := G.Hits + 1; if Double or else Triple then G.Fancy_Hits := G.Fancy_Hits + 1; end if; -- # hits
end;

   
