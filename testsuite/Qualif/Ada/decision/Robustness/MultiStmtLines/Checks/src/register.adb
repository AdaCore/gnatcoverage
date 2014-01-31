procedure Register
  (Hit : Natural; Double, Triple : Boolean; G : in out Game)
is
   This_Score : Natural;
begin
   
   --  Here, the inner computations within if statements might
   --  involve runtime checks.
   
   -- if/else; simple decision
   if Hit > 0 then This_Score := Hit; else This_Score := 0; End if; -- # init
   
   -- if/no-else; simple decision
   if Double then This_Score := This_Score * 2; end if; -- # double
   
   -- if/no-else; stmt; simple decision
   if Triple then This_Score := This_Score * 3; end if; G.Score := G.Score + This_Score; -- # triple
   
   -- stmt; if/no-else; complex decision
   G.Hits := G.Hits + 1; if Double or else Triple then G.Fancy_Hits := G.Fancy_Hits + 1; end if; -- # hits
end;


   
