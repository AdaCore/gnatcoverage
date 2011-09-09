function Sort (X, Min, Max : Integer) return Integer is
begin
   if X < Min then     -- # testmin
      return -1;       -- # ltmin
   elsif X > Max then  -- # testmax
      return 1;        -- # gtmax
   else
      return 0;        -- # inrange
   end if;
end;


