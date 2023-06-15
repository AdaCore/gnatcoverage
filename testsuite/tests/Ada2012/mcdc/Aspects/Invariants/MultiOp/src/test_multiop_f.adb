with Multiop; use Multiop;
with Silent_Last_Chance;

procedure Test_Multiop_F is
   O : Int (LB => 1, UB => 5);
begin
   Set (O, V => 45);
exception
   when others => null;
end;

--# multiop.ads
--  /eval/ l! ## c!:"Int.Value >= Int.LB"
