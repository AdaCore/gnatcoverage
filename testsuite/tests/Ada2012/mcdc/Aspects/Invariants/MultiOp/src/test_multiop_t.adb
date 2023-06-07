with Multiop; use Multiop;

procedure Test_Multiop_T is
   O : Int (LB => 1, UB => 5);
begin
   Set (O, V => 3);
end;

--# multiop.ads
--  /eval/ l! ## dF-
