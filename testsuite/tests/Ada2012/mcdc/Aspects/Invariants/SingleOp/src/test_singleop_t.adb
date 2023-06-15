with Singleop; use Singleop;

procedure Test_Singleop_T is
   O : Int (UB => 5);
begin
   Set (O, V => 3);
end;

--# singleop.ads
--  /eval/ l! ## dF-
