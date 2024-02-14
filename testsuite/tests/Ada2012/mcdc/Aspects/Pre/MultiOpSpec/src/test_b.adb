with Support, Values; use Support, Values;
with Silent_Last_Chance;

procedure Test_B is
   X : Integer;
begin
   Assert
     (Plus ((Value => 5, Valid => True),
            (Value => 7, Valid => True)) = 12
     );
   X := Plus ((Value => 5, Valid => False), (Value => -7, Valid => True));
exception
   when others => null;
end;

--# values.ads
--  /eval/ l! ## c!:"B.Valid"
