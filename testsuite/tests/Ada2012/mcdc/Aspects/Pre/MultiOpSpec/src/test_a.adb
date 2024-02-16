with Support, Values; use Support, Values;
with Silent_Last_Chance;

procedure Test_A is
   X : Integer;
begin
   Assert
     (Plus ((Value => 5, Valid => True),
            (Value => 7, Valid => True)) = 12
     );
   X := Plus ((Value => 5, Valid => True), (Value => -7, Valid => False));
exception
   when others => null;
end;

--# values.ads
--  /eval/ l! ## c!:"A.Valid"
