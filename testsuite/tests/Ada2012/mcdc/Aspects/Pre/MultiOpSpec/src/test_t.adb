with Support, Values; use Support, Values;

procedure Test_T is
begin
   Assert
     (Plus ((Value => 5, Valid => True),
            (Value => 7, Valid => True)) = 12
     );
end;

--# values.ads
--  /eval/ l! ## dF-
