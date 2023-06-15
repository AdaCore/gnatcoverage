with Services;

procedure Test_Services is
   type T1 is digits 1;
   package P1 is new Services (Number => T1);

   type T8 is digits 8;
   package P8 is new Services (number => T8);

   B1, B8 : Boolean;
begin
   B1 := P1.Has_Mid_Precision (1.0);
   B8 := P8.Has_Mid_Precision (2.0);
end;
