pragma Ada_2012;

with Lib1;

procedure Main is

   procedure Increment (J : in out Integer)
   is
   begin
      J := J + 1;
   end Increment;

   I : Integer := 1;
begin
   Increment (I);
   I := I + Lib1.Foo;
   Increment (I);
end Main;
