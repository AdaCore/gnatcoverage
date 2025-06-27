pragma Ada_2012;

with Ada.Text_IO; use Ada.Text_IO;
with Pref; use Pref;

procedure Main_With_Pragma is
   procedure Foo (B: Boolean) is
      BB : Preference := Preference(B);
   begin
      if B then
         Put_Line ("Coucou");
      elsif Get_Pref (BB) then
         Put_Line ("test");
      end if;
   end Foo;
begin
    Put_Line ("Hello World");
    Foo (True);
end Main_With_Pragma;
