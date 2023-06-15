with Ada.Text_IO; use Ada.Text_IO;

procedure Foo
  (A : Boolean;
   B : Boolean;
   C : Boolean;
   D : Boolean) is
begin
   if A and then (B or else
                    (C and then D))
   then
      Put_Line
        ("Hello world!");
   end if;
end Foo;
