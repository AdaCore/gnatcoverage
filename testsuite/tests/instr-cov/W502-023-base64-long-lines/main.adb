with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
begin
   for I in 1 .. 10_000 loop
      Put ((1 .. 1000 => 'A'));
   end loop;
end Main;
