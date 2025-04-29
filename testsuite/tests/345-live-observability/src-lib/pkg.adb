with Ada.Text_IO; use Ada.Text_IO;

package body Pkg is

   procedure Foo (B : Boolean) is
   begin
      if B then                          --  if-stmt is 1 SCO
         Put_Line("B is True");          --  1 SCO, unless --block
         Put_Line("I swear it is True"); --  1 SCO
      else
         Put_Line("B is False :(");      --  1 SCO
      end if;
   end Foo;

   procedure Bar (A : Integer) is
      I : Integer := 155 + A; --  1 SCO, unless --block
   begin
      Put_Line (Integer'Image (I)); --  1 SCO
   end Bar;

end Pkg;
