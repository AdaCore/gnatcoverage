with Lib;
with Interfaces.C; use Interfaces.C;

procedure Main is
   function Get_Foo_C return unsigned;
   pragma Import (C, Get_Foo_C, "get_foo");

   Bar   : constant Natural := Natural (Get_Foo_C);
   Dummy : constant Natural := Lib.Square (Bar);
begin
   null;
end;
