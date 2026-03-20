with Aspect; use Aspect;

procedure Foo is

   Dummy : Foo_Type := Foo'Access; -- # d
begin
   null;
end;
