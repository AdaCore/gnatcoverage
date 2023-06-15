with Pkg; use Pkg;

procedure Main is
   X : Integer := 4;
   pragma Volatile (X);
begin
   X := Ident (X);
end Main;