pragma Ada_2005;

with Pkg; use Pkg;

procedure Main is
   Arg    : aliased Arg_Type (1);
   Object : T;
begin
   Object.P1 (Arg);
   Object.P2 (Arg'Access);
end Main;
