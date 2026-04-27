with Ada.Text_IO; use Ada.Text_IO;

with Params_A;
with Pkg;       use Pkg;
with Pkg.Child; use Pkg.Child;
with Pkg.Gen;

procedure Main is
   package G is new Pkg.Gen;
   Dummy : Boolean := Pkg.Opposite (True);
begin
   Put_Line ("Fact (0) = " & Integer'Image (Pkg.Fact (0)));
   Pkg.Do_Nothing_2;
   Pkg.Child.Do_Nothing_3;
   Dummy := G.Identity (Dummy);
   Put_Line (Boolean'Image (Params_A.Param_1));
end Main;
