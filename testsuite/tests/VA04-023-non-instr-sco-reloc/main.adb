with Pkg_1;
with Pkg_2;

procedure Main is
   function Incr is new Pkg_1.Generic_Incr;
   function Decr is new Pkg_2.Generic_Decr;
   Dummy : constant Integer := Decr (Incr (1));
begin
   null;
end Main;
