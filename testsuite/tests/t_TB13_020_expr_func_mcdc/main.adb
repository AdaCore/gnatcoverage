pragma Ada_2012;

with Pkg;

procedure Main is
   Dummy : Integer := (if Pkg.Flag then 1 else 2);
   pragma Volatile (Dummy);

   function Is_Null (Self : Pkg.Point) return Boolean is
     (Pkg.Is_Null (Self.X) and then Pkg.Is_Null (Self.Y));

   P1 : constant Pkg.Point := (0, 0);
   P2 : constant Pkg.Point := (0, 1);
begin

   if not Is_Null (P1) then
      raise Program_Error;
   end if;

   if Is_Null (P2) then
      raise Program_Error;
   end if;

end Main;
