with Pkg;

procedure Main is
   Dummy : Integer :=
      Pkg.Compute (True, False) + Pkg.Compute_In_Stub (False, True);
begin
   null;
end Main;
