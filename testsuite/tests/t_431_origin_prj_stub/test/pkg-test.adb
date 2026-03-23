with Support;

procedure Pkg.Test is
begin
   --  We are using the stub version of Pkh, which always returns 0, so expect
   --  Pkg to return 0.

   Support.Assert (Pkg.Foo (3) = 0);
end Pkg.Test;
