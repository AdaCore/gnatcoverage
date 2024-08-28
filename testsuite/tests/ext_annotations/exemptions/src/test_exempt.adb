with Pkg;

with Support;

with Interfaces.C;

procedure Test_Exempt is
   use type Interfaces.C.int;
   X : aliased Boolean := True;
   Y : aliased Boolean := False;
   X_Acc : Pkg.Bool_Acc := X'Unchecked_Access;
   Y_Acc : Pkg.Bool_Acc := Y'Unchecked_Access;
   function Ident (X : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Ident, "ident");
begin
   Pkg.Swap (X_Acc, Y_Acc);
   Support.Assert (Y and then not X);
   Support.Assert (Ident (1) = 1);
end Test_Exempt;

--# pkg.adb
--
-- /exempt_1/   l* ## x+
-- /exempt_1_d/ l= ## XoT-
-- /exempt_1_s/ l= ## Xs-
-- /exempt_2/   l* ## x+
-- /exempt_2_d/ l= ## XoT-
-- /exempt_2_s/ l= ## Xs-
-- /ok/         l+ ## 0

--# foo.h
--
-- /exempt_h/   l* ## x+
-- /exempt_h_d/ l= ## XoT-
-- /exempt_h_s/ l= ## Xs-
-- /ok/         l+ ## 0

--# foo.c
--
-- /exempt_c/   l* ## x+
-- /exempt_c_d/ l= ## XoT-
-- /exempt_c_s/ l= ## Xs-
-- /ok/         l+ ## 0
