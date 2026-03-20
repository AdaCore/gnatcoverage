pragma Ada_2012;

with Gen;  use Gen;
with Prim; use Prim;
with Rec;  use Rec;

with Support; use Support;

procedure Test_True is
   function Identity (X : Natural) return Natural is (X);
   function Nat_Sum is new Gen.Weight_Sum
     (Value_Type => Natural, Weight => Identity);
   function Nat_Both_Null is new Gen.Both_Weights_Null
     (Value_Type => Natural, Weight => Identity);
   Val    : Prim.T := (X => 0);
   Res_T  : Prim.T;
   Res_TT : Prim.TT;
begin
   Assert (Nat_Sum (1, 1) = 2);
   Assert (Nat_Both_Null (0, 0));
   Assert (Fact ((X => 1)) = 1);
   Assert (Or_Else (Val, False, True));
   Assert (And_Then (Val, True, True));
   Res_T := Make (True);
   Assert (Res_T.X = 1);
   Res_TT := Make (False);
   Assert (Res_TT.X = 3 and then Res_TT.Y = 4);
end Test_True;

--# rec.ads prim.ads prim.adb gen.ads
--
-- /expr_st/ l+ ## 0
-- /expr_dc/ l! ## oF-
