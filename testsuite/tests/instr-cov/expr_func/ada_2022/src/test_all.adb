pragma Ada_2012;

with Gen;  use Gen;
with Prim; use Prim;
with Rec;  use Rec;

with Support; use Support;

procedure Test_All is
   function Identity (X : Natural) return Natural is (X);
   function Nat_Sum is new Gen.Weight_Sum
     (Value_Type => Natural, Weight => Identity);
   function Nat_Both_Null is new Gen.Both_Weights_Null
     (Value_Type => Natural, Weight => Identity);
   Val    : Prim.T := (X => 0);
   Res_T  : Prim.T;
   Res_TT : Prim.TT;
begin
   --  Ensure we get full coverage
   Assert (Nat_Sum (1, 1) = 2);
   Assert (Nat_Both_Null (0, 0));
   Assert (not Nat_Both_Null (0, 1));
   Assert (not Nat_Both_Null (1, 0));
   Assert (Fact ((X => 3)) = 6);
   Assert (And_Then (Val, True, True));
   Assert (not And_Then (Val, True, False));
   Assert (not And_Then (Val, False, True));
   Assert (not Or_Else (Val, False, False));
   Assert (Or_Else (Val, True, False));
   Assert (Or_Else (Val, False, True));
   Res_T := Make (True);
   Assert (Res_T.X = 1);
   Res_T := Make (False);
   Assert (Res_T.X = 2);
   Res_TT := Make (False);
   Assert (Res_TT.X = 3 and then Res_TT.Y = 4);
end Test_All;

--# rec.ads prim.ads prim.adb gen.ads
--
-- /expr/ l+ ## 0
