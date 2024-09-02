pragma Ada_2012;

With Pkg; use Pkg;

procedure Make_Call
is
   Create_Acc : constant Pt_Acc := Create'Access; -- # decl

   A : Float       := Run_Callback (Create_Acc)   -- # A1
                     .X;                          -- # A2

   B : Float       := Run_Callback (Create_Acc)   -- # B1
                     .Get_X;                      -- # B2

   C : Point'Class := Id                          -- # C1
                     (Run_Callback (Create_Acc)); -- # C2

   D : Float       := Id                          -- # D1
                     (Run_Callback (Create_Acc)   -- # D2
                     .Get_X);                     -- # D3

   E : Float       := Id                          -- # E1
                     (Run_Callback (Create_Acc))  -- # E2
                     .Id                          -- # E3
                     .Get_X;                      -- # E4

   F : Float       := Point'(1.0, 2.0)            -- # F1
                     .Id                          -- # F2
                     .Get_X;                      -- # F3
begin
   null;                                          -- # stmt
end Make_Call;
