pragma Ada_2012;

with Pkg; use Pkg;

procedure Make_Call                                            -- # test
is
   type Pt_Acc is access function (X, Y : Float) return Point; -- # decl

   function Run_Callback (Callback : Pt_Acc) return Point is   -- # fun
   begin
      return Callback (1.0, 2.00);                             -- # call_0
   end;

   Set_Acc : constant Pt_Acc := Set'Access;                    -- # decl

   --  LIMITATION: Selected component with a call expression for a prefix
   Dummy : Float := Run_Callback (Set_Acc).X;                  -- # call_1
begin
   null;                                                       -- # null
end Make_Call;
