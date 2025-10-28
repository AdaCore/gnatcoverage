pragma Ada_2012;

with Pkg; use Pkg;

procedure Make_Call (Call_Number : Integer)                    -- # test
is
   type Pt_Acc is access function (X, Y : Float) return Point; -- # decl

   function Run_Callback (Callback : Pt_Acc) return Point is   -- # fun
   begin
      return Callback (1.0, 2.00);                             -- # call_0
   end;

   Set_Acc   : constant Pt_Acc := Set'Access;                  -- # decl
   Reset_Acc : constant Pt_Acc := Reset'Access;                -- # decl

   Dispatch_Table : constant array (1 .. 2) of Pt_Acc :=       -- # arr_d
     (Set_Acc, Reset_Acc);                                     -- # arr_i

   Dummy : Point;                                              -- # decl
begin
   case Call_Number is                                         -- # case_0

      --  Indirect call through calling another subprogram
      when 1 => Dummy := Run_Callback (Set_Acc);               -- # call_1

      --  Call made by dereferencing the access to the subprogram
      when 2 => Dummy := Set_Acc.all (1.0, 2.0);               -- # call_2

      --  Call through an access value as an array element
      when 3 => Dummy := Dispatch_Table (2) (1.0, 3.0);        -- # call_3

      when others => null;                                     -- # n_case

   end case;
end Make_Call;
