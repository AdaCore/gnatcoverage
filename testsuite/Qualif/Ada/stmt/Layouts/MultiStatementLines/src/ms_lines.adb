package body Ms_Lines is
   procedure Eval_And_Not (A, B : Boolean; E : out Boolean) is
      Not_B : Boolean;
   begin
      Not_B := not B; E := A and then not B;  -- # Partial
   end;

   procedure Eval_Half_Add (A, B : Boolean; S, Carry : out Boolean) is
   begin
      Carry := False;  -- # setCarry
      S := A xor B; if A and then B then Carry := True; end if; -- # Partial
   end Eval_Half_Add;
end;
