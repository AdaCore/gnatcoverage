package body Ms_Lines is
   procedure Eval_And_Not (A, B : Boolean; E : out Boolean) is
      Not_B : Boolean;
   begin
      --  Straight sequence of statements on a single line, without
      --  conditional control here.

      Not_B := not B; E := A and then not B;  -- # doAndNot
   end;

   procedure Eval_Half_Add (A, B : Boolean; S, Carry : out Boolean) is
   begin
      Carry := False;  -- # setCarry

      --  Straight + conditional statement on a single line here.

      S := A xor B; if A and then B then Carry := True; end if; -- # doHalfAdd
   end Eval_Half_Add;
end;
