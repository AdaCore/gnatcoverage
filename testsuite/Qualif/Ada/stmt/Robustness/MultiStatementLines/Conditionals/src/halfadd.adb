
procedure Halfadd (A, B : Boolean; S, Carry : out Boolean) is
begin
   --  Straight + conditional statement on a single line here.

   S := A xor B; -- # sum
   if A and then B then Carry := True; else Carry := False; end if; -- # carry
end;
