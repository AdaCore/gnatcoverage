
procedure Halfadd (A, B : Boolean; S, Carry : out Boolean) is
begin
   --  Straight + conditional statement on a single line here.

   S := (A or else B) and then not (A and then B); -- # sum
   if A and then B then Carry := True; else Carry := False; end if; -- # carry
end;
