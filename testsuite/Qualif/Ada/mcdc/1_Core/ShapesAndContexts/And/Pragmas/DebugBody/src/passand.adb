with Support; use Support;

pragma Debug_Policy (Check);

procedure Passand (A, B : Boolean) is
begin
   pragma Debug (Assert (A and then B)); -- # eval
   null; -- # stmt
end;

