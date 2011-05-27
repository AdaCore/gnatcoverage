with Support; use Support;

pragma Debug_Policy (Check);

procedure Passor (A, B : Boolean) is
begin
   pragma Debug (Assert (A or else B)); -- # eval
   null; -- # stmt
end;

