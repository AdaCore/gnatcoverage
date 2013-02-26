with Support; use Support;

procedure Passand (A, B : Boolean) is
   pragma Debug_Policy (Check); -- # decl
begin
   pragma Debug (Assert (A and then B)); -- # eval
   null; -- # stmt
end;

