with Support; use Support;

procedure Passor (A, B : Boolean) is
   pragma Debug_Policy (Check); -- # decl
begin
   pragma Debug (Assert (A or else B)); -- # eval
   null; -- # stmt
end;

