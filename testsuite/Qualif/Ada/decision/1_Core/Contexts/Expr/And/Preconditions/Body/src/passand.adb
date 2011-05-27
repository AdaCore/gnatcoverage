
pragma Check_Policy (Precondition, On);

procedure Passand (A, B : Boolean) is
   pragma Precondition (A and then B); -- # eval
begin
   null; -- # stmt
end;

