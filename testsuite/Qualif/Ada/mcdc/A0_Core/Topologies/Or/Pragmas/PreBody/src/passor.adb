
pragma Check_Policy (Precondition, On);

procedure Passor (A, B : Boolean) is
   pragma Precondition (A or else B); -- # eval :s/e:
begin
   null; -- # stmt
end;
