
procedure Passand (A, B : Boolean) is
   pragma Precondition (A and then B); -- # pre
begin
   null; -- # stmt
end;

