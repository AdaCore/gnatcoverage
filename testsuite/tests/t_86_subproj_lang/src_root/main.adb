with Procedure_Under_Test; use Procedure_Under_Test;

procedure Main is
begin
   Test (Control => 1, In_A => 1, In_B => 2);
   Test (Control => -11, In_A => 1, In_B => 2);
end Main;
