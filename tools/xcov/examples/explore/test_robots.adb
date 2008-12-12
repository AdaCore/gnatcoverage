
with Robots, Robot_Controls; use Robots, Robot_Controls;

procedure Test_Robots is
   R : Robot_Access := new Robot;
begin
   Init (R);
   Robot_Control_Links.Push (Nop, Robot_Control_Inport (R.all));
   Robot_Control_Links.Push (Step_Forward, Robot_Control_Inport (R.all));
   Robot_Control_Links.Push (Nop, Robot_Control_Inport (R.all));
end;
