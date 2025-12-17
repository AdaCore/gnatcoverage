pragma Extensions_Allowed (On);

with Ada.Text_IO; use Ada.Text_IO;

procedure Proc (A, B : Boolean) is
   M1 : constant String := "at least one is True";             -- # stmt
   M2 : constant String := "both are false";                   -- # stmt
begin
   Put_Line                                                    -- # stmt
     (f"A={A}, B={B}, so {(if A or else B then M1 else M2)}"); -- # decision
end Proc;
