pragma Check_Policy (Precondition, On);

procedure Passand (A, B : Boolean);
pragma Precondition (A and then B); -- # pre
