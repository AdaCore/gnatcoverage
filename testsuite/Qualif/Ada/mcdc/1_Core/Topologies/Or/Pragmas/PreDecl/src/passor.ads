pragma Check_Policy (Precondition, On);

procedure Passor (A, B : Boolean);
pragma Precondition (A or else B); -- # eval
