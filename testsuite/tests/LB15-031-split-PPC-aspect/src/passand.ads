pragma Ada_2012;
pragma Check_Policy (Pre, On);
procedure Passand (A, B : Boolean)
  with Pre => A and then B; -- # eval
