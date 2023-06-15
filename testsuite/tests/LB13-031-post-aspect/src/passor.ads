pragma Ada_2012;
pragma Check_Policy (Post, On);
procedure Passor (A, B : Boolean)
  with Post => A or else B; -- # eval
