package Ops is
   
   type T_Action is (Hold, Step);
   type T_What_Ahead is (Ground, Pit);
   
   type T_Safety_Status is (Safe, Unsafe);
   
   function Qualify (A : T_Action; W : T_What_Ahead) return T_Safety_Status;
end;
