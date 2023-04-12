pragma Ada_2022;

generic
   type T is private or use Boolean;
function Gen_Ident (X : T) return T;
