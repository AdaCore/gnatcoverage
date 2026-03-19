procedure CheckI (I, Lb, Hb : Integer);
pragma Precondition (Lb > 0 and then I >= Lb and then I <= Hb);
