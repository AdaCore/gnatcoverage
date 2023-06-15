procedure CheckX (X, Lb, Hb : Float);
pragma Precondition (Lb > 0.0 and then X >= Lb and then X <= Hb);
