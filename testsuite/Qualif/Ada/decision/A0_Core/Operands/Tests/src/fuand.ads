package FUAND is

   type XYrange is record
      X, Y  : Integer;
   end record;

   function Eval (R1, R2 : XYrange) return Boolean;

   -- Whether R2 overlaps R1, if
   -- R2 starts before end of R1 and then R2 ends past start of R1


   R1 : XYrange := (X => 2, Y => 6);

   R2_FX : XYrange := (X => 7, Y => 9);
   R2_TF  : XYrange := (X => 0, Y => 1);

   R2_TT : XYrange := (X => 3, Y => 12);
end;
