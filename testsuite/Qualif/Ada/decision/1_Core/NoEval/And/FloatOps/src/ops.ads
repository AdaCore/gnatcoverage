package Ops is
   
   type Constrained_Float is new Float range Float'Range;
   
   type Capsum is record
      X, Y, Cap : Constrained_Float;
   end record;
   
   Cs_Ok : constant Capsum := (X => 1.0, Y => 1.0, Cap => 3.0);
   Cs_Ko : constant Capsum := (X => 1.0, Y => 2.0, Cap => 1.0);
   Cs_Ov : constant Capsum := (X => Constrained_Float'Last,
			       Y => Constrained_Float'Last,
			       Cap => 3.0);
   
   N_Ok, N_Ko, N_Ov : Integer := 0;
   
   function Both_Ok (A, B : Capsum) return Boolean;
   
end;
