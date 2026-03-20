package Args is

   type Intval is new Integer;
   function Bool (X : Intval) return Boolean;
   --  False if X = 0, True if X = 1, Constraint_Error otherwise

   type Boolval is range 0 .. 1;
   function Bool (X : Boolval) return Boolean;
   --  False if X = 0, True if X = 1. Possible range check error
   --  on conversion to type.

   Bool_For : array (Integer range 0 .. 1) of Boolean
     := (0 => False, 1 => True);
   --  Possible index check failures on out of range accesses

end;
