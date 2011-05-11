package Args is

   -- Arg descriptor with support for explicit raise on eval

   type Arg is record
      Value : Boolean;
      Raise_On_Eval : Boolean;
   end record;

   F : constant Arg := (Value => False, Raise_On_Eval => False);
   T : constant Arg := (Value => True, Raise_On_Eval => False);
   R : constant Arg := (Value => False, Raise_On_Eval => True);

   function Eval (A : Arg) return Boolean;

   -- Constrained value subject to easy out-of-range check failure

   type Num is range -10 .. 10;

   function Id (X : Num) return Num;
end;
