--  Provider of an and-then decision evaluator which features conditions
--  involving integer modulo computations

package FUAND is
   type Modop is record
      X, Y : Integer;
   end record;

   function Mod0_And (OpA, OpB : Modop) return Boolean;
   --  Whether OpA.X mod OpA.Y and OpB.X mod OpB.Y are null
end;



