--  Provider of an and-then decision evaluator which features conditions
--  involving attribute references

package FUAND is

   type String_Access is access all String;
   type Keys is record
      A, B : String_Access;
   end record;

   function Poslen_And_Eql (Ops : Keys) return Boolean;
   --  Whether Ops.A'Length is positive and the same as Ops.B'Length
end;



