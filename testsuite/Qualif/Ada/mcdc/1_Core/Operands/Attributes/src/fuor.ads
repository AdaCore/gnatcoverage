--  Provider of an or-else decision evaluator which features conditions
--  involving attribute references

package FUOR is

   type String_Access is access all String;
   type Keys is record
      A, B : String_Access;
   end record;

   function Empty_Or_Eql (Ops : Keys) return Boolean;
   --  Whether Ops.A'Length is null or the same as Ops.B'Length
end;



