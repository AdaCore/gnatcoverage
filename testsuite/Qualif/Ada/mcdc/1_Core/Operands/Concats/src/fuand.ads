--  Provider of an and-then decision evaluator which features conditions
--  involving string concatenations

package FUAND is
   function Postfits_And
     (S, Post1, Post2 : String; Max : Integer) return Boolean;
   --  Whether Op & Post1 and Op & Post2 remain <= Max in length
end;

