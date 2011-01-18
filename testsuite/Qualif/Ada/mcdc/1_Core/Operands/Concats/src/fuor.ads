--  Provider of an or-else decision evaluator which features conditions
--  involving string concatenations

package FUOR is
   function Postfits_Or
     (S, Post1, Post2 : String; Max : Integer) return Boolean;
   --  Whether Op & Post1 or Op & Post2 remain <= Max in length
end;



