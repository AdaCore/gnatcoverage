pragma Extensions_Allowed (On);

package Example is
   type Root is tagged null record;
   procedure P (V : Integer; V2 : Root);

   type Child is new Root with null record
     with First_Controlling_Parameter;

   overriding
   procedure P (V : Integer; V2 : Child);

   procedure P2 (V : Integer; V2 : Child);

   function F return Child is ((null record));  -- # f1

   function F2 (V : Child) return Child is ((null record));  -- # f2

end Example;
