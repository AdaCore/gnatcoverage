pragma Ada_2012;
package Latin1 is
   function Id (V : Boolean) return Boolean is (V);
   function F (A, B : Boolean) return Boolean
   is (Id (-- …a…
           A) and then B);
end Latin1;
