pragma Ada_2012;
package UTF8 is
   function Id (V : Boolean) return Boolean is (V);
   function F (A, B : Boolean) return Boolean
   is (Id (-- ÉaÉ
           A) and then B);
end UTF8;
