pragma Ada_2012;

package Pak is
   A, B : Boolean;
   function And_Then return Boolean is (A and then B);
end Pak;
