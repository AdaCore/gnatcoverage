generic
   Size : Natural;
package Vectors is
   
   type Vector is array (1 .. Size) of Integer;
   
   type Opkind is (Reset, Bump);
   
   procedure Apply (Op : Opkind; V : in out Vector);
end;
