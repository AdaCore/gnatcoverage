
package Trigger is
   type Opkind is (Read, Write);
   procedure Run (Op : Opkind);
end;
 
