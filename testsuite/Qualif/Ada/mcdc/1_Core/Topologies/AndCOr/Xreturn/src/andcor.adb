
package body AndCor is
   function Orelse (B, C : Boolean) return Boolean is
   begin
      return V : boolean := B or else C -- # orelse :o/e:
      do  null;                         -- # returnOr
      end return;
   end;

   function F (A, B, C : Boolean) return Boolean is
   begin
      return V : Boolean :=       -- # returnValue
        A and then Orelse (B, C)  -- # andthen :o/e:
      do null;                    -- # returnValue
      end return;
   end;
end;

