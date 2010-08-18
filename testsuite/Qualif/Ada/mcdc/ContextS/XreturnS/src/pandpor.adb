package body PandPor is
   function F (A, B, C : Boolean) return Boolean is
   begin
      return V : boolean := (A and then B) or else C do -- # evaluate
        null; -- # returnValue
      end return;
   end;
end;

