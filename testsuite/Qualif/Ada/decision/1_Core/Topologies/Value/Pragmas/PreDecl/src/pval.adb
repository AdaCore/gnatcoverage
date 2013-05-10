pragma Check_Policy (Precondition, On);

package body Pval is

   function F (X : Boolean) return Boolean is
   begin
      return X; -- # returnVal
   end;
end;
