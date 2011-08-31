pragma Check_Policy (Precondition, On);

package body Pval is

   function F (X : Boolean) return Boolean is
      pragma Precondition (not X); -- # eval :o/0:
   begin
      return not X; -- # returnVal
   end;
end;
