pragma Check_Policy (Precondition, On);

package Pval is
   function F (X : Boolean) return Boolean;
   pragma Precondition (not X); -- # eval :o/0:
end;

