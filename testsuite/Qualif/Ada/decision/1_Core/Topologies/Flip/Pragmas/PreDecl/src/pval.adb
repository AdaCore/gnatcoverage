package body Pval is

   function F (X : Boolean) return Boolean is
   begin
      return not X; -- # returnVal
   end;
end;
