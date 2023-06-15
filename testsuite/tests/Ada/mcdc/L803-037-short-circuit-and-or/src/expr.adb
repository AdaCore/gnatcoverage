pragma Short_Circuit_And_Or;

package body Expr is
   
   function Bool_And (A, B : Boolean) return Boolean is
   begin
      return A and B; -- # lone-and
   end;
   
   function Bool_Or (A, B : Boolean) return Boolean is
   begin
      return A or B; -- # lone-or
   end;
   
   function Bool_And_Or (A, B, C : Boolean) return Boolean is
   begin
      return (A and B) or C; -- # and-or
   end;
   
end;

