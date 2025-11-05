pragma Ada_2012;

package body Pkg is

   function Run_Callback (Callback : Pt_Acc) return Point is   -- # fun
   begin
      return Callback (1.0, 2.00);                             -- # call
   end;
end Pkg;
