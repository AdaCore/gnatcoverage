package body Vops is
   
   procedure Inc (V : in out Vector_Type; Amount : Integer) is
   begin
      for I in V'Range loop     -- # inc
         V(I) := V(I) + Amount; -- # inc
      end loop;
   end;
   
   procedure Mult (V : in out Vector_Type; Amount : Integer) is
   begin
      for I in V'Range loop     -- # mult
         V(I) := V(I) * Amount; -- # mult
      end loop;
   end;
   
end;
