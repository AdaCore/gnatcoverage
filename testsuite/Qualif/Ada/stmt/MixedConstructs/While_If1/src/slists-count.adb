
package body Slists.Count is

   procedure Count_In
     (SL : Sensor_List;
      SP : SP_Access := Pass'Access;
      NT, NF : out Natural)
   is
      NA : Sensor_Node_Access; -- # CO_decl
   begin
      NA := SL.Head;             -- # CO_init
      NT := 0;                   -- # CO_init
      NF := 0;                   -- # CO_init
      while NA /= null loop      -- # CO_while
         if SP (Na.S.all) then   -- # CO_test
            NT := NT + 1;        -- # CO_incT
         else
            NF := NF + 1;        -- # CO_incF
         end if;
         NA := NA.Next;          -- # CO_next
      end loop;
   end;

end;
