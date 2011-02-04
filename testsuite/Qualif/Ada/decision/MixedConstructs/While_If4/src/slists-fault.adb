with Vbufs; use Vbufs;

package body Slists.Fault is

   procedure Control
     (SL : Sensor_List; Active_Only : Boolean;
      Skipped, Fault, Ok : out Sensor_List)
   is
      NA : Sensor_Node_Access; -- # AF_decl
   begin
      NA := SL.Head;                                  -- # AF_init
      while NA /= null loop                           -- # AF_while
         declare
            This : Sensor_Access renames Na.S;        -- # AF_ren
         begin
            if Active_Only and then not This.Active then -- # AF_evA
               Prepend (This, Skipped);                  -- # AF_skip
            elsif (This.V < This.ALB                     -- # AF_evLB
                     or else This.V > This.AHB)          -- # AF_evHB
            then
               Prepend (This, Fault);                    -- # AF_fault
            else
               Prepend (This, Ok);                       -- # AF_ok
            end if;
         end;
         NA := NA.Next;                               -- # AF_next
      end loop;
   end;

end;
