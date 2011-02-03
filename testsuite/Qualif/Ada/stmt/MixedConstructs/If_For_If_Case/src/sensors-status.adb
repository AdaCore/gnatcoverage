
package body Sensors.Status is

   function Status_Of (S : Sensor) return Sensor_Status is
      Status : Sensor_Status;
   begin

      if S.History.Len = 0 then                          -- # SO_t0
         Status := Undecidable;                          -- # SO_undecide
      else
         Status := Ok;                                   -- # SO_decide

         for I in                                        -- # SO_loop0
           S.History.Store'First                         -- # SO_loop1
           .. S.History.Store'First + S.History.Len - 1  -- # SO_loop1
         loop
            if S.History.Store (I) < S.ALB               -- # SO_tfaultLB
              or else S.History.Store (I) > S.AHB        -- # SO_tfaultHB
            then
               case Status is                            -- # SO_fault
                  when Ok =>
                     Status := Check;                    -- # SO_check
                  when Check | Broken =>
                     Status := Broken;                   -- # SO_broken
                  when others =>
                     raise Program_Error;                -- # SO_PE
               end case;
            end if;
         end loop;
      end if;

      return Status; -- # SO_ret

   end;

end;

