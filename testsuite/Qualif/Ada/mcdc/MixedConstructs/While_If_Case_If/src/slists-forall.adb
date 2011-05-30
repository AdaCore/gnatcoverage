with Vbufs; use Vbufs;

package body Slists.Forall is

   procedure Forall_In
     (SL : Sensor_List; Op : Sensor_Op; Active_Only : Boolean := False)
   is
      NA : Sensor_Node_Access;
   begin
      NA := SL.Head;         -- # FA_init
      while NA /= null loop  -- # FA_while
         if Na.S.Active or else not Active_Only then -- # FA_tactive
            case Op is                        -- # FA_case
               when Activate =>
                  Na.S.Active := True;        -- # FA_activate
               when Inhibit =>
                  if Na.S.V < Na.S.ALB        -- # FA_tinhibitLB
                    or else Na.S.V > Na.S.AHB -- # FA_tinhibitHB
                  then
                     Na.S.Active := False;    -- # FA_inhibit
                  end if;
            end case;
         end if;
         NA := NA.Next;  -- # FA_next
      end loop;
   end;

end;

