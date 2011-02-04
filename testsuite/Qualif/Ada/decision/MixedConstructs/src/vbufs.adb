package body Vbufs is

   procedure Push (V : Value; VB : in out Vbuffer) is
   begin
      -- Shift right, store at VB'first, and adjust length

      for I in                                                 -- # PU_loop0
        reverse VB.Store'First .. VB.Store'First + VB.Len - 1  -- # PU_loop1
      loop
         if I + 1 < VB.Store'Last then           -- # PU_tshift
            VB.Store (I + 1) := VB.Store (i);    -- # PU_shift
         end if;
      end loop;

      VB.Store (VB.Store'First) := V;              -- # PU_update
      VB.Len := Integer'Min (VB.Len + 1, VB.Size); -- # PU_update
   end;

end;
